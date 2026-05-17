# Tests for spec_parser.mdparsing

# SPDX-License-Identifier: Apache-2.0

from __future__ import annotations

import logging
from pathlib import Path
from textwrap import dedent

import pytest

from spec_parser.mdparsing import ContentSection, NestedListSection, SingleListSection, SpecFile


class TestSpecFile:
    def test_loads_valid_file(self, fixture_model_path: Path) -> None:
        sf = SpecFile(fixture_model_path / "Core" / "Core.md")
        assert sf.name == "Core"
        assert sf.license == "Community-Spec-1.0"
        assert "Summary" in sf.sections
        assert "Metadata" in sf.sections

    def test_frontmatter_populated(self, fixture_model_path: Path) -> None:
        sf = SpecFile(fixture_model_path / "Core" / "Core.md")
        assert sf.frontmatter.get("SPDX-License-Identifier") == "Community-Spec-1.0"

    def test_missing_frontmatter_logs_error(self, tmp_path: Path, caplog: pytest.LogCaptureFixture) -> None:
        f = tmp_path / "Bad.md"
        f.write_text("# Bad\n\n## Summary\n\nSome summary.\n\n## Metadata\n\n- name: Bad\n")
        with caplog.at_level(logging.ERROR, logger="spec_parser.mdparsing"):
            SpecFile(f)
        assert any("frontmatter" in msg.lower() for msg in caplog.messages)

    def test_unparseable_section_logs_error(self, tmp_path: Path, caplog: pytest.LogCaptureFixture) -> None:
        f = tmp_path / "Broken.md"
        # A section heading with no content body cannot satisfy the fullmatch.
        f.write_text("---\nSPDX-License-Identifier: Apache-2.0\n---\n\n# Broken\n\n## EmptySection\n\n")
        with caplog.at_level(logging.ERROR, logger="spec_parser.mdparsing"):
            SpecFile(f)
        assert any("cannot parse" in msg for msg in caplog.messages)

    def test_loads_class_file(self, fixture_model_path: Path) -> None:
        sf = SpecFile(fixture_model_path / "Core" / "Classes" / "Tool.md")
        assert sf.name == "Tool"
        assert "Properties" in sf.sections
        assert "Metadata" in sf.sections


class TestFrontmatter:
    def _write(self, tmp_path: Path, content: str) -> SpecFile:
        f = tmp_path / "Test.md"
        f.write_text(content)
        return SpecFile(f)

    def test_frontmatter_license_parsed(self, tmp_path: Path) -> None:
        sf = self._write(tmp_path, dedent("""\
            ---
            SPDX-License-Identifier: Apache-2.0
            ---

            # Test

            ## Metadata

            - name: Test
        """))
        assert sf.license == "Apache-2.0"
        assert sf.frontmatter["SPDX-License-Identifier"] == "Apache-2.0"

    def test_frontmatter_name_and_sections_parsed(self, tmp_path: Path) -> None:
        sf = self._write(tmp_path, dedent("""\
            ---
            SPDX-License-Identifier: Apache-2.0
            ---

            # MyThing

            ## Summary

            A summary.

            ## Metadata

            - name: MyThing
        """))
        assert sf.name == "MyThing"
        assert "Summary" in sf.sections
        assert "Metadata" in sf.sections

    def test_frontmatter_extra_keys_stored(self, tmp_path: Path) -> None:
        sf = self._write(tmp_path, dedent("""\
            ---
            SPDX-License-Identifier: Apache-2.0
            version: 1.2.3
            author: Test Suite
            ---

            # X

            ## Metadata

            - name: X
        """))
        assert sf.frontmatter.get("version") == "1.2.3"
        assert sf.frontmatter.get("author") == "Test Suite"

    def test_frontmatter_lone_spdx_line_treated_as_text(self, tmp_path: Path) -> None:
        sf = self._write(tmp_path, dedent("""\
            ---
            SPDX-License-Identifier: Apache-2.0
            ---

            # X

            ## Summary

            SPDX-License-Identifier: something-else

            ## Metadata

            - name: X
        """))
        assert sf.license == "Apache-2.0"
        assert "SPDX-License-Identifier: something-else" in sf.sections.get("Summary", "")

    def test_frontmatter_missing_license_logs_error(self, tmp_path: Path, caplog: pytest.LogCaptureFixture) -> None:
        f = tmp_path / "NoLicense.md"
        f.write_text("---\nauthor: Someone\n---\n\n# X\n\n## Metadata\n\n- name: X\n")
        with caplog.at_level(logging.ERROR, logger="spec_parser.mdparsing"):
            SpecFile(f)
        assert any("license" in msg.lower() for msg in caplog.messages)


class TestContentSection:
    def test_preserves_content(self) -> None:
        s = ContentSection("Hello world.\n\nSecond paragraph.")
        assert s.content == "Hello world.\n\nSecond paragraph."

    def test_empty_content(self) -> None:
        s = ContentSection("")
        assert s.content == ""

    def test_none_content(self) -> None:
        s = ContentSection(None)
        assert s.content == ""


class TestSingleListSection:
    def test_parses_key_value(self) -> None:
        s = SingleListSection("- name: MyClass\n- Instantiability: Concrete")
        assert s.kv == {"name": "MyClass", "Instantiability": "Concrete"}

    def test_logs_error_for_bad_content(self, caplog: pytest.LogCaptureFixture) -> None:
        with caplog.at_level(logging.ERROR, logger="spec_parser.mdparsing"):
            SingleListSection("this is not a list", filename="test.md", context="metadata")
        assert caplog.messages

    def test_strips_whitespace_from_value(self) -> None:
        s = SingleListSection("- key: value with spaces")
        assert s.kv["key"] == "value with spaces"

    def test_values_are_strings(self) -> None:
        s = SingleListSection("- count: 42\n- flag: true")
        assert s.kv["count"] == "42"
        assert s.kv["flag"] == "true"

    def test_backtick_wrapping_stripped(self) -> None:
        s = SingleListSection("- pattern: `^(0|[1-9][0-9]*).*$`")
        assert s.kv["pattern"] == "^(0|[1-9][0-9]*).*$"

    def test_backtick_already_absent_leaves_value(self) -> None:
        s = SingleListSection("- name: SemVer")
        assert s.kv["name"] == "SemVer"

    def test_double_backtick_wrapping_with_inner_backtick(self) -> None:
        s = SingleListSection("- pattern: ``has`backtick``")
        assert s.kv["pattern"] == "has`backtick"


class TestNestedListSection:
    def test_parses_nested(self) -> None:
        content = dedent("""\
            - propName:
              - minCount: 1
        """)
        s = NestedListSection(content)
        assert "propName" in s.ikv
        assert s.ikv["propName"]["minCount"] == "1"

    def test_multiple_properties(self) -> None:
        content = dedent("""\
            - alpha:
              - minCount: 0
            - beta:
              - minCount: 1
              - maxCount: 3
        """)
        s = NestedListSection(content)
        assert set(s.ikv.keys()) == {"alpha", "beta"}
        assert s.ikv["beta"]["minCount"] == "1"
        assert s.ikv["beta"]["maxCount"] == "3"

    def test_cross_namespace_property(self) -> None:
        content = dedent("""\
            - /Core/name:
              - minCount: 0
              - maxCount: 1
        """)
        s = NestedListSection(content)
        assert "/Core/name" in s.ikv
        assert s.ikv["/Core/name"]["maxCount"] == "1"

    def test_values_are_strings(self) -> None:
        content = dedent("""\
            - prop:
              - minCount: 0
              - maxCount: 1
        """)
        s = NestedListSection(content)
        assert isinstance(s.ikv["prop"]["minCount"], str)
        assert isinstance(s.ikv["prop"]["maxCount"], str)
