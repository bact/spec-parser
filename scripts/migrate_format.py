#!/usr/bin/env python3
# SPDX-License-Identifier: Apache-2.0
"""Migrate spec Markdown files from old format to new format.

Old format
----------
The file starts with a bare ``SPDX-License-Identifier:`` line, and
Properties / External-properties-restrictions sections use a two-level list
where the top-level item is a plain identifier::

    SPDX-License-Identifier: Community-Spec-1.0

    # ClassName

    ## Properties

    - propName
      - type: xsd:string
      - minCount: 1
      - maxCount: 1

New format
----------
The license is in a YAML frontmatter block.  Properties use the item name as
a YAML mapping key so the nesting is both valid YAML and renders as a proper
Markdown nested list.  The ``type:`` attribute is removed (redundant; type
information lives in the property definition file's ``Range:`` field)::

    ---
    SPDX-License-Identifier: Community-Spec-1.0
    ---

    # ClassName

    ## Properties

    - propName:
      - minCount: 1
      - maxCount: 1

Usage
-----
    python scripts/migrate_format.py <path-to-model-dir>

The script modifies files in-place and prints each changed path.  Run on a
working copy or inside a git repository so the diff can be reviewed.
"""

from __future__ import annotations

import re
import sys
from pathlib import Path

RE_SPDX_LINE = re.compile(r"^SPDX-License-Identifier:\s+(.+?)\s*$")
RE_SECTION_HEADER = re.compile(r"^## (.+)$")
# Top-level nested-list item: `- identifier` with no `: value` part.
RE_NESTED_TOP = re.compile(r"^(-\s+)([\w/]+)\s*$")
# Indented type attribute to remove.
RE_TYPE_ATTR = re.compile(r"^  -\s+type:\s+.+$")
# Indented attribute value that needs `*` quoted (YAML alias indicator).
RE_STAR_VALUE = re.compile(r"^(  -\s+\w+:\s+)\*(\s*)$")

NESTED_SECTIONS = {"Properties", "External properties restrictions"}
# Sections whose values may contain Markdown-special chars (e.g. * in regex) and need backtick quoting.
BACKTICK_SECTIONS = {"Format"}
RE_KV_LINE = re.compile(r"^(-\s+\w[\w-]*:\s+)(.+)$")


def _migrate_lines(lines: list[str]) -> list[str] | None:
    """Return migrated lines, or *None* if the file is already new-format."""
    if not lines:
        return None

    first = lines[0].rstrip("\r\n")
    m = RE_SPDX_LINE.match(first)
    if not m:
        return None  # no bare SPDX line → already new format or unknown

    license_id = m.group(1)

    result: list[str] = [
        "---\n",
        f"SPDX-License-Identifier: {license_id}\n",
        "---\n",
        "\n",
    ]

    # Skip the bare SPDX line and any immediately following blank lines.
    i = 1
    while i < len(lines) and lines[i].strip() == "":
        i += 1

    current_section: str | None = None

    for line in lines[i:]:
        stripped = line.rstrip("\r\n")

        # Track current section.
        m_sec = RE_SECTION_HEADER.match(stripped)
        if m_sec:
            current_section = m_sec.group(1).strip()
            result.append(line)
            continue

        if current_section in NESTED_SECTIONS:
            # Remove `  - type: ...` lines entirely.
            if RE_TYPE_ATTR.match(stripped):
                continue

            # Convert `- propName` → `- propName:`.
            m_top = RE_NESTED_TOP.match(stripped)
            if m_top:
                result.append(f"{m_top.group(1)}{m_top.group(2)}:\n")
                continue

            # Quote bare `*` values so YAML does not treat them as aliases.
            m_star = RE_STAR_VALUE.match(stripped)
            if m_star:
                result.append(f'{m_star.group(1)}"*"\n')
                continue

        if current_section in BACKTICK_SECTIONS:
            # Wrap values in backticks if not already (protects Markdown-special chars).
            m_kv = RE_KV_LINE.match(stripped)
            if m_kv:
                value = m_kv.group(2)
                if not (value.startswith("`") and value.endswith("`")):
                    result.append(f"{m_kv.group(1)}`{value}`\n")
                    continue

        result.append(line)

    return result


def migrate_file(path: Path) -> bool:
    """Migrate *path* in-place.  Returns ``True`` if the file was changed."""
    content = path.read_text(encoding="utf-8")
    lines = content.splitlines(keepends=True)
    new_lines = _migrate_lines(lines)
    if new_lines is None:
        return False
    new_content = "".join(new_lines)
    if new_content == content:
        return False
    path.write_text(new_content, encoding="utf-8")
    return True


def migrate_directory(root: Path) -> None:
    """Recursively migrate all ``.md`` files under *root*."""
    changed = 0
    skipped = 0
    for md_file in sorted(root.rglob("*.md")):
        if migrate_file(md_file):
            print(f"  migrated: {md_file.relative_to(root)}")
            changed += 1
        else:
            skipped += 1
    print(f"\n{changed} file(s) migrated, {skipped} already up-to-date.")


def main() -> None:
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} <model-directory>", file=sys.stderr)
        sys.exit(1)
    root = Path(sys.argv[1])
    if not root.is_dir():
        print(f"Error: {root} is not a directory.", file=sys.stderr)
        sys.exit(1)
    migrate_directory(root)


if __name__ == "__main__":
    main()
