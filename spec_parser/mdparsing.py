# SPDX-License-Identifier: Apache-2.0
"""Parse spec Markdown files into typed section objects (SpecFile, ContentSection, SingleListSection, NestedListSection)."""

from __future__ import annotations

import logging
import re
from typing import TYPE_CHECKING

import yaml

if TYPE_CHECKING:
    from pathlib import Path

logger = logging.getLogger(__name__)

# Matches a backtick-wrapped value on a ``- key: `value` `` line.
# Supports both single-backtick (`value`) and double-backtick (``val`ue``) delimiters,
# mirroring the Markdown inline-code convention for values that contain a literal backtick.
# Backtick is invalid in YAML plain scalars but renders as inline code in Markdown/GitHub.
_RE_BACKTICK_VALUE = re.compile(r"(:\s+)(``|`)(.+?)\2(\s*)$", re.MULTILINE)


def _backtick_to_single_quoted(m: re.Match[str]) -> str:
    """Convert a backtick-wrapped YAML value to a YAML single-quoted string."""
    escaped = m.group(3).replace("'", "''")  # YAML: '' is an escaped single quote
    return f"{m.group(1)}'{escaped}'"


def _sentence_case(s: str) -> str:
    """Normalize a section heading to sentence case (capitalize first char only)."""
    return s[:1].upper() + s[1:].lower() if s else s


class SpecFile:
    """Parsed representation of a single spec Markdown file.

    Exposes ``name``, ``license``, ``frontmatter``, ``sections`` (base language
    content), and ``translations`` (per-language content).

    Files must begin with a YAML frontmatter block::

        ---
        SPDX-License-Identifier: Community-Spec-1.0
        ---

        # ElementName
        ...

    Translated sections use BCP 47 language tags in the heading::

        ## Summary @ja

        日本語の要約。
    """

    RE_SPLIT_TO_SECTIONS = re.compile(r"\n(?=(?:\Z|# |## ))")
    RE_FRONTMATTER = re.compile(r"\A---[ \t]*\r?\n(.*?)\r?\n---[ \t]*\r?\n?", re.DOTALL)
    RE_EXTRACT_NAME = re.compile(r"#\s+(\w+)\s*")
    RE_EXTRACT_HEADER_CONTENT = re.compile(r"##\s+(.*)\s+((.|\s)+)")
    # ``## Section Name @bcp47tag``
    RE_LANG_TAG = re.compile(r"^(.+?)\s+@([\w-]+)$")

    def __init__(self, fpath: Path | None = None) -> None:
        self.license: str | None = None
        self.name: str = ""
        self.sections: dict[str, str] = {}
        self.frontmatter: dict[str, str] = {}
        self.translations: dict[str, dict[str, str]] = {}
        if fpath is not None:
            self.load(fpath)

    def load(self, fpath: Path) -> None:
        """Read and parse *fpath*, populating all attributes in place."""
        logger.debug("### loading %s/%s", fpath.parent, fpath.name)
        filecontent = fpath.read_text(encoding="utf-8")

        fm_match = self.RE_FRONTMATTER.match(filecontent)
        if fm_match:
            try:
                # BaseLoader intentional: all scalars stay as strings (no bool/int coercion).
                self.frontmatter = yaml.load(fm_match.group(1), Loader=yaml.BaseLoader) or {}  # noqa: S506
            except yaml.YAMLError:
                logger.exception("File %s: frontmatter YAML error.", fpath)
                self.frontmatter = {}
            self.license = self.frontmatter.get("SPDX-License-Identifier")
            if not self.license:
                logger.error("File %s: frontmatter missing SPDX-License-Identifier.", fpath)
            filecontent = filecontent[fm_match.end():].lstrip("\r\n")
        else:
            logger.error("File %s: missing YAML frontmatter block (--- ... ---).", fpath)

        parts = re.split(self.RE_SPLIT_TO_SECTIONS, filecontent)

        if parts:
            m = re.fullmatch(self.RE_EXTRACT_NAME, parts[0])
            if m is None:
                logger.error("File %s does not have a name heading.", fpath)
            else:
                self.name = m.group(1)

        for p in parts[1:]:
            if not p.strip():
                continue
            m = re.fullmatch(self.RE_EXTRACT_HEADER_CONTENT, p)
            if m is None:
                first_line = p.splitlines()[0] if p.splitlines() else repr(p[:80])
                logger.error("File %s: cannot parse section starting with: %r", fpath, first_line)
                continue
            header = m.group(1).strip()
            if not header:
                logger.error("File %s: section with empty heading.", fpath)
                continue
            content = m.group(2).strip()
            if not content:
                continue
            m_lang = self.RE_LANG_TAG.match(header)
            if m_lang:
                sec_name = _sentence_case(m_lang.group(1).strip())
                lang = m_lang.group(2)
                self.translations.setdefault(lang, {})[sec_name] = content
            else:
                self.sections[_sentence_case(header)] = content


class Section:
    """Abstract base for all section content parsers."""

    def __init__(
        self,
        content: str | None,
        filename: str | None = None,
        context: str | None = None,
    ) -> None:
        self.filename = filename
        self.context = context
        if content is not None:
            self.load(content)

    def _fmt_err_msg(self, s: str, detail: str) -> str:
        return f"{s} in {self.context} of '{self.filename}': {detail}"

    def load(self, content: str) -> None:
        """Parse *content* and populate this section's data attributes."""
        raise NotImplementedError


class ContentSection(Section):
    """Section whose content is stored verbatim as a plain string."""

    def __init__(
        self,
        content: str | None,
        filename: str | None = None,
        context: str | None = None,
    ) -> None:
        self.content: str = ""
        super().__init__(content, filename, context)

    def load(self, content: str) -> None:
        """Store *content* as-is."""
        self.content = content


class SingleListSection(Section):
    """Section whose content is a flat YAML list of ``- key: value`` pairs.

    Parsed into a ``kv`` dict mapping each key to its string value.
    Values may be wrapped in backticks (`` `value` ``) for Markdown readability
    (e.g. regex patterns containing ``*``); the backticks are stripped on load.
    """

    def __init__(
        self,
        content: str | None,
        filename: str | None = None,
        context: str | None = None,
    ) -> None:
        self.kv: dict[str, str] = {}
        super().__init__(content, filename, context)

    def load(self, content: str) -> None:
        """Parse *content* into ``kv`` using PyYAML with string-only scalars."""
        self.kv = {}
        # Pre-convert `value` → 'value' so backtick-wrapped items parse as YAML strings.
        content = _RE_BACKTICK_VALUE.sub(_backtick_to_single_quoted, content)
        try:
            # BaseLoader intentional: all scalars stay as strings (no bool/int coercion).
            data = yaml.load(content, Loader=yaml.BaseLoader)  # noqa: S506
        except yaml.YAMLError as e:
            mark = getattr(e, "problem_mark", None)
            loc = f" at line {mark.line + 1}, col {mark.column + 1}" if mark else ""
            logger.error(self._fmt_err_msg(f"YAML parse error{loc}", getattr(e, "problem", str(e))))  # noqa: TRY400
            return
        if not isinstance(data, list):
            logger.error(self._fmt_err_msg(
                "expected YAML list of '- key: value' pairs",
                f"got {type(data).__name__}: {content[:60]!r}",
            ))
            return
        for item in data:
            if not isinstance(item, dict) or len(item) != 1:
                n = f"{len(item)}-key dict" if isinstance(item, dict) else type(item).__name__
                logger.error(self._fmt_err_msg("expected single-key mapping '- key: value'", f"got {n}: {item!r}"))
                continue
            (k, v), = item.items()
            self.kv[k] = str(v) if v is not None else ""


_VOCAB_ENTRY_DEFAULTS: dict[str, object] = {
    "from": ["Element"],
    "to": ["Element"],
    "relationshipClass": "Relationship",
}


class VocabularySection(Section):
    """Section for vocabulary Entries supporting both simple and structured formats.

    Simple format (plain description)::

        - entryName: A description of this entry.

    Structured format (with relationship constraints and optional deprecation)::

        - entryName:
          - description: A description of this entry.
          - from: Agent, Tool
          - to: Element
          - relationshipClass: RoleRelationship
          - sinceVersion: 3.0
          - deprecated: true
          - deprecatedVersion: 3.1
          - isReplacedBy: /Core/otherEntry

    Every entry is normalised to a dict with at least ``description``, ``from``,
    ``to``, and ``relationshipClass``.  Missing structural fields receive defaults
    from ``_VOCAB_ENTRY_DEFAULTS`` (``from``/``to`` → ``["Element"]``,
    ``relationshipClass`` → ``"Relationship"``).  ``from`` and ``to`` are stored as
    ``list[str]`` split on commas; all other fields are plain strings.
    Optional fields ``sinceVersion``, ``deprecated``, ``deprecatedVersion``, and
    ``isReplacedBy`` are stored verbatim when present.
    """

    def __init__(
        self,
        content: str | None,
        filename: str | None = None,
        context: str | None = None,
    ) -> None:
        self.entries: dict[str, dict[str, object]] = {}
        super().__init__(content, filename, context)

    def load(self, content: str) -> None:
        """Parse *content* into ``entries``."""
        self.entries = {}
        content = _RE_BACKTICK_VALUE.sub(_backtick_to_single_quoted, content)
        try:
            # BaseLoader intentional: all scalars stay as strings (no bool/int coercion).
            data = yaml.load(content, Loader=yaml.BaseLoader)  # noqa: S506
        except yaml.YAMLError as e:
            mark = getattr(e, "problem_mark", None)
            loc = f" at line {mark.line + 1}, col {mark.column + 1}" if mark else ""
            logger.error(self._fmt_err_msg(f"YAML parse error{loc}", getattr(e, "problem", str(e))))  # noqa: TRY400
            return
        if not isinstance(data, list):
            logger.error(self._fmt_err_msg(
                "expected YAML list of '- name: description' or '- name:\\n  - key: value' entries",
                f"got {type(data).__name__}: {content[:60]!r}",
            ))
            return
        for item in data:
            if not isinstance(item, dict) or len(item) != 1:
                n = f"{len(item)}-key dict" if isinstance(item, dict) else type(item).__name__
                logger.error(self._fmt_err_msg("expected single-key entry mapping", f"got {n}: {item!r}"))
                continue
            (name, val), = item.items()
            if val is None or isinstance(val, str):
                entry: dict[str, object] = {"description": str(val) if val is not None else ""}
            elif isinstance(val, list):
                entry = {}
                for attr in val:
                    if not isinstance(attr, dict) or len(attr) != 1:
                        n = f"{len(attr)}-key dict" if isinstance(attr, dict) else type(attr).__name__
                        logger.error(self._fmt_err_msg(
                            f"expected single-key attribute under {name!r}",
                            f"got {n}: {attr!r}",
                        ))
                        continue
                    (k, v), = attr.items()
                    entry[k] = str(v) if v is not None else ""
            else:
                logger.error(self._fmt_err_msg(
                    f"unexpected value type for entry {name!r}",
                    f"got {type(val).__name__}: {val!r}",
                ))
                continue
            if "description" not in entry:
                entry["description"] = ""
            for field in ("from", "to"):
                raw = entry.get(field)
                if raw is None:
                    entry[field] = list(_VOCAB_ENTRY_DEFAULTS[field])  # type: ignore[arg-type]
                elif isinstance(raw, str):
                    entry[field] = [s.strip() for s in raw.split(",") if s.strip()]
            if "relationshipClass" not in entry:
                entry["relationshipClass"] = _VOCAB_ENTRY_DEFAULTS["relationshipClass"]
            self.entries[name] = entry


class NestedListSection(Section):
    """Section whose content is a YAML list of single-key mappings.

    Each entry maps a name to a list of ``- key: value`` attribute pairs::

        - propName:
          - minCount: 1
          - maxCount: 1

    Parsed into an ``ikv`` dict mapping each item name to its attributes dict.
    """

    def __init__(
        self,
        content: str | None,
        filename: str | None = None,
        context: str | None = None,
    ) -> None:
        self.ikv: dict[str, dict[str, str]] = {}
        super().__init__(content, filename, context)

    def load(self, content: str) -> None:
        """Parse *content* into ``ikv`` using PyYAML with string-only scalars."""
        self.ikv = {}
        try:
            # BaseLoader intentional: all scalars stay as strings (no bool/int coercion).
            data = yaml.load(content, Loader=yaml.BaseLoader)  # noqa: S506
        except yaml.YAMLError as e:
            mark = getattr(e, "problem_mark", None)
            loc = f" at line {mark.line + 1}, col {mark.column + 1}" if mark else ""
            logger.error(self._fmt_err_msg(f"YAML parse error{loc}", getattr(e, "problem", str(e))))  # noqa: TRY400
            return
        if not isinstance(data, list):
            logger.error(self._fmt_err_msg(
                "expected YAML list of '- name:\\n  - key: value' entries",
                f"got {type(data).__name__}: {content[:60]!r}",
            ))
            return
        for item in data:
            if not isinstance(item, dict) or len(item) != 1:
                n = f"{len(item)}-key dict" if isinstance(item, dict) else type(item).__name__
                logger.error(self._fmt_err_msg("expected single-key mapping '- name:'", f"got {n}: {item!r}"))
                continue
            (prop_name, attrs), = item.items()
            if attrs is None:
                self.ikv[prop_name] = {}
            elif isinstance(attrs, list):
                self.ikv[prop_name] = {}
                for attr in attrs:
                    if not isinstance(attr, dict) or len(attr) != 1:
                        n = f"{len(attr)}-key dict" if isinstance(attr, dict) else type(attr).__name__
                        logger.error(self._fmt_err_msg(f"expected single-key attribute under {prop_name!r}", f"got {n}: {attr!r}"))
                        continue
                    (k, v), = attr.items()
                    self.ikv[prop_name][k] = str(v) if v is not None else ""
            else:
                logger.error(self._fmt_err_msg(
                    f"expected list of '- key: value' attributes under {prop_name!r}",
                    f"got {type(attrs).__name__}: {attrs!r}",
                ))
