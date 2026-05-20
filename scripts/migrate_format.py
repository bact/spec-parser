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
# Metadata key-value: `- key: value` (flat list).
RE_META_KV = re.compile(r"^(-\s+)(\w+)(\s*:\s*)(.*)$")
# Deprecation notice: ``**DEPRECATED in SPDX 3.1.**`` or ``**DEPRECATED.**``
RE_DEPRECATED_NOTICE = re.compile(
    r"^\*\*DEPRECATED(?:\s+in\s+(?:SPDX\s+)?([\d.]+(?:\.\d+)*))?\.\*\*\s*$",
    re.IGNORECASE,
)
# ``Use [/Core/endTime](../../...) instead.``
RE_USE_INSTEAD = re.compile(
    r"^Use\s+\[([^\]]+)\]\([^)]+\)\s+instead\.\s*$",
    re.IGNORECASE,
)

NESTED_SECTIONS = {"Properties", "External properties restrictions"}
METADATA_SECTIONS = {"Metadata"}
ENTRIES_SECTIONS = {"Entries"}
# Sections whose values may contain Markdown-special chars (e.g. * in regex) and need backtick quoting.
BACKTICK_SECTIONS = {"Format"}
RE_KV_LINE = re.compile(r"^(-\s+\w[\w-]*:\s+)(.+)$")
# Entry line: `- name: description text`
RE_ENTRY_LINE = re.compile(r"^(-\s+)([\w/]+)(\s*:\s*)(.+)$")
# Relationship entry patterns for from/to/relationshipClass extraction.
# Matches any entry where `from` is followed by an uppercase class name.
RE_HAS_FROM = re.compile(r"`from`\s+[A-Z]")
# Capture text between `from` and `to` markers.
RE_FROM_TO_SPAN = re.compile(r"`from`\s+(.+?)`to`", re.IGNORECASE)
# Capture text immediately after `to` marker.
RE_TO_AFTER = re.compile(r"`to`\s+(?:each\s+|the\s+)?(.+)", re.IGNORECASE)
# PascalCase class name: starts uppercase, contains at least one lowercase (filters out acronyms like SPDX).
RE_PASCAL_NAME = re.compile(r"(?<![a-zA-Z])[A-Z][a-zA-Z0-9]*[a-z][a-zA-Z0-9]*")
# Possessive split: "Element's Action or ..." → take the part after "'s".
RE_POSSESSIVE_SPLIT = re.compile(r"^[A-Z]\w+\s*'s\s+(.+)")
# Patterns for constraining relationship class.
RE_CONSTRAINED_CLASS = re.compile(
    r"(?:constrained to|Shall be (?:a|an)|To be used with)\s+`([A-Z]\w+)`",
    re.IGNORECASE,
)

_METADATA_RENAMES = {
    "subclassof": "subclassOf",
    "nature": "nature",
    "range": "range",
    "iri": "iri",
}


def _extract_deprecation(lines: list[str]) -> dict[str, str]:
    """Scan *lines* for a deprecation notice in the Description section.

    Returns a dict with up to three keys: ``deprecated``, ``deprecatedVersion``,
    ``isReplacedBy``.  Returns an empty dict when no notice is found.
    """
    in_desc = False
    result: dict[str, str] = {}
    after_depr = False

    for line in lines:
        stripped = line.rstrip("\r\n")
        m = RE_SECTION_HEADER.match(stripped)
        if m:
            in_desc = m.group(1).strip() == "Description"
            after_depr = False
            continue
        if not in_desc:
            continue
        if not result:
            m_dn = RE_DEPRECATED_NOTICE.match(stripped)
            if m_dn:
                result["deprecated"] = "true"
                if m_dn.group(1):
                    result["deprecatedVersion"] = m_dn.group(1)
                after_depr = True
        elif after_depr:
            if not stripped:
                continue  # blank line between notice and "Use instead."
            m_ui = RE_USE_INSTEAD.match(stripped)
            if m_ui:
                result["isReplacedBy"] = m_ui.group(1).strip()
            after_depr = False

    return result


def _collect_class_names(text: str) -> list[str]:
    """Scan *text* left-to-right and collect leading PascalCase class names.

    Stops at the first token that is neither a PascalCase name nor a connector
    word (``or``, ``and``), or at the end of the first sentence (token ending
    with ``.``).  Parenthetical suffixes like ``(s)`` are stripped before
    matching.  Duplicates are dropped while preserving order.
    """
    seen: set[str] = set()
    names: list[str] = []
    for raw in re.split(r"[\s,]+", text.strip()):
        sentence_end = raw.endswith(".")
        # Strip trailing punctuation first, then parenthetical suffix.
        token = re.sub(r"\([^)]*\)$", "", raw.rstrip(".,;:"))
        if not token:
            if sentence_end:
                break
            continue
        if token.lower() in ("or", "and"):
            continue
        if RE_PASCAL_NAME.fullmatch(token):
            if token not in seen:
                seen.add(token)
                names.append(token)
            if sentence_end:
                break  # stop at sentence boundary even if name was valid
        else:
            break  # stop at first non-class-name token
    return names


def _extract_rel_entry_fields(desc: str) -> dict[str, object]:
    """Best-effort extraction of from/to/relationshipClass from a relationship entry description.

    Detects patterns like::

        The `from` Artifact has each `to` Agent as a contact point.
        The use of `hasContactPoint` type is constrained to `ContactPointRelationship` ...
        Relates a `from` Vulnerability and each `to` Element ...
        Shall be a `SupportRelationship` type.
        To be used with `VulnAssessmentRelationship` types.

    Returns a dict with keys ``from`` (list), ``to`` (list), ``relationshipClass`` (str)
    for whichever fields could be found.  Returns an empty dict if not a relationship entry.
    """
    if not RE_HAS_FROM.search(desc):
        return {}
    result: dict[str, object] = {}

    m_ft = RE_FROM_TO_SPAN.search(desc)
    if m_ft:
        from_text = m_ft.group(1).strip()
        # Handle possessive: "Element's Action or DefinedProcess" → "Action or DefinedProcess"
        m_poss = RE_POSSESSIVE_SPLIT.match(from_text)
        if m_poss:
            from_text = m_poss.group(1)
        names = RE_PASCAL_NAME.findall(from_text)
        if names:
            # Deduplicate while preserving order (e.g. "Agent ... Agent" → ["Agent"])
            seen: set[str] = set()
            result["from"] = [n for n in names if not (n in seen or seen.add(n))]  # type: ignore[func-returns-value]

    m_to = RE_TO_AFTER.search(desc)
    if m_to:
        names = _collect_class_names(m_to.group(1))
        if names:
            result["to"] = names

    m_rc = RE_CONSTRAINED_CLASS.search(desc)
    if m_rc:
        result["relationshipClass"] = m_rc.group(1)

    # Only return a result when `from` was found; inverted entries (e.g. "Every
    # `to` ... `from` ...") would otherwise produce a misleading partial dict.
    if "from" not in result:
        return {}
    return result


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

    body = lines[i:]

    # Pre-scan for deprecation info in the Description section.
    dep_info = _extract_deprecation(body)
    # Track which dep keys are already explicitly in Metadata (avoid duplicates).
    meta_dep_seen: set[str] = set()

    current_section: str | None = None
    # State machine for stripping the deprecation notice block from Description.
    # "none" → haven't seen it; "saw_notice" → saw **DEPRECATED**, looking for Use;
    # "done" → finished stripping.
    desc_strip: str = "none"
    desc_skip_trailing_blank: bool = False

    def _flush_dep_into_metadata() -> None:
        for key in ("deprecated", "deprecatedVersion", "isReplacedBy"):
            if key in dep_info and key not in meta_dep_seen:
                result.append(f"- {key}: {dep_info[key]}\n")

    for line in body:
        stripped = line.rstrip("\r\n")

        m_sec = RE_SECTION_HEADER.match(stripped)
        if m_sec:
            if current_section in METADATA_SECTIONS:
                _flush_dep_into_metadata()
            current_section = m_sec.group(1).strip()
            desc_strip = "none"
            desc_skip_trailing_blank = False
            result.append(line)
            continue

        # Strip the deprecation notice block out of the Description section.
        if current_section == "Description" and dep_info:
            skip = False
            if desc_strip == "none":
                if RE_DEPRECATED_NOTICE.match(stripped):
                    desc_strip = "saw_notice"
                    skip = True
            elif desc_strip == "saw_notice":
                if not stripped:
                    skip = True  # blank between notice and "Use instead."
                elif RE_USE_INSTEAD.match(stripped):
                    desc_strip = "done"
                    desc_skip_trailing_blank = True
                    skip = True
                else:
                    desc_strip = "done"  # no "Use" line; stop stripping
            if not skip and desc_skip_trailing_blank:
                if not stripped:
                    desc_skip_trailing_blank = False
                    skip = True  # one trailing blank after the notice block
            if skip:
                continue

        if current_section in METADATA_SECTIONS:
            m_meta = RE_META_KV.match(stripped)
            if m_meta:
                key = m_meta.group(2)
                value = m_meta.group(4).strip()
                kl = key.lower()
                if kl in ("deprecated", "deprecatedversion", "isreplacedby"):
                    meta_dep_seen.add(kl)
                if kl in _METADATA_RENAMES:
                    result.append(f"{m_meta.group(1)}{_METADATA_RENAMES[kl]}{m_meta.group(3)}{value}\n")
                    continue
                if kl == "instantiability":
                    if value.lower() == "abstract":
                        result.append(f"{m_meta.group(1)}abstract{m_meta.group(3)}true\n")
                    # Concrete → omit (false is default)
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

        if current_section in ENTRIES_SECTIONS:
            m_entry = RE_ENTRY_LINE.match(stripped)
            if m_entry:
                prefix = m_entry.group(1)
                name = m_entry.group(2)
                desc = m_entry.group(4).strip()
                rel = _extract_rel_entry_fields(desc)
                if rel:
                    result.append(f"{prefix}{name}:\n")
                    result.append(f"  - description: {desc}\n")
                    for field in ("from", "to", "relationshipClass"):
                        if field in rel:
                            val = rel[field]
                            if isinstance(val, list):
                                result.append(f"  - {field}: {', '.join(val)}\n")
                            else:
                                result.append(f"  - {field}: {val}\n")
                    continue

        result.append(line)

    # EOF while still in Metadata.
    if current_section in METADATA_SECTIONS:
        _flush_dep_into_metadata()

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
