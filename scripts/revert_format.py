#!/usr/bin/env python3
# SPDX-License-Identifier: Apache-2.0
"""Revert spec Markdown files from new format back to old format.

Inverse of ``migrate_format.py``.  Use when you need to feed files back into
the legacy spec-parser toolchain that expects the old format.

New format (input):
  - YAML frontmatter block (``--- ... ---``)
  - Properties section: ``- propName:\\n  - key: val`` (no ``type:`` line)
  - ``Format`` section values wrapped in backticks: ``- pattern: `regex```

Old format (output):
  - Lone ``SPDX-License-Identifier:`` line at top
  - Properties section: ``- propName\\n  - type: X\\n  - key: val``
  - Plain values in ``Format`` section: ``- pattern: regex``

Because the old format requires ``type:`` in each class property entry, this
script reads the sibling ``Properties/<name>.md`` files to look up the
``Range:`` field and reconstruct the ``type:`` value automatically.

Usage::

    python scripts/revert_format.py <path-to-model-dir>

The script modifies files in-place and prints each changed path.  Run on a
working copy or inside a git repository so the diff can be reviewed.
"""

from __future__ import annotations

import re
import sys
from pathlib import Path

# ---------------------------------------------------------------------------
# Patterns
# ---------------------------------------------------------------------------

RE_FRONTMATTER = re.compile(r"\A---[ \t]*\r?\n(.*?)\r?\n---[ \t]*\r?\n?", re.DOTALL)
RE_SPDX_IN_FM = re.compile(r"^SPDX-License-Identifier:\s+(.+?)\s*$", re.MULTILINE)
# Case-insensitive: new format uses lowercase ``range:``.
RE_RANGE_LINE = re.compile(r"^-\s+range:\s+(.+?)\s*$", re.MULTILINE | re.IGNORECASE)
RE_SECTION_HEADER = re.compile(r"^## (.+)$")
# New-format nested top: ``- propName:`` (trailing colon, no value after it).
RE_NESTED_TOP_NEW = re.compile(r"^(-\s+)([\w/]+):\s*$")
# Indented ``"*"`` quoted value → bare ``*``.
RE_QUOTED_STAR = re.compile(r'^(  -\s+\w+:\s+)"(\*)"(\s*)$')
# Backtick-wrapped value on a ``- key: `val` `` line (single or double backticks).
RE_BACKTICK_KV = re.compile(r"^(-\s+\w[\w-]*:\s+)(``|`)(.+?)\2\s*$")
# Metadata key-value in new format.
RE_META_KV = re.compile(r"^(-\s+)(\w+)(\s*:\s*)(.*)$")
# Deprecation notice patterns (same as in migrate_format.py).
RE_DEPRECATED_NOTICE = re.compile(
    r"^\*\*DEPRECATED(?:\s+in\s+(?:SPDX\s+)?([\d.]+(?:\.\d+)*))?\.\*\*\s*$",
    re.IGNORECASE,
)
RE_USE_INSTEAD = re.compile(
    r"^Use\s+\[([^\]]+)\]\([^)]+\)\s+instead\.\s*$",
    re.IGNORECASE,
)

NESTED_SECTIONS = {"Properties", "External properties restrictions"}
FORMAT_SECTIONS = {"Format"}
METADATA_SECTIONS = {"Metadata"}


def _has_depr_notice(text: str) -> bool:
    """Return True if *text* already contains a deprecation notice in Description."""
    in_desc = False
    for line in text.splitlines():
        m = RE_SECTION_HEADER.match(line)
        if m:
            in_desc = m.group(1).strip() == "Description"
            continue
        if in_desc and RE_DEPRECATED_NOTICE.match(line):
            return True
    return False


def _collect_depr_metadata(text: str) -> dict[str, str]:
    """Pre-scan *text* for deprecated/deprecatedVersion/isReplacedBy in Metadata."""
    in_meta = False
    result: dict[str, str] = {}
    for line in text.splitlines():
        m = RE_SECTION_HEADER.match(line)
        if m:
            in_meta = m.group(1).strip() == "Metadata"
            continue
        if not in_meta:
            continue
        m_meta = RE_META_KV.match(line)
        if m_meta:
            kl = m_meta.group(2).lower()
            val = m_meta.group(4).strip()
            if kl == "deprecated":
                result["deprecated"] = val
            elif kl == "deprecatedversion":
                result["deprecatedVersion"] = val
            elif kl == "isreplacedby":
                result["isReplacedBy"] = val
    return result


# ---------------------------------------------------------------------------
# Range lookup
# ---------------------------------------------------------------------------

def _build_range_map(root: Path) -> dict[str, str]:
    """Scan all ``Properties/*.md`` files and return ``{/NS/name: Range}``."""
    range_map: dict[str, str] = {}
    for prop_file in root.rglob("Properties/*.md"):
        content = prop_file.read_text(encoding="utf-8")
        ns_name = prop_file.parent.parent.name
        fqname = f"/{ns_name}/{prop_file.stem}"
        m = RE_RANGE_LINE.search(content)
        if m:
            range_map[fqname] = m.group(1).strip()
    return range_map


# ---------------------------------------------------------------------------
# Per-file revert
# ---------------------------------------------------------------------------

def _revert_lines(content: str, ns_name: str, range_map: dict[str, str]) -> str | None:
    """Return reverted content, or *None* if already old-format or unchanged."""
    fm_match = RE_FRONTMATTER.match(content)
    if not fm_match:
        return None  # no frontmatter → already old format or unknown

    spdx_m = RE_SPDX_IN_FM.search(fm_match.group(1))
    license_id = spdx_m.group(1) if spdx_m else "Community-Spec-1.0"

    result: list[str] = [
        f"SPDX-License-Identifier: {license_id}\n",
        "\n",
    ]

    body = content[fm_match.end():]

    # Pre-scan: collect dep metadata and check if notice already in Description.
    # (Description appears before Metadata in the file so we can't collect on the fly.)
    _pre = _collect_depr_metadata(body)
    depr_val: str | None = _pre.get("deprecated")
    depr_ver: str | None = _pre.get("deprecatedVersion")
    repl_by: str | None = _pre.get("isReplacedBy")
    notice_already_present = _has_depr_notice(body)

    current_section: str | None = None
    meta_saw_abstract: bool = False

    # Flag: inject the notice at the first non-blank line of Description.
    depr_inject_pending: bool = False

    def _flush_metadata_defaults() -> None:
        if current_section in METADATA_SECTIONS and not meta_saw_abstract:
            result.append("- Instantiability: Concrete\n")

    for line in body.splitlines(keepends=True):
        stripped = line.rstrip("\r\n")

        m_sec = RE_SECTION_HEADER.match(stripped)
        if m_sec:
            _flush_metadata_defaults()
            current_section = m_sec.group(1).strip()
            meta_saw_abstract = False
            # Set up injection when entering Description.
            depr_inject_pending = (
                current_section == "Description"
                and depr_val == "true"
                and not notice_already_present
            )
            result.append(line)
            continue

        # Inject deprecation notice before the first non-blank content line in Description.
        if depr_inject_pending and current_section == "Description" and stripped:
            ver_part = f" in SPDX {depr_ver}" if depr_ver else ""
            result.append(f"**DEPRECATED{ver_part}.**\n")
            if repl_by:
                result.append(f"Use [{repl_by}]({repl_by}) instead.\n")
            result.append("\n")
            depr_inject_pending = False

        if current_section in METADATA_SECTIONS:
            m_meta = RE_META_KV.match(stripped)
            if m_meta:
                key = m_meta.group(2)
                value = m_meta.group(4).strip()
                kl = key.lower()
                _RENAMES_BACK = {
                    "subclassof": "SubclassOf",
                    "nature": "Nature",
                    "range": "Range",
                    "iri": "IRI",
                }
                if kl in _RENAMES_BACK:
                    result.append(f"{m_meta.group(1)}{_RENAMES_BACK[kl]}{m_meta.group(3)}{value}\n")
                    continue
                if kl == "abstract":
                    meta_saw_abstract = True
                    if value.lower() == "true":
                        result.append("- Instantiability: Abstract\n")
                    else:
                        result.append("- Instantiability: Concrete\n")
                    continue
                # Collect and strip dep fields — converted back to Description text.
                if kl == "deprecated":
                    depr_val = value
                    continue
                if kl == "deprecatedversion":
                    depr_ver = value
                    continue
                if kl == "isreplacedby":
                    repl_by = value
                    continue

        if current_section in NESTED_SECTIONS:
            m_star = RE_QUOTED_STAR.match(stripped)
            if m_star:
                result.append(f"{m_star.group(1)}*\n")
                continue

            m_top = RE_NESTED_TOP_NEW.match(stripped)
            if m_top:
                prop_name = m_top.group(2)
                result.append(f"{m_top.group(1)}{prop_name}\n")
                fqname = prop_name if prop_name.startswith("/") else f"/{ns_name}/{prop_name}"
                prop_range = range_map.get(fqname)
                if prop_range:
                    result.append(f"  - type: {prop_range}\n")
                else:
                    matches = [r for k, r in range_map.items() if k.endswith(f"/{prop_name}")]
                    if len(matches) == 1:
                        result.append(f"  - type: {matches[0]}\n")
                continue

        if current_section in FORMAT_SECTIONS:
            m_bt = RE_BACKTICK_KV.match(stripped)
            if m_bt:
                result.append(f"{m_bt.group(1)}{m_bt.group(3)}\n")
                continue

        result.append(line)

    _flush_metadata_defaults()
    return "".join(result)


def revert_file(path: Path, range_map: dict[str, str]) -> bool:
    """Revert *path* in-place.  Returns ``True`` if the file was changed."""
    content = path.read_text(encoding="utf-8")
    # Derive namespace name from directory structure: <root>/<NS>/.../<file>.md
    # For namespace files themselves: <root>/<NS>/<NS>.md → parent is <NS> dir.
    ns_name = path.parent.name if path.parent.name[0].isupper() else path.parent.parent.name
    new_content = _revert_lines(content, ns_name, range_map)
    if new_content is None or new_content == content:
        return False
    path.write_text(new_content, encoding="utf-8")
    return True


def revert_directory(root: Path) -> None:
    """Recursively revert all ``.md`` files under *root*."""
    range_map = _build_range_map(root)
    changed = 0
    skipped = 0
    for md_file in sorted(root.rglob("*.md")):
        if revert_file(md_file, range_map):
            print(f"  reverted: {md_file.relative_to(root)}")
            changed += 1
        else:
            skipped += 1
    print(f"\n{changed} file(s) reverted, {skipped} already up-to-date.")


def main() -> None:
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} <model-directory>", file=sys.stderr)
        sys.exit(1)
    root = Path(sys.argv[1])
    if not root.is_dir():
        print(f"Error: {root} is not a directory.", file=sys.stderr)
        sys.exit(1)
    revert_directory(root)


if __name__ == "__main__":
    main()
