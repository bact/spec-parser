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
RE_RANGE_LINE = re.compile(r"^- Range:\s+(.+?)\s*$", re.MULTILINE)
RE_SECTION_HEADER = re.compile(r"^## (.+)$")
# New-format nested top: ``- propName:`` (trailing colon, no value after it).
RE_NESTED_TOP_NEW = re.compile(r"^(-\s+)([\w/]+):\s*$")
# Indented ``"*"`` quoted value → bare ``*``.
RE_QUOTED_STAR = re.compile(r'^(  -\s+\w+:\s+)"(\*)"(\s*)$')
# Backtick-wrapped value on a ``- key: `val` `` line (single or double backticks).
RE_BACKTICK_KV = re.compile(r"^(-\s+\w[\w-]*:\s+)(``|`)(.+?)\2\s*$")

NESTED_SECTIONS = {"Properties", "External properties restrictions"}
FORMAT_SECTIONS = {"Format"}


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

    current_section: str | None = None

    for line in content[fm_match.end():].splitlines(keepends=True):
        stripped = line.rstrip("\r\n")

        m_sec = RE_SECTION_HEADER.match(stripped)
        if m_sec:
            current_section = m_sec.group(1).strip()
            result.append(line)
            continue

        if current_section in NESTED_SECTIONS:
            # ``"*"`` → ``*``
            m_star = RE_QUOTED_STAR.match(stripped)
            if m_star:
                result.append(f"{m_star.group(1)}*\n")
                continue

            # ``- propName:`` → ``- propName`` + ``  - type: X``
            m_top = RE_NESTED_TOP_NEW.match(stripped)
            if m_top:
                prop_name = m_top.group(2)
                result.append(f"{m_top.group(1)}{prop_name}\n")
                fqname = prop_name if prop_name.startswith("/") else f"/{ns_name}/{prop_name}"
                prop_range = range_map.get(fqname)
                if prop_range:
                    result.append(f"  - type: {prop_range}\n")
                else:
                    # Cross-namespace ref without leading slash — search all namespaces.
                    matches = [r for k, r in range_map.items() if k.endswith(f"/{prop_name}")]
                    if len(matches) == 1:
                        result.append(f"  - type: {matches[0]}\n")
                continue

        if current_section in FORMAT_SECTIONS:
            # Strip backtick wrapping: ``- pattern: `regex` `` → ``- pattern: regex``
            m_bt = RE_BACKTICK_KV.match(stripped)
            if m_bt:
                result.append(f"{m_bt.group(1)}{m_bt.group(3)}\n")
                continue

        result.append(line)

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
