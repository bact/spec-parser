# Tests validating SPDX 3.x example JSON-LD files

# SPDX-License-Identifier: Apache-2.0

from __future__ import annotations

import json
from pathlib import Path


class TestExampleFileFormat:
    """Validate the structure of downloaded .spdx3.json / .json example files."""

    def test_valid_json(self, spdx3_example: Path) -> None:
        data = json.loads(spdx3_example.read_text(encoding="utf-8"))
        assert isinstance(data, dict)

    def test_has_context(self, spdx3_example: Path) -> None:
        data = json.loads(spdx3_example.read_text(encoding="utf-8"))
        assert "@context" in data, f"{spdx3_example.name} missing @context"

    def test_has_graph(self, spdx3_example: Path) -> None:
        data = json.loads(spdx3_example.read_text(encoding="utf-8"))
        assert "@graph" in data, f"{spdx3_example.name} missing @graph"

    def test_context_is_spdx(self, spdx3_example: Path) -> None:
        data = json.loads(spdx3_example.read_text(encoding="utf-8"))
        ctx = data["@context"]
        ctx_str = ctx if isinstance(ctx, str) else json.dumps(ctx)
        assert "spdx.org" in ctx_str, f"{spdx3_example.name} context not from spdx.org"

    def test_graph_non_empty(self, spdx3_example: Path) -> None:
        data = json.loads(spdx3_example.read_text(encoding="utf-8"))
        assert len(data["@graph"]) > 0

    def test_graph_has_creation_info(self, spdx3_example: Path) -> None:
        data = json.loads(spdx3_example.read_text(encoding="utf-8"))
        types = [node.get("type", "") for node in data["@graph"] if isinstance(node, dict)]
        assert "CreationInfo" in types, f"{spdx3_example.name} has no CreationInfo node"

    def test_rdflib_can_parse(self, spdx3_example: Path) -> None:
        """rdflib must parse the file without error."""
        from rdflib import ConjunctiveGraph
        g = ConjunctiveGraph()
        g.parse(str(spdx3_example), format="json-ld")
        assert len(g) > 0, f"{spdx3_example.name} parsed to empty graph"
