# Tests for spec_parser.rdf generation

# SPDX-License-Identifier: Apache-2.0

from __future__ import annotations

import json

import pytest
from rdflib import Graph, URIRef
from rdflib.namespace import OWL, RDF, RDFS, SH, SKOS, VANN

from spec_parser.model import Model

BASE = "https://example.org/rdf/terms/"
CORE = BASE + "Core/"


class TestRDFGraph:
    def test_graph_non_empty(self, rdf_graph: Graph) -> None:
        assert len(rdf_graph) > 0

    def test_ontology_node(self, rdf_graph: Graph) -> None:
        ont = URIRef(BASE)
        assert (ont, RDF.type, OWL.Ontology) in rdf_graph

    def test_all_classes_present(self, rdf_graph: Graph, model: Model) -> None:
        for c in model.classes.values():
            node = URIRef(c.iri)
            assert (node, RDF.type, OWL.Class) in rdf_graph, f"Missing owl:Class for {c.iri}"

    def test_vocabularies_present(self, rdf_graph: Graph, model: Model) -> None:
        for v in model.vocabularies.values():
            node = URIRef(v.iri)
            assert (node, RDF.type, OWL.Class) in rdf_graph

    def test_properties_present(self, rdf_graph: Graph, model: Model) -> None:
        for fq, p in model.properties.items():
            if fq == "/Core/spdxId":
                continue
            node = URIRef(p.iri)
            types = set(rdf_graph.objects(node, RDF.type))
            assert OWL.ObjectProperty in types or OWL.DatatypeProperty in types, f"{p.iri} missing property type"


class TestRDFLabels:
    def test_rdfs_label_on_classes(self, rdf_graph: Graph, model: Model) -> None:
        for c in model.classes.values():
            node = URIRef(c.iri)
            labels = list(rdf_graph.objects(node, RDFS.label))
            assert labels, f"No rdfs:label on {c.iri}"

    def test_rdfs_label_on_vocabularies(self, rdf_graph: Graph, model: Model) -> None:
        for v in model.vocabularies.values():
            node = URIRef(v.iri)
            labels = list(rdf_graph.objects(node, RDFS.label))
            assert labels, f"No rdfs:label on {v.iri}"

    def test_skos_definition_on_classes(self, rdf_graph: Graph, model: Model) -> None:
        for c in model.classes.values():
            if not c.summary:
                continue
            node = URIRef(c.iri)
            defs = list(rdf_graph.objects(node, SKOS.definition))
            assert defs, f"No skos:definition on {c.iri}"

    def test_skos_note_on_class_with_description(self, rdf_graph: Graph, model: Model) -> None:
        for c in model.classes.values():
            if not c.description:
                continue
            node = URIRef(c.iri)
            notes = list(rdf_graph.objects(node, SKOS.note))
            assert notes, f"No skos:note on {c.iri}"


class TestRDFIsDefinedBy:
    def test_classes_have_is_defined_by(self, rdf_graph: Graph, model: Model) -> None:
        for c in model.classes.values():
            node = URIRef(c.iri)
            defined_by = list(rdf_graph.objects(node, RDFS.isDefinedBy))
            assert defined_by, f"No rdfs:isDefinedBy on {c.iri}"
            assert URIRef(c.ns.iri) in defined_by

    def test_properties_have_is_defined_by(self, rdf_graph: Graph, model: Model) -> None:
        for fq, p in model.properties.items():
            if fq == "/Core/spdxId":
                continue
            node = URIRef(p.iri)
            defined_by = list(rdf_graph.objects(node, RDFS.isDefinedBy))
            assert defined_by, f"No rdfs:isDefinedBy on {p.iri}"

    def test_vocabularies_have_is_defined_by(self, rdf_graph: Graph, model: Model) -> None:
        for v in model.vocabularies.values():
            node = URIRef(v.iri)
            defined_by = list(rdf_graph.objects(node, RDFS.isDefinedBy))
            assert defined_by, f"No rdfs:isDefinedBy on {v.iri}"


class TestRDFVocabularies:
    def test_owl_one_of_on_vocabulary(self, rdf_graph: Graph, model: Model) -> None:
        for v in model.vocabularies.values():
            node = URIRef(v.iri)
            equiv_classes = list(rdf_graph.objects(node, OWL.equivalentClass))
            assert equiv_classes, f"No owl:equivalentClass on vocabulary {v.iri}"
            found_one_of = any(
                (ec, OWL.oneOf, None) in rdf_graph
                for ec in equiv_classes
            )
            assert found_one_of, f"owl:oneOf missing on {v.iri}"

    def test_vocabulary_entries_are_named_individuals(self, rdf_graph: Graph, model: Model) -> None:
        for v in model.vocabularies.values():
            for entry in v.entries:
                enode = URIRef(v.iri + "/" + entry)
                assert (enode, RDF.type, OWL.NamedIndividual) in rdf_graph

    def test_no_sh_node_kind_iri_with_sh_in(self, rdf_graph: Graph) -> None:
        """sh:nodeKind sh:IRI must not appear alongside sh:in (issue #1152)."""
        for bnode in rdf_graph.subjects(SH["in"], None):
            assert (bnode, SH.nodeKind, SH.IRI) not in rdf_graph, \
                "Superfluous sh:nodeKind sh:IRI alongside sh:in"


class TestRDFSHACL:
    def test_abstract_class_has_not_constraint(self, rdf_graph: Graph, model: Model) -> None:
        """Abstract classes get sh:not sh:hasValue for SHACL validators."""
        for c in model.classes.values():
            if c.metadata["Instantiability"] != "Abstract":
                continue
            node = URIRef(c.iri)
            sh_props = list(rdf_graph.objects(node, SH.property))
            found_not = any((sp, SH["not"], None) in rdf_graph for sp in sh_props)
            assert found_not, f"Abstract class {c.iri} missing sh:not constraint"

    def test_abstract_class_has_disjoint_union(self, rdf_graph: Graph, model: Model) -> None:
        """Abstract classes with subclasses get owl:disjointUnionOf for OWL reasoners."""
        for c in model.classes.values():
            if c.metadata["Instantiability"] != "Abstract" or not c.direct_subclasses:
                continue
            node = URIRef(c.iri)
            assert (node, OWL.disjointUnionOf, None) in rdf_graph, \
                f"Abstract class {c.iri} missing owl:disjointUnionOf"

    def test_no_sh_node_kind_literal_with_sh_datatype(self, rdf_graph: Graph) -> None:
        """sh:nodeKind sh:Literal must not appear alongside sh:datatype (issue #1152)."""
        for bnode in rdf_graph.subjects(SH.datatype, None):
            assert (bnode, SH.nodeKind, SH.Literal) not in rdf_graph, \
                "Superfluous sh:nodeKind sh:Literal alongside sh:datatype"

    def test_concrete_class_is_node_shape(self, rdf_graph: Graph, model: Model) -> None:
        for c in model.classes.values():
            if not c.properties:
                continue
            node = URIRef(c.iri)
            assert (node, RDF.type, SH.NodeShape) in rdf_graph

    def test_element_class_has_iri_node_kind(self, rdf_graph: Graph) -> None:
        # Element has spdxId → sh:IRI.
        element = URIRef(CORE + "Element")
        assert (element, SH.nodeKind, SH.IRI) in rdf_graph

    def test_agent_class_has_iri_node_kind_inherited(self, rdf_graph: Graph) -> None:
        # Agent inherits spdxId from Element → sh:IRI.
        agent = URIRef(CORE + "Agent")
        assert (agent, SH.nodeKind, SH.IRI) in rdf_graph


class TestJSONLDContext:
    @pytest.fixture(scope="class")
    def ctx(self, rdf_graph: Graph, model: Model) -> dict:
        from spec_parser.rdf import _jsonld_context
        return _jsonld_context(rdf_graph, model.base_uri)

    def test_context_has_spdx_key(self, ctx: dict) -> None:
        assert "spdx" in ctx["@context"]
        assert ctx["@context"]["spdx"] == BASE

    def test_spdx_id_maps_to_at_id(self, ctx: dict) -> None:
        assert ctx["@context"]["spdxId"] == "@id"

    def test_type_maps_to_at_type(self, ctx: dict) -> None:
        assert ctx["@context"]["type"] == "@type"

    def test_data_property_has_at_type(self, ctx: dict) -> None:
        # name is DataProperty → should have @type pointing to xsd:string.
        name_entry = ctx["@context"].get("name")
        assert name_entry is not None
        assert "@type" in name_entry

    def test_class_object_property_uses_at_id_not_at_vocab(self, ctx: dict) -> None:
        """Object properties whose range is a class use @type: @id, not @vocab (PR #205)."""
        # No class object property in fixture since Tool→Agent uses SupportType vocab.
        # Test indirectly: confirm no entry with @type: @vocab and missing @context.
        for key, val in ctx["@context"].items():
            if isinstance(val, dict) and val.get("@type") == "@vocab":
                # Any @vocab entry must have a @context (explicit enum mapping).
                assert "@context" in val, \
                    f"'{key}' uses @type:@vocab but has no @context (should enumerate enum values)"

    def test_vocab_property_has_explicit_enum_context(self, ctx: dict) -> None:
        # supportLevel range is SupportType (vocabulary) → @type: @vocab + @context with entries.
        support_entry = ctx["@context"].get("supportLevel")
        assert support_entry is not None
        assert support_entry.get("@type") == "@vocab"
        local_ctx = support_entry.get("@context", {})
        # Each SupportType entry should appear in the local context.
        assert "developerSupport" in local_ctx
        assert "noSupport" in local_ctx


class TestRDFPrefixes:
    def test_spdx_prefix_bound(self, rdf_graph: Graph) -> None:
        prefixes = dict(rdf_graph.namespaces())
        assert "spdx" in prefixes

    def test_namespace_prefix_bound(self, rdf_graph: Graph) -> None:
        prefixes = dict(rdf_graph.namespaces())
        assert "spdx-core" in prefixes

    def test_vann_prefix_on_ontology(self, rdf_graph: Graph) -> None:
        ont = URIRef(BASE)
        assert (ont, VANN.preferredNamespacePrefix, None) in rdf_graph
        assert (ont, VANN.preferredNamespaceUri, None) in rdf_graph

    def test_vann_prefix_on_namespace(self, rdf_graph: Graph) -> None:
        ns = URIRef(CORE.rstrip("/"))
        assert (ns, VANN.preferredNamespacePrefix, None) in rdf_graph


class TestRDFSerialization:
    def test_turtle_serialization(self, rdf_graph: Graph, tmp_path) -> None:
        out = tmp_path / "test.ttl"
        rdf_graph.serialize(out, format="ttl", encoding="utf-8")
        content = out.read_text()
        assert "spdx-core" in content or "example.org" in content

    def test_jsonld_serialization(self, rdf_graph: Graph, tmp_path) -> None:
        out = tmp_path / "test.jsonld"
        rdf_graph.serialize(out, format="json-ld", encoding="utf-8")
        data = json.loads(out.read_text())
        assert "@graph" in data or isinstance(data, list)
