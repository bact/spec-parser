# Tests for spec_parser.model

# SPDX-License-Identifier: Apache-2.0

from __future__ import annotations

from spec_parser.model import Model


class TestModelLoading:
    def test_namespace_count(self, model: Model) -> None:
        assert len(model.namespaces) == 1
        assert model.namespaces[0].name == "Core"

    def test_class_count(self, model: Model) -> None:
        assert len(model.classes) == 3

    def test_property_count(self, model: Model) -> None:
        assert len(model.properties) == 4

    def test_vocabulary_count(self, model: Model) -> None:
        assert len(model.vocabularies) == 1

    def test_datatype_count(self, model: Model) -> None:
        assert len(model.datatypes) == 1

    def test_individual_count(self, model: Model) -> None:
        assert len(model.individuals) == 1

    def test_base_uri_derived_from_core(self, model: Model) -> None:
        assert model.base_uri == "https://example.org/rdf/terms/"

    def test_class_iris(self, model: Model) -> None:
        assert model.classes["/Core/Element"].iri == "https://example.org/rdf/terms/Core/Element"
        assert model.classes["/Core/Tool"].iri == "https://example.org/rdf/terms/Core/Tool"

    def test_namespace_iri(self, model: Model) -> None:
        # Real model stores namespace id without trailing slash (e.g. ".../Core").
        assert model.namespaces[0].iri == "https://example.org/rdf/terms/Core"


class TestInheritance:
    def test_tool_inherits_from_agent(self, model: Model) -> None:
        tool = model.classes["/Core/Tool"]
        assert tool.fqsupercname == "/Core/Agent"

    def test_agent_inherits_from_element(self, model: Model) -> None:
        agent = model.classes["/Core/Agent"]
        assert agent.fqsupercname == "/Core/Element"

    def test_element_has_no_parent(self, model: Model) -> None:
        element = model.classes["/Core/Element"]
        assert element.fqsupercname is None

    def test_tool_all_properties_includes_inherited(self, model: Model) -> None:
        tool = model.classes["/Core/Tool"]
        # spdxId and name come from Element, toolVersion and supportLevel from Tool.
        assert "spdxId" in tool.all_properties
        assert "name" in tool.all_properties
        assert "toolVersion" in tool.all_properties
        assert "supportLevel" in tool.all_properties

    def test_tool_inheritance_stack(self, model: Model) -> None:
        tool = model.classes["/Core/Tool"]
        assert "/Core/Agent" in tool.inheritance_stack
        assert "/Core/Element" in tool.inheritance_stack

    def test_direct_subclasses(self, model: Model) -> None:
        element = model.classes["/Core/Element"]
        assert "/Core/Agent" in element.direct_subclasses
        agent = model.classes["/Core/Agent"]
        assert "/Core/Tool" in agent.direct_subclasses

    def test_class_hierarchy(self, model: Model) -> None:
        assert "/Core/Agent" in model.class_hierarchy["/Core/Element"]
        assert "/Core/Tool" in model.class_hierarchy["/Core/Agent"]


class TestInstantiability:
    def test_element_is_abstract(self, model: Model) -> None:
        assert model.classes["/Core/Element"].metadata["Instantiability"] == "Abstract"

    def test_tool_is_concrete(self, model: Model) -> None:
        assert model.classes["/Core/Tool"].metadata["Instantiability"] == "Concrete"


