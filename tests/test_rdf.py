# SPDX-License-Identifier: Apache-2.0

from types import SimpleNamespace

from rdflib import Graph, Literal, URIRef

from spec_parser.rdf import CREATION_INFO_BASE, URI_BASE, gen_rdf_individuals

CORE = URI_BASE + "Core/"


def model_with(name, **meta):
    ns = SimpleNamespace
    ind = ns(name=name, iri=CORE + name, summary=None, metadata={"type": "T", **meta}, ns=ns(name="Core"))
    return ns(individuals={name: ind}, types={"/Core/T": ns(iri=CORE + "T")})


def test_creation_info_from_metadata():
    g = Graph()
    gen_rdf_individuals(model_with("NoneElement", specVersion="3.0.1", created="2024-11-22T03:00:01Z"), g)
    ci = URIRef(CREATION_INFO_BASE + "NoneElement")
    assert str(ci) == "https://spdx.org/rdf/3/creationInfo_NoneElement"
    assert g.value(URIRef(CORE + "NoneElement"), URIRef(CORE + "creationInfo")) == ci
    assert g.value(ci, URIRef(CORE + "specVersion")) == Literal("3.0.1")
    assert str(g.value(ci, URIRef(CORE + "created"))) == "2024-11-22T03:00:01Z"


def test_missing_metadata_is_reported(caplog):
    g = Graph()
    gen_rdf_individuals(model_with("NewIndividual", specVersion="3.1"), g)
    assert "NewIndividual" in caplog.text
    assert len(g) == 0
