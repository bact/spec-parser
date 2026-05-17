# SPDX-License-Identifier: Apache-2.0
"""Generate an OWL ontology with SHACL shapes and JSON-LD context from the parsed model."""

from __future__ import annotations

import json
import logging
import re
from pathlib import Path
from typing import TYPE_CHECKING, Any

from rdflib import BNode, Graph, Literal, Namespace, URIRef
from rdflib.collection import Collection
from rdflib.namespace import DCTERMS, OWL, RDF, RDFS, SH, SKOS, VANN
from rdflib.tools.rdf2dot import rdf2dot

from .model import Instantiability, PropertyNature

if TYPE_CHECKING:
    from .model import Class, Model

logger = logging.getLogger(__name__)

# ---------------------------------------------------------------------------
# Markdown → plain text helper
# ---------------------------------------------------------------------------

_RE_MD_LINK_EXT = re.compile(r"\[([^\]]+)\]\((https?://[^)]+)\)")
_RE_MD_LINK_INT = re.compile(r"\[([^\]]+)\]\([^)]+\)")
_RE_MD_CODE_BLOCK = re.compile(r"```[^\n]*\n(.*?)```", re.DOTALL)
_RE_MD_INLINE_CODE = re.compile(r"`([^`]+)`")


def _md_to_text(md: str | None) -> str:
    """Strip Markdown markup to produce readable plain text for RDF literals."""
    if not md:
        return ""
    text = _RE_MD_CODE_BLOCK.sub(r"\1", md)
    text = _RE_MD_LINK_EXT.sub(r"\1 <\2>", text)
    text = _RE_MD_LINK_INT.sub(r"\1", text)
    text = _RE_MD_INLINE_CODE.sub(r"\1", text)
    return text.strip()


# ---------------------------------------------------------------------------
# XSD range resolution
# ---------------------------------------------------------------------------


def _xsd_range(rng: str, propname: str) -> URIRef | None:
    if rng.startswith("xsd:"):
        return URIRef("http://www.w3.org/2001/XMLSchema#" + rng[4:])
    logger.warning("Unknown namespace in range <%s> of property %s", rng, propname)
    return None


# ---------------------------------------------------------------------------
# Public entry point
# ---------------------------------------------------------------------------


def gen_rdf(model: Model, outpath: Path, cfg: Any) -> None:  # pylint: disable=unused-argument
    """Serialise the ontology to all supported RDF formats and write the JSON-LD context."""
    g = gen_rdf_ontology(model)

    for ext in ["hext", "json-ld", "longturtle", "n3", "nt", "pretty-xml", "trig", "ttl", "xml"]:
        f = outpath / f"spdx-model.{ext}"
        g.serialize(f, format=ext, encoding="utf-8")

    ctx = _jsonld_context(g, model.base_uri)
    fn = outpath / "spdx-context.jsonld"
    with fn.open("w") as fh:
        json.dump(ctx, fh, sort_keys=True, indent=2)

    fn = outpath / "spdx-model.dot"
    with fn.open("w") as fh:
        rdf2dot(g, fh)


# ---------------------------------------------------------------------------
# Ontology graph construction
# ---------------------------------------------------------------------------


def gen_rdf_ontology(model: Model) -> Graph:
    """Build and return an rdflib Graph containing the full OWL+SHACL ontology."""
    g = Graph()

    uri_base = model.base_uri

    g.bind("spdx", Namespace(uri_base))

    # Per-namespace prefix bindings (spdx-<nsname>) with VANN annotations.
    for ns in model.namespaces:
        prefix = "spdx-" + ns.name.lower()
        ns_iri = Namespace(ns.iri)
        g.bind(prefix, ns_iri)
        ns_node = URIRef(ns.iri)
        g.add((ns_node, VANN.preferredNamespacePrefix, Literal(prefix)))
        g.add((ns_node, VANN.preferredNamespaceUri, Literal(str(ns_iri))))

    OMG_ANN = Namespace("https://www.omg.org/spec/Commons/AnnotationVocabulary/")
    g.bind("omg-ann", OMG_ANN)
    g.bind("vann", VANN)

    # Ontology node.
    ont_node = URIRef(uri_base)
    g.add((ont_node, RDF.type, OWL.Ontology))
    g.add((ont_node, OWL.versionIRI, ont_node))

    # owl:versionInfo from Core namespace metadata (W3C OWL2 BCP).
    version_str: str | None = None
    for ns in model.namespaces:
        v = ns.metadata.get("version") or ns.metadata.get("Version")
        if v:
            version_str = v
            break
    if version_str:
        g.add((ont_node, OWL.versionInfo, Literal(version_str)))

    # vann:preferredNamespacePrefix / vann:preferredNamespaceUri (W3C BCP for vocab publishing).
    g.add((ont_node, VANN.preferredNamespacePrefix, Literal("spdx")))
    g.add((ont_node, VANN.preferredNamespaceUri, Literal(uri_base)))

    g.add((ont_node, RDFS.label, Literal("System Package Data Exchange™ (SPDX®) Ontology", lang="en")))
    g.add(
        (
            ont_node,
            DCTERMS.abstract,
            Literal(
                "This ontology defines the terms and relationships used in the SPDX specification "
                "to describe system packages",
                lang="en",
            ),
        )
    )
    g.add((ont_node, DCTERMS.creator, Literal("SPDX Project", lang="en")))
    g.add((ont_node, DCTERMS.license, URIRef("https://spdx.org/licenses/Community-Spec-1.0.html")))
    g.add((ont_node, DCTERMS.references, URIRef("https://spdx.dev/specifications/")))
    g.add((ont_node, DCTERMS.title, Literal("System Package Data Exchange (SPDX) Ontology", lang="en")))
    g.add((ont_node, OMG_ANN.copyright, Literal("Copyright (C) SPDX Project", lang="en")))

    _gen_classes(model, g)
    _gen_properties(model, g)
    _gen_vocabularies(model, g)
    _gen_individuals(model, g)

    return g


# ---------------------------------------------------------------------------
# Per-element generators
# ---------------------------------------------------------------------------


def _get_parent(model: Model, c: Class) -> Class | None:
    parent = c.metadata.get("SubclassOf")
    if parent:
        pns = "" if parent.startswith("/") else f"/{c.ns.name}/"
        return model.classes[pns + parent]
    return None


def _gen_classes(model: Model, g: Graph) -> None:
    for c in model.classes.values():
        node = URIRef(c.iri)
        ns_node = URIRef(c.ns.iri)

        g.add((node, RDF.type, OWL.Class))
        g.add((node, RDFS.isDefinedBy, ns_node))
        g.add((node, RDFS.label, Literal(c.name, lang="en")))

        if c.summary:
            plain_summary = _md_to_text(c.summary)
            g.add((node, RDFS.comment, Literal(plain_summary, lang="en")))
            g.add((node, SKOS.definition, Literal(plain_summary, lang="en")))

        if c.description:
            g.add((node, SKOS.note, Literal(_md_to_text(c.description), lang="en")))

        parent = _get_parent(model, c)
        if parent is not None:
            g.add((node, RDFS.subClassOf, URIRef(parent.iri)))

        if c.metadata["Instantiability"] == Instantiability.ABSTRACT:
            # SHACL layer: reject instances typed directly as this abstract class.
            bnode = BNode()
            g.add((node, SH.property, bnode))
            g.add((bnode, SH.path, RDF.type))
            not_node = BNode()
            g.add((bnode, SH["not"], not_node))
            g.add((not_node, SH["hasValue"], node))
            g.add((bnode, SH.message, Literal(
                f"{node} is abstract and must not be instantiated directly.",
                lang="en",
            )))
            # OWL layer: coverage + disjointness among direct subclasses.
            if c.direct_subclasses:
                sub_list = Collection(g, None)
                for fq in c.direct_subclasses:
                    sub_list.append(URIRef(model.classes[fq].iri))
                g.add((node, OWL.disjointUnionOf, sub_list.uri))

        if "spdxId" in c.all_properties:
            g.add((node, SH.nodeKind, SH.IRI))
        else:
            g.add((node, SH.nodeKind, SH.BlankNodeOrIRI))

        if c.properties:
            g.add((node, RDF.type, SH.NodeShape))
            for p in c.properties:
                fqprop = c.properties[p]["fqname"]
                if fqprop == "/Core/spdxId":
                    continue
                bnode = BNode()
                g.add((node, SH.property, bnode))
                prop = model.properties[fqprop]
                g.add((bnode, SH.path, URIRef(prop.iri)))

                prop_rng = prop.metadata["Range"]
                typename = prop_rng if ":" in prop_rng else (
                    prop_rng if prop_rng.startswith("/") else f"/{prop.ns.name}/{prop_rng}"
                )

                if typename in model.classes:
                    dt = model.classes[typename]
                    if typename == "/Extension/Extension":
                        # Extension subclasses cannot be fully validated.
                        ext_node = BNode()
                        lst = Collection(g, None)
                        for cls in model.classes.values():
                            if cls.metadata["Instantiability"] == "Abstract":
                                continue
                            cls_parent = _get_parent(model, cls)
                            if cls_parent is not None and cls_parent.fqname == "/Extension/Extension":
                                continue
                            cls_node = BNode()
                            g.add((cls_node, SH["class"], URIRef(cls.iri)))
                            lst.append(cls_node)
                        not_node = BNode()
                        g.add((ext_node, SH["not"], not_node))
                        g.add((not_node, SH["or"], lst.uri))
                        g.add((ext_node, SH.message, Literal("Class is known to not derive from Extension and cannot be used", lang="en")))
                        g.add((ext_node, SH.path, URIRef(prop.iri)))
                        g.add((node, SH.property, ext_node))
                    else:
                        g.add((bnode, SH["class"], URIRef(dt.iri)))

                    # sh:nodeKind distinguishes IRI-identified from blank-node resources.
                    if "spdxId" in dt.all_properties:
                        g.add((bnode, SH.nodeKind, SH.IRI))
                    else:
                        g.add((bnode, SH.nodeKind, SH.BlankNodeOrIRI))

                elif typename in model.vocabularies:
                    dt_v = model.vocabularies[typename]
                    g.add((bnode, SH["class"], URIRef(dt_v.iri)))
                    # sh:nodeKind sh:IRI is redundant when sh:in already enumerates IRI members.
                    lst = Collection(g, None)
                    for e in dt_v.entries:
                        lst.append(URIRef(dt_v.iri + "/" + e))
                    g.add((bnode, SH["in"], lst.uri))

                elif typename in model.datatypes:
                    dt_d = model.datatypes[typename]
                    if "pattern" in dt_d.format:
                        g.add((bnode, SH.pattern, Literal(dt_d.format["pattern"])))
                    t = _xsd_range(dt_d.metadata["SubclassOf"], prop.iri)
                    if t:
                        g.add((bnode, SH.datatype, t))
                        # sh:nodeKind sh:Literal is redundant when sh:datatype is present.

                else:
                    t = _xsd_range(typename, prop.iri)
                    if t:
                        g.add((bnode, SH.datatype, t))
                        # sh:nodeKind sh:Literal is redundant when sh:datatype is present.

                mincount = c.properties[p]["minCount"]
                if int(mincount) != 0:
                    g.add((bnode, SH.minCount, Literal(int(mincount))))
                maxcount = c.properties[p]["maxCount"]
                if maxcount != "*":
                    g.add((bnode, SH.maxCount, Literal(int(maxcount))))


def _gen_properties(model: Model, g: Graph) -> None:
    for fqname, p in model.properties.items():
        if fqname == "/Core/spdxId":
            continue
        node = URIRef(p.iri)
        ns_node = URIRef(p.ns.iri)

        g.add((node, RDFS.isDefinedBy, ns_node))
        g.add((node, RDFS.label, Literal(p.name, lang="en")))

        if p.summary:
            plain_summary = _md_to_text(p.summary)
            g.add((node, RDFS.comment, Literal(plain_summary, lang="en")))
            g.add((node, SKOS.definition, Literal(plain_summary, lang="en")))

        if p.description:
            g.add((node, SKOS.note, Literal(_md_to_text(p.description), lang="en")))

        match p.metadata["Nature"]:
            case PropertyNature.OBJECT_PROPERTY:
                g.add((node, RDF.type, OWL.ObjectProperty))
            case PropertyNature.DATA_PROPERTY:
                g.add((node, RDF.type, OWL.DatatypeProperty))

        rng = p.metadata["Range"]
        if ":" in rng:
            t = _xsd_range(rng, p.name)
            if t:
                g.add((node, RDFS.range, t))
        else:
            typename = rng if rng.startswith("/") else f"/{p.ns.name}/{rng}"
            if typename in model.datatypes:
                t = _xsd_range(model.datatypes[typename].metadata["SubclassOf"], p.name)
                if t:
                    g.add((node, RDFS.range, t))
            else:
                dt = model.types[typename]
                g.add((node, RDFS.range, URIRef(dt.iri)))


def _gen_vocabularies(model: Model, g: Graph) -> None:
    for v in model.vocabularies.values():
        node = URIRef(v.iri)
        ns_node = URIRef(v.ns.iri)

        g.add((node, RDF.type, OWL.Class))
        g.add((node, RDFS.isDefinedBy, ns_node))
        g.add((node, RDFS.label, Literal(v.name, lang="en")))

        if v.summary:
            plain_summary = _md_to_text(v.summary)
            g.add((node, RDFS.comment, Literal(plain_summary, lang="en")))
            g.add((node, SKOS.definition, Literal(plain_summary, lang="en")))

        # owl:equivalentClass + owl:oneOf makes this a closed enumeration in OWL.
        individuals_list = Collection(g, None)
        for e in v.entries:
            enode = URIRef(v.iri + "/" + e)
            individuals_list.append(enode)
        equiv_node = BNode()
        g.add((equiv_node, OWL.oneOf, individuals_list.uri))
        g.add((node, OWL.equivalentClass, equiv_node))

        for e, d in v.entries.items():
            enode = URIRef(v.iri + "/" + e)
            g.add((enode, RDF.type, OWL.NamedIndividual))
            g.add((enode, RDF.type, node))
            g.add((enode, RDFS.isDefinedBy, ns_node))
            g.add((enode, RDFS.label, Literal(e, lang="en")))
            if d:
                g.add((enode, RDFS.comment, Literal(_md_to_text(d), lang="en")))
                g.add((enode, SKOS.definition, Literal(_md_to_text(d), lang="en")))


def _gen_individuals(model: Model, g: Graph) -> None:
    uri_base = model.base_uri

    def _ci_ref(s: str) -> URIRef:
        return URIRef(uri_base + "Core/" + s)

    for i in model.individuals.values():
        ns_node = URIRef(i.ns.iri)
        ci_node = URIRef(uri_base + "creationInfo_" + i.name)
        g.add((ci_node, RDF.type, _ci_ref("CreationInfo")))
        g.add((ci_node, RDFS.comment, Literal("This individual element was defined by the spec.", lang="en")))
        g.add((ci_node, _ci_ref("createdBy"), _ci_ref("SpdxOrganization")))

        node = URIRef(i.iri)
        g.add((node, RDF.type, OWL.NamedIndividual))
        g.add((node, RDFS.isDefinedBy, ns_node))
        g.add((node, _ci_ref("creationInfo"), ci_node))

        if i.summary:
            g.add((node, RDFS.comment, Literal(_md_to_text(i.summary), lang="en")))
            g.add((node, SKOS.definition, Literal(_md_to_text(i.summary), lang="en")))

        typ = i.metadata["type"]
        typename = typ if typ.startswith("/") else f"/{i.ns.name}/{typ}"
        dt = model.types[typename]
        g.add((node, RDF.type, URIRef(dt.iri)))

        custom_iri = i.metadata.get("IRI")
        if custom_iri and custom_iri != i.iri:
            g.add((node, OWL.sameAs, URIRef(custom_iri)))


# ---------------------------------------------------------------------------
# JSON-LD context generation
# ---------------------------------------------------------------------------


def _jsonld_context(g: Graph, uri_base: str) -> dict[str, Any]:
    uri_base_stripped = uri_base.rstrip("/")

    terms: dict[str, str | dict[str, Any]] = {}

    # Collect vocabulary classes (those with sh:in enumerations).
    vocab_named_individuals: set[URIRef] = set()
    vocab_classes: set[URIRef] = set()
    for lst in g.objects(None, SH["in"]):
        c = Collection(g, lst)
        for e in c:
            vocab_named_individuals.add(e)
            for typ in g.objects(e, RDF.type):
                if typ != OWL.NamedIndividual:
                    vocab_classes.add(typ)

    # Build per-vocabulary-class enum entries for local contexts.
    # Maps vocab_class IRI → { entry_name: entry_IRI_str }
    vocab_entries: dict[str, dict[str, str]] = {}
    for v_cls in vocab_classes:
        entries: dict[str, str] = {}
        for ind in vocab_named_individuals:
            if (ind, RDF.type, v_cls) in g:
                # Entry name is the last segment of the IRI.
                entry_name = str(ind).rsplit("/", 1)[-1]
                entries[entry_name] = str(ind)
        vocab_entries[str(v_cls)] = entries

    def _get_subject_term(subject: URIRef) -> str | dict[str, Any]:
        if (subject, RDF.type, OWL.ObjectProperty) in g:
            for _, _, o in g.triples((subject, RDFS.range, None)):
                o_str = str(o)
                if o in vocab_classes:
                    # Enum range: @type:@vocab with a local @context that enumerates valid
                    # members explicitly, preventing wildcard expansion to unknown IRIs.
                    local_ctx: dict[str, Any] = {}
                    for entry_name, entry_iri in vocab_entries.get(o_str, {}).items():
                        local_ctx[entry_name] = {"@id": entry_iri}
                    return {
                        "@id": str(subject),
                        "@type": "@vocab",
                        "@context": local_ctx,
                    }
                if (o, RDF.type, OWL.Class) in g:
                    # Class range: @type:@id so the value expands to a full IRI, not @vocab.
                    return {
                        "@id": str(subject),
                        "@type": "@id",
                    }
        elif (subject, RDF.type, OWL.DatatypeProperty) in g:
            for _, _, o in g.triples((subject, RDFS.range, None)):
                return {
                    "@id": str(subject),
                    "@type": str(o),
                }

        return str(subject)

    for subject in sorted(g.subjects(unique=True)):
        if not isinstance(subject, URIRef):
            continue

        # Skip named individuals in vocabularies.
        if (subject, RDF.type, OWL.NamedIndividual) in g and subject in vocab_named_individuals:
            continue

        s = str(subject)
        if not s.startswith(uri_base_stripped + "/"):
            continue

        remainder = s[len(uri_base_stripped) + 1:]
        parts = remainder.split("/", 1)
        if len(parts) != 2:
            continue
        ns, name = parts

        key = name if ns == "Core" else ns.lower() + "_" + name

        if key in terms:
            current = terms[key]["@id"] if isinstance(terms[key], dict) else terms[key]
            logger.error(
                "Duplicate JSON-LD context key %r: <%s> conflicts with already-mapped <%s>. "
                "Rename one term or move it to a different namespace.",
                key, subject, current,
            )
            continue

        terms[key] = _get_subject_term(subject)

    terms["spdx"] = uri_base
    terms["spdxId"] = "@id"
    terms["type"] = "@type"

    return {"@context": terms}
