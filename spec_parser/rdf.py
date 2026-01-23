# saving the model as RDF

# SPDX-License-Identifier: Apache-2.0

import json
import logging

from rdflib import BNode, Graph, Literal, Namespace, URIRef
from rdflib.collection import Collection
from rdflib.namespace import DCTERMS, OWL, RDF, RDFS, SH, XSD
from rdflib.tools.rdf2dot import rdf2dot

PROFILE_NAMESPACE_PREFIX = {
    "Core": "spdx-c",
    "Software": "spdx-sw",
    "Security": "spdx-s",
    "Licensing": "spdx-l",
    "SimpleLicensing": "spdx-ls",
    "ExpandedLicensing": "spdx-lx",
    "Dataset": "spdx-d",
    "AI": "spdx-ai",
    "Build": "spdx-b",
    "Lite": "spdx-lite",
    "Extension": "spdx-x",
    "Hardware": "spdx-hw",
    "Service": "spdx-sv",
    "SupplyChain": "spdx-sc",
    "Operations": "spdx-o",
    "FunctionalSafety": "spdx-fs",
}

logger = logging.getLogger(__name__)

def gen_rdf(model, outpath, cfg):
    p = outpath

    base_uri = "https://example.com/"
    # Determine actual base URI from Core namespace
    for ns in model.namespaces:
        if ns.name == "Core":
            iri = ns.iri
            # Remove trailing "Core"
            if iri.endswith("/Core/"):
                base_uri = iri[: -len("/Core/")]
            elif iri.endswith("/Core"):
                base_uri = iri[: -len("/Core")]
            else:
                base_uri = iri.rstrip("/") + "/"
            # Ensure trailing slash
            if not base_uri.endswith("/"):
                base_uri += "/"
            break

    ret = gen_rdf_ontology(model, base_uri)
    for ext in ["hext", "json-ld", "longturtle", "n3", "nt", "pretty-xml", "trig", "ttl", "xml"]:
        f = p / ("spdx-model." + ext)
        ret.serialize(f, format=ext, encoding="utf-8")

    ctx = jsonld_context(ret, base_uri)
    fn = p / "spdx-context.jsonld"
    with fn.open("w") as f:
        json.dump(ctx, f, sort_keys=False, indent=2)

    fn = p / "spdx-model.dot"
    with fn.open("w") as f:
        rdf2dot(ret, f)


def xsd_range(rng, propname):
    if rng.startswith("xsd:"):
        return URIRef("http://www.w3.org/2001/XMLSchema#" + rng[4:])

    logger.warning(f"Uknown namespace in range <{rng}> of property {propname}")
    return None


def gen_rdf_ontology(model, base_uri):
    g = Graph()
    g.bind("spdx", Namespace(base_uri))
    for name, prefix in PROFILE_NAMESPACE_PREFIX.items():
        g.bind(prefix, Namespace(f"{base_uri}{name}/"))
    OMG_ANN = Namespace("https://www.omg.org/spec/Commons/AnnotationVocabulary/")
    g.bind("omg-ann", OMG_ANN)

    node = URIRef(base_uri)
    g.add((node, RDF.type, OWL.Ontology))
    g.add((node, OWL.versionIRI, node))
    g.add((node, RDFS.label, Literal("System Package Data Exchange™ (SPDX®) Ontology", lang="en")))
    g.add(
        (
            node,
            DCTERMS.abstract,
            Literal(
                "This ontology defines the terms and relationships used in the SPDX specification to describe system packages",
                lang="en",
            ),
        ),
    )
    g.add((node, DCTERMS.created, Literal("2024-04-05", datatype=XSD.date)))
    g.add((node, DCTERMS.creator, Literal("SPDX Project", lang="en")))
    g.add((node, DCTERMS.license, URIRef("https://spdx.org/licenses/Community-Spec-1.0.html")))
    g.add((node, DCTERMS.references, URIRef("https://spdx.dev/specifications/")))
    g.add((node, DCTERMS.title, Literal("System Package Data Exchange (SPDX) Ontology", lang="en")))
    g.add((node, OMG_ANN.copyright, Literal("Copyright (C) 2024-2026 SPDX Project", lang="en")))

    gen_rdf_classes(model, g)
    gen_rdf_properties(model, g)
    #     gen_rdf_datatypes(model, g)
    gen_rdf_vocabularies(model, g)
    gen_rdf_individuals(model, g, base_uri)

    return g

def get_parent(model, c):
    parent = c.metadata.get("SubclassOf")
    if parent:
        pns = "" if parent.startswith("/") else f"/{c.ns.name}/"
        return model.classes[pns + parent]
    return None


def gen_rdf_classes(model, g):
    for c in model.classes.values():
        node = URIRef(c.iri)
        g.add((node, RDF.type, OWL.Class))
        if c.summary:
            g.add((node, RDFS.comment, Literal(c.summary, lang="en")))
        p = get_parent(model, c)
        if p is not None:
            g.add((node, RDFS.subClassOf, URIRef(p.iri)))
        if c.metadata["Instantiability"] == "Abstract":
            bnode = BNode()
            g.add((node, SH.property, bnode))
            g.add((bnode, SH.path, RDF.type))
            notNode = BNode()
            g.add((bnode, SH["not"], notNode))
            g.add((notNode, SH["hasValue"], node))
            msg = Literal(
                f"{node} is an abstract class and should not be instantiated directly. Instantiate a subclass instead.",
                lang="en",
            )
            g.add((bnode, SH.message, msg))

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
                if ":" not in prop_rng:
                    typename = "" if prop_rng.startswith("/") else f"/{prop.ns.name}/"
                    typename += prop_rng
                else:
                    typename = prop_rng
                if typename in model.classes:
                    dt = model.classes[typename]

                    # Extension subclasses cannot be validated, since they are
                    # unknown. Any unknown class is assumed to be derived from
                    # extension
                    if typename == "/Extension/Extension":
                        extnode = BNode()
                        lst = Collection(g, None)
                        for cls in model.classes.values():
                            if cls.metadata["Instantiability"] == "Abstract":
                                continue
                            cls_parent = get_parent(model, cls)
                            if cls_parent is not None and cls_parent.fqname == "/Extension/Extension":
                                continue
                            clsNode = BNode()
                            g.add((clsNode, SH["class"], URIRef(cls.iri)))
                            lst.append(clsNode)
                        notNode = BNode()
                        g.add((extnode, SH["not"], notNode))
                        g.add((notNode, SH["or"], lst.uri))
                        msg = Literal(
                            "Class is known to not derive from Extension and cannot be used",
                            lang="en",
                        )
                        g.add((extnode, SH.message, msg))
                        g.add((extnode, SH.path, URIRef(prop.iri)))
                        g.add((node, SH.property, extnode))
                    else:
                        g.add((bnode, SH["class"], URIRef(dt.iri)))

                    if "spdxId" in dt.all_properties:
                        g.add((bnode, SH.nodeKind, SH.IRI))
                    else:
                        g.add((bnode, SH.nodeKind, SH.BlankNodeOrIRI))
                elif typename in model.vocabularies:
                    dt = model.vocabularies[typename]
                    g.add((bnode, SH["class"], URIRef(dt.iri)))
                    g.add((bnode, SH.nodeKind, SH.IRI))
                    lst = Collection(g, None)
                    for e in dt.entries:
                        lst.append(URIRef(dt.iri + "/" + e))
                    g.add((bnode, SH["in"], lst.uri))
                elif typename in model.datatypes:
                    dt = model.datatypes[typename]
                    if "pattern" in dt.format:
                        g.add((bnode, SH.pattern, Literal(dt.format["pattern"])))
                    t = xsd_range(dt.metadata["SubclassOf"], prop.iri)
                    if t:
                        g.add((bnode, SH.datatype, t))
                        g.add((bnode, SH.nodeKind, SH.Literal))
                else:
                    t = xsd_range(typename, prop.iri)
                    if t:
                        g.add((bnode, SH.datatype, t))
                        g.add((bnode, SH.nodeKind, SH.Literal))
                mincount = c.properties[p]["minCount"]
                if int(mincount) != 0:
                    g.add((bnode, SH.minCount, Literal(int(mincount))))
                maxcount = c.properties[p]["maxCount"]
                if maxcount != "*":
                    g.add((bnode, SH.maxCount, Literal(int(maxcount))))


def gen_rdf_properties(model, g):
    for fqname, p in model.properties.items():
        if fqname == "/Core/spdxId":
            continue
        node = URIRef(p.iri)
        if p.summary:
            g.add((node, RDFS.comment, Literal(p.summary, lang="en")))
        if p.metadata["Nature"] == "ObjectProperty":
            g.add((node, RDF.type, OWL.ObjectProperty))
        # to add: g.add((node, RDFS.domain, xxx))
        elif p.metadata["Nature"] == "DataProperty":
            g.add((node, RDF.type, OWL.DatatypeProperty))
        rng = p.metadata["Range"]
        if ":" in rng:
            t = xsd_range(rng, p.name)
            if t:
                g.add((node, RDFS.range, t))
        else:
            typename = "" if rng.startswith("/") else f"/{p.ns.name}/"
            typename += rng
            if typename in model.datatypes:
                t = xsd_range(model.datatypes[typename].metadata["SubclassOf"], p.name)
                if t:
                    g.add((node, RDFS.range, t))
            else:
                dt = model.types[typename]
                g.add((node, RDFS.range, URIRef(dt.iri)))


def gen_rdf_vocabularies(model, g):
    for v in model.vocabularies.values():
        node = URIRef(v.iri)
        g.add((node, RDF.type, OWL.Class))
        if v.summary:
            g.add((node, RDFS.comment, Literal(v.summary, lang="en")))
        for e, d in v.entries.items():
            enode = URIRef(v.iri + "/" + e)
            g.add((enode, RDF.type, OWL.NamedIndividual))
            g.add((enode, RDF.type, node))
            g.add((enode, RDFS.label, Literal(e)))
            g.add((enode, RDFS.comment, Literal(d, lang="en")))


def gen_rdf_individuals(model, g, base_uri):
    def ci_ref(s):
        return URIRef(base_uri + "Core/" + s)

    for i in model.individuals.values():
        ci_node = URIRef("https://spdx.org/rdf/3.0.1/creationInfo_" + i.name)
        g.add((ci_node, RDF.type, ci_ref("CreationInfo")))
        g.add((ci_node, RDFS.comment, Literal("This individual element was defined by the spec.", lang="en")))
        g.add((ci_node, ci_ref("created"), Literal("2024-11-22T03:00:01Z", datatype=XSD.dateTimeStamp)))
        g.add((ci_node, ci_ref("createdBy"), ci_ref("SpdxOrganization")))
        g.add((ci_node, ci_ref("specVersion"), Literal("3.0.1")))
        node = URIRef(i.iri)
        g.add((node, RDF.type, OWL.NamedIndividual))
        g.add((node, ci_ref("creationInfo"), ci_node))
        if i.summary:
            g.add((node, RDFS.comment, Literal(i.summary, lang="en")))
        typ = i.metadata["type"]
        typename = "" if typ.startswith("/") else f"/{i.ns.name}/"
        typename += typ
        dt = model.types[typename]
        g.add((node, RDF.type, URIRef(dt.iri)))
        custom_iri = i.metadata.get("IRI")
        if custom_iri and custom_iri != i.iri:
            g.add((node, OWL.sameAs, URIRef(custom_iri)))


def jsonld_context(g, base_uri):
    prefixes = dict()
    definitions = dict()

    def get_subject_term(subject):
        if (subject, RDF.type, OWL.ObjectProperty) in g:
            for _, _, o in g.triples((subject, RDFS.range, None)):
                if (o, RDF.type, SH.NodeShape) in g:
                    return {
                        "@id": subject,
                        "@type": "@id",
                    }
                elif o in vocab_classes:
                    ctx = {}
                    for ind in sorted(g.subjects(RDF.type, o)):
                        key = get_key(ind)
                        if key:
                            local_name = str(ind).split("/")[-1]
                            ctx[local_name] = key
                    return {
                        "@id": subject,
                        "@type": "@vocab",
                        "@context": ctx,
                    }
                elif (o, RDF.type, OWL.Class) in g:
                    return {
                        "@id": subject,
                        "@type": "@id",
                    }
        elif (subject, RDF.type, OWL.DatatypeProperty) in g:
            for _, _, o in g.triples((subject, RDFS.range, None)):
                if o == XSD.string:
                    return {
                        "@id": subject,
                    }
                return {
                    "@id": subject,
                    "@type": o,
                }

        return subject

    vocab_named_individuals = set()
    vocab_classes = set()
    for s in g.subjects(RDF.type, OWL.NamedIndividual):
        if not str(s).startswith(base_uri):
            continue
        vocab_named_individuals.add(s)
        for typ in g.objects(s, RDF.type):
            if typ == OWL.NamedIndividual:
                continue
            vocab_classes.add(typ)

    def get_key(subject):
        s_str = str(subject)
        if not s_str.startswith(base_uri):
            return None

        rel_path = s_str[len(base_uri) :]
        parts = rel_path.split("/")

        vocab = None
        if len(parts) == 2:
            ns, name = parts
        elif len(parts) == 3:
            ns, vocab, name = parts
            if subject not in vocab_named_individuals:
                return None
        else:
            return None

        if ns == "Core":
            key = f"{vocab}_{name}" if vocab else name
        else:
            key = f"{ns.lower()}_{vocab}_{name}" if vocab else f"{ns.lower()}_{name}"

        return key

    profiles = set()
    for s in g.subjects():
        s_str = str(s)
        if s_str.startswith(base_uri):
            rel = s_str[len(base_uri) :]
            if "/" in rel:
                profiles.add(rel.split("/")[0])

    prefix_map = {}
    for p, prefix in PROFILE_NAMESPACE_PREFIX.items():
        if p in profiles:
            ns = f"{base_uri}{p}/"
            prefixes[prefix] = ns
            prefix_map[ns] = f"{prefix}:"

    for p in sorted(profiles):
        if p not in PROFILE_NAMESPACE_PREFIX:
            prefix = f"spdx-{p.lower()}"
            ns = f"{base_uri}{p}/"
            prefixes[prefix] = ns
            prefix_map[ns] = f"{prefix}:"

    prefixes["spdx"] = base_uri
    prefixes["xsd"] = str(XSD)
    prefix_map[str(XSD)] = "xsd:"

    for subject in sorted(g.subjects(unique=True)):
        key = get_key(subject)
        if not key:
            continue

        if key in definitions:
            current = definitions[key]["@id"] if isinstance(definitions[key], dict) else definitions[key]
            logger.error(f"ERROR: Duplicate context key '{key}' for '{subject}'. Already mapped to '{current}'")
            continue

        if key in prefixes:
            logger.error(f"ERROR: Context key '{key}' for '{subject}' collides with a namespace prefix.")
            continue

        t = get_subject_term(subject)
        if not isinstance(t, dict):
            t = {"@id": t}

        uid = str(t["@id"])
        for ns, pfx in prefix_map.items():
            if uid.startswith(ns):
                t["@id"] = f"{pfx}{uid[len(ns) :]}"
                break

        if "@type" in t:
            utype = str(t["@type"])
            for ns, pfx in prefix_map.items():
                if utype.startswith(ns):
                    t["@type"] = f"{pfx}{utype[len(ns) :]}"
                    break

        t["@protected"] = True

        definitions[key] = t

    definitions["spdxId"] = {
        "@id": "@id",
        "@protected": True,
    }
    definitions["type"] = {
        "@id": "@type",
        "@protected": True,
    }

    terms = prefixes.copy()
    for k in sorted(definitions.keys()):
        terms[k] = definitions[k]

    return {"@context": terms}
