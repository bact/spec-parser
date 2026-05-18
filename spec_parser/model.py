# SPDX-License-Identifier: Apache-2.0
"""In-memory model of the parsed spec: namespaces, classes, properties, vocabularies, datatypes, individuals, and inheritance."""

from __future__ import annotations

import logging
from collections import defaultdict
from copy import deepcopy
from enum import StrEnum
from pathlib import Path
from typing import Any

from .mdparsing import ContentSection, NestedListSection, SingleListSection, SpecFile, VocabularySection

logger = logging.getLogger(__name__)


class PropertyNature(StrEnum):
    """Allowed values for the ``Nature`` metadata field on properties."""
    OBJECT_PROPERTY = "ObjectProperty"
    DATA_PROPERTY = "DataProperty"


class Model:
    """Top-level container for an entire parsed spec: all namespaces and their contents."""

    def __init__(self, inpath: Path | None = None) -> None:
        self.name: str | None = None
        self.namespaces: list[Namespace] = []
        self.classes: dict[str, Class] = {}
        self.properties: dict[str, Property] = {}
        self.vocabularies: dict[str, Vocabulary] = {}
        self.individuals: dict[str, Individual] = {}
        self.datatypes: dict[str, Datatype] = {}
        self.types: dict[str, Class | Vocabulary | Datatype] = {}
        self.class_hierarchy: dict[str, list[str]] = {}
        self.toplevel_classes: list[str] = []
        self.base_uri: str = ""

        if inpath is not None:
            self.load(inpath)

    def load(self, inpath: Path) -> None:
        """Scan *inpath* for namespace directories and populate all model collections."""
        for d in [d for d in inpath.iterdir() if d.is_dir() and d.name[0].isupper()]:
            nsp = inpath / d.name / f"{d.name}.md"
            if not nsp.is_file():
                logger.error("Missing top-level namespace file: %s", nsp)
                continue

            ns = Namespace(nsp)
            self.namespaces.append(ns)

            dp = inpath / d.name / "Classes"
            if dp.is_dir():
                for f in [f for f in dp.iterdir() if f.is_file() and f.name[0].isupper() and f.name.endswith(".md")]:
                    n = Class(f, ns)
                    k = n.fqname
                    self.classes[k] = n
                    ns.classes[k] = n

            dp = inpath / d.name / "Properties"
            if dp.is_dir():
                for f in [f for f in dp.iterdir() if f.is_file() and f.name[0].islower() and f.name.endswith(".md")]:
                    n = Property(f, ns)
                    k = n.fqname
                    self.properties[k] = n
                    ns.properties[k] = n

            dp = inpath / d.name / "Vocabularies"
            if dp.is_dir():
                for f in [f for f in dp.iterdir() if f.is_file() and f.name[0].isupper() and f.name.endswith(".md")]:
                    n = Vocabulary(f, ns)
                    k = n.fqname
                    self.vocabularies[k] = n
                    ns.vocabularies[k] = n

            dp = inpath / d.name / "Individuals"
            if dp.is_dir():
                for f in [f for f in dp.iterdir() if f.is_file() and f.name[0].isupper() and f.name.endswith(".md")]:
                    n = Individual(f, ns)
                    k = n.fqname
                    self.individuals[k] = n
                    ns.individuals[k] = n

            dp = inpath / d.name / "Datatypes"
            if dp.is_dir():
                for f in [f for f in dp.iterdir() if f.is_file() and f.name[0].isupper() and f.name.endswith(".md")]:
                    n = Datatype(f, ns)
                    k = n.fqname
                    self.datatypes[k] = n
                    ns.datatypes[k] = n

        logger.info(
            "Loaded %d namespaces, %d classes, %d properties, %d vocabularies, %d individuals, %d datatypes",
            len(self.namespaces),
            len(self.classes),
            len(self.properties),
            len(self.vocabularies),
            len(self.individuals),
            len(self.datatypes),
        )
        self._process_after_load()

    def _process_after_load(self) -> None:
        self.types = {**self.classes, **self.vocabularies, **self.datatypes}
        logger.info("Total %d types", len(self.types))

        self.base_uri = self._derive_base_uri()

        for c in self.classes.values():
            for p in c.properties:
                pname = p if p.startswith("/") else f"/{c.ns.name}/{p}"
                self.properties[pname].used_in.append(c.fqname)

        inheritances: list[tuple[str, str]] = []
        for c in self.classes.values():
            parent = c.fqsupercname
            if parent:
                inheritances.append((c.fqname, parent))
                if parent in self.classes:
                    self.classes[parent].direct_subclasses.append(c.fqname)

        tree: dict[str, list[str]] = defaultdict(list)
        children: set[str] = set()
        nodes: set[str] = set()
        for child, parent in inheritances:
            tree[parent].append(child)
            children.add(child)
            nodes.add(parent)
            nodes.add(child)
        self.class_hierarchy = dict(tree)
        self.toplevel_classes = list(nodes - children)

        # Topological sort so that parent classes are processed before children.
        visited: dict[str, bool] = {c.fqname: False for c in self.classes.values()}
        stack: list[str] = []

        def _tsort(cn: str) -> None:
            visited[cn] = True
            for chd, par in inheritances:
                if chd == cn and not visited[par]:
                    _tsort(par)
            stack.append(cn)

        for c in self.classes.values():
            if not visited[c.fqname]:
                _tsort(c.fqname)

        for cn in stack:
            c = self.classes[cn]
            pcn = c.fqsupercname
            while pcn:
                c.inheritance_stack.append(pcn)
                pcn = self.classes[pcn].fqsupercname

        for cn in stack:
            c = self.classes[cn]
            c.all_properties = {}
            for p, pkv in c.properties.items():
                shortname = p.rpartition("/")[-1]
                fullname = p if p.startswith("/") else f"/{c.ns.name}/{p}"
                prop = self.properties[fullname]
                prop_range = prop.metadata["range"]
                fulltype = (
                    prop_range
                    if prop_range.startswith(("/", "xsd:"))
                    else f"/{prop.ns.name}/{prop_range}"
                )
                c.all_properties[shortname] = deepcopy(pkv)
                c.all_properties[shortname]["fullname"] = fullname
                c.all_properties[shortname]["fulltype"] = fulltype

            if c.inheritance_stack:
                parent_cn = c.inheritance_stack[0]
                c.all_properties.update(deepcopy(self.classes[parent_cn].all_properties))

            for p, pkv in c.ext_prop_restrs.items():
                (_, pns, _, shortname) = p.split("/")
                assert c.all_properties[shortname]["fullname"] == f"/{pns}/{shortname}"
                for k, v in pkv.items():
                    if c.all_properties[shortname][k] == v:
                        logger.warning(
                            "In class %s property %s has same %s as the parent class",
                            c.fqname, p, k,
                        )
                    c.all_properties[shortname][k] = v

    def _derive_base_uri(self) -> str:
        """Derive the ontology base URI from the Core namespace IRI, if present."""
        for ns in self.namespaces:
            iri = ns.metadata.get("id", "")
            if iri.endswith("/Core/"):
                return iri[: -len("Core/")]
            if iri.endswith("/Core"):
                prefix = iri[: -len("Core")]
                return prefix if prefix.endswith("/") else prefix + "/"
        # Fall back: use the longest common prefix of all namespace IRIs.
        iris = [ns.metadata.get("id", "") for ns in self.namespaces if ns.metadata.get("id")]
        if not iris:
            return ""
        common = iris[0]
        for iri in iris[1:]:
            while not iri.startswith(common):
                common = common[: common.rfind("/", 0, -1) + 1]
        logger.warning("No Core namespace found; derived base URI: %s", common)
        return common

    def generate(self, cfg: Any) -> None:
        """Invoke each enabled generator in turn, passing *cfg* for output paths."""
        if cfg.generate_jsondump:
            from .jsondump import gen_jsondump
            gen_jsondump(self, cfg.output_jsondump_path, cfg)
        if cfg.generate_mkdocs:
            from .mkdocs import gen_mkdocs
            gen_mkdocs(self, cfg.output_mkdocs_path, cfg)
        if cfg.generate_plantuml:
            from .plantuml import gen_plantuml
            gen_plantuml(self, cfg.output_plantuml_path, cfg)
        if cfg.generate_rdf:
            from .rdf import gen_rdf
            gen_rdf(self, cfg.output_rdf_path, cfg)
        if cfg.generate_tex:
            from .tex import gen_tex
            gen_tex(self, cfg.output_tex_path, cfg)
        if cfg.generate_webpages:
            from .webpages import gen_webpages
            gen_webpages(self, cfg.output_webpages_path, cfg)
        if cfg.generate_singlefile:
            from .singlefile import gen_singlefile
            gen_singlefile(self, cfg.output_singlefile_path, cfg)


class Namespace:
    """A single namespace directory (e.g. ``Core``), holding all its model elements."""

    def __init__(self, fname: Path) -> None:
        self.classes: dict[str, Class] = {}
        self.properties: dict[str, Property] = {}
        self.vocabularies: dict[str, Vocabulary] = {}
        self.individuals: dict[str, Individual] = {}
        self.datatypes: dict[str, Datatype] = {}

        sf = SpecFile(fname)
        self.license: str | None = sf.license
        self.name: str = sf.name

        s = ContentSection(sf.sections["Summary"])
        self.summary: str = s.content

        s = ContentSection(sf.sections["Description"])
        self.description: str = s.content

        s = SingleListSection(sf.sections["Metadata"])
        self.metadata: dict[str, str] = s.kv

        if "Profile conformance" in sf.sections:
            s = ContentSection(sf.sections["Profile conformance"])
            self.conformance: str | None = s.content
        else:
            self.conformance = None

        assert self.name == self.metadata["name"], (
            f"Namespace name {self.name} does not match metadata {self.metadata['name']}"
        )

        self.iri: str = self.metadata["id"]


def _normalize_metadata(kv: dict[str, str], renames: dict[str, str]) -> dict[str, str]:
    """Rename keys case-insensitively according to *renames* (lowercase key → canonical key)."""
    result: dict[str, str] = {}
    for k, v in kv.items():
        result[renames.get(k.lower(), k)] = v
    return result


def _normalize_class_metadata(kv: dict[str, str]) -> dict[str, str]:
    """Normalise old-format metadata keys to canonical new-format keys.

    Accepted legacy forms (case-insensitive):
    - ``SubclassOf`` → ``subclassOf``
    - ``Instantiability: Abstract`` → ``abstract: true``
    - ``Instantiability: Concrete`` → omitted (false is the default)
    """
    result: dict[str, str] = {}
    for k, v in kv.items():
        kl = k.lower()
        if kl == "subclassof":
            result["subclassOf"] = v
        elif kl == "instantiability":
            if v.lower() == "abstract":
                result["abstract"] = "true"
            # Concrete → omit; false is the default
        else:
            result[k] = v
    return result


class Class:
    """A class definition parsed from a ``Classes/<Name>.md`` file."""

    VALID_METADATA: tuple[str, ...] = (
        "abstract",
        "deprecated",
        "deprecatedVersion",
        "isReplacedBy",
        "name",
        "sinceVersion",
        "subclassOf",
    )
    VALID_PROP_METADATA: tuple[str, ...] = (
        "maxCount",
        "minCount",
    )

    def __init__(self, fname: Path, ns: Namespace) -> None:
        self.ns: Namespace = ns

        sf = SpecFile(fname)
        self.license: str | None = sf.license
        self.name: str = sf.name
        self.fqname: str = f"/{ns.name}/{sf.name}"

        s = ContentSection(sf.sections["Summary"], filename=self.fqname, context="summary")
        self.summary: str = s.content

        s = ContentSection(sf.sections["Description"], filename=self.fqname, context="description")
        self.description: str = s.content

        s = SingleListSection(sf.sections["Metadata"], filename=self.fqname, context="metadata")
        self.metadata: dict[str, str] = _normalize_class_metadata(s.kv)

        if "Properties" in sf.sections:
            s2 = NestedListSection(sf.sections["Properties"], filename=self.fqname, context="properties")
            self.properties: dict[str, dict[str, str]] = s2.ikv
        else:
            self.properties = {}

        if "External properties restrictions" in sf.sections:
            s2 = NestedListSection(
                sf.sections["External properties restrictions"],
                filename=self.fqname,
                context="external properties restrictions",
            )
            self.ext_prop_restrs: dict[str, dict[str, str]] = s2.ikv
        else:
            self.ext_prop_restrs = {}

        if self.name != self.metadata.get("name", ""):
            logger.error("%s: heading name %r does not match metadata name: %r", fname, self.name, self.metadata.get("name"))
        for p in self.metadata:
            if p not in self.VALID_METADATA:
                logger.error("%s: unknown metadata key %r; expected one of: %s", fname, p, ", ".join(sorted(self.VALID_METADATA)))
        for prop, pkv in self.properties.items():
            for p in pkv:
                if p not in self.VALID_PROP_METADATA:
                    logger.error(
                        "%s: property %r has unknown attribute %r; expected one of: %s",
                        fname, prop, p, ", ".join(sorted(self.VALID_PROP_METADATA)),
                    )

        self.iri: str = f"{self.ns.iri}/{self.name}"
        if self.metadata.get("subclassOf") == "none":
            del self.metadata["subclassOf"]
        for prop in self.properties:
            self.properties[prop]["fqname"] = prop if prop.startswith("/") else f"/{ns.name}/{prop}"
            if "minCount" not in self.properties[prop]:
                self.properties[prop]["minCount"] = "0"
            if "maxCount" not in self.properties[prop]:
                self.properties[prop]["maxCount"] = "*"

        parent = self.metadata.get("subclassOf")
        if parent and not parent.startswith("/"):
            parent = f"/{ns.name}/{parent}"
        self.fqsupercname: str | None = parent

        self.inheritance_stack: list[str] = []
        self.direct_subclasses: list[str] = []
        self.all_properties: dict[str, dict[str, str]] = {}


class Property:
    """A property definition parsed from a ``Properties/<name>.md`` file."""

    _METADATA_RENAMES: dict[str, str] = {"nature": "nature", "range": "range"}

    VALID_METADATA: tuple[str, ...] = (
        "deprecated",
        "deprecatedVersion",
        "isReplacedBy",
        "name",
        "nature",
        "range",
        "sinceVersion",
    )
    REQUIRED_METADATA: tuple[str, ...] = ("name", "nature", "range")

    def __init__(self, fname: Path, ns: Namespace) -> None:
        self.ns: Namespace = ns

        sf = SpecFile(fname)
        self.license: str | None = sf.license
        self.name: str = sf.name
        self.fqname: str = f"/{ns.name}/{sf.name}"

        s = ContentSection(sf.sections["Summary"], filename=self.fqname, context="summary")
        self.summary: str = s.content

        s = ContentSection(sf.sections["Description"], filename=self.fqname, context="description")
        self.description: str = s.content

        s = SingleListSection(sf.sections["Metadata"], filename=self.fqname, context="metadata")
        self.metadata: dict[str, str] = _normalize_metadata(s.kv, self._METADATA_RENAMES)

        if self.name != self.metadata.get("name", ""):
            logger.error("%s: heading name %r does not match metadata name: %r", fname, self.name, self.metadata.get("name"))
        for p in self.metadata:
            if p not in self.VALID_METADATA:
                logger.error("%s: unknown metadata key %r; expected one of: %s", fname, p, ", ".join(sorted(self.VALID_METADATA)))
        for p in self.REQUIRED_METADATA:
            if p not in self.metadata:
                logger.error("%s: missing required metadata field %r", fname, p)

        self.iri: str = f"{self.ns.iri}/{self.name}"
        self.used_in: list[str] = []


class Vocabulary:
    """A closed enumeration parsed from a ``Vocabularies/<Name>.md`` file."""

    VALID_METADATA: tuple[str, ...] = (
        "deprecated",
        "deprecatedVersion",
        "isReplacedBy",
        "name",
        "sinceVersion",
    )

    def __init__(self, fname: Path, ns: Namespace) -> None:
        self.ns: Namespace = ns

        sf = SpecFile(fname)
        self.license: str | None = sf.license
        self.name: str = sf.name
        self.fqname: str = f"/{ns.name}/{sf.name}"

        s = ContentSection(sf.sections["Summary"], filename=self.fqname, context="summary")
        self.summary: str = s.content

        s = ContentSection(sf.sections["Description"], filename=self.fqname, context="description")
        self.description: str = s.content

        s = SingleListSection(sf.sections["Metadata"], filename=self.fqname, context="metadata")
        self.metadata: dict[str, str] = s.kv

        vs = VocabularySection(sf.sections["Entries"], filename=self.fqname, context="entries")
        self.entries: dict[str, dict[str, object]] = vs.entries

        if self.name != self.metadata.get("name", ""):
            logger.error("%s: heading name %r does not match metadata name: %r", fname, self.name, self.metadata.get("name"))
        for p in self.metadata:
            if p not in self.VALID_METADATA:
                logger.error("%s: unknown metadata key %r; expected one of: %s", fname, p, ", ".join(sorted(self.VALID_METADATA)))

        self.iri: str = f"{self.ns.iri}/{self.name}"


class Individual:
    """A named individual parsed from an ``Individuals/<Name>.md`` file."""

    _METADATA_RENAMES: dict[str, str] = {"iri": "iri"}

    VALID_METADATA: tuple[str, ...] = (
        "deprecated",
        "deprecatedVersion",
        "iri",
        "isReplacedBy",
        "name",
        "sameAs",
        "sinceVersion",
        "type",
    )

    def __init__(self, fname: Path, ns: Namespace) -> None:
        self.ns: Namespace = ns

        sf = SpecFile(fname)
        self.license: str | None = sf.license
        self.name: str = sf.name
        self.fqname: str = f"/{ns.name}/{sf.name}"

        s = ContentSection(sf.sections["Summary"], filename=self.fqname, context="summary")
        self.summary: str = s.content

        s = ContentSection(sf.sections["Description"], filename=self.fqname, context="description")
        self.description: str = s.content

        s = SingleListSection(sf.sections["Metadata"], filename=self.fqname, context="metadata")
        self.metadata: dict[str, str] = _normalize_metadata(s.kv, self._METADATA_RENAMES)

        s = SingleListSection(sf.sections["Property Values"], filename=self.fqname, context="property values")
        self.values: dict[str, str] = s.kv

        if self.name != self.metadata.get("name", ""):
            logger.error("%s: heading name %r does not match metadata name: %r", fname, self.name, self.metadata.get("name"))
        for p in self.metadata:
            if p not in self.VALID_METADATA:
                logger.error("%s: unknown metadata key %r; expected one of: %s", fname, p, ", ".join(sorted(self.VALID_METADATA)))

        self.iri: str = f"{self.ns.iri}/{self.name}"
        if "iri" not in self.metadata:
            self.metadata["iri"] = self.iri


class Datatype:
    """A scalar datatype definition parsed from a ``Datatypes/<Name>.md`` file."""

    _METADATA_RENAMES: dict[str, str] = {"subclassof": "subclassOf"}

    VALID_METADATA: tuple[str, ...] = (
        "deprecated",
        "deprecatedVersion",
        "isReplacedBy",
        "name",
        "sinceVersion",
        "subclassOf",
    )

    def __init__(self, fname: Path, ns: Namespace) -> None:
        self.ns: Namespace = ns

        sf = SpecFile(fname)
        self.license: str | None = sf.license
        self.name: str = sf.name
        self.fqname: str = f"/{ns.name}/{sf.name}"

        s = ContentSection(sf.sections["Summary"], filename=self.fqname, context="summary")
        self.summary: str = s.content

        s = ContentSection(sf.sections["Description"], filename=self.fqname, context="description")
        self.description: str = s.content

        s = SingleListSection(sf.sections["Metadata"], filename=self.fqname, context="metadata")
        self.metadata: dict[str, str] = _normalize_metadata(s.kv, self._METADATA_RENAMES)

        s = SingleListSection(sf.sections["Format"], filename=self.fqname, context="format")
        self.format: dict[str, str] = s.kv

        if self.name != self.metadata.get("name", ""):
            logger.error("%s: heading name %r does not match metadata name: %r", fname, self.name, self.metadata.get("name"))
        for p in self.metadata:
            if p not in self.VALID_METADATA:
                logger.error("%s: unknown metadata key %r; expected one of: %s", fname, p, ", ".join(sorted(self.VALID_METADATA)))

        self.iri: str = f"{self.ns.iri}/{self.name}"
