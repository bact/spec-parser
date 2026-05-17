# SPDX-License-Identifier: Apache-2.0
"""Generate MkDocs site input: per-term pages, navigation YAML, and class hierarchy page."""

from __future__ import annotations

import logging
from pathlib import Path
from typing import TYPE_CHECKING, Any

from jinja2 import Environment, PackageLoader, select_autoescape

if TYPE_CHECKING:
    from .model import Model

logger = logging.getLogger(__name__)


def gen_mkdocs(model: Model, outpath: Path, cfg: Any) -> None:
    """Write per-term Markdown pages, the hierarchy page, and the MkDocs nav YAML."""
    jinja = Environment(
        loader=PackageLoader("spec_parser", package_path="templates/mkdocs"),
        autoescape=select_autoescape(),
        trim_blocks=True,
        lstrip_blocks=True,
    )
    jinja.globals = cfg.all_as_dict
    jinja.globals["class_link"] = class_link
    jinja.globals["property_link"] = property_link
    jinja.globals["ext_property_link"] = ext_property_link
    jinja.globals["type_link"] = lambda x, showshort=False: type_link(x, model, showshort=showshort)
    jinja.globals["not_none"] = lambda x: str(x) if x is not None else ""

    for ns in model.namespaces:
        d = outpath / ns.name
        d.mkdir()
        f = d / f"{ns.name}.md"
        template = jinja.get_template("namespace.md.j2")
        f.write_text(template.render(vars(ns)))

    def _generate_in_dir(dirname: str, group: dict[str, Any], tmplfname: str) -> None:
        for s in group.values():
            d = outpath / s.ns.name / dirname
            d.mkdir(exist_ok=True)
            f = d / f"{s.name}.md"
            template = jinja.get_template(tmplfname)
            f.write_text(template.render(vars(s)))

    _generate_in_dir("Classes", model.classes, "class.md.j2")
    _generate_in_dir("Properties", model.properties, "property.md.j2")
    _generate_in_dir("Vocabularies", model.vocabularies, "vocabulary.md.j2")
    _generate_in_dir("Individuals", model.individuals, "individual.md.j2")
    _generate_in_dir("Datatypes", model.datatypes, "datatype.md.j2")

    def _gen_filelist(nsname: str, itemslist: dict[str, Any], heading: str) -> list[str]:
        nameslist = [c.name for c in itemslist.values()]
        if not nameslist:
            return []
        lines = [f"    - {heading}:"]
        lines.extend(f"      - '{n}': model/{nsname}/{heading}/{n}.md" for n in sorted(nameslist))
        return lines

    all_ns_names = [ns.name for ns in model.namespaces]
    files: dict[str, list[str]] = {}
    for ns in model.namespaces:
        nsn = ns.name
        lines: list[str] = [f"  - {nsn}:", f"    - 'Description': model/{nsn}/{nsn}.md"]
        lines.extend(_gen_filelist(nsn, ns.classes, "Classes"))
        lines.extend(_gen_filelist(nsn, ns.properties, "Properties"))
        lines.extend(_gen_filelist(nsn, ns.vocabularies, "Vocabularies"))
        lines.extend(_gen_filelist(nsn, ns.individuals, "Individuals"))
        lines.extend(_gen_filelist(nsn, ns.datatypes, "Datatypes"))
        files[nsn] = lines

    filelines: list[str] = ["- model:"]
    for nsname in all_ns_names:
        filelines.extend(files[nsname])

    fn = outpath / "class-hierarchy.md"
    template = jinja.get_template("hierarchy.md.j2")
    fn.write_text(template.render(vars(model)))

    fn = outpath / "model-files.yml"
    fn.write_text("\n".join(filelines))


def class_link(name: str) -> str:
    """Return a Markdown link to the class page for *name*."""
    if name.startswith("/"):
        _, other_ns, name = name.split("/")
        return f"[/{other_ns}/{name}](../../{other_ns}/Classes/{name}.md)"
    return f"[{name}](../Classes/{name}.md)"


def property_link(name: str, *, showshort: bool = False) -> str:
    """Return a Markdown link to the property page for *name*."""
    if name.startswith("/"):
        _, other_ns, name = name.split("/")
        showname = name if showshort else f"/{other_ns}/{name}"
        return f"[{showname}](../../{other_ns}/Properties/{name}.md)"
    return f"[{name}](../Properties/{name}.md)"


def ext_property_link(name: str) -> str:
    """Return a Markdown link for an external property restriction *name* (``/ns/Class/prop``)."""
    _, pns, pclass, pname = name.split("/")
    return (
        f"[{pname}](../../{pns}/Properties/{pname}.md)"
        f" from [/{pns}/{pclass}](../../{pns}/Classes/{pclass}.md)"
    )


def type_link(name: str, model: Model, *, showshort: bool = False) -> str:
    """Return a Markdown link to the correct type page (class, vocabulary, or datatype)."""
    if name.startswith("/"):
        dirname = "Classes"
        if name in model.vocabularies:
            dirname = "Vocabularies"
        elif name in model.datatypes:
            dirname = "Datatypes"
        _, other_ns, name = name.split("/")
        showname = name if showshort else f"/{other_ns}/{name}"
        return f"[{showname}](../../{other_ns}/{dirname}/{name}.md)"
    if name[0].isupper():
        dirname = "Classes"
        if any(x.endswith("/" + name) for x in model.vocabularies):
            dirname = "Vocabularies"
        elif any(x.endswith("/" + name) for x in model.datatypes):
            dirname = "Datatypes"
        return f"[{name}](../{dirname}/{name}.md)"
    return name
