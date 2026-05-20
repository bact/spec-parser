# SPDX-License-Identifier: Apache-2.0
"""Generate PlantUML class diagram input from the model."""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING, Any

if TYPE_CHECKING:
    from pathlib import Path

    from .model import Model

logger = logging.getLogger(__name__)


def gen_plantuml(model: Model, outpath: Path, cfg: Any) -> None:  # pylint: disable=unused-argument
    """Write a single ``model.plantuml`` file describing the full class diagram."""
    f = outpath / "model.plantuml"

    s = f"""
@startuml
'{cfg.autogen_header}

title SPDXv3 model
scale 4000*4000
hide methods
skinparam packageStyle folder

"""
    for ns in model.namespaces:
        s += f"package {ns.name} {{\n}}\n"

    inheritances: list[tuple[str, str]] = []
    prop2class: list[tuple[str, str]] = []
    for c in model.classes.values():
        if c.metadata.get("abstract") == "true":
            s += "abstract "
        else:
            s += "class "
        s += f"{c.ns.name}.{c.name} {{\n"
        if "subclassOf" in c.metadata:
            parent = c.metadata["subclassOf"]
            inheritances.append((f"{c.ns.name}.{c.name}", parent.split("/")[-1]))
        for p in sorted(c.properties):
            s += f'\t{p} {c.properties[p]["minCount"]}:{c.properties[p]["maxCount"]}\n'
            t = c.properties[p]["type"]
            if ":" not in t:
                prop2class.append((f"{c.ns.name}.{c.name}::{p}", t.split("/")[-1]))
        s += "}\n"

    for v in model.vocabularies.values():
        s += f"enum {v.ns.name}.{v.name} {{\n}}\n"

    for d in model.datatypes.values():
        s += f"class {d.ns.name}.{d.name} {{\n}}\n"

    for l, r in inheritances:
        s += f"{l} --|> {r}\n"
    for l, r in prop2class:
        s += f"{l} --> {r}\n"

    s += "\n@enduml\n"

    f.write_text(s)
