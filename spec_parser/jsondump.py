# SPDX-License-Identifier: Apache-2.0
"""Serialise the parsed model to JSON via jsonpickle."""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING, Any

import jsonpickle

if TYPE_CHECKING:
    from pathlib import Path

    from .model import Model

logger = logging.getLogger(__name__)


def gen_jsondump(model: Model, outpath: Path, cfg: Any) -> None:  # pylint: disable=unused-argument
    """Write the full model as a jsonpickle-encoded JSON file."""
    f = outpath / "model.json"
    f.write_text(jsonpickle.encode(model, indent=2, warn=True))
