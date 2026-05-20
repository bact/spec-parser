# SPDX-License-Identifier: Apache-2.0
"""Generate standalone web pages from the model (stub — delegates to MkDocs pipeline)."""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

if TYPE_CHECKING:
    from pathlib import Path

    from .model import Model


def gen_webpages(model: Model, outpath: Path, cfg: Any) -> None:  # pylint: disable=unused-argument
    """Stub — web-page generation delegates to the MkDocs pipeline."""
