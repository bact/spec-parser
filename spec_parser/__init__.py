# SPDX-License-Identifier: Apache-2.0
"""spec_parser — parse a specification written in Markdown and generate RDF ontology, MkDocs pages, and diagrams."""

__version__ = "3.0.1"

from .model import Instantiability, Model, PropertyNature

__all__ = ["Instantiability", "Model", "PropertyNature", "__version__"]
