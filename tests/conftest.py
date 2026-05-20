# SPDX-License-Identifier: Apache-2.0

from __future__ import annotations

from pathlib import Path

import pytest

from spec_parser.model import Model

FIXTURE_MODEL = Path(__file__).parent / "fixtures" / "model"
FIXTURE_EXAMPLES = Path(__file__).parent / "fixtures" / "spdx3-examples"


@pytest.fixture(scope="session")
def fixture_model_path() -> Path:
    return FIXTURE_MODEL


@pytest.fixture(scope="session")
def model() -> Model:
    return Model(FIXTURE_MODEL)


@pytest.fixture(scope="session")
def rdf_graph(model: Model):  # type: ignore[type-arg]
    from spec_parser.rdf import gen_rdf_ontology
    return gen_rdf_ontology(model)


@pytest.fixture(params=list(FIXTURE_EXAMPLES.glob("*.json")), ids=lambda p: p.name)
def spdx3_example(request: pytest.FixtureRequest) -> Path:
    return request.param  # type: ignore[return-value]
