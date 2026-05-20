# SPDX-License-Identifier: Apache-2.0
"""Entry point for ``python -m spec_parser`` and the ``spec-parser`` CLI command."""

import sys

from spec_parser._logging import setup_logging
from spec_parser._params import RunParams
from spec_parser.model import Model


def main() -> None:
    """Parse a spec model directory and generate the requested output artefacts."""
    # --quiet is pre-scanned before RunParams so the handler is quiet from the start.
    quiet = "--quiet" in sys.argv or "-q" in sys.argv
    root_logger, handler = setup_logging(quiet=quiet)
    root_logger.info("spec-parser starts.")

    cfg = RunParams("spec-parser")
    if handler.has_errors():
        root_logger.error("Errors during parameter processing. Exiting.")
        sys.exit(1)

    cfg.create_output_dirs()
    if handler.has_errors():
        root_logger.error("Errors creating output directories. Exiting.")
        sys.exit(1)

    m = Model(cfg.input_path)
    if handler.has_errors():
        root_logger.error("Errors loading the model. Exiting.")
        sys.exit(1)

    if not cfg.no_output:
        m.generate(cfg)

    if handler.has_errors():
        errors = handler.error_messages()
        sys.stderr.write(f"\nTotal errors: {len(errors)}\n")
        for i, msg in enumerate(errors, 1):
            sys.stderr.write(f"  {i}: {msg}\n")
        sys.exit(1)


if __name__ == "__main__":
    main()
