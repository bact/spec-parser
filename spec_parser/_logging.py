# SPDX-License-Identifier: Apache-2.0
"""Logging setup: a counting handler that tracks error/warning totals and stores error messages."""

import logging
from typing import ClassVar


class LogCountingHandler(logging.StreamHandler):  # type: ignore[type-arg]
    """Stream handler that counts emitted records by level and stores error messages."""

    _LEVELS: ClassVar[tuple[int, ...]] = (
        logging.DEBUG,
        logging.INFO,
        logging.WARNING,
        logging.ERROR,
        logging.CRITICAL,
    )

    def __init__(self) -> None:
        super().__init__()
        self.count: dict[int, int] = dict.fromkeys(self._LEVELS, 0)
        self._messages: dict[int, list[str]] = {lvl: [] for lvl in self._LEVELS}

    def emit(self, record: logging.LogRecord) -> None:
        """Count and store the record, then forward it to the stream."""
        self.count[record.levelno] += 1
        self._messages[record.levelno].append(self.format(record))
        super().emit(record)

    def num_errors(self) -> int:
        """Return the total number of ERROR-level records emitted."""
        return self.count[logging.ERROR]

    def num_warnings(self) -> int:
        """Return the total number of WARNING-level records emitted."""
        return self.count[logging.WARNING]

    def error_messages(self) -> list[str]:
        """Return a copy of all formatted ERROR messages in emission order."""
        return list(self._messages[logging.ERROR])

    def has_errors(self) -> bool:
        """Return True if at least one ERROR-level record has been emitted."""
        return self.count[logging.ERROR] > 0


def setup_logging(*, quiet: bool = False) -> tuple[logging.Logger, LogCountingHandler]:
    """Set up root logger with a counting handler.

    Returns the root logger and the counting handler so callers can inspect counts.
    """
    handler = LogCountingHandler()
    handler.setFormatter(logging.Formatter("%(levelname)s: %(message)s"))
    root = logging.getLogger()
    root.addHandler(handler)

    if quiet:
        root.setLevel(logging.WARNING)
        logging.getLogger("rdflib").setLevel(logging.CRITICAL)
    else:
        logging.getLogger("rdflib").setLevel(logging.ERROR)

    return root, handler
