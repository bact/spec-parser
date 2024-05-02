# parsing the spec .md files

# SPDX-License-Identifier: Apache-2.0

import logging
import re


class SpecFile:
    RE_SPLIT_TO_SECTIONS = re.compile(r"\n(?=(?:\Z|# |## ))")
    RE_EXTRACT_LICENSE = re.compile(r"\s*SPDX-License-Identifier\s*:\s+(.+)\s*")
    RE_EXTRACT_NAME = re.compile(r"#\s+(\w+)\s*")
    RE_EXTRACT_HEADER_CONTENT = re.compile(r"##\s+(.*)\s+((.|\s)+)")

    def __init__(self, fpath=None):
        self.license = None
        self.sections = dict()
        if fpath is not None:
            self.load(fpath)

    def load(self, fpath):
        logging.debug(f"### loading {fpath.parent}/{fpath.name}")
        filecontent = fpath.read_text(encoding="utf-8")

        parts = re.split(self.RE_SPLIT_TO_SECTIONS, filecontent)

        m = re.fullmatch(self.RE_EXTRACT_LICENSE, parts[0])
        if m is None:
            logging.error(f"File {fpath!s} does not start with license.")
        else:
            self.license = m.group(1)

        m = re.fullmatch(self.RE_EXTRACT_NAME, parts[1])
        if m is None:
            logging.error(f"File {fpath!s} does not have name after license.")
        else:
            self.name = m.group(1)

        for p in parts[2:]:
            if p.strip():
                m = re.fullmatch(self.RE_EXTRACT_HEADER_CONTENT, p)
                header = m.group(1)
                content = m.group(2).strip()
                if content:
                    self.sections[header] = content


class Section:
    def __init__(self, content):
        if content is not None:
            self.load(content)


class ContentSection(Section):
    def load(self, content):
        self.content = content


class SingleListSection(Section):
    RE_EXTRACT_KEY_VALUE = re.compile(r"-\s+(\w+):\s+(.+)")

    def load(self, content):
        self.content = content
        self.kv = dict()

        lines = content.splitlines()
        lines_len = len(lines)
        i = 0
        while i < lines_len:
            m = re.fullmatch(self.RE_EXTRACT_KEY_VALUE, lines[i])
            if m is None:
                logging.error(f"Single list parsing error in line `{lines[i]}'")
                i = i + 1
            else:
                key = m.group(1)
                val = m.group(2).strip()  # first line of the value

                # until detect next list item,
                # consume the remaining lines of multiline-value
                j = i + 1
                while (j < lines_len) and \
                        (len(lines[j].strip()) > 0) and \
                            (not lines[j].startswith("-")):
                    val = val + " " + lines[j].strip()
                    j = j + 1

                self.kv[key] = val

                i = j  # jump i to the last line of the multiline-value


class NestedListSection(Section):
    RE_EXTRACT_TOP_LEVEL = re.compile(r"-\s+((\w|/)+)")
    RE_EXTRACT_KEY_VALUE = re.compile(r"\s+-\s+(\w+):\s+(.+)")

    def load(self, content):
        self.content = content
        self.ikv = dict()

        lines = content.splitlines()
        lines_len = len(lines)
        i = 0
        while i < lines_len:
            l = lines[i]
            if l.startswith("-"):
                m = re.fullmatch(self.RE_EXTRACT_TOP_LEVEL, l)
                if m is None:
                    logging.error(f"Top-level nested list parsing error in line `{l}'")
                else:
                    item = m.group(1)
                    self.ikv[item] = dict()
                i = i + 1
            else:
                m = re.fullmatch(self.RE_EXTRACT_KEY_VALUE, l)
                if m is None:
                    logging.error(f"Nested list parsing error in line `{l}'")
                    i = i + 1
                else:
                    key = m.group(1)
                    val = m.group(2).strip()  # first line of the value

                    # until detect next list item,
                    # consume the remaining lines of multiline-value
                    j = i + 1
                    while (j < lines_len) and \
                            (len(lines[j].strip()) > 0) and \
                                (not lines[j].startswith("-")) and \
                                    (not re.fullmatch(self.RE_EXTRACT_KEY_VALUE, lines[j])):
                        val = val + " " + lines[j].strip()
                        j = j + 1

                    self.ikv[item][key] = val

                    i = j  # jump i to the last line of the multiline-value

if __name__ == "__main__":
    fn = "/home/zvr/github/spdx/spdx-3-model/model/Core/Classes/Element.md"
