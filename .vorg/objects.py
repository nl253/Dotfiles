#!/usr/bin/env python
# -*- coding: utf-8 -*-

import re
from typing import Any, Tuple, Optional


class VorgNode:

    indent_regexp = r"(^( {4}|\t)*)"

    def __init__(self, raw_text: str):
        self._raw: str = raw_text
        self._indent = self._get_indent(raw_text)

    @property
    def indent(self) -> Any:
        return self._indent

    @property
    def raw(self) -> str:
        return self._raw

    @staticmethod
    def _get_indent(raw_text) -> int:

        indent_regexp: str = r"^( {4}|\t)*"
        match = re.compile(indent_regexp).search(raw_text)
        indentation = match.group(0)
        # repace tabs with spaces
        indentation: str = indentation.replace("\t", "    ")
        return indentation.count(" ")

    def __str__(self) -> str:
        return self.raw


class VorgMeta(VorgNode, dict):

    meta_regex = f"(?P<meta>{VorgNode.indent_regexp}:(?P<tag>\w+): (?P<values>.*?)$)"
    metas_regex = f"(?P<metas>({meta_regex}\\n)*)"

    def __init__(self, raw_text: str):
        super().__init__(raw_text)
        self._parse()

    def _parse(self) -> dict:
        pattern = re.compile(self.meta_regex, re.M)
        for match in pattern.finditer(self.raw):
            if match.group("tag"):
                self[match.group("tag")] = match.group("values") if match.group('values') else True


class VorgHeading(VorgNode):

    heading_regex: str = f"(?P<heading>{VorgNode.indent_regexp}(?P<level>[\(>]+)(?: )(?P<title>.*?)$)"

    def __init__(self, raw_text: str):
        super().__init__(raw_text)
        self._level, self._title = VorgHeading._parse(raw_text)

    @property
    def title(self) -> Any:
        return self._title

    @property
    def level(self) -> int:
        return self._level

    @staticmethod
    def _parse(raw_text: str) -> Tuple[int, str]:
        heading_regex: str = VorgHeading.heading_regex
        regex = re.compile(heading_regex, re.M)
        match = regex.search(raw_text)
        title: str = match.group('title')
        markers: str = match.group('level')
        level: int = markers.count('>') + markers.count('(')

        return level, title


class VorgSection:

    section_regex: str = f"{VorgHeading.heading_regex}\\n{VorgMeta.metas_regex}(?P<content>.*)"

    def __init__(self, raw_text):
        self._raw = raw_text
        self._heading, self._meta, self._content = VorgSection._parse_section(raw_text)
        self._sections: List[VorgSection] = []

    @staticmethod
    def _parse_section(section_text: str) -> Tuple[Optional[VorgHeading], Optional[VorgMeta], Optional[str]]:
        """ Use regexp to separate heading from meta from content.
        """

        pattern: str = VorgSection.section_regex

        regex = re.compile(pattern, re.M|re.S)

        match = regex.search(section_text)

        if not match:
            return "", "", ""

        h = VorgHeading(match.group("heading")) if VorgHeading(match.group("heading")) else ""
        m = VorgMeta(match.group("metas"))
        c = match.group("content")

        return h, m, c

    @property
    def content(self) -> str:
        return self._content

    @property
    def meta(self) -> str:
        return self._meta

    @property
    def title(self) -> str:
        try:
            return self.heading.title
        except AttributeError as e:
            print(e)
            return ""

    @property
    def level(self) -> Any:
        try:
            return self.heading.level
        except AttributeError as e:
            print(e)
            return ""

    @property
    def heading(self) -> Any:
        return self._heading

    @property
    def raw(self) -> Any:
        return self._raw

    def __str__(self) -> str:
        return self.raw

# vim: nu
