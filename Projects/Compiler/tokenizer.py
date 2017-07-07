#!/usr/bin/env python
# -*- coding: utf-8 -*-

import re
import collections
from pathlib import Path
from typing import Match, List, Iterable, Iterator

f = Path('./file.vs')
code: str = f.read_text()

token = collections.namedtuple('TOKEN', ['TYPE', 'DATUM'])

SKIP = r'(?P<SKIP>[ ])'
INT = r'(?P<INT>\d+)'
DOUBLE = r'(?P<DOUBLE>(\d+)?\.\d+d?)'
FLOAT = r'(?P<FLOAT>(\d+)?\.\d+f)'
IMAGINARY = r'(?P<IMAG>(\d+)?\.\d+i)'
BOOL = r'(?P<BOOL>' + "|".join(['true', 'false']) + ")"
NUMBER = r'(?P<NUMBER>' + "|".join([IMAGINARY, FLOAT, DOUBLE, INT]) + ")"
MATRIX = r'(?P<MATRIX>\d{1,2}(x\d{1,2})+)'
STRING = r'(?P<STRING>"\S+?")'
BOP = "(?P<BOP>" + "|".join([
    r'=~', r'==', r'\+', r'-', r'\*', r'=', '->', '=>', r'\*\*', '>>>', '>>',
    '<<', '<<<'
]) + ")"
UOP = "(?P<UOP>" + "|".join([r'\?', r'\!', r'\#']) + ")"
OP = "(?P<OP>" + "|".join([BOP, UOP]) + ")"
BUILTIN = "(?P<BUILTIN>" + "|".join(['add', 'sub', 'mult', 'div', 'mean', 'type', '']) + ")"
KEYWORD = "(?P<KEYWORD>" + "|".join([
    'if', 'for', 'while', 'unless', 'until', 'else', 'of', 'as', 'where', 'then'
]) + ")"
ID = r'(?P<ID>[a-z_]\w*)'
UNKNOWN = r"(?P<UNKNOWN>.)"
DTYPE = "(?P<DTYPE>" + "|".join([MATRIX, NUMBER, STRING, BOOL]) + ")"
NL = "(?P<NL>[\n])"
LSKIP = "(?P<LSKIP>^(" + "|".join([r'-{3,}.*?', r'[ \t]+']) + r")\n)"
ATOM = "(?P<ATOM>" + "|".join([SKIP, LSKIP, KEYWORD, DTYPE, OP, BUILTIN, ID, NL, UNKNOWN]) + ")"
ATOMS = "(?P<ATOMS>" + ATOM + "*)"

for atom in re.compile(ATOM, re.VERBOSE|re.M).finditer(code):
    dict_matches = atom.groupdict()
    print({k: v for k, v in dict_matches.items() if v})
