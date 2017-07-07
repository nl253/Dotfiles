#!/usr/bin/env python
# -*- coding: utf-8 -*-

import re
import collections
from pathlib import Path
from typing import Match, List, Iterable, Iterator

f = Path('./file.vs')

code: str = f.read_text()

token = collections.namedtuple('TOKEN', ['TYPE', 'DATUM'])

INT = r'(?P<INT>\d+)'
DOUBLE = r'(?P<DOUBLE>\d+\.\d+)'
BOOL = r'(?P<BOOL>' + "|".join(['true', 'false']) + ")"
FLOAT = r'(?P<FLOAT>\d+\.\d+f)'
NUMBER = r'(?P<NUMBER>' + "|".join([INT, DOUBLE, FLOAT]) + ")"
MATRIX = r'(?P<MATRIX>\d{1,2}(x\d{1,2})+)'
OPERATORS = "(?P<OPERATOR>" + "|".join([r'=~', r'==', r'\+', r'-', '\*', '=', r'\?']) + ")"
STRING = r'(?P<STRING>"\w+")'
SKIP = r'(?P<SKIP>[ ])'
FUNCTION = "(?P<FUNCTION>" + "|".join(['add', 'sub', 'mult', 'div']) + ")"
KEYWORD = "(?P<KEYWORD>" + "|".join([
    'if', 'for', 'while', 'unless', 'until', 'else', 'of', 'as', 'such that',
    'where', 'then' 'when'
]) + ")"
IDENTIFIER = r'(?P<IDENTIFIER>\w+)'
DTYPE = "(?P<DTYPE>" + "|".join([NUMBER, STRING, FUNCTION, MATRIX, BOOL]) + ")"
ATOM = "(?P<ATOM>" + "|".join([SKIP, DTYPE, OPERATORS]) + ")"
ATOMS = "(?P<ATOMS>" + ATOM + "*)"
#CONDITIONALSTATEMENT = "(?P<CONDITIONALSTATEMENT>if " + BOOL " then
#" + "|".join([ATOM, EXPRESSION, CONDITIONALSTATEMENT, kI])
#FORLOOP = "(?P<FORLOOP>for " + ATOMS + ")"
#LOOP = "(?P<LOOP>" + ATOMS + ")"
LSKIP = "(?P<LSKIP>" + "|".join([r'^-{3} ', r'^[ \t]+$']) + ")"
#STATEMENT = '(?P<STATEMENT>' + "|".join(['^.*?$', '^.*;']) + ")"

# 'EXPRESSION': [['DTYPE', 'OPERATOR', 'DTYPE'], ['FUNCTION', 'DTYPE'],
#           ['DTYPE', 'FUNCTION', 'DTYPE'], ['FUNCTION', 'DTYPE', 'DTYPE']]

#import ipdb ; ipdb.set_trace() # XXX

for line in code.splitlines():
    if not re.compile(LSKIP, re.M).search(line):
        for atom in re.compile(ATOM, re.VERBOSE).finditer(line):
            dict_matches = atom.groupdict()
            print({k: v for k, v in dict_matches.items() if v})
