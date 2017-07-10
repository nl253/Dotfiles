#!/usr/bin/env python
# -*- coding: utf-8 -*-

import re
import collections
from pathlib import Path
from typing import List, Tuple

token = collections.namedtuple('TOKEN', ["DATA", 'TYPE', 'START', 'COL', 'LINE'])
Token = Tuple[str, str, int, int, int]

SKIP = r'(?P<SKIP>(?P<SPACE>[ ])|(?P<COMMENT>-{3}.*?\n))'
INT = r'(?P<INT>\d+)'
DOUBLE = r'(?P<DOUBLE>(\d+)?\.\d+d?)'
FLOAT = r'(?P<FLOAT>(\d+)?\.\d+f)'
REAL = "(?P<REAL>" + r"|".join([FLOAT, DOUBLE, INT]) + ")"
IMAGINARY = r'(?P<IMAG>(\d+)?\.\d+i)'
BOOL = r'(?P<BOOL>true|false)'
NUMBER = r'(?P<NUMBER>' + "|".join([IMAGINARY, REAL]) + ")"
MATRIX = r'(?P<MATRIX>\d{1,2}(x\d{1,2})+)'
STRING = r'(?P<STRING>"\S+?")'
REGEXP = r'(?P<REGEXP>(?P<LSLASH>/).*?(?P<RSLASH>/))'
COMPOP = "(?P<COMPOP>" + "|".join([r'=~', r'==',  r'<=', r'=>', '<', '>']) + ")"
ARITHOP = "(?P<ARITHOP>" + "|".join(['-', r'\+', r'\*', '/', r'\^']) + ")"
ASSIGNOP = "(?P<ASSIGNOP>::)"
FDEFDELIM = "(?P<FDEFDELIM>:)"
FDEFRETDELIM = "(?P<FDEFRETDELIM>->)"
BOP = "(?P<BOP>" + "|".join(
        [COMPOP, ARITHOP, ASSIGNOP, FDEFDELIM, FDEFRETDELIM]) + ")"
UOP = "(?P<UOP>" + "|".join([r'\!', r'\#']) + ")"
OP = "(?P<OP>" + "|".join([BOP, UOP]) + ")"
LOOP = "(?P<LOOP>" + \
        "|".join(['if', 'while', 'unless', 'until']) + ")"
ID = r'(?P<ID>[a-z_]\w*)'
UNKNOWN = r"(?P<UNKNOWN>.)"
PRIM = "(?P<PRIM>" + "|".join([MATRIX, REGEXP, NUMBER, STRING, BOOL]) + ")"
LPAREN = "(?P<LPAREN>[\(])"
RPAREN = "(?P<RPAREN>[\)])"
PAREN = "(?P<PAREN>" "|".join([LPAREN, RPAREN]) + ")"
LBRACK = '(?P<LBRACK>[\[])'
RBRACK = '(?P<RBRACK>[\]])'
TYPE = "(?P<TYPE>" + "|".join(
        ['str(ing)?', 'int(eger)?', 'imag(inary)?', 'num(ber)?', 'regexp?',
            'real', 'seq(uence)?', 'float', 'double', 'funct(ion)?',
            'matrix', 'bool(ean)?']) + ")"
NL = r"(?P<NL>[\n\r])"
INDENT = r"(?P<INDENT>[\t]|[ ]{4})"
LSKIP = "(?P<LSKIP>^(" + "|".join([r'[ \t]+']) + r")\n)"
ATOM = "(?P<ATOM>" + "|".join(
        [NL, INDENT, LSKIP, SKIP, LPAREN, RPAREN, TYPE, LOOP, PRIM, OP, ID, UNKNOWN]) + ")"


def tokenize(code: str) -> List[Token]:

    outcome: List[Token] = []

    for atom in re.compile(ATOM, re.VERBOSE | re.M).finditer(code):

        dict_matches = atom.groupdict()

        START = atom.start()
        COL = atom.start() - atom.string[:atom.start()].rfind('\n') \
                if atom.string[:atom.start()].rfind('\n') > 0 \
                else atom.start()
        LINE = atom.string[:atom.start()].count('\n') + 1
        TYPE = [t for t in dict_matches.keys() if dict_matches[t]][-1]
        DATA = dict_matches['ATOM']

        entry = token(DATA, TYPE, START, COL, LINE)

        outcome.append(entry)

    return outcome

print(tokenize(open('./file.vs').read()))

# vim: nu
