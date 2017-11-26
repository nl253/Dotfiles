#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Standard Library
import os, re, sys, logging
from logging import Logger
from typing import *
from collections import deque, namedtuple

# initalise logging with sane configuration
logging.basicConfig(
    level=logging.DEBUG, format='%(levelname)s:%(asctime)s  %(message)s'
)

Token = namedtuple('Tuple', ['type', 'data'])

EQ = r"(?P<EQ>={1,2}|(is )?equal( to)?)"
IFF = r"(?P<IFF>iff|(is )?equiv(alent)?( to)?)"
DISJ = r"(?P<DISJ>or|v|\|{1,2})"
CONJ = r"(?P<CONJ>and|&{1,2})"
IMPL = r"(?P<IMPL>->|implies)"
XOR = r"(?P<XOR>(exclusive |e)?xor)"
BOP = r"(?P<BOP>" + "|".join([IFF, EQ, IMPL, XOR, DISJ, CONJ]) + ")"
UOP = r"(?P<UOP>not|!)"
ID = r"(?P<ID>[P-Z])"
LPAREN = r"(?P<LPAREN>\()"
RPAREN = r"(?P<RPAREN>\))"
NL = r"(?P<NL>\n)"
ATOM = r"(?P<ATOM>true|false)"
pat = re.compile(
    r"|".join([UOP, BOP, ATOM, ID, NL, LPAREN, RPAREN]), flags=re.MULTILINE
)


def tokenize(input: str) -> Iterable[Token]:
    for match in pat.finditer(input):
        matches = match.groupdict()
        yield match.group(0), [k for k in matches.keys() if matches[k]]


# run only if run as script
if __name__ == "__main__":
    f = sys.argv[1] if len(sys.argv) > 1 \
        else os.path.join(os.path.dirname(__file__), 'file.txt')

    with open(f) as f:
        for line in f:
            for token in tokenize(line):
                print(token)
