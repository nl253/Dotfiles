#!/usr/bin/env python
# -*- coding: utf-8 -*-

from pathlib import Path
from tokenizer import tokenize
from parser import parse

f = Path('./file.vs')
code: str = f.read_text()

tokens = tokenize(code)

print(parse(tokens))
