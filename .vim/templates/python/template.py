#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
AUTHOR: {{ author }}
DATE: {{ now }}
PYTHON: {{ python }}
""" 

# Standard Library
import os, sys
import logging
from logging import Logger

# import types for static typing (mypy, pycharm etc)
from typing import List, Iterator, Iterable

# initalise logging with sane configuration
logging.basicConfig(
    level=logging.DEBUG, format='%(levelname)s:%(asctime)s  %(message)s')

# declare a logger
log: Logger = logging.getLogger()

# run only if run as script
if __name__ == "__main__":
    pass 
