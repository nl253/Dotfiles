#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""{{ project }}

Description:
    Project Description

Contact:
    Author: {{ author }}
    Github: {{ github }}
    Email:  {{ email }}

Todo:
    * For module TODOs
    * You have to also use sphinx.ext.todo extension
"""

# Standard Library
import os
import sys
import logging
from logging import Logger

# import types for static typing (mypy, pycharm etc)
from typing import List, Iterator, Iterable

# initalise logging with sane configuration
logging.basicConfig(
    level=logging.DEBUG,
    format="%(levelname)s:%(asctime)s  %(message)s"
)

log: Logger = logging.getLogger()

# run only if run as script
if __name__ == "__main__":
    pass
