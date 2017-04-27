#!/usr/bin/env python
# -*- coding: utf-8 -*-

# TODO
# -----------------------------
#
# -----------------------------

import os, pathlib, sys
from pprint import pprint
import glob
import subprocess
import logging
from typing import Any
import shlex
import re

from prompt_toolkit import prompt
from prompt_toolkit.contrib.completers import WordCompleter

logging.basicConfig(
    level=logging.DEBUG, format='%(levelname)s:%(asctime)s  %(message)s')

logger = logging.getLogger()


def report(item: Any, length_of_var=True, type_of_var=True, pprint_var=True):
    if pprint_var:
        pprint(item)
    if length_of_var:
        try:
            print("length : {}".format(len(item)))
        except:  # throws an Exception if that item has no len
            print("length : ??? ")
            if type_of_var:
                try:
                    print("type : {}".format(type(item)))
                    print("\n")
                except:
                    print("type : ??? ")
                    print("\n")


def exists(var_name: str) -> bool:
    try:
        exec(var_name)
        if var_name is not None:
            return True
        else:
            return False
    except (KeyError, NameError, AttributeError):
        return False




def main():
    pass


if __name__ == "__main__":
    main()

