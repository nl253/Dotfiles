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
from typing import Any, List, Set, Dict
import shlex
import re

logging.basicConfig(
    level=logging.DEBUG, format='%(levelname)s:%(asctime)s  %(message)s')

logger = logging.getLogger()

def report(item: Any, name_of_var=None, print_cwd=False, length_of_var=True, type_of_var=True, pprint_var=True):
    if print_cwd:
        print("PWD : " + os.getcwd())
    if pprint_var:
        pprint(item)
    if length_of_var:
        try:
            print("LENGTH : {}".format(len(item)))
        except:  # throws an Exception if that item has no len
            print("LENGTH : ??? ")
    if type_of_var:
        try:
            print("TYPE : {}".format(type(item)))
            print("\n")
        except:
            print("TYPE : ??? ")
            print("\n")
    if type(name_of_var) is str and len(name_of_var) > 0:
        print(name_of_var)


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

