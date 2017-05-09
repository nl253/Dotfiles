#!/usr/bin/env python
# -*- coding: utf-8 -*-

# TODO
# -----------------------------
#
# -----------------------------

import os
import pathlib
import sys
from pprint import pprint
import glob
import subprocess
import logging
import shlex
import re
import inspect
from typing import Any, List, Set, Dict

logging.basicConfig(
    level=logging.DEBUG, format='%(levelname)s:%(asctime)s  %(message)s')

logger = logging.getLogger()

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

