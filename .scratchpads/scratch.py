#!/usr/bin/env python
# -*- coding: utf-8 -*-

###########################
# TODO
###########################

# parsers['todo'] = parsers['subparsers'].add_parser('todo')

# parsers['todo'].add_argument('-d', '--description',)

# parsers['todo'].add_argument('--due')

# parsers['todo'].add_argument('-w','--with-whom')

# parsers['todo'].add_argument('-u','--urgency')

# parsers['todo'].add_argument('-t', '--tags')

import os, sys, subprocess, pathlib
import shlex
from fuzzywuzzy import fuzz
import re
from typing import List, Any
from pprint import pprint
import logging

logging.basicConfig(
    level=logging.DEBUG, format='%(levelname)s:%(asctime)s  %(message)s')

logger = logging.getLogger()

def sh(commands: List[str] or str) -> List[str]:  # takes a list parameter
    # test it that exectuable exists # it will always be the first `arg`
    if type(commands) is str:
        commands = shlex.split(commands)
    print_blacklist = ['.', '..', '']
    if len(commands) > 1 and any(
            map(lambda x: "~" in x, commands)):  # check if it is a collection
        for i in range(len(commands)):
            if '~' in commands[i]:
                commands[i] = resolve(commands[i])
    outcome = [i for i in subprocess.run(commands, stdout=subprocess.PIPE)
        .stdout.decode('utf-8').split('\n')
        if i not in print_blacklist and i.isprintable()]
    return outcome


def word_tokenize(text: str) -> List[str]:
    return re.split("(?:(?:[^a-zA-Z]+')|(?:'[^a-zA-Z]+))|(?:[^a-zA-Z']+)", text)


def read_file(path: str) -> str:
    function_guard(path, str, 'read_file')
    resolved = pathlib.Path(resolve(path))
    assert os.path.exists(str(resolved)), 'The path doesn\'t exist on the system.'
    return resolved.read_text()


def resolve(path: str):
    function_guard(path, str, 'resolve')
    return os.path.realpath(os.path.abspath(os.path.expandvars(os.path.expanduser(path))))


def function_guard(param: Any, expected_type, funct_name: str):
    assert type(param) is expected_type, funct_name + " only accepts " + str(expected_type) + "you passed on {}".format(type(param))


def infer_fromat() -> str:
    """Only call when you know create has been chosen.
    """
    note_name = args.name
    try:
        if re.compile('.*\.md').fullmatch(note_name):
            return 'markdown'
        elif re.compile('.*\.rst').fullmatch(note_name):
            return 'rst'
        elif re.compile('.*\.txt').fullmatch(note_name):
            return 'asciidoc'
        elif re.compile('.*\.textile').fullmatch(note_name):
            return 'textile'
        else:
            return 'other'
    except:
        logger.info(
            'You are trying to infer the format of a note without it\'t name.')
        raise Exception(
            'You are trying to infer the format of a note without it\'t name. \
            \n`create` wasn\'t chosen! The program is running in {} mode'
            .format(args.mode))


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


def retrieve(option_name: str) -> Any:
    """Safely get a value by passing it's name as a string.
    If it doesn't exist, return None.
    """
    assert type(option_name) is not None, 'The argument passed \
        to retrieve was `None`, this method requires a string.'
    assert type(option_name) is str, 'The type of parameter passed \
        to retrieve was {}, this function only accepts strings!'.format(
        type(option_name))
    logger.info('checking args.mode')
    ####report(args.mode)
    logger.info('checking argument in retrieve')
    #report(option_name)
    if exists("vars(args)['" + option_name + "']"):
        return vars(args)[option_name]
    elif exists("configuration[args.mode][infer_fromat()]['" + option_name +
                "']"):
        return configuration[args.mode][infer_fromat()][option_name]
    elif exists("configuration[args.mode]['default']['" + option_name +
                "']"):
        return configuration[args.mode][infer_fromat()][option_name]
    elif exists("configuration[args.mode]['" + option_name + "']"):
        return configuration[args.mode][option_name]
    elif exists("configuration['*']['" + option_name + "']"):
        return configuration['*'][option_name]
    else:
        raise Exception(
            'Option {} not found anywhere in the application. You might have removed it by accident.'.
            format(option_name))


def list_to_str(word_list: List[str]) -> str:
    function_guard(word_list, list, 'list_to_str')
    return "".join([i + " " for i in word_list])


def sent_tokenize(text: str) -> List[str]:
    function_guard(text, str, 'sent_tokenize')
    sents = re.split('(?<!\w\.\w.)(?<![A-Z][a-z]\.)(?<=\.|\?)(\s|[A-Z].*)',text)
    return sents


def fuzzy_path_match(query: str, accuracy=83) -> List[str]:
    home = sh('find', '~')  # use find to get listing of files
    shortlist = [i for i in home if fuzz.ratio(os.path.basename(i), query) >= accuracy]
    return shortlist


def main():
    pprint(sh("ls -a"))


if __name__  == '__main__':
    main()
