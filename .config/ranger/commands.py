# -*- coding: utf-8 -*-
"""
This file is part of ranger, the console file manager.
This configuration file is licensed under the same terms as ranger.
===================================================================

NOTE: If you copied this file to ~/.config/ranger/commands_full.py,
then it will NOT be loaded by ranger, and only serve as a reference.

===================================================================
This file contains ranger's commands.
It's all in python; lines beginning with # are comments.

Note that additional commands are automatically generated from the methods
of the class ranger.core.actions.Actions.

You can customize commands in the file ~/.config/ranger/commands.py.
It has the same syntax as this file.  In fact, you can just copy this
file there with `ranger --copy-config=commands' and make your modifications.
But make sure you update your configs when you update ranger.

===================================================================
Every class defined here which is a subclass of `Command' will be used as a
command in ranger.  Several methods are defined to interface with ranger:
  execute():   called when the command is executed.
  cancel():    called when closing the console.
  tab(tabnum): called when <TAB> is pressed.
  quick():     called after each keypress.

tab() argument tabnum is 1 for <TAB> and -1 for <S-TAB> by default

The return values for tab() can be either:
  None: There is no tab completion
  A string: Change the console to this string
  A list/tuple/generator: cycle through every item in it

The return value for quick() can be:
  False: Nothing happens
  True: Execute the command afterwards

The return value for execute() and cancel() doesn't matter.

===================================================================
Commands have certain attributes and methods that facilitate parsing of
the arguments:

self.line: The whole line that was written in the console.
self.args: A list of all (space-separated) arguments to the command.
self.quantifier: If this command was mapped to the key "X" and
     the user pressed 6X, self.quantifier will be 6.
self.arg(n): The n-th argument, or an empty string if it doesn't exist.
self.rest(n): The n-th argument plus everything that followed.  For example,
     if the command was "search foo bar a b c", rest(2) will be "bar a b c"
self.start(n): Anything before the n-th argument.  For example, if the
     command was "search foo bar a b c", start(2) will be "search foo"

===================================================================
And this is a little reference for common ranger functions and objects:

self.fm: A reference to the "fm" object which contains most information
     about ranger.
self.fm.notify(string): Print the given string on the screen.
self.fm.notify(string, bad=True): Print the given string in RED.
self.fm.reload_cwd(): Reload the current working directory.
self.fm.thisdir: The current working directory. (A File object.)
self.fm.thisfile: The current file. (A File object too.)
self.fm.thistab.get_selection(): A list of all selected files.
self.fm.execute_console(string): Execute the string as a ranger command.
self.fm.open_console(string): Open the console with the given string
     already typed in for you.
self.fm.move(direction): Moves the cursor in the given direction, which
     can be something like down=3, up=5, right=1, left=1, to=6, ...

File objects (for example self.fm.thisfile) have these useful attributes and
methods:

tfile.path: The path to the file.
tfile.basename: The base name only.
tfile.load_content(): Force a loading of the directories content (which
     obviously works with directories only)
tfile.is_directory: True/False depending on whether it's a directory.

For advanced commands it is unavoidable to dive a bit into the source code
of ranger.
===================================================================
"""

from __future__ import absolute_import, division, print_function

# Standard Library
import re, os, fnmatch
from copy import deepcopy
from glob import iglob
from json import load as jsonLoad
from os import environ, makedirs, mknod, getenv
from os.path import exists, expanduser, isfile, join, lexists, relpath
from pathlib import Path
from subprocess import DEVNULL, PIPE, Popen, run
from typing import Any, Iterable, List, Optional, Pattern, Set, Text, Tuple

# 3rd Party
# You always need to import ranger.api.commands here to get the Command class:
from ranger.api.commands import Command


class git(Command):
    """git <subcommand> [<option>, ...] [--] [<file>, ...]

    Wrapper for git commands. Good completion.
    """

    non_i_cmds: Set[Text] = {
        'add',
        'archive',
        'clean',
        'clone',
        'fetch',
        'init',
        'mv',
        'pull',
        'push',
        'rm',
    }

    opts: Set[Text] = {
        '--bare',
        '--exec-path',
        '--git-dir',
        '--help',
        '--html-path',
        '--info-path',
        '--man-path',
        '--namespace',
        '--no-pager',
        '--no-replace-objects',
        '--paginate',
        '--version',
        '--work-tree',
        '-C',
        '-c',
        '-p',
    }

    refs: Set[Text] = {
        'HEAD',
        'FETCH_HEAD',
        'ORIG_HEAD',
        'master',
    }

    cmds: Set[Text] = {
        'am',
        'apply',
        'bisect',
        'blame',
        'branch',
        'cat-file',
        'checkout',
        'checkout-index',
        'cherry',
        'commit',
        'commit-tree',
        'count-objects',
        'diff',
        'diff-files',
        'diff-index',
        'diff-tree',
        'fast-export',
        'filter-branch',
        'for-each-ref',
        'for-ls-tree',
        'grep',
        'hash-object',
        'help',
        'index-pack',
        'log',
        'ls-files',
        'merge',
        'merge-file',
        'merge-index',
        'mktag',
        'mktree',
        'name-rev',
        'pack-redundant',
        'prune-packed',
        'read-tree',
        'rebase',
        'reflog',
        'remote',
        'repack',
        'reset',
        'rev-list',
        'show',
        'show-branches',
        'status',
        'symbolic-ref',
        'tag',
        'unpack-file',
        'update-ref',
        'var',
        'verify-pack',
        'worktree',
        'write-tree',
    }

    def _get_subcmd(self) -> Optional[Text]:
        if not self.args or len(self.args) == 1:
            return None
        for word in self.args[1:]:
            if not word.startswith('-') and (word in git.cmds or word in git.non_i_cmds):
                return word
        return None

    def execute(self):

        if len(self.args) < 2:
            return

        subcmd: Optional[Text] = self._get_subcmd()

        if subcmd is None:
            return

        elif (subcmd not in git.cmds) and (subcmd not in git.non_i_cmds):
            return

        is_i: bool = subcmd in git.cmds

        cmd: List[Text] = ['git'] + \
            (['--paginate'] if is_i else []) + self.args[1:]

        # if any files marked add them to args
        if len(self.fm.thistab.get_selection()) > 1:
            cmd.append('--')
            cmd.extend((i.path for i in self.fm.thistab.get_selection()))

        try:
            # synchronized
            if is_i:
                run(cmd)
            # async
            else:
                from threading import Thread
                thread: Thread = Thread(target=Popen, name=f"git-{subcmd}", kwargs={
                    'args': cmd, 'stdout': DEVNULL, 'stderr': DEVNULL})
                thread.start()
                self.fm.notify(f'{" ".join(cmd)} spawned')

            self.fm.ui.redraw_main_column()
            self.fm.ui.need_redraw = True

        except Exception as e:
            self.fm.notify(f'An error has occurred {e}', bad=True)

    def tab(self, tabnum):

        if len(self.args) == 1:
            return (f"git {cmd}" for cmd in
                    (git.cmds | git.non_i_cmds | git.opts))

        elif len(self.args) > 1:

            # complete flags
            if self.args[-1].startswith('-'):

                subcmd: Optional[Text] = self._get_subcmd()

                flags: Iterable[Text] = None

                if subcmd:
                    pat: Pattern[Text] = re.compile(
                        r'--[a-z][-a-z_]+|-[a-zA-Z]')
                    s: Text = run(['git', '--no-pager', subcmd, '-h'],
                                  stdout=PIPE,
                                  stderr=DEVNULL).stdout.decode('utf-8')
                    flags = (
                        (" ".join(self.args[:-1]) +
                         ' ' + match.group(0)).strip()
                        for match in pat.finditer(s))
                else:
                    flags = (
                        (" ".join(self.args[:-1]) + " " + opt).strip()
                        for opt in git.opts)

                if self.args[-1] == '-' or self.args[-1] == '--':
                    return flags

                else:
                    return (flag for flag in flags
                            if (flag in self.args[-1]) or
                            (self.args[-1] in flag))

            # relative paths
            elif self.args[-1].startswith('./'):
                return ((" ".join(self.args[:-1]) + " " + node).strip()
                        for node in iglob(join(self.args[-1], '*'))
                        if self.args[-1] in node or node in self.args[-1])

            else:

                pat: Pattern[Text] = re.compile(r'\w+')
                stdout: Text = run(
                    ['git', '--no-pager', 'branch'], stdout=PIPE, stderr=DEVNULL).stdout.decode('utf-8')
                branches: Iterable[Text] = set((match.group(0)
                                                for match in pat.finditer(stdout)))

                return (
                    (" ".join(self.args[:-1]) + " " + cmd)
                    for cmd in (git.cmds | git.non_i_cmds | git.refs | deepcopy(branches) | {f'origin/{ref}' for ref in (git.refs | deepcopy(branches))})
                    if self.args[-1].startswith(cmd) or cmd.startswith(self.args[-1])
                )


class toPDF(Command):
    """:toPDF [<file>, ...]
    Convert files to PDF.

    NOTE requires libreoffice
    """
    def execute(self):
        d = Path('.')
        did_files = []
        args = ['libreoffice', '--headless', '--invisible', '--convert-to', 'pdf']
        if len(self.args) > 1:
            for node in self.args[1:]:
                try:
                    run(args + [node])
                    did_files.append(node)
                except Exception as e:
                    self.fm.notify(str(e), bad=True)
        else:
            for ext in ['ppt', 'pptx', 'doc', 'docx']:
                for node in d.glob(f'*.{ext}'):
                    try:
                        run(args + [node])
                        did_files.append(node)
                    except Exception as e:
                        self.fm.notify(str(e), bad=True)
        if len(did_files) == 0:
            self.fm.notify('no *.{doc,docx,ppt,pptx} files', bad=True)
        else:
            self.fm.notify(f'converted {", ".join(did_files)}')


    def tab(self, tabnum):
        return self._tab_directory_content()


class vim(Command):
    """:vim [<option>, ...] [<filename>, ...]
    Open marked files in vim.

    NOTE marking only works for the current directory.
    """

    def execute(self):
        cmd = [getenv('EDITOR', 'vim')] + self.args[1:]
        if len(self.fm.thistab.get_selection()) > 1:
            cmd.append('--')
            cmd.extend((i.path for i in self.fm.thistab.get_selection()))
        run(cmd, stdout=DEVNULL)
        self.fm.ui.need_redraw = True
        self.fm.ui.redraw_main_column()

    def tab(self, tabnum):

        opts: Set[Text] = {'-t',
                           '+',
                           '-S',
                           '--cmd',
                           '-d',
                           '-M',
                           '-m',
                           '-o',
                           '-O',
                           '-p',
                           '-R'}

        if len(self.args) > 1:
            if self.args[-1].startswith('-'):
                return ((" ".join(self.args[:-1]) + " " + opt).strip()
                        for opt in opts
                        if self.args[-1] in opt or opt in self.args[-1])
            else:
                return ((" ".join(self.args[:-1]) + " " + node).strip()
                        for node in iglob(join(self.args[-1], '*'))
                        if self.args[-1] in node or node in self.args[-1])
        elif len(self.args) == 1:
            return (f"vim {i}" for i in opts)
        else:
            return None


class lines_of_code(Command):
    """:lines_of_code

    Counts lines of code recursively from the current directory.
    Optionally accepts an extension.
    """

    extensions = {
        'vim',
        'sh',
        'java',
        'c',
        'cpp',
        'py',
        'rs',
        'hs',
        'pl',
        'rb',
        'tex',
        'txt',
        'md',
        'rst',
        'js',
        'xml',
        'html',
        'css',
        'php',
        'yml',
        'ini',
        'zsh',
        'json',
        'toml',
        'cfg',
        'conf',
    }

    def execute(self):

        pattern = re.compile(r'^\s*([1-9]\d*)\s+.*')

        files = iglob(join(self.fm.thisdir.path, '**'), recursive=True)

        if self.args[1:]:
            files = fnmatch.filter(files, '*.' + self.args[1])

        files = filter(pattern.search, filter(isfile, files))

        files = sorted((f for f in map(lambda i: relpath(i, self.fm.thisdir.path), files)
                        if not 'node_modules' in f.strip()), key=(lambda x: pattern.search(x.lstrip()).group(1)), reverse=True)

        return run(['less'], input=run(['wc', '-l'] + files, stdout=PIPE, stderr=DEVNULL).stdout)

    def _pred(self, extension):
        return any({
            self.args[-1] in extension, extension in self.args[-1],
            len(self.args) == 1
        })

    def tab(self, tabnum):
        return {
            "lines_of_code " + i
            for i in filter(self._pred, lines_of_code.extensions)
        }


class grep(Command):
    """:grep <string>

    Looks for a string in all marked files or directories.

    Ripgrep will be attempted before ranger will fallback on git grep and grep.
    """

    def execute(self):

        if self.rest(1):

            # try ripgrep
            if exists(expanduser('~/.cargo/bin/rg')):

                x = run([
                    'rg', '--pretty', '--smart-case', '--threads=4',
                    '--after-context=1', '--before-context=1', '--regexp'
                ] + self.args[1:],
                    stdout=PIPE)

            # try git grep if in a git repo
            elif exists(
                run(['git', 'rev-parse', '--show-toplevel'],
                    PIPE).stdout.decode('utf-8')
            ):

                x = run([
                    'git', 'grep', '--line-number', '--color=always', '-I',
                    '--after-context=3', '--threads=4', '--extended-regexp',
                    '--heading', '--break', '-e'
                ] + self.args[1:],
                    stdout=PIPE)

            # fallback on grep
            else:
                x = run([
                    'grep', '--line-number', '--extended-regexp',
                    '--color=always', '--with-filename', '-r', '-e'
                ] + self.args[1:])

            if x.stdout:
                run(['less', '-R', '-X', '-I'], input=x.stdout)
                self.fm.ui.need_redraw = True
                self.fm.ui.redraw_main_column()
            else:
                self.fm.notify('no matches', bad=True)


    def tab(self, tabnum):
        for flag in ('--after-context', '--basic-regexp', '--before-context', '--binary-files', '--byte-offset', '--dereference-recursive', '--exclude-dir', '--exclude-from', '--extended-regexp', '--files-with', '--files-without', '--fixed-strings', '--ignore-case', '--initial-tab', '--invert-match', '--line-buffered', '--line-number', '--line-regexp', '--max-count', '--no-filename', '--no-messages', '--null-data', '--only-matching', '--perl-regexp', '--unix-byte', '--with-filename', '--word-regexp'):
            if self.args[-1] in flag or flag in self.args[-1]:
                yield f'{" ".join(self.args[:-1])} {flag}'


class untracked(Command):
    """:untracked

    List files not tracked by git (ignored).

    Same as git --paginate ls-files --others
    """

    def execute(self):
        run(['git', '--paginate', 'ls-files', '--others'])


class tracked(Command):
    """:tracked

    List files tracked by git.

    Same as git --paginate ls-files
    """

    def execute(self):
        run(['git', '--paginate', 'ls-files'])
        self.fm.ui.need_redraw = True
        self.fm.ui.redraw_main_column()


class yarn(Command):
    """:yarn <subcommand>
    """

    def execute(self):
        run(['yarn'] + self.args[1:])

    def tab(self, tabnum):

        if len(self.args) > 2 and self.args[1] == 'run':

            if not isfile('package.json'):
                self.fm.notify('No package.json in this dir.', bad=True)
                return

            with open('./package.json', mode='r', encoding='utf-8') as f:
                text = f.read()

            x: dict = jsonLoad(open('./package.json', encoding='utf-8'))

            if x.get('scripts', None) == None:
                return

            return {
                'yarn run ' + i
                for i in x['scripts']
                if self.args[2] in i
                or len(self.args[-1]) == 1 and i.startswith(self.args[-1])
            }

        elif len(self.args) == 1 or self.args[1] != 'run':
            return (
                f'yarn {i}'
                for i in (
                    'install',
                    'update',
                    'upgrade',
                    'remove',
                    'pack',
                    'run',
                    'unlink',
                    'generate-lock-entry',
                    'import',
                    'access',
                    'add',
                    'autoclean',
                    'create',
                    'exec',
                    'publish'
                ) if self.args[-1] in i or len(self.args) == 1
            )
        else:
            return


class mkdir(Command):
    """:mkdir [<dirname>, ...]

    Creates a directories with given names.
    """

    def execute(self):
        if not self.args[1:]:
            return

        for i in self.args[1:]:

            dirname = join(self.fm.thisdir.path, expanduser(i))

            if not lexists(dirname):
                makedirs(dirname, exist_ok=True)

            else:
                self.fm.notify(f"directory {dirname} exists!", bad=True)
                break

    def tab(self, tabnum):
        return self._tab_directory_content()


class touch(Command):
    """:touch [<fname>, ...]

    Creates files with given names.
    """

    def execute(self):
        if not self.args[1:]:
            return

        for arg in self.args[1:]:
            fname = join(self.fm.thisdir.path, expanduser(arg))

            if not lexists(fname):
                mknod(fname)

            else:
                self.fm.notify(f"file {fname} exists!", bad=True)
                break

    def tab(self, tabnum):
        return self._tab_directory_content()


class make(Command):
    """:make <subcommand>

    Run make with specified rule.

    NOTE Must have a Makefile in the same directory.
    """

    def execute(self):
        run(['make'] + self.args[1:], stdout=DEVNULL)

    def tab(self, tabnum):

        if not isfile('Makefile'):
            self.fm.notify('No Makefile in this dir', bad=True)
            return

        with open('./Makefile', mode='r', encoding='utf-8') as f:
            text: Text = f.read()

        pat = re.compile(r'^(\w+):', flags=re.M)

        return (f'make {match.group(1)}' for match in pat.finditer(text))



class modified(Command):
    """:modified

    List files modified (Git).

    Same as git --paginate ls-files --modified
    """

    def execute(self):
        run(['git', '--paginate', 'ls-files', '--modified'])

        self.fm.ui.need_redraw = True
        self.fm.ui.redraw_main_column()


class vimdiff(Command):
    """:vimdiff <file1> <file2> | <file1> <file2> <file3>

    Open vim in diff mode with passed files.

    NOTE when you mark them, only the ones in the current directory's will be passed to vim.
         Also, you can diff at most 3 files.
    """

    def execute(self):

        command = ['vim', '-d', '--']

        if len(self.fm.thistab.get_selection()) > 1:
            command.extend([i.path
                            for i in self.fm.thistab.get_selection()][:3])

        elif self.args[1:]:
            command.extend(self.args[1:4])

        else:
            return

        run(command, stdout=DEVNULL)

        self.fm.ui.need_redraw = True
        self.fm.ui.redraw_main_column()


class edit(Command):
    """:edit [<filename>, ...]

    Open file in $EDITOR
    """

    def execute(self):
        if not self.arg(1):
            run([environ['EDITOR'], self.fm.thisfile.path])
        else:
            run([environ['EDITOR']] + self.args[1:])

    def tab(self, tabnum):
        return self._tab_directory_content()
