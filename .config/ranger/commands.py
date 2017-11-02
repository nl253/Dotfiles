# This is a sample commands.py.  You can add your own commands here.
#
# Please refer to commands_full.py for all the default commands and a complete
# documentation.  Do NOT add them all here, or you may end up with defunct
# commands when upgrading ranger.

# A simple command for demonstration purposes follows.
# -----------------------------------------------------------------------------

from __future__ import (absolute_import, division, print_function)

# You always need to import ranger.api.commands here to get the Command class:
from ranger.api.commands import Command

from os import listdir
from subprocess import run

# Any class that is a subclass of "Command" will be integrated into ranger as a
# command.  Try typing ":my_edit<ENTER>" in ranger!
class git(Command):
    """git <subcommand> [<option>, ...] [--] [<file>, ...]
    """
    def execute(self):
        command = ['git', '--paginate']

        command.extend(self.args[1:])

        if len(self.fm.thistab.get_selection()) > 1:
            command.append('--')
            command.extend((i.path for i in self.fm.thistab.get_selection()))

        run(command)

        self.fm.ui.need_redraw = True
        self.fm.ui.redraw_main_column()


    def tab(self, tabnum):

        opts = {'--no-pager', '-C'}
        commands = {
            'add',
            'am',
            'apply',
            'archive',
            'bisect',
            'blame',
            'branch',
            'cat-file',
            'checkout',
            'checkout-index',
            'cherry',
            'clean',
            'clone',
            'commit',
            'commit-tree',
            'count-objects',
            'diff',
            'diff-files',
            'diff-index',
            'diff-tree',
            'fast-export',
            'fetch',
            'filter-branch',
            'for-each-ref',
            'for-ls-tree',
            'grep',
            'hash-object',
            'help',
            'index-pack',
            'init',
            'log',
            'ls-files',
            'merge',
            'merge-file',
            'merge-index',
            'mktag',
            'mktree',
            'mv',
            'name-rev',
            'pack-redundant',
            'prune-packed',
            'pull',
            'push'
            'read-tree',
            'rebase',
            'reflog',
            'remote',
            'repack',
            'reset',
            'rev-list',
            'rm',
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

        #  return {f'git {i}' for i in commands if self.arg(1) in i or i in self.arg(1)}  if len(self.args) > 1 else {f'git {i}' for i in commands if self.arg(1) in i or i in self.arg(1)}

        return {(" ".join(self.args[:-1]) + " " + i).strip()
                for i in commands if self.args[-1] in i or i in self.args[-1]
                } if len(self.args) > 1 else {f"git {i}" for i in opts} 


class vim(Command):
    """:vim [<option>, ...] [<filename>, ...]
    """

    def execute(self):
        command = ['vim'] + self.args[1:]
        command.append('--')
        command.extend(map(lambda x: x.path, self.fm.thistab.get_selection()))
        run(command)
        self.fm.ui.need_redraw = True
        self.fm.ui.redraw_main_column()

    def tab(self, tabnum):

        opts = {'-t', '+', '-S', '--cmd', '-d', '-M', '-m', '-o', '-O', '-p', '-R'}

        return {(" ".join(self.args[:-1]) + " " + i).strip()
                for i in opts | set(listdir(self.fm.thisdir.path))
                if self.args[-1] in i or i in self.args[-1]
                } if len(self.args) > 1 else {f"vim {i}" for i in opts}


class todos(Command):
    """:todos

    Looks for TODOs in the current repo.
    """

    def execute(self):

        run([
            'git', '--paginate', 'grep', '--line-number', '--color=always', '-I', '--after-context', '3', '--threads', '16',
            '--extended-regexp', '--full-name', '--heading', '--break', "'TODO|FIXME'"
            ])

        self.fm.ui.need_redraw = True
        self.fm.ui.redraw_main_column()


class grep(Command):

    """:grep <string>

    Looks for a string in all marked files or directories
    """

    def execute(self):
        from subprocess import PIPE
        from os.path import exists, expanduser

        if self.rest(1):

            # try ripgrep
            if exists(expanduser('~/.cargo/bin/rg')):

                x = run([
                    'rg', '--pretty', '--smart-case', '--threads', '16',
                    '--after-context', '1', '--before-context', '1', '--regexp'
                ] + self.args[1:],
                        stdout=PIPE)

            # try git grep if in a git repo
            elif exists(
                run(['git', 'rev-parse', '--show-toplevel'],
                    PIPE).stdout.decode('utf-8')
            ):

                x = run([
                    'git', 'grep', '--line-number', '--color=always', '-I',
                    '--after-context=3', '--threads=16', '--extended-regexp',
                    '--heading', '--break', '-e'
                ] + self.args[1:],
                        stdout=PIPE)

            # fallback on grep
            else:
                x = run([
                    'grep', '--line-number', '--extended-regexp',
                    '--color=always', '--with-filename', '-r', '-e'
                ] + self.args[1:])

            run(['less', '-R', '-X', '-I'], input=x.stdout)

            self.fm.ui.need_redraw = True
            self.fm.ui.redraw_main_column()


class untracked(Command):
    """:untracked

    List files not tracked by git (ignored).
    """

    def execute(self):
        run(['git', '--paginate', 'ls-files', '--others'])

        self.fm.ui.need_redraw = True
        self.fm.ui.redraw_main_column()

class tracked(Command):
    """:tracked

    List files tracked by git.
    """

    def execute(self):
        run(['git', '--paginate', 'ls-files'])

        self.fm.ui.need_redraw = True
        self.fm.ui.redraw_main_column()

class modified(Command):
    """:modified

    List files modified (Git).
    """

    def execute(self):
        run(['git', '--paginate', 'ls-files', '--modified'])

        self.fm.ui.need_redraw = True
        self.fm.ui.redraw_main_column() 

class vimdiff(Command):
    """:vimdiff <file1> <file2> | <file1> <file2> <file3>

    Open default diff tool with passed files.
    """

    def execute(self):

        command = ['vim', '-d', '--']

        if len(self.fm.thistab.get_selection()) > 1:
            command.extend([i.path for i in self.fm.thistab.get_selection()][:3])
            
        elif self.args[1:]:
            command.extend(self.args[1:4])

        else:
            return

        run(command)

        self.fm.ui.need_redraw = True
        self.fm.ui.redraw_main_column()
