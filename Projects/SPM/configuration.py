import os
from sys import exit
from configparser import ConfigParser, ExtendedInterpolation
from urllib.request import urlopen
from typing import List

from iniparsing import get_iniparser

ROOT = os.path.expanduser('~/Projects/SPM')


class ConfigSeeker:
    def __init__(self, look_in: List[str] = 
            [ '~/.spm.ini', '~/.spm/config.ini', '~/.config/spm/config.ini' '~/.config/SPM/config.ini']):

        # initialise
        self._path = False

        for i in look_in:
            i = os.path.expanduser(i)
            if os.path.isfile(i):
                self._path = i
                return

        if not self._path:
            ConfigSeeker._not_found()
            self.__init__(look_in)

    @staticmethod
    def _not_found():
        if input('Config not found... \nWould you like to download it? [Yes/No] \n\t').lower() == 'yes':

            with urlopen('https://raw.githubusercontent.com/nl253/SQLiteREPL/master/completer.py') as f:
                text = f.read()
                f.close()
            assert len(text) > 0, 'There was a problem with downloading the config file...'

            with open(os.path.expanduser('~/.spm.ini'), mode='w') as f:
                f.write(text)

            assert os.path.exists(os.path.expanduser('~/.spm.ini')), 'There was a problem with ' \
                                                                     'saving to ' \
                                                                     '~/.spm.ini ' \
                                                                     '... '


        else:
            print('\nOK. Nothing to be done, aborting.\n')
            exit()

    @property
    def path(self) -> str:
        return self._path

    @property
    def text(self) -> str:
        fs = open(self._path, encoding='utf-8')
        text = fs.read()
        fs.close()
        return text


class ConfigVerifier(ConfigSeeker):
    def __init__(self):
        self.parser: ConfigParser = ConfigParser(interpolation=ExtendedInterpolation())
        self.parser.read_string(self.text)

    @property
    def parser(self) -> ConfigParser:
        return self.parser

    @property
    def verify(self):
        directives = \
            ['gists', 'recipies', 'curled', 'repos', 'executables']
        assert all(map(lambda directive: directive in directives,
                       self.parser['USER'])), f'You need to have all of {directives} in your ' \
                                              f'config file.\nAborting.\nFix please.\n '
        for i in self.parser['USER']:
            i = os.path.expanduser(i)
            if not os.path.exists(i) and input(
                    f' {i} doesn\'t seem to exist, \ncreate dir [Yes/No] ?\n').lower() == 'yes':
                os.makedirs(i)


class IniConfig(ConfigVerifier):
    def get(directive: str, section='User') -> str:
        return self.parser[section][directive]

