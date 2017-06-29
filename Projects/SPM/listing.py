import os
import re
from glob import glob
from typing import List, Tuple, Any
from configuration import IniConfig

NamedDatum = Tuple[str, Any]


class Directory:

    def __init__(self, path: str):
        self.path: str = path

    @property
    def _nodes(self) -> List[str]:
        return os.listdir(self.path)

    @property
    def _files(self) -> List[str]:
        return [node for node in self._full_paths(self._nodes) if os.path.isfile(node)]

    @property
    def _dirs(self) -> List[str]:
        return [node for node in self._full_paths(self._nodes) if os.path.isdir(node)]

    @property
    def _symlinks(self) -> List[str]:
        return [node for node in self._full_paths(self._nodes()) if os.path.islink(node)]

    def _full_paths(self, nodes: List[str]) -> List[str]:
        return [os.path.join(self.path, node) for i in nodes]

    def ls(self, what='all', full_paths=False) -> List[str]:
        data = None

        if what == 'all':
            data: List[str] = self._nodes()

        elif what == 'dirs':
            data: List[str] = self._dirs()

        elif what == 'files':
            data: List[str] = self._files()

        if full_paths:
            data: List[str] = self._full_paths(data)

        return data
        
        
    @property
    def path(self) -> str:
        return self.path

        

class Presentation:

    def __init__(self, data: Any, 
            pre: List[NamedDatum], 
            post: List[NamedDatum]):

        self.data: Any  = data
        self.pre: List[NamedDatum] = pre
        self.post: List[NamedDatum] = post

    @propery
    def text(self) -> str:
        return str(self.data)

    @property
    def present(self) -> str:
        return self.pre + self.data + self.post
    
    @property
    def data(self) -> str:
        if type(self.data) == list:
            return Presentation._preformat_list(self.data)
        else:
            return str(self.data)

    @property
    def post(self) -> str:
        text: str = ""
        for label, datum in self.post:
            if label and datum:
                entry:str = f"{label} {str(datum)}\n"
                text:str += entry
        return text

    @property
    def pre(self) -> str:
        text: str = ""
        for label, datum in self.pre:
            if label and datum:
                entry:str = f"{label} {str(datum)}\n"
                text:str += entry
        return text

    @staticmethod
    def _preformat_list(data: List[str]) -> str:
        return "".join(map(lambda datum: datum + "\n", data))

    def __str__(self) -> str:
        return self.present


class Listing:

    configuration = IniConfig()
    
    @staticmethod
    def cloned() -> List[str]:
        directory = Directory(configuration.get('repos'))
        return directory.ls('dirs')

    @staticmethod
    def gists(gists_dir) -> List[Tuple[str, str]]:
        directory = Directory(configuration.get('gists'))
        g = list()
        for d in directory.ls('dirs'):
            for node in Directory(d).ls():
                g.append(node)
        return g

    @staticmethod
    def recipes(recipes_dir) -> List[str]:
        directory = Directory(configuration.get('repos'))
        return directory.ls('dirs')

    @staticmethod
    def curled(recipes_dir) -> List[str]:
        directory = Directory(configuration.get('curled'))
        return directory.ls('files')

def gists() -> str:
    pass
def repos() -> str:
    pass
def curled() -> str:
    pass
def recipies() -> str:
