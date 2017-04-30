#!/usr/bin/env python
# -*- coding: utf-8 -*-

from PythonUtils.debugging import os


class Directory:

    def __init__(self):
        pass

    def create(self):
        os.mknod(self._path)

    @property
    def rel_path(self):
        return os.path.relpath(self._path)

    @property
    def size(self) -> str:
        return os.path.getsize(self._path)

    @property
    def access_time(self) -> str:
        return os.path.getatime(self._path)

    @property
    def modify_time(self) -> str:
        return os.path.getmtime(self._path)

    @property
    def name(self) -> str:
        return os.path.basename(self._path)

    @name.setter
    def name(self, new_name: str):
        self._path = os.path.join(os.path.dirname(self._path), new_name)

    @property
    def path(self) -> str:
        return self._path

    @path.setter
    def path(self, new_location: str):
        return self._path


class NoteLous(Directory):
    pass
