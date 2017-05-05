#!/usr/bin/env python
# -*- coding: utf-8 -*-

from typing import Callable, List, Any

def reduce(function: Callable, _iterable: List) -> Any:
    if len(_iterable) == 1:
        return _iterable[0]
    else:
        _iterable[1] = function(_iterable[0], _iterable[1])
        return reduce(function, _iterable[1:])

print(reduce(lambda x, y : y + x, [1,2,3,4]))



