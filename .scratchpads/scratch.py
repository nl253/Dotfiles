q#!/usr/bin/env python
 regex = re.compile(
        r'^(?:http|ftp)s?://' # http:// or https://
        r'(?:(?:[A-Z0-9](?:[A-Z0-9-]{0,61}[A-Z0-9])?\.)+(?:[A-Z]{2,6}\.?|[A-Z0-9-]{2,}\.?)|' #domain...
        r'localhost|' #localhost...
        r'\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3})' # ...or ip
        r'(?::\d+)?' # optional port
        r'(?:/?|[/?]\S+)$', re.IGNORECASE)
# -*- coding: utf-8 -*-

from typing import Callable, List, Any

def reduce(function: Callable, _iterable: List) -> Any:
    if len(_iterable) == 1:
        return _iterable[0]
    else:
        _iterable[1] = function(_iterable[0], _iterable[1])
        return reduce(function, _iterable[1:])

print(reduce(lambda x, y : y + x, [1,2,3,4]))






