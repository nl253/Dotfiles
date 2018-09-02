#!/usr/bin/env python3

from typing import Text, Iterator, Tuple, Set, List, Iterable, Dict, Optional
import functools


@functools.lru_cache(maxsize=None)
def subseqs(xs: Text) -> Set[Text]:
    if len(xs) == 0:
        return set()
    else:
        results: Set[Text] = subseqs(xs[:-2]) | subseqs(xs[1:])
        results.add(xs)
        return results


def match(s: Text, pat: Text) -> bool:
    pat_size: int = len(pat)
    lst_pat_idx = pat_size - 1
    s_size: int = len(s)
    i: int = pat_size - 1
    rev_pat: Iterable[Text] = pat[::-1]
    cache: Dict[Text, int] = {c: pat.index(c) for c in pat}

    # pattern larger than input text always fails
    if pat_size > s_size:
        return False

    # empty pattern matches everything
    elif pat_size == 0:
        return True

    while i < s_size:

        # if not you can skip by the len of the pattern
        maybe_idx: Optional[int] = cache.get(s[i], None)

        if maybe_idx is None:
            i += pat_size
        else:
            i += lst_pat_idx - maybe_idx

            pairs: Iterator[Tuple[Text, Text]] = zip(s[i : i - pat_size : -1], rev_pat)

            if all((pair[0] == pair[1] for pair in pairs)):
                return True
            else:
                i += 1

    return False


def test(s, non_matches: List[Text] = ["wooo", "asdlkfjasd", "alsjd"]):

    for i in subseqs(s):
        assert match(s, i), f"false negative on {i}"

    for i in non_matches:
        assert not match(s, i), f"false positive %(something)s on {i}"

    print("OK!")


if __name__ == "__main__":
    test("copycat")
