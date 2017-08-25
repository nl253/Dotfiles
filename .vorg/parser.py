import re
from re import findall, finditer
from typing import Pattern, Dict, Any



document: VorgDocument = VorgDocument()

indent_regexp: str = r"(?P<indent>^(( {4}|\t))*)"


# needs multiline
title: str = f"(?:{indent}{heading_marker})(.*?)$"

counter: int = 1

# find first Header aka title
match: Pattern = re.compile(f"{indent_regexp}{}")

# surrounded
document["H"]
document["meta"] = ""
document["title"] = ""
document["content"] = ""

for i in match:
    # DO_SOMETHING
    pattern: Pattern = re.compile("PATTERN")

    # modify match object
    pattern.findall(STRING_VARIABLE)

