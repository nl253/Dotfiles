#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import pathlib
import sys
from pprint import pprint
import re
import textwrap

# TODO:
# support gentle table formatting

try:
    text = open(sys.argv[1]).read()
except:
    text = sys.stdin.read()

if len(text) < 1 or len(sys.argv) > 2:
    print('\nmarkdown-formatter.py - format markdown documents (works great with Vim)\n\nUsage:\nmarkdown-formatter.py [FILE]\nOR\nDATA | markdown-formatter.py\n')
    sys.exit()

pylog = open('pylog.log', 'w+')

pylog.write(text)

# begins with a capital letter, has length no longer than 190 items (including newlines)
# and ends with a dot. Additionally, it doesn't contain a list item such as
# * todo
# or
# - done
def init():
    if re.compile('^[ \t]*`{2,3} *\w+\S*$', flags=re.DOTALL|re.MULTILINE).search(text):
        # detect source code to avoid breaking the synatx
        MODE = 'sourcecode'
        pylog.write('\nsourcecode\n')
    elif not re.compile('^ *[-*+1-9] *', flags=re.MULTILINE).search(text):
    # paragraphs do not include bulletpoints
    # separate lists from the paragraph (insert a newline), then wrap paragraph, smart wrap lists
        MODE = 'paragraph'
        pylog.write('\nparagraph\n')
    elif re.compile('(^[ \t]*[-1-9*+]\.?[ \t]*[\w[(]+)+', flags=re.DOTALL|re.MULTILINE).search(text):
    # at least one bulletpoint (starting with [-+*]) with a length of at most 300 characters
    # wrap smartly using the position of the first `\w` character after a bullet point `[-+*]`
    # indent using 8 spaces
        MODE = 'list'
        pylog.write('\nlist\n')
    else:
    # fallback, make sure this is safe and doesn't harm lists or codeblocks
        MODE = 'default'
        pylog.write('\ndefault\n')
    return MODE

MODE = init()

# max 2 newlines at the time, avoids massive gaps in text
text = re.compile('\n{3,}').sub('\n\n', text)

# remove duplicate punctuation
# that requires escaping
if not MODE == 'sourcecode':
    for i in [':', '$', '|', '^', '(', ')', ']', '[', '{', '}', '+']:
        text = re.compile("\\" + i + '{2,}').sub(i, text)

# doesn't require escaping
# also will remove double (or multiple) whitespace
for i in [',', '%', '£', '"', "'", '¬', '&', '“' '”', '‘', '’', '@', '§', '~']:
    text = re.compile(i + '{2,}').sub(i, text)

# more than 2 whitespaces between words reduced to 1
text = re.compile('(?<=[\w. ]{2}) {2,}(?=\w{2})').sub(' ', text)

if MODE == 'paragraph':
    # if it has bulletpoints
    if re.compile('^ * [-*+] * \w{2}', flags=re.MULTILINE|re.DOTALL):
        # insert a \n to separate it from the text body
        re.compile('\n(^ * [-*+] * \w{2})', flags=re.MULTILINE|re.DOTALL).subn('\n', text, 1)

# normalise space around punctuation
# colon, needs to be like this word: subword1, sub2 [...]
text = re.compile('(?<=[a-zA-Z\]]{2}) *\: *(?=[a-zA-Z]{2})').sub(': ', text)

text = re.compile('(?<=[a-zA-Z\]]{2}) *\:[ \n]$', flags=re.MULTILINE).sub(':\n', text)

for i in [',', ';']:
    text = re.compile('(?<=[A-Za-z0-9_*~]{2}) *' + i + '+ *(?=[A-Za-z0-9_*~]{2})').sub(i + ' ', text)


# emails anne @ soooo to anne@soooo
text = re.compile('(?<=[a-zA-Z]{2}) *@ *(?=[A-Za-z]{2})').sub('@', text)

# math
text = re.compile('(?<=\d)( *)\*( *)(?=\d)', flags=re.ASCII).sub(' • ', text)
text = re.compile('(?<=\d)( *)\/( *)(?=\d)', flags=re.ASCII).sub(' ÷ ', text)
text = re.compile('(?<=\d)( *)-( *)(?=\d)', flags=re.ASCII).sub(' - ', text)
text = re.compile('(?<=\d)( *)\+( *)(?=\d)', flags=re.ASCII).sub(' + ', text)
text = re.compile('(?<=\d)( *)\=( *)(?=\d)', flags=re.ASCII).sub(' = ', text)
text = re.compile('(?<=\d)( *)>( *)(?=\d)', flags=re.ASCII).sub(' > ', text)
text = re.compile('(?<=\d)( *)<( *)(?=\d)', flags=re.ASCII).sub(' < ', text)

# bulletpoints
# minus bullet '-'
# text = re.compile('(?<=\n)( *)-( *)(?=\w{2})', flags=re.MULTILINE).sub(i + '    - ', text)

# bulletpoints that need escaping
# for i in ['*', '+']:  # 3-5 spaces at the beginning becomes 8 , 1-2 becomes 4, 6-7 becomes 8
    # re.compile('\s|\b*\\' + i + ' *(?=\S)', flags=re.MULTILINE).sub('\n    ' + i + ' ', text)

# fixes apostrophes
for i in ['wosn', 'weren', 'ins', 'aren', 'won', 'wouldn', 'ain', 'don', 'didn', 'shouldn', 'haven', 'couldn', 'can', 'hadn']:
    text = re.compile('\b' + i + " *['’]t\b").sub(i + "'t", text)

text = re.compile("(?<=\w{2}) +(?=['’]s\b\w{2})").sub("", text)

text = re.compile("(?<=\w{2}) +(?=['’]d\w\b\w{2})").sub("", text)


# sentence ending
# ----------------
# exclemation and question mark
for i in ['!', '?']:  # normalies sdlkfjl  !!!!! to  sdlfjsd??? and sfdsdf!!!,
    text = re.compile('(?<=\w{2}) *\\' + i).sub(i, text)  # deal with spaces
    text = re.compile('\\' + i + '{3,}').sub(i + i + i, text)    # no more than 3

text = re.compile('(?<=[A-Za-z]{2}) *\.\b').sub(".", text)

# [ ... ] to [...]
text = re.compile('(?<=[A-Za-z]{2}) *\[+ *\.{3} *\]+ *').sub(' [...] ', text)

# elipsis no more than 3
text = re.compile('\.{4,8}').sub('...', text)

# percent after numbers to %
text = re.compile('(?<=\d) +percent *').sub('% ', text)

# PI to π
text = re.compile('(?<=[-+*% ]) +PI *').sub(' π ', text)
text = re.compile('(?<=\d) +PI *').sub('π ', text)
text = re.compile('(?<=\d) +degrees *').sub('° ', text)
text = re.compile('(?<=\d) +theta *').sub('θ ', text)

#  TerraBytes to TB Megabytes to MB kilobytes to KB
text = re.compile('(?<=\d) +[Kk](ilo)?[bB]ytes *').sub('kB ', text)
text = re.compile('(?<=\d) +[Mm](ega)?[bB]ytes *').sub('MB ', text)
text = re.compile('(?<=\d) +[Gg](iga)?[bB]ytes *').sub('GB ', text)
text = re.compile('(?<=\d) +[Tt](era)?[bB]ytes *').sub('TB ', text)
text = re.compile('(?<=\d) +[pP](eta)?[bB]ytes *').sub('PB ', text)

text = re.compile('(?<=\d) +[Kk]ilo([gG]rams)? *').sub('kg ', text)

text = re.compile('(?<=\d) +min *').sub('minutes ', text)

text = re.compile('(?<=\d) +sec *').sub('seconds ', text)

# brackets and quotes
text = re.compile('(?<=[A-Za-z]{2}) *\{+ *(?=[A-Za-z]{2})').sub(" {", text)
text = re.compile('(?<=[A-Za-z]{2}) *\}+ *(?=[A-Za-z]{2})').sub("} ", text)

text = re.compile('(?<=[A-Za-z]{2}) *\(+ *(?=[A-Za-z]{2})').sub(" (", text)
text = re.compile('(?<=[A-Za-z]{2}) *\)+ *(?=[A-Za-z]{2})').sub(") ", text)

text = re.compile('(?<=[A-Za-z]{2}) *\[+ *(?=[A-Za-z]{2})').sub(" [", text)
text = re.compile('(?<=[A-Za-z]{2}) *\]+ *(?=[A-Za-z]{2})').sub("] ", text)

text = re.compile('\bXOR\b', flags=re.MULTILINE).sub("⊕", text)

# a )sdlkf to a) assdf
text = re.compile('(?<=[a-zA-Z1-9]) *\) *(?=\w{2})', flags=re.MULTILINE).sub(") ", text)

# numbered lists
text = re.compile('(?<=\b\d)\.? *', flags=re.MULTILINE).sub('. ', text)

if MODE == 'list':
    text = re.compile('^( {,3}|\t)(?=[-+*1-9])', flags=re.MULTILINE).sub('', text)
    text = re.compile('^( {5,7}|\t{2})(?=[-+*1-9])', flags=re.MULTILINE).sub('    ', text)
    text = re.compile('^( {9,}|\t{2,})(?=[-+*1-9])', flags=re.MULTILINE).sub('        ', text)

# MARKDOWN SPECIFIC
############################
# fenced languages backtics
text = re.compile('^ *[`]{2,} *(?=\w{2})', flags=re.MULTILINE).sub('```', text)

# headings
# remove excess headers max is 6 anyway
text = re.compile('^[#]{6,} *', flags=re.MULTILINE).sub("###### ", text)

# add a whitespace after the last pound sign
text = re.compile('^# *(?=\w{2})', flags=re.MULTILINE).sub('# ', text)
text = re.compile('^#{2} *(?=\w{2})', flags=re.MULTILINE).sub('## ', text)
text = re.compile('^#{3} *(?=\w{2})', flags=re.MULTILINE).sub('### ', text)
text = re.compile('^#{4} *(?=\w{2})', flags=re.MULTILINE).sub('#### ', text)
text = re.compile('^#{5} *(?=\w{2})', flags=re.MULTILINE).sub('##### ', text)
text = re.compile('^#{6} *(?=\w{2})', flags=re.MULTILINE).sub('###### ', text)

# normalise linebreaks to consisten len
text = re.compile('\n-{5,} *\n', flags=re.MULTILINE).sub("\n---------------------------------------------------------------\n", text)

text = re.compile('^-{5,} *$').sub("---------------------------------------------------------------", text)

text = re.compile('\n\*{5,} *\n', flags=re.MULTILINE).sub("\n******\n", text)

# not 5 because it breaks syntax highlighting
text = re.compile('^\*{5,} *$').sub("******", text)

# comments / notes / indent sections with > , normalise whitespace to 4 spaces
# always, a single space after the first word, this will sort out >, >> and >>>
text = re.compile('\n *> *> *> *(?=[A-Za-z]{2})', flags=re.MULTILINE).sub("\n> > > ", text)

text = re.compile('^ *> *> *> *(?=[A-Za-z]{2})').sub("> > > ", text)

text = re.compile('\n *> *> *(?=[A-Za-z]{2})', flags=re.MULTILINE).sub("\n> > ", text)

text = re.compile('^ *> *> *(?=[A-Za-z]{2})').sub("> > ", text)

text = re.compile('\n *> *(?=[A-Za-z]{2})', flags=re.MULTILINE).sub("\n> ", text)

text = re.compile('^ *> *(?=\[A-Za-z]{2})').sub("> ", text)

# `code` text

text = re.compile('(?<=`) +(?=\S{10}`)').sub('', text)
text = re.compile('(?<=`) +(?=\S{2}`)').sub('', text)
text = re.compile('(?<=`) +(?=\S{3}`)').sub('', text)
text = re.compile('(?<=`) +(?=\S{4}`)').sub('', text)
text = re.compile('(?<=`) +(?=\S{5}`)').sub('', text)
text = re.compile('(?<=`) +(?=\S{6}`)').sub('', text)
text = re.compile('(?<=`) +(?=\S{7}`)').sub('', text)
text = re.compile('(?<=`) +(?=\S{8}`)').sub('', text)
text = re.compile('(?<=`) +(?=\S{9}`)').sub('', text)

text = re.compile('(?<=\S{10}`) +(?=`)').sub('', text)
text = re.compile('(?<=\S{3}`) +(?=`)').sub('', text)
text = re.compile('(?<=\S{4}`) +(?=`)').sub('', text)
text = re.compile('(?<=\S{5}`) +(?=`)').sub('', text)
text = re.compile('(?<=\S{6}`) +(?=`)').sub('', text)
text = re.compile('(?<=\S{7}`) +(?=`)').sub('', text)
text = re.compile('(?<=\S{8}`) +(?=`)').sub('', text)
text = re.compile('(?<=\S{9}`) +(?=`)').sub('', text)
text = re.compile('(?<=`\S{2}) +(?=`)').sub('', text)

############################

# remove trailing whitespace
text = re.compile(' *$', flags=re.MULTILINE).sub("", text)

# wrapping
if MODE == 'sentence':
    [print(line.lstrip(), end="\n") for line in textwrap.TextWrapper().wrap(text)]
elif MODE == 'word':
    print(text, end="")
elif MODE == 'paragraph':
    [print(line.lstrip(), end="\n") for line in textwrap.TextWrapper().wrap(text)]
elif MODE == 'list':
    print(text,end="")
else:
    # fallback on default, safe mode
    print(text, end="")

