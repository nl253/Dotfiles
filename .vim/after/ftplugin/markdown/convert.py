#!/usr/bin/env python3
"""
The purpose of this script is to convert $1 to a file in /tmp.
"""

import os
import re
import shutil
import subprocess
import sys
from typing import Iterable, List


def _up_by_n_dirs(path: str, n: int) -> str:
    return path if n == 0 else _up_by_n_dirs(os.path.dirname(path), n - 1)


class Settings:
    input_fpath: str = sys.argv[1]
    output_dir: str = '/tmp/vim'
    output_fname: str = os.path.basename(sys.argv[1]).replace('.md', '.html')
    executables: Iterable[str] = ['pandoc']
    css_path: str = os.path.join(
            _up_by_n_dirs(os.path.abspath(__file__), 4), 'css', 'styles.css'
    )
    js_path: str = os.path.join(
            _up_by_n_dirs(os.path.abspath(__file__), 4), 'js', 'script.js'
    )


class Checker:

    def __init__(self,
                 executables: Iterable[str] = Settings.executables) -> None:
        self._executables: List[str] = executables

    def check(self) -> None:
        self._check_input()
        Checker._check_executables(*self.executables)

    @property
    def executables(self) -> Iterable[str]:
        return self._executables

    @executables.setter
    def executables(self, new_executables: Iterable[str]) -> None:
        self._executables = new_executables

    @staticmethod
    def _check_input() -> None:
        assert len(sys.argv[1:2]) == 1, "not given any files"
        assert os.path.isfile(sys.argv[1]
                              ), f"{sys.argv[1]} is not a valid file"

    @staticmethod
    def _check_executables(*executables):
        for exe in executables:
            path: str = shutil.which(exe)
            if type(path) == str:
                path = os.path.realpath(path)
            assert path is not None and os.path.exists(
                path), f"one of the required executables '{exe if not path else path}' not installed"


class Cmd:

    def __init__(self, args: List[str],
                 input_bytes: bytes = str.encode('')) -> None:
        self._args: List[str] = args
        self._input: bytes = input_bytes

    @property
    def input(self) -> bytes:
        return self._input

    @input.setter
    def input(self, _input: bytes) -> None:
        self._input = _input

    @property
    def args(self) -> List[str]:
        return self._args

    @args.setter
    def args(self, new_args: List[str]) -> None:
        self._args = new_args

    def execute(self) -> str:
        err_log: str = '/tmp/vim/errors.log'
        if not os.path.isdir(os.path.dirname(err_log)):
            os.makedirs(os.path.dirname(err_log))
        if not os.path.isfile(err_log):
            os.mknod(err_log)
        with open(err_log, 'w') as log:
            result = subprocess.run(
                    self.args, encoding='utf-8', input=self.input, stderr=log,
                    stdout=subprocess.PIPE
            )
            return result.stdout


class PandocMeta:
    exts: Iterable[str] = {
        'footnotes', 'inline_notes', 'pandoc_title_block',
        'yaml_metadata_block', 'mmd_title_block', 'table_captions',
        'implicit_figures', 'simple_tables', 'multiline_tables', 'grid_tables',
        'pipe_tables', 'citations', 'raw_tex', 'raw_html', 'tex_math_dollars',
        'tex_math_single_backslash', 'tex_math_double_backslash',
        'latex_macros', 'fenced_code_blocks', 'fenced_code_attributes',
        'backtick_code_blocks', 'inline_code_attributes', 'raw_attribute',
        'markdown_in_html_blocks', 'native_divs', 'fenced_divs',
        'native_spans', 'bracketed_spans', 'markdown_attribute',
        'escaped_line_breaks', 'link_attributes', 'mmd_link_attributes',
        'autolink_bare_uris', 'fancy_lists',
        'lists_without_preceding_blankline', 'four_space_rule', 'startnum',
        'definition_lists', 'compact_definition_lists', 'example_lists',
        'all_symbols_escapable', 'angle_brackets_escapable',
        'intraword_underscores', 'blank_before_blockquote',
        'blank_before_header', 'space_in_atx_header', 'strikeout',
        'superscript', 'subscript', 'hard_line_breaks', 'ignore_line_breaks',
        'east_asian_line_breaks', 'literate_haskell', 'abbreviations', 'emoji',
        'auto_identifiers', 'gfm_auto_identifiers', 'ascii_identifiers',
        'header_attributes', 'mmd_header_identifiers',
        'implicit_header_references', 'line_blocks', 'epub_html_exts',
        'shortcut_reference_links', 'smart', 'old_dashes',
        'spaced_reference_links'
    }
    code_styles: Iterable[str] = {
        'pygments', 'tango', 'espresso', 'zenburn', 'kate', 'monochrome',
        'breezedark', 'haddock'
    }
    to_fmts: Iterable[str] = {
        'asciidoc', 'beamer', 'commonmark', 'context', 'docbook', 'docbook4',
        'docbook5', 'docx', 'dokuwiki', 'dzslides', 'epub', 'epub2', 'epub3',
        'fb2', 'gfm', 'haddock', 'html', 'html4', 'html5', 'icml', 'jats',
        'json', 'latex', 'man', 'markdown', 'markdown_github', 'markdown_mmd',
        'markdown_phpextra', 'markdown_strict', 'mediawiki', 'ms', 'muse',
        'native', 'odt', 'opendocument', 'opml', 'org', 'plain', 'revealjs',
        'rst', 'rtf', 's5', 'slideous', 'slidy', 'tei', 'texinfo', 'textile',
        'zimwiki'
    }
    from_fmts: Iterable[str] = {
        'commonmark', 'creole', 'docbook', 'docx', 'epub', 'gfm', 'haddock',
        'html', 'json', 'latex', 'markdown', 'markdown_github', 'markdown_mmd',
        'markdown_phpextra', 'markdown_strict', 'mediawiki', 'muse', 'native',
        'odt', 'opml', 'org', 'rst', 't2t', 'textile', 'tikiwiki', 'twiki',
        'vimwiki'
    }


class PandocCmd(Cmd):

    def __init__(
            self,
            stylesheet: str = Settings.css_path,
            from_fmt: str = 'markdown',
            to_fmt: str = 'html5',
            width: int = 80,
            toc_depth: int = 3,
            long_opts: List[str] = [
                'atx-headers',
                'reference-location=document',
                'mathjax=https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.4/MathJax.js?config=TeX-MML-AM_CHTML',
                'section-divs',
                'email-obfuscation=javascript',
            ],
            exts: List[str] = [
                'ascii_identifiers', 'markdown_attribute', 'autolink_bare_uris',
                'mmd_link_attributes', 'fenced_divs', 'gfm_auto_identifiers',
                'mmd_header_identifiers', 'compact_definition_lists', 'intraword_underscores',
                'tex_math_double_backslash', 'tex_math_single_backslash', 'tex_math_dollars' , 'smart'
            ],
            no_exts: List[str] = [
                'all_symbols_escapable', 'escaped_line_breaks'
            ],
            code_style: str = "tango"
    ) -> None:

        assert to_fmt in PandocMeta.to_fmts, f"to format '{to_fmt}' invalid"
        assert from_fmt in PandocMeta.from_fmts, f"from format '{from_fmt}' invalid"
        for ext in map(lambda ext: ext in PandocMeta.exts, exts + no_exts):
            assert ext, f"unrecognised extension '{ext}'"
        assert code_style in PandocMeta.code_styles, f"code style '{code_style}' invalid"

        self._to_fmt: str = to_fmt
        self._from_fmt = from_fmt + '+' + '+'.join(exts) + '-' + '-'.join(no_exts)

        self._long_opts: List[str] = [
            f'--{opt}' for opt in long_opts + ['standalone']
        ]

        if width and width > 0:
            self._add_long_opt('columns', str(width))

        if toc_depth and toc_depth > 0:
            self._add_long_opt_('toc')
            self._add_long_opt('toc-depth', toc_depth)

        self._add_long_opt('css', stylesheet)
        self._add_long_opt('highlight-style', code_style)

    def _add_long_opt(self, key, val) -> None:
        self._long_opts.append(f'--{key}={val}')

    def _add_long_opt_(self, val: str) -> None:
        self._long_opts.append(f'--{val}')

    @property
    def to_fmt(self) -> str:
        return self._to_fmt

    @to_fmt.setter
    def to_fmt(self, new_to_fmt: str) -> None:
        self._to_fmt = new_to_fmt

    @property
    def from_fmt(self) -> str:
        return self._from_fmt

    @from_fmt.setter
    def from_fmt(self, new_from_fmt: str) -> None:
        self._from_fmt = new_from_fmt

    @property
    def args(self) -> List[str]:
        return [
                   'pandoc', '-f', self._from_fmt, '-t', self._to_fmt
               ] + self._long_opts

    @args.setter
    def args(self, new_args: List[str]) -> None:
        print("not availible in PandocCmd")

    @property
    def long_opts(self) -> List[str]:
        return self._long_opts

    @long_opts.setter
    def long_opts(self, new_long_opts: List[str]) -> None:
        self._long_opts = new_long_opts


class PathUtils:

    def __init__(
            self,
            input_fpath: str = Settings.input_fpath,
            output_dir: str = Settings.output_dir,
            output_fname: str = Settings.output_fname,
            css_path: str = Settings.css_path,
            js_path: str = Settings.js_path
    ) -> None:
        self._input_fpath = input_fpath
        self._output_dir = output_dir
        self._output_fname = output_fname
        self._js_path = js_path
        self._css_path = css_path

    @property
    def input_fpath(self) -> str:
        return self._input_fpath

    @input_fpath.setter
    def input_fpath(self, new_fpath: str) -> None:
        self._input_fpath = new_fpath

    @property
    def js_path(self) -> str:
        assert os.path.isfile(
                self._js_path
        ), f'invalid js path {self._js_path}'
        return self._js_path

    @js_path.setter
    def js_path(self, new_js_path: str) -> None:
        self._js_path = new_js_path

    @property
    def css_path(self) -> str:
        assert os.path.isfile(
                self._css_path
        ), f'invalid css path {self._css_path}'
        return self._css_path

    @css_path.setter
    def css_path(self, new_css_path: str) -> None:
        self._css_path = new_css_path

    @property
    def output_dir(self) -> str:
        if not os.path.isdir(self._output_dir):
            os.makedirs(self._output_dir)
        return self._output_dir

    @output_dir.setter
    def output_dir(self, new_output_dir: str) -> None:
        self._output_dir = new_output_dir

    @property
    def output_fname(self) -> str:
        return self._output_fname

    @output_fname.setter
    def output_fname(self, new_output_fname: str) -> None:
        self._output_fname = new_output_fname

    @property
    def output_path(self) -> str:
        p: str = os.path.join(self.output_dir, os.path.basename(
                os.path.dirname(self.input_fpath)), self.output_fname)
        if not os.path.isfile(p):
            os.makedirs(os.path.dirname(p), exist_ok=True)
            os.mknod(p)
        return p

    @property
    def input_text(self) -> str:
        with open(self._input_fpath, encoding='utf-8') as f:
            return f.read()


class Transformer:

    @staticmethod
    def before(text: str) -> str:
        """
        Preprocess before passing it to pandoc for conversion:

        - remove the need for double backslashes  in LaTeX.
        - fix badly formatted markdown where heading marker `#` is not followed by space
        :param text: input text before conversion
        :return: output text after transformations
        """

        return re.sub(r'(#+)([A-Z])', '\1 \2', text, re.MULTILINE)
        #  return re.sub(r'(#+)([A-Z])', '\1 \2', text.replace('\\', '\\\\'), re.MULTILINE)

    @staticmethod
    def after(text: str) -> str:
        """
        Transform relative links and references.
        :param text: input text
        :return: output after transformations
        """
        # match on either src or href e.g.: `src="./script.js"` and `href="styles.css"`
        # skip over whitespace e.g.: `src="    address/file.png"`
        # match if relative e.g.: `./`
        # or match if not an external link with a protocol e.g.: `https://stackoverflow.com`
        # or match if not a valid directory reference e.g.: `/srv/log/log.txt` and `~/file.txt` 
        pat = re.compile(r'(href|src)="\s*(\./|(?![a-z]{2,10}://|~|\#|/))')
        d: str = os.path.dirname(Settings.input_fpath)
        with open(Settings.js_path) as js:
            return re.sub(pat, f'\\1="{d}/', text).replace('</body>', f'<script>{js.read()}</script></body>')


def main() -> None:
    Checker().check()
    utils: PathUtils = PathUtils()
    text: str = utils.input_text
    text = Transformer.before(text)
    cmd: PandocCmd = PandocCmd()
    cmd.input = text

    with open(utils.output_path, 'w') as dest:
        dest.write(Transformer.after(cmd.execute()))
        print(dest.name)


if __name__ == '__main__':
    main()

# vim:foldmethod=indent:
