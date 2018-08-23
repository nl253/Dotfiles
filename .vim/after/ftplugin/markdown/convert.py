#!/usr/bin/env python3
"""
Convert $1 -- a markdown file to an HTML file in /tmp.
"""

# Standard Library
import os
import re
import shutil
import subprocess
import sys
from typing import Iterable, List, Optional, Set, Text, Union


def _up_by_n_dirs(path: Text, n: int) -> Text:
    return path if n == 0 else _up_by_n_dirs(os.path.dirname(path), n - 1)


def vimdir_path(*components) -> Text:
    return os.path.join(
        _up_by_n_dirs(os.path.abspath(__file__), 4),
        *components)


def ensure_exists(fpath: Text) -> None:
    if not os.path.isdir(os.path.dirname(fpath)):
        os.makedirs(os.path.dirname(fpath))
    if not os.path.isfile(fpath):
        os.mknod(fpath)


class MathJaxMeta:
    """List of all availible configuration for MathJax.
    This is used to later check if the chosen config is valid.
    """
    confs: Set[Text] = {
        "AM_CHTML",
        "AM_HTMLorMML",
        "AM_SVG",
        "MML_CHTML",
        "MML_HTMLorMML",
        "MML_SVG",
        "TeX-AMS-MML_HTMLorMML",
        "TeX-AMS-MML_SVG",
        "TeX-AMS_CHTML",
        "TeX-AMS_HTML",
        "TeX-AMS_SVG",
        "TeX-MML-AM_HTMLorMML",
        "TeX-MML-AM_SVG",
        "TeX-MML-AM_CHTML"}


class PandocMeta:
    """List of all availible configuration for MathJax.
    This is used to later check if the chosen config is valid."""
    exts: Set[Text] = {
        'abbreviations',
        'all_symbols_escapable',
        'angle_brackets_escapable',
        'ascii_identifiers',
        'auto_identifiers',
        'autolink_bare_uris',
        'backtick_code_blocks',
        'blank_before_blockquote',
        'blank_before_header',
        'bracketed_spans',
        'citations',
        'compact_definition_lists',
        'definition_lists',
        'east_asian_line_breaks',
        'emoji',
        'epub_html_exts',
        'escaped_line_breaks',
        'example_lists',
        'fancy_lists',
        'fenced_code_attributes',
        'fenced_code_blocks',
        'fenced_divs',
        'footnotes',
        'four_space_rule',
        'gfm_auto_identifiers',
        'grid_tables',
        'hard_line_breaks',
        'header_attributes',
        'ignore_line_breaks',
        'implicit_figures',
        'implicit_header_references',
        'inline_code_attributes',
        'inline_notes',
        'intraword_underscores',
        'latex_macros',
        'line_blocks',
        'link_attributes',
        'lists_without_preceding_blankline',
        'literate_haskell',
        'markdown_attribute',
        'markdown_in_html_blocks',
        'mmd_header_identifiers',
        'mmd_link_attributes',
        'mmd_title_block',
        'multiline_tables',
        'native_divs',
        'native_spans',
        'old_dashes',
        'pandoc_title_block',
        'pipe_tables',
        'raw_attribute',
        'raw_html',
        'raw_tex',
        'shortcut_reference_links',
        'simple_tables',
        'smart',
        'space_in_atx_header',
        'spaced_reference_links'
        'startnum',
        'strikeout',
        'subscript',
        'superscript',
        'table_captions',
        'tex_math_dollars',
        'tex_math_double_backslash',
        'tex_math_single_backslash',
        'yaml_metadata_block',
    }
    code_styles: Set[Text] = {
        'breezedark',
        'espresso',
        'haddock'
        'kate',
        'monochrome',
        'pygments',
        'tango',
        'zenburn',
    }
    to_fmts: Set[Text] = {
        'asciidoc',
        'beamer',
        'commonmark',
        'context',
        'docbook',
        'docbook4',
        'docbook5',
        'docx',
        'dokuwiki',
        'dzslides',
        'epub',
        'epub2',
        'epub3',
        'fb2',
        'gfm',
        'haddock',
        'html',
        'html4',
        'html5',
        'icml',
        'jats',
        'json',
        'latex',
        'man',
        'markdown',
        'markdown_github',
        'markdown_mmd',
        'markdown_phpextra',
        'markdown_strict',
        'mediawiki',
        'ms',
        'muse',
        'native',
        'odt',
        'opendocument',
        'opml',
        'org',
        'plain',
        'revealjs',
        'rst',
        'rtf',
        's5',
        'slideous',
        'slidy',
        'tei',
        'texinfo',
        'textile',
        'zimwiki'
    }
    from_fmts: Set[Text] = {
        'commonmark',
        'creole',
        'docbook',
        'docx',
        'epub',
        'gfm',
        'haddock',
        'html',
        'json',
        'latex',
        'markdown',
        'markdown_github',
        'markdown_mmd',
        'markdown_phpextra',
        'markdown_strict',
        'mediawiki',
        'muse',
        'native',
        'odt',
        'opml',
        'org',
        'rst',
        't2t',
        'textile',
        'tikiwiki',
        'twiki',
        'vimwiki'
    }


class Settings:

    def __init__(self) -> None:

        self.input_fpath: Text = sys.argv[1]

        assert self.input_fpath and os.path.isfile(self.input_fpath), \
            f'invalid input file path {str(self.input_fpath)}'

        output_fname: Text = os.path.basename(
            self.input_fpath).replace('.md', '.html')

        self.output_path: Text = \
            os.path.join('/tmp/vim',
                         os.path.basename(os.path.dirname(self.input_fpath)),
                         output_fname)

        ensure_exists(self.output_path)

        self.js_path: Text = vimdir_path('js', 'script.js')
        assert os.path.isfile(self.js_path), f'invalid js path {self.js_path}'

        self.css_path: Text = vimdir_path('css', 'styles.css')
        assert os.path.isfile(
            self.css_path), f'invalid css path {self.css_path}'

        self.executables: Iterable[Text] = ['pandoc']

        # verify that all necessary executables are present
        for exe in self.executables:
            path: Optional[Text] = shutil.which(exe)
            if path is not None and (type(path) == str):
                path = os.path.realpath(path)
            assert path is not None and os.path.exists(
                path), f"one of the required executables \
                        '{exe if not path else path}' not installed"

    @property
    def input_text(self) -> Text:
        with open(self.input_fpath, encoding='utf-8') as f:
            return f.read()


class PandocCmd:

    def __init__(
            self,
            stylesheet: Text = Settings().css_path,
            from_fmt: Text = 'markdown',
            to_fmt: Text = 'html5',
            width: int = 80,
            toc_depth: int = 3,
            long_opts: Set[Text] = {
                'atx-headers',
                'reference-location=document',
                'section-divs',
                'email-obfuscation=javascript',
            },
            exts: Set[Text] = {
                'ascii_identifiers',
                'autolink_bare_uris',
                'compact_definition_lists',
                'fenced_divs',
                'gfm_auto_identifiers',
                'intraword_underscores',
                'markdown_attribute',
                'mmd_header_identifiers',
                'mmd_link_attributes',
                'smart',
                'tex_math_dollars',
                'tex_math_double_backslash',
                'tex_math_single_backslash',
            },
            no_exts: Set[Text] = {
                'all_symbols_escapable', 'escaped_line_breaks'
            },
            code_style: Text = "tango",
            mathjax_version: Text = "2.7.4",
            mathjax_conf: Text = "TeX-AMS_HTML"
    ) -> None:

        self.input: Union[bytes, Text] = b""

        PandocCmd._validate_to_fmt(to_fmt)
        self.to_fmt: Text = to_fmt

        PandocCmd._validate_from_fmt(from_fmt)
        self.from_fmt = f"{from_fmt}+{'+'.join(exts)}-{'-'.join(no_exts)}"

        self.long_opts: Set[Text] = {f'--{opt}' for opt in long_opts}

        def add_long_opt(key: Text, val=None):
            self.long_opts.add(f'--{key}'if not val else f'--{key}={val}')

        add_long_opt('standalone')

        PandocCmd._validate_pandoc_exts(exts | no_exts)

        PandocCmd._validate_mathjax(mathjax_version, mathjax_conf)
        add_long_opt(
            'mathjax',
            f'https://cdnjs.cloudflare.com/ajax/libs/mathjax/{mathjax_version}/MathJax.js?config={mathjax_conf}')

        PandocCmd._validate_n(width)
        add_long_opt('columns', str(width))

        PandocCmd._validate_n(toc_depth)
        add_long_opt('toc')
        add_long_opt('toc-depth', toc_depth)

        PandocCmd._validate_file(stylesheet)
        add_long_opt('css', stylesheet)

        PandocCmd._validate_code_style(code_style)
        add_long_opt('highlight-style', code_style)

    @staticmethod
    def _validate_file(fpath: Text) -> None:
        assert fpath and os.path.isfile(fpath), f"invalid file {str(fpath)}"

    @staticmethod
    def _validate_from_fmt(fmt: Text) -> None:
        assert fmt in PandocMeta.from_fmts, f"from format '{fmt}' invalid"

    @staticmethod
    def _validate_to_fmt(fmt: Text) -> None:
        assert fmt in PandocMeta.to_fmts, f"to format '{fmt}' invalid"

    @staticmethod
    def _validate_n(n: int) -> None:
        assert n and n >= 0, f"invalid value {str(n)}"

    @staticmethod
    def _validate_code_style(style: Text) -> None:
        assert style and style in PandocMeta.code_styles, \
            f"code style {str(style)} is invalid"

    @staticmethod
    def _validate_mathjax(version: Text, cfg: Text) -> None:
        assert cfg and cfg in MathJaxMeta.confs, \
            f"unreconginsed MathJax config {str(cfg)}"
        assert version and len(version) >= 3 and version[0] == '2' and version[1] == '.' and str.isdigit(
            version[2]), \
            f"unrecognised MathJax version {str(version)}"

    @staticmethod
    def _validate_pandoc_exts(extensions: Iterable[Text]) -> None:
        for ext in extensions:
            assert ext and (
                ext in PandocMeta.exts), f"unrecognised extension '{ext}'"

    @property
    def args(self) -> List[Text]:
        return ['pandoc', '-f', self.from_fmt, '-t', self.to_fmt] + list(self.long_opts)

    def execute(self) -> Text:
        err_log: Text = '/tmp/vim/errors.log'
        ensure_exists(err_log)
        with open(err_log, 'w') as log:
            pipe = subprocess.run(
                self.args, encoding='utf-8',
                input=self.input, stderr=log,
                stdout=subprocess.PIPE)
            return pipe.stdout


class Transformer:

    @staticmethod
    def before(text: Text) -> Text:
        """
        Preprocess before passing it to pandoc for conversion:

        - remove the need for double backslashes  in LaTeX.
        - fix badly formatted markdown where heading marker `#` is not followed by space
        :param text: input text before conversion
        :return: output text after transformations
        """
        return re.sub(r'(#+)([A-Z])', '\1 \2', text, re.MULTILINE)

    @staticmethod
    def after(text: Text) -> Text:
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
        cfg: Settings = Settings()
        pat = re.compile(r'(href|src)="\s*(\./|(?![a-z]{2,10}://|~|\#|/))')
        d: Text = os.path.dirname(cfg.input_fpath)
        with open(cfg.js_path) as js:
            return re.sub(pat, f'\\1="{d}/', text).replace('</body>', f'<script>{js.read()}</script></body>')


def main() -> None:
    cfg: Settings = Settings()
    text: Text = Transformer.before(cfg.input_text)
    cmd: PandocCmd = PandocCmd()
    cmd.input = text

    with open(cfg.output_path, 'w') as dest:
        dest.write(Transformer.after(cmd.execute()))
        print(dest.name)


if __name__ == '__main__':
    main()

# vim:foldmethod=manual:
