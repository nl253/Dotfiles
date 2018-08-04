setl shiftwidth=4 tabstop=8 linebreak formatoptions=torcn spell

" jump to headings 
nn <buffer> [[ ?\v^#<CR>
nn <buffer> ]] /\v^#<CR>

nn <buffer> K     :exe '!wn '.expand('<cword>').' -over \| fold --spaces --width='.(join(systemlist("tput cols"), '') - 5).' \| head -n '.(join(systemlist('tput lines'), '') - 5)<CR>
nn @b viw<Esc>a**<Esc>hbi**<Esc>ll
nn @e viw<Esc>a*<Esc>hbi*<Esc>ll

let s:plugin_root = expand('<sfile>:p:h:h:h:h') 

if !exists('g:markdown_fenced_languages')
    let g:markdown_fenced_languages = [
                \ 'haskell', 
                \ 'html', 
                \ 'java',
                \ 'javascript', 
                \ 'python', 
                \ 'sh', 
                \ 'vim', 
                \ ]
endif

" avoid nesting
if !executable('pandoc') | finish | endif

exe 'setl makeprg='.expand('<sfile>:p:h').'/convert.py\ \"%:p\"'

let s:no_md_exts = [
            \ 'ascii_identifiers', 
            \ 'citations', 
            \ 'fancy_lists', 
            \ 'four_space_rule', 
            \ 'grid_tables', 
            \ 'header_attributes', 
            \ 'ignore_line_breaks', 
            \ 'lists_without_preceding_blankline', 
            \ 'mmd_link_attributes', 
            \ 'mmd_title_block', 
            \ 'spaced_reference_links', 
            \ 'startnum', 
            \ 'mmd_header_identifiers', 
            \ 'pandoc_title_block']

let s:md_exts = ['smart', 'intraword_underscores']

exe 'setl formatprg='.escape(join([
            \ 'pandoc', '--quiet', 
            \ '-f', 'markdown', 
            \ '--ascii',
            \ '--atx-headers',
            \ '-t', 'markdown-'.join(s:no_md_exts, '-').'+'.join(s:md_exts, '+'),
            \ '\| sed -E \"s/([-*+]) {2,3}/\\1 /\"'
            \ ], ' '), ' |')

" " guess browser
let s:browser = get(filter([
            \ 'google-chrome-unstable', 
            \ 'google-chrome-beta', 
            \ 'google-chrome-stable', 
            \ 'chromium', 
            \ 'firefox', 
            \ 'firefox-developer-edition'
            \ ], 'executable(v:val)'), 0, $BROWSER)

" preview generated HTML
if !empty(s:browser)
    exe 'nn <buffer> <M-p> :!'.s:browser.' "/tmp/vim/%:p:h:t:r/%:r.html" &<CR>'
endif
