setl shiftwidth=2 tabstop=4 linebreak formatoptions=torcn spell conceallevel=0

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

if !executable('pandoc') | finish | endif

exe 'setl makeprg='.expand('<sfile>:p:h').'/convert.py\ \"%:p\"'
exe 'setl formatprg='.escape('pandoc --ascii --atx-headers -f markdown+abbreviations+angle_brackets_escapable+ascii_identifiers+autolink_bare_uris+compact_definition_lists+empty_paragraphs+gfm_auto_identifiers+lists_without_preceding_blankline+markdown_attribute+mmd_header_identifiers+mmd_link_attributes+mmd_title_block+ntb+spaced_reference_links+styles+tex_math_single_backslash -t markdown-fenced_divs-inline_code_attributes-fenced_code_attributes-header_attributes-link_attributes-all_symbols_escapable', ' ')

" preview generated HTML
exe 'nn <buffer> <M-p> :!'.$BROWSER.' "/tmp/vim/%:p:h:t:r/%:r.html" &<CR>'
