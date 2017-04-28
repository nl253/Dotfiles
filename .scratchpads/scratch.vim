
nnoremap <Leader>sg :execute 'vimgrep '.expand('<cword>')." ".glob('~/Notes/**.md')." ".glob('~/Notes/**.rst')<CR>
