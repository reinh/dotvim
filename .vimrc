" Set up pathogen {{{
silent! call pathogen#runtime_append_all_bundles()
silent! call pathogen#helptags()
" }}}

" Settings {{{
set background=dark
colorscheme reinh

set nocompatible " This is Vim, not Vi!
syntax on " Enable syntax highlighting
filetype plugin indent on " Enable filetype detection, ftplugin, and indent

set sw=2 ts=2 sts=2 expandtab smarttab " Two space indent is a good default

set autoindent " Copy indent by default
set visualbell " Get rid of audio bell
set ml mls=5   " Force evaluation of modelines
set number     " Show line numbers
set incsearch  " Incremental search
set showmatch  " Show matching braces
set undofile   " Keep undo history in a file

set grepprg=ack " Use Ack instead of grep
set virtualedit=block " Allow virtual editing in visual block mode
set scrolloff=3 " Always show three lines above/below cursor
set cmdheight=1 " Set command line height to 1 line
set list        " Show tabs and trailing spaces

set splitbelow splitright " Create split windows in more intuitive places

" Show tabs and trailing spaces using UTF characters where available
if (&termencoding ==# 'utf-8' || &encoding ==# 'utf-8') && version >= 700
  let &listchars = "tab:\u21e5\u00b7,trail:\u2423,extends:\u21c9,precedes:\u21c7,nbsp:\u26ad"
else
  set listchars=tab:>\ ,trail:-,extends:>,precedes:<
endif
"}}}

" Filetype Options {{{
augroup FileTypeOptions
  autocmd!
  autocmd FileType vim setlocal fdm=marker keywordprg=:help
augroup END
" }}}

" Mappings {{{
let mapleader = "," " Map leader to comma

" Map <Leader>e to glob path matching (**/)
nmap <Leader>e :e **/
cmap <Leader>e **/
" }}}

" Commands {{{

command! Trim %s/\v\s+$//
command! Reload source ~/.vimrc | source ~/.gvimrc


" allow a more natural style of line editing in :ex mode
cnoremap <C-A> <Home>
cnoremap <C-E> <End>
cnoremap <C-F> <Right>
cnoremap <C-B> <Left>
cnoremap <Esc>b <S-Left>
cnoremap <Esc>f <S-Right>

" }}}

" Extras {{{

" highlight end of line whitespace as Error
hi link ExtraWhitespace Error
au BufNewFile,BufRead,InsertLeave * match ExtraWhitespace /\s\+$/

" except the line I am typing on
au InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/

" }}}

" Abbreviations {{{

" http://vim.wikia.com/wiki/Easy_edit_of_files_in_the_same_directory
cabbr <expr> %% expand('%:p:h')

" }}}

" Status Line {{{
" }}}
