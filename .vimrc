set nocompatible               " be iMproved
filetype off                   " required!

" Vundle Setup {{{
 set rtp+=~/.vim/bundle/vundle/
 call vundle#rc()

 " let Vundle manage Vundle
 " required! 
 Bundle 'gmarik/vundle'
" }}}

" Vundles {{{
" }}}

" Basic Settings {{{
set number

set visualbell

set wildmenu
set wildmode=list:longest,full

set splitright
set splitbelow

set guifont=Source\ Code\ Pro\ Light:h28
set guioptions-=T guioptions-=e guioptions-=L guioptions-=r
set shell=zsh
" }}}

if filereadable(expand('~/.vimrc.local'))
  source ~/.vimrc.local
endif

" vim:fdm=marker
