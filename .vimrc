" ModeLine and Notes {
" vim: set foldmarker={,} foldlevel=0 foldmethod=marker spell:
"
" This is the personal .vimrc file of Rein Henrichs.
" }

" Environment {
  " Basics
  set nocompatible          " This is Vim, not Vi! (Most be first line)
  set background=dark       " Assume a dark background
  colorscheme reinh         " Load my Tomorrow-Night-Bright variant

  syntax on                 " Enable syntax highlighting
  filetype plugin indent on " Enable filetype detection, ftplugin, and indent

  " Set up pathogen
  call pathogen#infect()

" }

" Settings {
  " General {
    set backspace=indent,eol,start
    set ml mls=5              " Force evaluation of modelines
    set undodir=~/.vim/tmp    " where to put undo files
    set undofile              " Keep undo history in a file
  " }

  " UI {
    set sw=2 ts=2 sts=2       " Two space indent is a good default
    set expandtab smarttab    " Handle tabs correctly
    set autoindent            " Copy indent to new line by default
    set cmdheight=1           " Set command line height to 1 line
    set grepprg=ack           " Use Ack instead of grep
    set incsearch             " Incremental search
    set laststatus=2          " Always show status line
    set list                  " Show tabs and trailing spaces
    set number                " Show line numbers
    set scrolloff=3           " Always show three lines above/below cursor
    set showmatch             " Show matching braces
    set splitbelow splitright " Create split windows in more intuitive places
    set virtualedit=block     " Allow virtual editing in visual block mode
    set visualbell            " Get rid of audio bell
    set foldenable            " Enable folds
  " }

  " Tabs & Trailing Spaces {
    " Show tabs and trailing spaces using UTF characters where available
    if (&termencoding ==# 'utf-8' || &encoding ==# 'utf-8') && version >= 700
      let &listchars = "tab:\u21e5\u00b7,trail:\u2423,extends:\u21c9,precedes:\u21c7,nbsp:\u26ad"
    else
      set listchars=tab:>\ ,trail:-,extends:>,precedes:<
    endif
  " }

  " Completion options {
    " Insert longest common text, always show menu
    set completeopt=longest,menuone
  "}

  " Wildmenu options {
    set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*/.sass-cache/*,tmp/*,.sass-cache/*
    set wildmenu
    set wildmode=longest:full,full " complete longest and open wildmenu, then cycle through full completions
  " }
" }

" Filetype Options {
  augroup FileTypeOptions
    autocmd!
    autocmd FileType vim     setlocal fdm=marker keywordprg=:help
    autocmd FileType haskell setlocal fdl=99 " Open all folds by default
  augroup END
" }

" Key Mappings {
  let mapleader = "," " Map leader to comma

  " Map <Leader>e to glob path matching (**/)
  nmap <Leader>e :e **/
  cmap <Leader>e **/

  " allow a more natural style of line editing in :ex mode
  cnoremap <C-A> <Home>
  cnoremap <C-E> <End>
  cnoremap <C-F> <Right>
  cnoremap <C-B> <Left>
  cnoremap <Esc>b <S-Left>
  cnoremap <Esc>f <S-Right>

  " visual shifting (does not exit Visual mode)
  vnoremap < <gv
  vnoremap > >gv

" }

" Commands {
  " Trim trailing whitespace. Abbrev: :Tr
  command! Trim %s/\v\s+$//

  " Reload .vimrc and .gvimrc
  command! Reload source ~/.vimrc | source ~/.gvimrc
" }

" Abbreviations {
  " http://vim.wikia.com/wiki/Easy_edit_of_files_in_the_same_directory
  cabbr <expr> %% expand('%:p:h')
" }

" Extras {
  " highlight end of line whitespace as Error
  hi link ExtraWhitespace Error
  au BufNewFile,BufRead,InsertLeave * match ExtraWhitespace /\s\+$/

  " except the line I am typing on
  au InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
" }

" Plugin Settings {
  " vim2hs {
    " Enable 'wide conceals' for type colons and function arrows
    let g:haskell_conceal_wide = 1
  " }

  " powerline {
    let g:Powerline_symbols = 'fancy'
  " }

  " haskellmode {
    " Configure browser for haskell_doc.vim
    let g:haddock_browser = "open"
    let g:haddock_browser_callformat = "%s %s"
  " }
" }
