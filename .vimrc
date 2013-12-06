set nocompatible               " be iMproved
filetype off                   " required!

" Basic Settings {{{
set background=light
colorscheme reinh

let mapleader=","

set dir=/tmp//
set undodir=/tmp//
set undofile

set number

set expandtab
set ts=2 sts=2 sw=2

set cmdheight=2

set visualbell

set list

set wildmenu
set wildmode=list:longest,full

set splitright
set splitbelow

set guifont=Source\ Code\ Pro\ Light:h18
set guioptions-=T guioptions-=e guioptions-=L guioptions-=r
set guioptions+=c

set shell=zsh

set grepprg=ag\ --nogroup\ --nocolor\ --column
" }}}

" Plugin Settings {{{

let g:sqlutil_keyword_case = '\U'

" Indent Guides {{{
let g:indent_guides_guide_size=1
" }}}

" Toggle Background {{{
let g:default_background_type = "light"
let g:dark_colorscheme = "Tomorrow-Night"
let g:light_colorscheme = "reinh"
" }}}

" CoffeeTags {{{
if executable('coffeetags')
  let g:tagbar_type_coffee = {
        \ 'ctagsbin' : 'coffeetags',
        \ 'ctagsargs' : '--include-vars',
        \ 'kinds' : [
        \ 'f:functions',
        \ 'o:object',
        \ ],
        \ 'sro' : ".",
        \ 'kind2scope' : {
        \ 'f' : 'object',
        \ 'o' : 'object',
        \ }
        \ }
endif
" }}}

" Commentary {{{
autocmd FileType cabal set commentstring=--\ %s
" }}}

" Haskell Mode {{{
" Configure browser for haskell_doc.vim
let g:haddock_browser = "open"
let g:haddock_browser_callformat = "%s %s"
" }}}

" vim2hs {{{
let g:haskell_conceal = 0
let g:haskell_autotags = 1
let g:haskell_tags_generator = 'hasktags'
let g:hpaste_author = 'ReinH'
" }}}

let g:neocomplete#enable_at_startup = 1
let g:neocomplete#force_overwrite_completefunc = 1
let g:neocomplete#enable_fuzzy_completion = 1
let g:necoghc_enable_detailed_browse = 1

let g:neosnippet#enable_snipmate_compatibility = 1
let g:neosnippet#snippets_directory='~/.vim/bundle/vim-snippets/snippets'

" Recommended key-mappings.
inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function()
  " return neocomplete#smart_close_popup() . "\<CR>"
  " For no inserting <CR> key.
  return pumvisible() ? neocomplete#close_popup() : "\<CR>"
endfunction
" <TAB>: completion.
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
" <C-h>, <BS>: close popup and delete backword char.
inoremap <expr><C-h> neocomplete#smart_close_popup()."\<C-h>"
inoremap <expr><BS> neocomplete#smart_close_popup()."\<C-h>"

" SuperTab like snippets behavior.
imap <expr><TAB> neosnippet#expandable_or_jumpable() ?
  \ "\<Plug>(neosnippet_expand_or_jump)"
  \: pumvisible() ? "\<C-n>" : "\<TAB>"
smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
  \ "\<Plug>(neosnippet_expand_or_jump)"
  \: "\<TAB>"

" For snippet_complete marker.
if has('conceal')
  set conceallevel=2 concealcursor=i
endif

" }}}

" Commands and Mappings {{{
nmap <Leader>e :e **/
cmap <Leader>e **/
cmap %% %:h

nmap <Leader>d :Dispatch<CR>

command! Trim :%s/\s\+$//
" }}}

" Filetype Detection {{{
au BufRead,BufNewFile *.ex compiler exunit
au BufRead,BufNewFile *.exs compiler exunit

au BufEnter *.hs compiler ghc
" }}}

" Vundle Setup {{{
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
" required! 
Bundle 'gmarik/vundle'
" }}}

" Vundles {{{

" General {{{
Bundle 'dahu/bisectly'
Bundle 'Spaceghost/vim-matchit'
Bundle 'scrooloose/syntastic'
Bundle 'nathanaelkane/vim-indent-guides'
Bundle 'gregsexton/gitv'
Bundle 'majutsushi/tagbar'
" Bundle 'kien/ctrlp.vim'
Bundle 'saghul/vim-colortoggle'
Bundle 'davidoc/todo.txt-vim'
Bundle 'godlygeek/tabular'
Bundle 'Align'
Bundle 'christoomey/vim-tmux-navigator'
Bundle 'Shougo/vimproc.vim'
Bundle 'Shougo/neocomplete.vim'
Bundle 'Shougo/neosnippet.vim'
Bundle 'honza/vim-snippets'
Bundle 'Shougo/vimshell.vim'
" }}}

" Unite {{{
Bundle 'Shougo/unite.vim'
Bundle 'ujihisa/unite-haskellimport'
Bundle 'Shougo/unite-help'
Bundle 'Shougo/unite-outline'
Bundle 'ujihisa/unite-gem'
Bundle 'ujihisa/unite-haskellimport'
Bundle 'ujihisa/unite-gem'
Bundle 'tsukkee/unite-tag'
" }}}

" Colors {{{
Bundle 'chriskempson/vim-tomorrow-theme'
" }}}

" tpopeification {{{
Bundle 'tpope/vim-commentary'
Bundle 'tpope/vim-dispatch'
Bundle 'tpope/vim-endwise'
Bundle 'tpope/vim-fugitive'
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-sensible'
Bundle 'tpope/vim-surround'
Bundle 'tpope/vim-unimpaired'
Bundle 'tpope/vim-abolish'
" }}}

" Ruby {{{
Bundle 'vim-ruby/vim-ruby'
Bundle 'tpope/vim-rake'
Bundle 'tpope/vim-rails'
Bundle 'joker1007/vim-ruby-heredoc-syntax'
" }}}

" CoffeeScript {{{
Bundle 'kchmck/vim-coffee-script'
" }}}

" Haskell {{{
" Bundle 'kana/vim-filetype-haskell'
Bundle 'haskell.vim'
Bundle 'lukerandall/haskellmode-vim'
Bundle 'bitc/lushtags'
Bundle 'Twinside/vim-syntax-haskell-cabal'

Bundle 'dag/vim2hs'
Bundle 'hspec/hspec.vim'
Bundle 'eagletmt/ghcmod-vim'
Bundle 'ujihisa/neco-ghc'
" }}}

" Tmux {{{
Bundle 'zaiste/tmux.vim'
" }}}

" Other Langauges {{{
Bundle 'rodjek/vim-puppet'
Bundle 'SQLUtilities'
Bundle 'exu/pgsql.vim'
Bundle 'nono/vim-handlebars'
Bundle 'othree/html5.vim'
Bundle 'wavded/vim-stylus'
Bundle 'reinh/vim-elixir'
" }}}
" }}}

if filereadable(expand('~/.vimrc.local'))
  source ~/.vimrc.local
endif

map <Leader>s :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
      \ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
      \ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>

call unite#filters#matcher_default#use(['matcher_fuzzy'])
call unite#filters#sorter_default#use(['sorter_rank'])
let g:unite_split_rule = 'botright'

nnoremap <C-p> :Unite -start-insert buffer tag file_rec/async<cr>
nnoremap <Leader>f :Unite -start-insert file_rec/async<cr>
nnoremap <Leader>/ :Unite grep:.<cr>
nnoremap <Leader>b :Unite -quick-match buffer <CR>
nnoremap <Leader>h :Unite -start-insert help <CR>
nnoremap <Leader>t :Unite -start-insert tag <CR>
nnoremap <Leader>] :UniteWithCursorWord -quick-match tag <CR>
nnoremap <Leader>o :Unite outline <CR>
nnoremap <leader>m :<C-u>Unite file_mru<CR>

" For ag.
if executable('ag')
  let g:unite_source_grep_command = 'ag'
  let g:unite_source_grep_default_opts = '-i --nogroup --nocolor'
  let g:unite_source_grep_recursive_opt = ''
endif

autocmd FileType unite call s:unite_settings()

function! s:unite_settings()
  inoremap <silent><buffer><expr> <C-s> unite#do_action('split')
  nnoremap <silent><buffer><expr> <C-s> unite#do_action('split')
  inoremap <silent><buffer><expr> <C-v> unite#do_action('vsplit')
  nnoremap <silent><buffer><expr> <C-v> unite#do_action('vsplit')
endfunction

autocmd FileType haskell nnoremap <Leader>i :Unite -start-insert haskellimport <CR>
autocmd FileType haskell nnoremap <Leader>I :UniteWithCursorWord haskellimport <CR>

command! Shell :VimShellPop

" set shell=$SHELL\ -l
set shell=bash

syntax on

" vim: fdm=marker ts=2 sw=2 sts=2

