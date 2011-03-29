" Vim compiler file
" Language:		Bacon - a Ruby BDD Framework
" Maintainer:		Rein Henrichs <reinh@reinh.com>

if exists("current_compiler")
  finish
endif
let current_compiler = "bacon"

if exists(":CompilerSet") != 2		" older Vim always used :setlocal
  command -nargs=* CompilerSet setlocal <args>
endif

let s:cpo_save = &cpo
set cpo-=C

CompilerSet makeprg=bacon\ -p

CompilerSet errorformat=%Enot\ ok%.%#,%C#\ %.%#facon%.%#:in\ %m,%Z#\ %f:%l:\ %m,%Z#\ %f:%l:%m,%C#\ %m

let &cpo = s:cpo_save
unlet s:cpo_save

" vim: nowrap sw=2 sts=2 ts=8:
