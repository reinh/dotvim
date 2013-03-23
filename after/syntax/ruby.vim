let s:bcs = b:current_syntax
unlet b:current_syntax

syntax include @XML syntax/xml.vim
syntax include @SQL syntax/sql.vim

let b:current_syntax = s:bcs
" match optional, surrounding single or double quote and any whitespace in the heredoc name
syntax region rubyHereDocXML matchgroup=Statement start=+<<-\?\(['"]\?\)\z(\s*XML\s*\)\1+ end=+^\s*\z1$+ contains=@XML
syntax region rubyHereDocSQL matchgroup=Statement start=+<<-\?\(['"]\?\)\z(\s*SQL\s*\)\1+ end=+^\s*\z1$+ contains=@SQL
