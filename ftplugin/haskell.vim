setlocal omnifunc=necoghc#omnifunc
setlocal iskeyword-=.-

let b:ghc_staticoptions = '-isrc'
setlocal path+=src

compiler ghc
