if has("gui_running")
    if has("mac")
        silent! set nomacatsui anti enc=utf-8 termencoding=macroman gfn=Monaco:h13
    else
        set lazyredraw
    end

    set guioptions=ecgmrt
    set fuopt=maxhorz,maxvert
endif
