identification division.
function-id. floor-divmod.
data division.
working-storage section.
    fdm-tmp pic s9(8) comp-5.
linkage section.
    fdm-x   pic s9(8) comp-5.
    fdm-y   pic s9(8) comp-5.
    result.
    05  fdm-div pic s9(8) comp-5.
    05  fdm-mod pic s9(8) comp-5.
procedure division using fdm-x fdm-y returning result.
    -main.
    if fdm-x >=  
        divide fdm-y into fdm-x giving fdm-div remainder fdm-mod
    else
        add 1 to fdm-x giving fdm-tmp
        subtract fdm-y from fdm-tmp
        divide fdm-y into fdm-tmp giving fdm-div
            remainder fdm-mod
        add fdm-y to fdm-mod
        subtract 1 from fdm-mod
    end-if.
    goback.
end function floor-divmod.
