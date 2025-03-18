identification division.
function-id. floor-div.
data division.
working-storage section.
    fdm-tmp pic s9(8) comp-5.
linkage section.
    fdm-x   pic s9(8) comp-5.
    fdm-y   pic s9(8) comp-5.
    fdm-div pic s9(8) comp-5.
procedure division using fdm-x fdm-y returning fdm-div.
    -main.
    if fdm-x >=  
        divide fdm-y into fdm-x giving fdm-div
    else
        add 1 to fdm-x giving fdm-tmp
        subtract fdm-y from fdm-tmp
        divide fdm-y into fdm-tmp giving fdm-div
    end-if.
    goback.
end function floor-div.
