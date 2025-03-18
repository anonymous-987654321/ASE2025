identification division.
function-id. c_yearend.
data division.
working-storage section.
    isvalid               usage   signed-int.
    88  notvalid value 0.
linkage section.
    ye_year             usage   signed-short.
    result.
    05  ye_lineardate       usage   signed-int.
    05  ye_bool              pic x.
        88  is_valid       value 'Y'.
        88  is_not_valid   value 'N'.
procedure division using ye_year returning result.
    -main.
    call 'du_yearend' using by value ye_year by reference ye_lineardate returning isvalid.
    if not notvalid
        move 'Y' to ye_bool
    else
        move 'N' to ye_bool
    end-if.
    goback.
end function c_yearend.
