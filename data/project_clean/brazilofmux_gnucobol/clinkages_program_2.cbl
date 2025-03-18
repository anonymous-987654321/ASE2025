identification division.
function-id. c_lineartofielded.
data division.
working-storage section.
    isvalid               usage   signed-int.
    88  notvalid value 0.
linkage section.
    ltf_lineardate       usage   signed-int.
    result.
    05  ltf_fieldeddate.
        10 year       sync usage   signed-short.
        10 month      sync usage unsigned-short.
        10 dayofweek  sync usage unsigned-short.
        10 dayofmonth sync usage unsigned-short.
        10 dayofyear  sync usage unsigned-short.
    05  ltf_bool           pic x.
        88  is_valid       value 'Y'.
        88  is_not_valid   value 'N'.
procedure division using ltf_lineardate returning result.
    -main.
    call 'du_lineartofielded' using by value ltf_lineardate by reference ltf_fieldeddate returning isvalid.
    if not notvalid
        move 'Y' to ltf_bool
    else
        move 'N' to ltf_bool
    end-if.
    goback.
end function c_lineartofielded.
