identification division.
function-id. c_newyear.
data division.
working-storage section.
01  isvalid               usage   signed-int.
    88  notvalid value 0.
linkage section.
01  ny_year             usage   signed-short.
01  results.
    05  ny_lineardate       usage   signed-int.
    05  ny_bool              pic x.
        88  is_valid       value 'Y'.
        88  is_not_valid   value 'N'.
procedure division using ny_year returning results.
0100-main.
    call 'du_newyear' using by value ny_year by reference ny_lineardate returning isvalid.
    if not notvalid
        move 'Y' to ny_bool
    else
        move 'N' to ny_bool
    end-if.
    goback.
end function c_newyear.
