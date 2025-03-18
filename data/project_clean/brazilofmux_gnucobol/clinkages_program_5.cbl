identification division.
function-id. c_dayofweek.
data division.
working-storage section.
    isvalid               usage   signed-int.
    88  notvalid value 0.
linkage section.
    ld                  usage   signed-int.
    results.
    05  dayofweek           usage   unsigned-short.
    05  bool              pic x.
        88  is_valid       value 'Y'.
        88  is_not_valid   value 'N'.
procedure division using ld returning results.
    -main.
    call 'du_dayofweek' using by value ld by reference dayofweek returning isvalid.
    if not notvalid
        move 'Y' to bool
    else
        move 'N' to bool
    end-if.
    goback.
end function c_dayofweek.
