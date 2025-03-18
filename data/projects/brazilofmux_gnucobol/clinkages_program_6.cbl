identification division.
function-id. c_kdayonorbefore.
data division.
working-storage section.
01  isvalid               usage   signed-int.
    88  notvalid value 0.
linkage section.
01  k                   usage   unsigned-short.
01  ld-max              usage   signed-int.
01  results.
    05  ld                  usage   signed-int.
    05  bool              pic x.
        88  is_valid       value 'Y'.
        88  is_not_valid   value 'N'.
procedure division using k ld-max returning results.
0100-main.
    call 'du_kdayonorbefore' using by value k ld-max by reference ld returning isvalid.
    if not notvalid
        move 'Y' to bool
    else
        move 'N' to bool
    end-if.
    goback.
end function c_kdayonorbefore.
