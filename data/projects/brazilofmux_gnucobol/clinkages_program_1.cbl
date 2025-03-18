identification division.
function-id. c_fieldedtolinear.
data division.
working-storage section.
01  isvalid               usage   signed-int.
    88  notvalid value 0.
linkage section.
01  ftl_fieldeddate.
    05 year       sync usage   signed-short.
    05 month      sync usage unsigned-short.
    05 dayofweek  sync usage unsigned-short.
    05 dayofmonth sync usage unsigned-short.
    05 dayofyear  sync usage unsigned-short.
01  result.
    05  ftl_lineardate       usage   signed-int.
    05  ftl_bool             pic x.
        88  is_valid       value 'Y'.
        88  is_not_valid   value 'N'.
procedure division using ftl_fieldeddate returning result.
0100-main.
    call 'du_fieldedtolinear' using by reference ftl_fieldeddate by reference ftl_lineardate returning isvalid.
    if not notvalid
        move 'Y' to ftl_bool
    else
        move 'N' to ftl_bool
    end-if
    goback.
end function c_fieldedtolinear.
