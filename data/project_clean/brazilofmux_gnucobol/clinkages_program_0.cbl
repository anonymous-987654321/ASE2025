identification division.
function-id. c_isvaliddate.
data division.
working-storage section.
    ft_year            usage   signed-short.
    ft_month           usage unsigned-short.
    ft_day             usage unsigned-short.
    isvalid            usage   signed-int.
    88  notvalid value 0.
linkage section.
    ivd_year            usage   signed-short.
    ivd_month           usage unsigned-short.
    ivd_day_of_month    usage unsigned-short.
    ivd_valid           pic x.
    88  ivd_is_valid_date       value 'Y'.
    88  ivd_is_not_valid_date   value 'N'.
procedure division using ivd_year ivd_month ivd_day_of_month returning ivd_valid.
    -main.
    move ivd_year to ft_year.
    move ivd_month to ft_month.
    move ivd_day_of_month to ft_day.
    call 'du_isvaliddate' using by value ft_year by value ft_month by value ft_day returning isvalid.
    if notvalid
        move 'N' to ivd_valid
    else
        move 'Y' to ivd_valid
    end-if.
    goback.
end function c_isvaliddate.
