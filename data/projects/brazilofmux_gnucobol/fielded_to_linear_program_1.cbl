identification division.
function-id. fielded_to_linear.
environment division.
configuration section.
repository.
    function gregorian_to_linear
    function all intrinsic.
data division.
linkage section.
01  year         pic s9(5) comp-5.
01  month        pic 99    comp-5.
01  dom          pic 99    comp-5.
01  linear       pic s9(8) comp-5.
procedure division using year month dom returning linear.
0100-main.
    subtract 584389 from gregorian_to_linear(year, month, dom) giving linear.
    goback.
end function fielded_to_linear.
