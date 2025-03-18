identification division.
function-id. linear_to_fielded.
environment division.
configuration section.
repository.
     function linear_to_gregorian
     function all intrinsic.
data division.
linkage section.
    linear           pic s9(8) comp-5.
    fielded.
    05  year             pic s9(5) comp-5.
    05  month            pic 99    comp-5.
    05  dom              pic 99    comp-5.
    05  doy              pic 999   comp-5.
    05  dow              pic 9     comp-5.
procedure division using linear returning fielded.
    -main.
    move linear_to_gregorian(584389 + linear) to fielded.
    goback.
end function linear_to_fielded.
