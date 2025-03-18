                     identification division.
                     function-id. newyear.
                     environment division.
                     configuration section.
                     repository.
                         function fielded_to_linear
                         function all intrinsic.
                     data division.
                     working-storage section.
                     01  month           pic 99      comp-5 value 1.
                     01  dom             pic 99      comp-5 value 1.
                     linkage section.
                     01  ny-year              pic s9(5)   comp-5.
                     01  results.
                         05  ny-lineardate        pic s9(8)   comp-5.
                         05  ny-success           pic x.
                     procedure division using ny-year returning results.
                     0100-main.
                         move 'N' to ny-success.
                         if (-27256 <= ny-year) and (ny-year <= 30826)
                             move fielded_to_linear(ny-year, month, dom) to ny-lineardate
                             move 'Y' to ny-success
                         end-if.
                         goback.
                     end function newyear.
