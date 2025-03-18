                     identification division.
                     function-id. yearend.
                     environment division.
                     configuration section.
                     repository.
                         function fielded_to_linear
                         function all intrinsic.
                     data division.
                     working-storage section.
                     01  month           pic 99      comp-5 value 12.
                     01  dom             pic 99      comp-5 value 31.
                     linkage section.
                     01  ye-year              pic s9(5)   comp-5.
                     01  results.
                         05  ye-lineardate        pic s9(8)   comp-5.
                         05  ye-success           pic x.
                     procedure division using ye-year returning results.
                     0100-main.
                         move 'N' to ye-success
                         if (-27256 <= ye-year) and (ye-year <= 30826)
                             move fielded_to_linear(ye-year, month, dom) to ye-lineardate
                             move 'Y' to ye-success
                         end-if.
                         goback.
                     end function yearend.
