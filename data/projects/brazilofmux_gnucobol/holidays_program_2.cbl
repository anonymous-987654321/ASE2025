                     identification division.
                     function-id. dayofweek.
                     environment division.
                     configuration section.
                     repository.
                         function floor-divmod
                         function all intrinsic.
                     data division.
                     working-storage section.
                     01  ld2                    signed-int.
                     01  c7           pic s9(8) comp-5 value 7.
                     01  divmod.
                         05  d            pic s9(8) comp-5.
                         05  m            pic s9(8) comp-5.
                     linkage section.
                     01  ld                 usage   signed-int.
                     01  results.
                         05  dow            unsigned-short.
                         05  dow-success    pic x.
                     procedure division using ld returning results.
                     0100-main.
                         add 1 to ld giving ld2.
                         move floor-divmod(ld2, c7) to divmod.
                         move m to dow.
                         move 'Y' to dow-success
                         goback.
                     end function dayofweek.
