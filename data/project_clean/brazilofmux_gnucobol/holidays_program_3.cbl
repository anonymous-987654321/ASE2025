                     identification division.
                     function-id. kdayonorbefore.
                     environment division.
                     configuration section.
                     repository.
                         function floor-divmod
                         function all intrinsic.
                     data division.
                     working-storage section.
                     01  ld2-max                signed-int.
                     01  c7           pic s9(8) comp-5 value 7.
                     01  divmod.
                         05  d            pic s9(8) comp-5.
                         05  m            pic s9(8) comp-5.
                     linkage section.
                     01  k                   usage   unsigned-short.
                     01  ld-max              usage   signed-int.
                     01  result.
                         05  ld                  usage   signed-int.
                         05  bool              pic x.
                     procedure division using k ld-max returning result.
                     0100-main.
                         compute ld2-max = ld-max - k + 1.
                         move floor-divmod(ld2-max, c7) to divmod.
                         subtract m from ld-max giving ld.
                         move 'Y' to bool.
                         goback.
                     end function kdayonorbefore.
