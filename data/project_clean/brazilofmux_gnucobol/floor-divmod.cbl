                     identification division.
                     function-id. floor-divmod.
                     data division.
                     working-storage section.
                     01  fdm-tmp pic s9(8) comp-5.
                     linkage section.
                     01  fdm-x   pic s9(8) comp-5.
                     01  fdm-y   pic s9(8) comp-5.
                     01  result.
                         05  fdm-div pic s9(8) comp-5.
                         05  fdm-mod pic s9(8) comp-5.
                     procedure division using fdm-x fdm-y returning result.
                     0100-main.
                         if fdm-x >= 0
                             divide fdm-y into fdm-x giving fdm-div remainder fdm-mod
                         else
                             add 1 to fdm-x giving fdm-tmp
                             subtract fdm-y from fdm-tmp
                             divide fdm-y into fdm-tmp giving fdm-div
                                 remainder fdm-mod
                             add fdm-y to fdm-mod
                             subtract 1 from fdm-mod
                         end-if.
                         goback.
                     end function floor-divmod.
