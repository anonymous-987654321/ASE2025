                     identification division.
                     function-id. floor-div.
                     data division.
                     working-storage section.
                     01  fdm-tmp pic s9(8) comp-5.
                     linkage section.
                     01  fdm-x   pic s9(8) comp-5.
                     01  fdm-y   pic s9(8) comp-5.
                     01  fdm-div pic s9(8) comp-5.
                     procedure division using fdm-x fdm-y returning fdm-div.
                     0100-main.
                         if fdm-x >= 0
                             divide fdm-y into fdm-x giving fdm-div
                         else
                             add 1 to fdm-x giving fdm-tmp
                             subtract fdm-y from fdm-tmp
                             divide fdm-y into fdm-tmp giving fdm-div
                         end-if.
                         goback.
                     end function floor-div.
