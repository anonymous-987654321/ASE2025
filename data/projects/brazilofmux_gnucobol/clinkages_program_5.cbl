                     identification division.
                     function-id. c_dayofweek.
                     data division.
                     working-storage section.
                     01  isvalid               usage   signed-int.
                         88  notvalid value 0.
                     linkage section.
                     01  ld                  usage   signed-int.
                     01  results.
                         05  dayofweek           usage   unsigned-short.
                         05  bool              pic x.
                             88  is_valid       value 'Y'.
                             88  is_not_valid   value 'N'.
                     procedure division using ld returning results.
                     0100-main.
                         call 'du_dayofweek' using by value ld by reference dayofweek returning isvalid.
                         if not notvalid
                             move 'Y' to bool
                         else
                             move 'N' to bool
                         end-if.
                         goback.
                     end function c_dayofweek.
