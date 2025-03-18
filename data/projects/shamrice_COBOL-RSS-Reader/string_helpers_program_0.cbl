       identification division.
       function-id. remove-leading-spaces.
       environment division.
       configuration section.
       repository.
       data division.
       working-storage section.
       01  ws-tab-char                     constant as x"09".
       local-storage section.
       01  ls-space-count                  pic 9(5) value zeros.
       01  ls-length                       pic 9(5) value zeros.
       01  ls-final-offset                 pic 9(5) value zeros.
       01  ls-non-space-found-sw           pic a.
           88  ls-non-space-found          value 'Y'.
           88  ls-non-space-not-found      value 'N'.
       linkage section.
       01  l-field                         pic x any length.
       01  l-updated-record                pic x(:BUFFER-SIZE:) 
                                               value spaces.
       procedure division 
           using l-field
           returning l-updated-record.
       main-procedure.
           initialize l-updated-record
           move function length(l-field) to ls-length
           call "logger" using 
               function concatenate("Removing leading spaces from: ", 
               function trim(l-field), " Length: ", ls-length)
           end-call
           set ls-non-space-not-found to true 
           perform varying ls-space-count from 1 by 1 
           until ls-non-space-found or ls-space-count >= ls-length 
               if l-field(ls-space-count:1) not = space then 
                   if l-field(ls-space-count:1) = ws-tab-char then 
                       add 1 to ls-space-count
                       call "logger" using function concatenate(
                           "Found tab at: " ls-space-count 
                           " tallying character as two spaces")
                   else  
                       set ls-non-space-found to true 
                       subtract 1 from ls-space-count
                   end-if 
               end-if 
           end-perform 
           if ls-space-count > 1 then 
               compute ls-final-offset = ls-length - ls-space-count
               call "logger" using function concatenate("Found ", 
                   ls-space-count, " leading spaces in field. Length: ",
                   ls-length, " : Final offset: ", ls-final-offset)
               end-call
               move l-field(ls-space-count:ls-final-offset) 
                   to l-updated-record
           else              
               call "logger" using function concatenate(
                   "No leading spaces in field. Length: ",
                   ls-length)
               end-call
               move l-field to l-updated-record
           end-if
           goback.
       end function remove-leading-spaces.
