       identification division.
       function-id. get-param-value.
       environment division.
       configuration section.
       data division.
       working-storage section.
       01  ws-max-keys                     constant as 100.
       local-storage section.
       01  ls-string-map                   occurs 0 to ws-max-keys times
                                           depending on ls-num-keys.
           05  ls-string-key               pic x(1024).
           05  ls-string-value             pic x(1024).
       01  ls-raw-key-value-strings        pic x(1024)
                                           occurs 0 to ws-max-keys times
                                           depending on ls-num-keys
                                           value spaces.
       01  ls-idx                          pic 9(5) comp-3.
       01  ls-starting-pointer             pic 9(5) comp-3.
       01  ls-num-keys                     pic 9(5) comp-3.
       linkage section.
       01  l-raw-map-string                pic x any length.
       01  l-param-search-key              pic x any length.
       01  l-found-value                   pic x(1024).
       procedure division
           using l-raw-map-string, l-param-search-key
           returning l-found-value.
           move 1 to ls-starting-pointer
           move space to l-found-value
           inspect function trim(l-raw-map-string)
           tallying ls-num-keys for all '='
           if ls-num-keys = 0 then
               goback
           end-if
           perform varying ls-idx from 1 by 1 until ls-idx > ls-num-keys
               unstring l-raw-map-string delimited by space
                   into ls-raw-key-value-strings(ls-idx)
                   with pointer ls-starting-pointer
               end-unstring
           end-perform
           perform varying ls-idx from 1 by 1 until ls-idx > ls-num-keys
               if ls-raw-key-value-strings(ls-idx) not = spaces then
                   unstring ls-raw-key-value-strings(ls-idx)
                       delimited by '=' into
                       ls-string-key(ls-idx)
                       ls-string-value(ls-idx)
                   end-unstring
                   if function trim(ls-string-key(ls-idx)) =
                       function trim(l-param-search-key) then
                       move function trim(ls-string-value(ls-idx))
                       to l-found-value
                       goback
                   end-if
               end-if
           end-perform
           goback.
       end function get-param-value.
