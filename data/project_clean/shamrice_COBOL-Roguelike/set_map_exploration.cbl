       identification division.
       program-id. set-map-exploration.
       environment division.
       data division.
       working-storage section.
       01  black                          constant as 0.
       01  blue                           constant as 1.
       01  green                          constant as 2.
       01  cyan                           constant as 3.
       01  red                            constant as 4.
       01  magenta                        constant as 5.
       01  yellow                         constant as 6.  
       01  white                          constant as 7.
       78  ws-no-tile-effect-id           value 0.    
       78  ws-teleport-effect-id          value 1.
       78  ws-conveyor-right-effect-id    value 2.
       78  ws-conveyor-down-effect-id     value 3.
       78  ws-conveyor-left-effect-id     value 4.
       78  ws-conveyor-up-effect-id       value 5.
       78  ws-conveyor-reverse-effect-id  value 6.
       78  ws-player-start-effect-id      value 98.
       78  ws-load-map-tele-return-code   value 1.
       78  ws-max-view-height             value 20.
       78  ws-max-view-width              value 50.
       78  ws-max-map-height              value 25.
       78  ws-max-map-width               value 80.
       78  ws-max-num-enemies             value 99.      
       78  ws-max-num-teleports           value 999.
       78  ws-max-num-items               value 999.
       78  ws-file-status-ok              value "00".
       78  ws-file-status-eof             value "10".
       78  ws-load-status-fail            value 9.
       78  ws-load-status-read-fail       value 8.
       78  ws-load-status-bad-data        value 7.
       78  ws-load-status-success         value 0.       
       78  ws-save-status-fail            value 9.
       78  ws-save-status-success         value 0.
       78  ws-data-file-ext               value ".DAT".
       78  ws-teleport-file-ext           value ".TEL".
       78  ws-enemy-file-ext              value ".BGS".
       78  ws-item-file-ext               value ".ITM".
       local-storage section.
       01  ls-start-idx-y             pic S99 comp.
       01  ls-end-idx-y               pic S99 comp.
       01  ls-start-idx-x             pic S99 comp.
       01  ls-end-idx-x               pic S99 comp.
       01  ls-idx-y                   pic S99 comp.
       01  ls-idx-x                   pic S99 comp.
       linkage section.
       01  l-map-explored-data.
           05  l-map-explored-y         occurs ws-max-map-height times.
               10  l-map-explored-x     occurs ws-max-map-width times.
                   15  l-map-explored        pic a value 'N'.
                       88  l-is-explored     value 'Y'.
                       88  l-is-not-explored value 'N'.
       01  l-cur-map-pos.
           05  l-cur-map-pos-y        pic S99.
           05  l-cur-map-pos-x        pic S99.
       01  l-tile-visibility          pic 999 comp.
       procedure division using 
           l-map-explored-data l-cur-map-pos l-tile-visibility.
       main-procedure.
           compute ls-start-idx-y = l-cur-map-pos-y - l-tile-visibility
           compute ls-end-idx-y = l-cur-map-pos-y + l-tile-visibility
           compute ls-start-idx-x = l-cur-map-pos-x - l-tile-visibility
           compute ls-end-idx-x = l-cur-map-pos-x + l-tile-visibility
           perform varying ls-idx-y from ls-start-idx-y by 1 
           until ls-idx-y > ls-end-idx-y
               perform varying ls-idx-x from ls-start-idx-x by 1 
               until ls-idx-x > ls-end-idx-x 
                   if ls-idx-y > 0 and ls-idx-x > 0 
                   and ls-idx-y < ws-max-map-height 
                   and ls-idx-x < ws-max-map-width then 
                       set l-is-explored(
                           ls-idx-y, ls-idx-x) to true 
                   end-if 
               end-perform
           end-perform
           goback.
       end program set-map-exploration.
