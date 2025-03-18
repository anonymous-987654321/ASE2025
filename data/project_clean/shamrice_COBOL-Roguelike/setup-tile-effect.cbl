       identification division.
       program-id. setup-tile-effect.
       environment division.
       configuration section.
       input-output section.
       file-control.
       data division.
       file section.
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
       01  ws-filler                   pic a.
       01  ws-blank-line               pic a(50) value spaces.
       linkage section.
       01  l-cursor-tile-effect-id            pic 99.
       01  l-cursor-teleport-settings.
           05  l-cursor-tel-dest-y            pic 99.
           05  l-cursor-tel-dest-x            pic 99.
           05  l-cursor-tel-dest-map          pic x(15).              
       01  l-cursor-draw-char                 pic x.
       procedure division using 
           l-cursor-tile-effect-id l-cursor-teleport-settings
           l-cursor-draw-char.
       main-procedure.
           evaluate l-cursor-tile-effect-id
               when ws-no-tile-effect-id
                   goback
               when ws-teleport-effect-id
                   perform setup-teleport
               when ws-conveyor-right-effect-id
                   move ">" to l-cursor-draw-char
               when ws-conveyor-down-effect-id
                   move "v" to l-cursor-draw-char
               when ws-conveyor-left-effect-id
                   move "<" to l-cursor-draw-char
               when ws-conveyor-up-effect-id
                   move "^" to l-cursor-draw-char
               when ws-conveyor-reverse-effect-id
                   move "\" to l-cursor-draw-char
               when ws-player-start-effect-id
                   display "Nothing to set." at 2101
               when other 
                   display ws-blank-line at 2101
                   display "Not implemented. Press any key." at 2101
                   accept ws-filler at 2150 with auto-skip no-echo 
                   display ws-blank-line at 2101
                   move zeros to l-cursor-tile-effect-id                   
           end-evaluate 
           goback.
       setup-teleport.
           display "Enter teleport destination map: " at 2101
           accept l-cursor-tel-dest-map at 2133 update upper           
           display "Enter teleport destination Y position: " at 2101
           accept l-cursor-tel-dest-y at 2140 update 
           display "Enter teleport destination X position: " at 2101
           accept l-cursor-tel-dest-x at 2140 update 
           if l-cursor-tel-dest-map = spaces or l-cursor-tel-dest-y <= 0 
           or l-cursor-tel-dest-x <= 0 then 
               move zeros to l-cursor-tile-effect-id 
               display "Tile effect canceled. Press Enter.  " at 2101
               display "                         " at 2135
               accept ws-filler at 2140
           end-if 
           exit paragraph.
       end program setup-tile-effect.
