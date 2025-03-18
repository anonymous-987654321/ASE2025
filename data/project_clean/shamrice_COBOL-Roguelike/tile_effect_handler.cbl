       identification division.
       program-id. tile-effect-handler.
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
       01  ws-tele-idx                   pic 999 comp.
       01  ws-idx-y                      pic 999 comp.
       01  ws-idx-x                      pic 999 comp.
       01  ws-color-temp              pic 9.
       01  ws-swap-colors-sw             pic a value 'N'.
           88  ws-swap-colors            value 'Y'.
           88  ws-not-swap-colors        value 'N'.
       local-storage section.
       01  ls-action-history-temp          pic x(75).      
       linkage section.
       01  l-tile-effect-id-src            pic 99 comp.
       01  l-tile-char-src                 pic x.       
       01  l-player.
           05  l-player-name              pic x(16).
           05  l-player-hp.
               10  l-player-hp-current    pic 999.
               10  l-player-hp-max        pic 999.
           05  l-player-pos.
               10  l-player-y             pic S99.
               10  l-player-x             pic S99.
           05  l-player-pos-delta.    
               10  l-player-pos-delta-y   pic S99.
               10  l-player-pos-delta-x   pic S99.
           05  l-player-scr-pos.  
               10  l-player-scr-y         pic 99 value 10.
               10  l-player-scr-x         pic 99 value 20.
           05  l-player-status              pic 9 value 0.
               88  l-player-status-alive    value 0.
               88  l-player-status-dead     value 1.
               88  l-player-status-attacked value 2.
               88  l-player-status-other    value 3.                   
           05  l-player-attack-damage.    
               10  l-player-atk-cur       pic 999.
               10  l-player-atk-base      pic 999.
           05  l-player-defense-power.
               10  l-player-def-cur       pic 999.
               10  l-player-def-base      pic 999.
           05  l-player-level             pic 999.
           05  l-player-experience.
               10  l-player-exp-total     pic 9(7).                   
               10  l-player-exp-next-lvl  pic 9(7).    
           78  l-player-char              value "@".
       01  l-temp-map-pos.
           05  l-temp-map-pos-y        pic S99.
           05  l-temp-map-pos-x        pic S99.
       01  l-teleport-data.
           05  l-cur-num-teleports        pic 999 comp.
           05  l-teleport-data-record     occurs 0 
                                          to ws-max-num-teleports
                                      depending on l-cur-num-teleports.
               10  l-teleport-pos.
                   15  l-teleport-y        pic S99.
                   15  l-teleport-x        pic S99.
               10  l-teleport-dest-pos.
                   15  l-teleport-dest-y   pic S99.
                   15  l-teleport-dest-x   pic S99.
               10  l-teleport-dest-map     pic x(15).  
       01  l-map-files.  
           05  l-map-name             pic x(15).
           05  l-map-name-temp        pic x(15).           
           05  l-map-dat-file         pic x(15).               
           05  l-map-tel-file         pic x(15).
           05  l-map-enemy-file       pic x(15).
       01  l-tile-map-table-matrix.
           05  l-tile-map           occurs ws-max-map-height times.
               10  l-tile-map-data  occurs ws-max-map-width times.
                   15  l-tile-fg                   pic 9.   
                   15  l-tile-bg                   pic 9.
                   15  l-tile-char                 pic x.
                   15  l-tile-highlight            pic a value 'N'.
                       88 l-tile-is-highlight      value 'Y'.
                       88 l-tile-not-highlight     value 'N'.
                   15  l-tile-blocking             pic a value 'N'.
                       88  l-tile-is-blocking      value 'Y'.
                       88  l-tile-not-blocking     value 'N'.  
                   15  l-tile-blinking             pic a value 'N'.
                       88  l-tile-is-blinking      value 'Y'.
                       88  l-tile-not-blinking     value 'N'.
                   15  l-tile-effect-id            pic 99 comp.  
                   15  l-tile-visibility           pic 999 comp.     
       01  l-tile-effect-return-code      pic 99.
       01  l-player-moved-sw              pic a.
           88  l-player-moved             value 'Y'.
           88  l-player-not-moved         value 'N'.
       01  l-action-history.
           05  l-action-history-item     occurs 150 times.
               10  l-action-history-text pic x(75).
       procedure division using 
           l-tile-effect-id-src 
           l-tile-char-src
           l-player l-temp-map-pos
           l-teleport-data l-map-files 
           l-tile-map-table-matrix
           l-player-moved-sw
           l-action-history
           l-tile-effect-return-code.
       main-procedure.
           move zeros to l-tile-effect-return-code
           if l-tile-effect-id-src is zeros then 
               goback
           end-if 
           evaluate l-tile-effect-id-src
               when ws-teleport-effect-id
                   perform check-teleport
               when ws-conveyor-right-effect-id
                   perform handle-conveyor-right
               when ws-conveyor-down-effect-id
                   perform handle-conveyor-down
               when ws-conveyor-left-effect-id
                   perform handle-conveyor-left
               when ws-conveyor-up-effect-id                   
                   perform handle-conveyor-up
               when ws-conveyor-reverse-effect-id 
                   if l-player-moved then                   
                       perform handle-conveyor-reverse-switch
                   end-if 
           end-evaluate
           goback.
       check-teleport.
           if l-cur-num-teleports = 0 then 
               exit paragraph
           end-if 
           perform varying ws-tele-idx 
           from 1 by 1 until ws-tele-idx > l-cur-num-teleports
               if l-teleport-pos(ws-tele-idx) = l-temp-map-pos then 
                   compute l-player-y = 
                       l-teleport-dest-y(ws-tele-idx) - l-player-scr-y
                   end-compute 
                   compute l-player-x = 
                       l-teleport-dest-x(ws-tele-idx) - l-player-scr-x
                   end-compute 
                   if l-teleport-dest-map(ws-tele-idx) 
                   not = l-map-name then
                       move l-teleport-dest-map(ws-tele-idx) 
                           to l-map-name-temp                       
                   end-if 
                   exit perform 
               end-if 
           end-perform           
           if l-map-name-temp not = l-map-name then    
               move l-map-name-temp to l-map-name             
               move ws-load-map-tele-return-code 
                   to l-tile-effect-return-code                   
           end-if    
           exit paragraph.    
       handle-conveyor-right.           
           add 1 to l-player-x              
           exit paragraph.
       handle-conveyor-down.           
           add 1 to l-player-y
           exit paragraph.
       handle-conveyor-left.           
           subtract 1 from l-player-x          
           exit paragraph.
       handle-conveyor-up.           
           subtract 1 from l-player-y           
           exit paragraph.           
       handle-conveyor-reverse-switch.
           if l-tile-char-src = '\' then 
               move '/' to l-tile-char-src
               move "Switch pressed. Conveyor belt direction: REVERSE"
                   to ls-action-history-temp
           else 
               move '\' to l-tile-char-src
               move "Switch pressed. Conveyor belt direction: FORWARD"
                   to ls-action-history-temp
           end-if   
           call "add-action-history-item" using
               ls-action-history-temp l-action-history
           end-call                         
           perform varying ws-idx-y 
           from 1 by 1 until ws-idx-y > ws-max-map-height
               perform varying ws-idx-x 
               from 1 by 1 until ws-idx-x > ws-max-map-width                   
                   set ws-not-swap-colors to true 
                   evaluate l-tile-effect-id(ws-idx-y, ws-idx-x)
                       when ws-conveyor-right-effect-id
                           move '<' to l-tile-char(ws-idx-y, ws-idx-x)
                           move ws-conveyor-left-effect-id
                               to l-tile-effect-id(ws-idx-y, ws-idx-x)
                           set ws-swap-colors to true 
                       when ws-conveyor-down-effect-id
                           move '^' to l-tile-char(ws-idx-y, ws-idx-x)
                           move ws-conveyor-up-effect-id
                               to l-tile-effect-id(ws-idx-y, ws-idx-x)
                           set ws-swap-colors to true 
                       when ws-conveyor-left-effect-id
                           move '>' to l-tile-char(ws-idx-y, ws-idx-x)
                           move ws-conveyor-right-effect-id
                               to l-tile-effect-id(ws-idx-y, ws-idx-x)
                           set ws-swap-colors to true 
                       when ws-conveyor-up-effect-id
                           move 'v' to l-tile-char(ws-idx-y, ws-idx-x)
                           move ws-conveyor-down-effect-id
                               to l-tile-effect-id(ws-idx-y, ws-idx-x)
                           set ws-swap-colors to true 
                   end-evaluate
                   if ws-swap-colors then 
                       move l-tile-bg(ws-idx-y, ws-idx-x) 
                           to ws-color-temp
                       move l-tile-fg(ws-idx-y, ws-idx-x)
                           to l-tile-bg(ws-idx-y, ws-idx-x) 
                       move ws-color-temp
                           to l-tile-fg(ws-idx-y, ws-idx-x)
                       if l-tile-is-blinking(ws-idx-y, ws-idx-x) then 
                           set l-tile-not-blinking(ws-idx-y, ws-idx-x)
                               to true
                       else
                           set l-tile-is-blinking(ws-idx-y, ws-idx-x)
                               to true
                       end-if  
                   end-if 
               end-perform
           end-perform
           exit paragraph.
       end program tile-effect-handler.
