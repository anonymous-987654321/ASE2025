       identification division.
       program-id. set-tile-effect.
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
       01  ws-temp-input               pic a.
       01  ws-counter-1                pic 999 comp.
       local-storage section.
       01  ls-teleport-found           pic a value 'N'.
           88  ls-teleport-is-found    value 'Y'.
           88  ls-teleport-not-found   value 'N'.
       01  ls-found-idx                pic 999 comp.
       linkage section.
       01  l-placement-pos.
           05  l-placement-pos-y         pic S99.
           05  l-placement-pos-x         pic S99.  
       01  l-cur-tile-effect-id          pic 99 comp.
       01  l-cursor.
           05  l-cursor-pos.
               10  l-cursor-pos-y         pic S99.
               10  l-cursor-pos-x         pic S99.
           05  l-cursor-pos-delta.               
               10  l-cursor-pos-delta-y   pic S99. 
               10  l-cursor-pos-delta-x   pic S99.
           05  l-cursor-scr-pos.  
               10  l-cursor-scr-y         pic 99 value 10.
               10  l-cursor-scr-x         pic 99 value 20.                      
           05  l-cursor-color             pic 9.
           05  l-cursor-draw-color-fg     pic 9.
           05  l-cursor-draw-color-bg     pic 9.
           05  l-cursor-draw-char         pic x value space.
           05  l-cursor-draw-highlight    pic a.
               88  l-cursor-highlight     value 'Y'.
               88  l-cursor-no-highlight  value 'N'.
           05  l-cursor-draw-blocking     pic a.
               88  l-cursor-blocking      value 'Y'.
               88  l-cursor-not-block     value 'N'.
           05  l-cursor-draw-blinking     pic a.
               88  l-cursor-blink         value 'Y'.
               88  l-cursor-not-blink     value 'N'. 
           05  l-cursor-draw-visibility   pic 999.
           05  l-cursor-enemy-settings.
               10  l-cursor-enemy-name            pic x(16).
               10  l-cursor-enemy-hp              pic 999.
               10  l-cursor-enemy-attack-damage   pic 999.
               10  l-cursor-enemy-color           pic 9.   
               10  l-cursor-enemy-char            pic x.
               10  l-cursor-enemy-movement-ticks  pic 999.    
               10  l-cursor-enemy-exp-worth       pic 9(4). 
           05  l-cursor-teleport-settings.
               10  l-cursor-tel-dest-y            pic 99.
               10  l-cursor-tel-dest-x            pic 99.
               10  l-cursor-tel-dest-map          pic x(15).                       
           05  l-cursor-draw-effect       pic 99.
           05  l-cursor-type              pic a.
               88  l-cursor-type-tile     value 'T'.
               88  l-cursor-type-enemy    value 'E'.                     
           78  l-cursor-char              value "+".
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
       procedure division using 
           l-placement-pos l-cur-tile-effect-id 
           l-cursor l-teleport-data.
       main-procedure.
           evaluate l-cursor-draw-effect
               when ws-no-tile-effect-id
                   move ws-no-tile-effect-id to l-cur-tile-effect-id              
               when ws-teleport-effect-id
                   perform set-teleport
               when ws-conveyor-right-effect-id
                   perform set-conveyor-right
               when ws-conveyor-down-effect-id
                   perform set-conveyor-down
               when ws-conveyor-left-effect-id
                   perform set-conveyor-left
               when ws-conveyor-up-effect-id
                   perform set-conveyor-up
               when ws-conveyor-reverse-effect-id
                   perform set-conveyor-reverse
               when ws-player-start-effect-id
                   move ws-player-start-effect-id 
                       to l-cur-tile-effect-id
               when other 
                   display "Not implemented" at 2525
           end-evaluate 
           goback.
       set-teleport.        
           set ls-teleport-not-found to true
           move zeros to ls-found-idx
           perform varying ws-counter-1 from 1 by 1
               until ws-counter-1 > l-cur-num-teleports
               if l-placement-pos = l-teleport-pos(ws-counter-1) then 
                   set ls-teleport-is-found to true 
                   move ws-counter-1 to ls-found-idx
                   exit perform 
               end-if 
           end-perform 
           if ls-teleport-is-found then 
               display "Remove placed teleport? [y/n] " at 2101                
               accept ws-temp-input at 2130 with auto-skip upper
               if ws-temp-input = 'Y' then                    
                   perform varying ws-counter-1 
                       from ls-found-idx by 1 
                       until ws-counter-1 > l-cur-num-teleports + 1
                       move l-teleport-data-record(ws-counter-1 + 1) to 
                           l-teleport-data-record(ws-counter-1)
                   end-perform 
                   subtract 1 from l-cur-num-teleports
                   move zeros to l-cur-tile-effect-id             
               end-if 
               exit paragraph 
           end-if 
           if l-cursor-tel-dest-y not = zeros 
               and l-cursor-tel-dest-x not = zeros
               and l-cursor-tel-dest-map not = spaces then 
               add 1 to l-cur-num-teleports
               move l-cursor-draw-effect to l-cur-tile-effect-id
               move l-placement-pos 
                   to l-teleport-pos(l-cur-num-teleports)
               move l-cursor-tel-dest-y 
                   to l-teleport-dest-y(l-cur-num-teleports)
               move l-cursor-tel-dest-x
                   to l-teleport-dest-x(l-cur-num-teleports)
               move l-cursor-tel-dest-map
                   to l-teleport-dest-map(l-cur-num-teleports)                   
               display 
                   "Teleport placed at:" at 2401 
                   l-teleport-pos(l-cur-num-teleports) at 2417                  
               end-display
           end-if    
           exit paragraph.
       set-conveyor-right.
           move ws-conveyor-right-effect-id to l-cur-tile-effect-id
           exit paragraph.
       set-conveyor-down.
           move ws-conveyor-down-effect-id to l-cur-tile-effect-id
           exit paragraph.
       set-conveyor-left.
           move ws-conveyor-left-effect-id to l-cur-tile-effect-id
           exit paragraph.
       set-conveyor-up.
           move ws-conveyor-up-effect-id to l-cur-tile-effect-id
           exit paragraph.
       set-conveyor-reverse.
           move ws-conveyor-reverse-effect-id to l-cur-tile-effect-id
           exit paragraph.
       end program set-tile-effect.
