       identification division.
       program-id. draw-tile-character.
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
       linkage section.
           01  l-scr-draw-pos.
               05  l-scr-draw-y            pic 99.
               05  l-scr-draw-x            pic 99.
           01  l-tile-map-data.
               10  l-tile-fg                   pic 9.   
               10  l-tile-bg                   pic 9.
               10  l-tile-char                 pic x.
               10  l-tile-highlight            pic a value 'N'.
                   88  l-tile-is-highlight      value 'Y'.
                   88  l-tile-not-highlight     value 'N'.
               10  l-tile-blocking             pic a value 'N'.
                   88  l-tile-is-blocking      value 'Y'.
                   88  l-tile-not-blocking     value 'N'.  
               10  l-tile-blinking             pic a value 'N'.
                   88  l-tile-is-blinking      value 'Y'.
                   88  l-tile-not-blinking     value 'N'.
               10  l-tile-effect-id            pic 99 comp.      
               10  l-tile-visibility           pic 999 comp.      
           01  l-char-to-draw                  pic x.
       procedure division using  
           l-scr-draw-pos l-tile-map-data l-char-to-draw.
       main-procedure.
           evaluate true 
               when l-tile-is-highlight and l-tile-not-blinking
                   display 
                       l-char-to-draw 
                       at l-scr-draw-pos 
                       background-color l-tile-bg 
                       foreground-color l-tile-fg
                       highlight
                   end-display
               when l-tile-is-highlight and l-tile-is-blinking
                   display 
                       l-char-to-draw
                       at l-scr-draw-pos 
                       background-color l-tile-bg 
                       foreground-color l-tile-fg
                       highlight blink 
                   end-display 
               when l-tile-not-highlight and l-tile-is-blinking
                   display 
                       l-char-to-draw
                       at l-scr-draw-pos 
                       background-color l-tile-bg 
                       foreground-color l-tile-fg
                       blink
                   end-display 
               when other  
                   display 
                       l-char-to-draw at l-scr-draw-pos 
                       background-color l-tile-bg 
                       foreground-color l-tile-fg 
                   end-display
           end-evaluate
           goback.
       end program draw-tile-character.
