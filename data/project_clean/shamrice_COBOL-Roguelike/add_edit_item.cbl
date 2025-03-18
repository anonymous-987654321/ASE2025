       identification division.
       program-id. add-edit-item.
       environment division.
       configuration section.
           special-names.
               crt status is ws-crt-status.
               cursor is ws-mouse-position.
       input-output section.
       file-control.
           select optional fd-item-list-data
               assign to dynamic ws-item-list-file-name
               organization is indexed
               access mode is dynamic 
               record key is f-item-id
               file status is ws-item-list-file-status.
       data division.
       file section.
       fd  fd-item-list-data.
       01  f-item-list-data-record.
           05  f-item-id              pic 999.
           05  f-item-name            pic x(16).
           05  f-item-effect-id       pic 99.
           05  f-item-worth           pic 999.
           05  f-item-color           pic 9. 
           05  f-item-char            pic x.
           05  f-item-highlight       pic a.
           05  f-item-blink           pic a.
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
       01  ws-item-data.
           05  ws-cur-num-items            pic 999 comp.
           05  ws-item-data-record         occurs 0 to ws-max-num-items
                                          depending on ws-cur-num-items.
               10  ws-item-name            pic x(16).                                          
               10  ws-item-pos.
                   15  ws-item-y           pic S99.
                   15  ws-item-x           pic S99.
               10  ws-item-taken           pic a value 'N'.
                   88  ws-item-is-taken    value 'Y'.
                   88  ws-item-not-taken   value 'N'.               
               10  ws-item-effect-id       pic 99.
               10  ws-item-worth           pic 999.
               10  ws-item-color           pic 9. 
               10  ws-item-char            pic x.
       01  ws-item-list-file-name      pic x(15) value "ITEM-LIST.LST".
       01  ws-item-list-file-status    pic xx.
       01  ws-item-list-data.
           05  ws-cur-num-list-items       pic 999 comp.
           05  ws-item-list-data-record    occurs 0 to 999 depending 
                                           on ws-cur-num-list-items.
               10  ws-item-list-id                 pic 999.
               10  ws-item-list-name               pic x(16).                                          
               10  ws-item-list-effect-id          pic 99.
               10  ws-item-list-worth              pic 999.
               10  ws-item-list-color              pic 9. 
               10  ws-item-list-char               pic x.
               10  ws-item-list-highlight-sw       pic a value 'N'.
                   88  ws-item-list-is-highlight   value 'Y'.
                   88  ws-item-lsit-not-highlight  value 'N'.
               10  ws-item-list-blink-sw           pic a value 'N'.    
                   88  ws-item-list-is-blink       value 'Y'.
                   88  ws-item-list-not-blink      value 'N'.           
       01  ws-mouse-flags              pic 9(4).
       01  ws-crt-status.
           05  ws-crt-status-key-1     pic 99.
           05  ws-crt-status-key-2     pic 99.
       01  ws-mouse-position.
           05  ws-mouse-row            pic 99.
           05  ws-mouse-col            pic 99.
       01  ws-mouse-click-status       pic a value 'N'.
           88  ws-mouse-clicked        value 'Y'.
           88  ws-mouse-not-clicked    value 'N'.
       01  ws-line-mask                   pic x(50) value spaces.
       01  ws-kb-input                    pic x.
       01  ws-eof                         pic a value 'N'.
           88  ws-is-eof                  value 'Y'.
           88  ws-not-eof                 value 'N'.
       01  ws-selected-idx              pic 999 comp value 0.
       01  ws-record-pos.
           05  ws-record-pos-y          pic 99.
           05  ws-record-pos-x          pic 99.
       01  ws-load-return-code          pic 9.
       01  ws-save-return-code          pic 9.
       linkage section.
       01  l-item-list-data-record.               
           10  l-item-list-id                 pic 999.
           10  l-item-list-name               pic x(16).                                          
           10  l-item-list-effect-id          pic 99.
           10  l-item-list-worth              pic 999.
           10  l-item-list-color              pic 9. 
           10  l-item-list-char               pic x.
           10  l-item-list-highlight-sw       pic a value 'N'.
               88  l-item-list-is-highlight   value 'Y'.
               88  l-item-lsit-not-highlight  value 'N'.
           10  l-item-list-blink-sw           pic a value 'N'.    
                88  l-item-list-is-blink       value 'Y'.
                88  l-item-list-not-blink      value 'N'.   
       01  l-return-code               pic 9.
       screen section.
       01  s-add-edit-item-screen.
           05  s-title-line foreground-color 7 background-color 1.
               10  line 4 column 15 pic x(50) value spaces. 
               10  line 4 column 20 value "Add/Edit Item".
           05  s-space-line foreground-color 0 background-color 7.
               10  line 5 column 15 pic x(50) value spaces.
           05  s-id-line foreground-color 0 background-color 7.
               10  line 6 column 15 pic x(50) value spaces. 
               10  line 6 column 16 value "         ID:".
               10  line 6 column 29 pic 9(6) using l-item-list-id.
           05  s-name-line foreground-color 0 background-color 7.
               10  line 7 column 15 pic x(50) value spaces. 
               10  line 7 column 16 value "       NAME:".
               10  line 7 column 29 pic x(16) using l-item-list-name.
           05  s-effect-id-line foreground-color 0 background-color 7.
               10  line 8 column 15 pic x(50) value spaces. 
               10  line 8 column 16 value "  EFFECT ID:".
               10  line 8 column 29 pic 99 using l-item-list-effect-id.
           05  s-worth-line foreground-color 0 background-color 7.
               10  line 9 column 15 pic x(50) value spaces. 
               10  line 9 column 16 value "WORTH/VALUE:".
               10  line 9 column 29 pic 999 using l-item-list-worth.
           05  s-color-line foreground-color 0 background-color 7.
               10  line 10 column 15 pic x(50) value spaces. 
               10  line 10 column 16 value "      COLOR:".
               10  line 10 column 29 pic 9 using l-item-list-color.
           05  s-char-line foreground-color 0 background-color 7.
               10  line 11 column 15 pic x(50) value spaces. 
               10  line 11 column 16 value "  CHARACTER:".
               10  line 11 column 29 pic x using l-item-list-char.
           05  s-highlight-line foreground-color 0 background-color 7.
               10  line 12 column 15 pic x(50) value spaces. 
               10  line 12 column 16 value "  HIGHLIGHT:".
               10  line 12 column 29 pic x 
                   using l-item-list-highlight-sw.
           05  s-blink-line foreground-color 0 background-color 7.
               10  line 13 column 15 pic x(50) value spaces. 
               10  line 13 column 16 value "      BLINK:".
               10  line 13 column 29 pic x using l-item-list-blink-sw.
           05  s-space-line foreground-color 0 background-color 7.
               10  line 14 column 15 pic x(50) value spaces.
           05  s-info-line foreground-color 0 background-color 7.
               10  line 15 column 15 pic x(50) value spaces. 
               10  line 15 column 16 
                   value "Arrow keys between fields ESC to cancel.".
           05  s-space-line foreground-color 0 background-color 7.
               10  line 16 column 15 pic x(50) value spaces.
       procedure division using 
           l-item-list-data-record l-return-code.
           set environment "COB_SCREEN_EXCEPTIONS" to 'Y'.
           set environment "COB_SCREEN_ESC" to 'Y'.
       main-procedure. 
           accept s-add-edit-item-screen
           evaluate ws-crt-status 
               when COB-SCR-ESC
                   move 9 to l-return-code
                   display space blank screen 
                   goback 
           end-evaluate
           if l-item-list-name not = spaces and l-item-list-id > zero 
           then 
               if l-item-list-color > 7 then 
                   move 7 to l-item-list-color
               end-if 
               move function upper-case(l-item-list-highlight-sw)
                   to l-item-list-highlight-sw
               move function upper-case(l-item-list-blink-sw)
                   to l-item-list-blink-sw
               if 'Y' not = l-item-list-highlight-sw then 
                   move 'N' to l-item-list-highlight-sw
               end-if
               if 'Y' not = l-item-list-blink-sw then 
                   move 'N' to l-item-list-blink-sw
               end-if 
               move 0 to l-return-code 
           else 
               move 1 to l-return-code 
           end-if 
           display space blank screen 
           goback.
       end program add-edit-item.
