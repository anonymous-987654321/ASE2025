       identification division.
       program-id. cobol-roguelike-engine.
       environment division.
       configuration section.
       special-names.
           crt status is ws-crt-status.
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
       01  ws-teleport-data.
           05  ws-cur-num-teleports        pic 999 comp.
           05  ws-teleport-data-record  occurs 0 to ws-max-num-teleports
                                      depending on ws-cur-num-teleports.
               10  ws-teleport-pos.
                   15  ws-teleport-y        pic S99.
                   15  ws-teleport-x        pic S99.
               10  ws-teleport-dest-pos.
                   15  ws-teleport-dest-y   pic S99.
                   15  ws-teleport-dest-x   pic S99.
               10  ws-teleport-dest-map     pic x(15).
       01  ws-map-files.  
           05  ws-map-name             pic x(15) value "VOIDSPACE".
           05  ws-map-name-temp        pic x(15) value "VOIDSPACE".           
           05  ws-map-dat-file         pic x(15).               
           05  ws-map-tel-file         pic x(15).
           05  ws-map-enemy-file       pic x(15).
           05  ws-map-item-file        pic x(15).
       01  ws-map-file-statuses.
           05  ws-map-file-status      pic xx.
           05  ws-teleport-file-status pic xx.
           05  ws-enemy-file-status    pic xx.
           05  ws-item-file-status     pic xx.
       01  ws-enemy-data.
           05  ws-cur-num-enemies           pic 99 comp value 0.
           05  ws-enemy             occurs 0 to ws-max-num-enemies times
                                    depending on ws-cur-num-enemies.
               10  ws-enemy-name           pic x(16) value 'NONAME'.
               10  ws-enemy-hp.
                   15  ws-enemy-hp-total    pic 999 comp value 10.
                   15  ws-enemy-hp-current  pic 999 comp value 10.
               10  ws-enemy-attack-damage   pic 999 comp value 1.
               10  ws-enemy-pos.
                   15  ws-enemy-y           pic 99.
                   15  ws-enemy-x           pic 99.
               10  ws-enemy-color           pic 9 value 4.
               10  ws-enemy-char            pic x.
               10  ws-enemy-status              pic 9 comp value 3.
                   88  ws-enemy-status-alive    value 0.
                   88  ws-enemy-status-dead     value 1.
                   88  ws-enemy-status-attacked value 2.
                   88  ws-enemy-status-other    value 3.
               10  ws-enemy-movement-ticks.
                   15  ws-enemy-current-ticks   pic 999 comp.
                   15  ws-enemy-max-ticks       pic 999 comp.
               10  ws-enemy-exp-worth           pic 9(4) comp.
       01  ws-tile-map-table-matrix.
           05  ws-tile-map           occurs ws-max-map-height times.
               10  ws-tile-map-data   occurs ws-max-map-width times.
                   15  ws-tile-fg                   pic 9.   
                   15  ws-tile-bg                   pic 9.
                   15  ws-tile-char                 pic x.
                   15  ws-tile-highlight            pic a value 'N'.
                       88 ws-tile-is-highlight      value 'Y'.
                       88 ws-tile-not-highlight     value 'N'.
                   15  ws-tile-blocking             pic a value 'N'.
                       88  ws-tile-is-blocking      value 'Y'.
                       88  ws-tile-not-blocking     value 'N'.  
                   15  ws-tile-blinking             pic a value 'N'.
                       88  ws-tile-is-blinking      value 'Y'.
                       88  ws-tile-not-blinking     value 'N'.
                   15  ws-tile-effect-id            pic 99 comp.  
                   15  ws-tile-visibility           pic 999 comp.    
       01  ws-action-history.
           05  ws-action-history-item      occurs 150 times.
               10  ws-action-history-text  pic x(75).
       01  ws-action-history-temp          pic x(75).      
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
       01  ws-crt-status.
           05  ws-crt-status-key-1     pic 99.
           05  ws-crt-status-key-2     pic 99.
       01  ws-temp-time                pic 9(9).
       01  ws-player.
           05  ws-player-name          pic x(16) value "Adventurer".
           05  ws-player-hp.
               10  ws-player-hp-current        pic 999 value 100.
               10  ws-player-hp-max            pic 999 value 100.
           05  ws-player-pos.
               10  ws-player-y             pic S99.
               10  ws-player-x             pic S99.
           05  ws-player-pos-delta.    
               10  ws-player-pos-delta-y   pic S99.
               10  ws-player-pos-delta-x   pic S99.
           05  ws-player-scr-pos.  
               10  ws-player-scr-y         pic 99 value 12. 
               10  ws-player-scr-x         pic 99 value 20. 
           05  ws-player-status              pic 9 value 0.
               88  ws-player-status-alive    value 0.
               88  ws-player-status-dead     value 1.
               88  ws-player-status-attacked value 2.
               88  ws-player-status-other    value 3.
           05  ws-player-attack-damage.    
               10  ws-player-atk-cur       pic 999 value 1.
               10  ws-player-atk-base      pic 999 value 1.
           05  ws-player-defense-power.
               10  ws-player-def-cur       pic 999 value 1.
               10  ws-player-def-base      pic 999 value 1.
           05  ws-player-level             pic 999 value 1.
           05  ws-player-experience.
               10  ws-player-exp-total     pic 9(7) value 0.
               10  ws-player-exp-next-lvl  pic 9(7) value 75.
           78  ws-player-char               value "@". 
       01  ws-equiped-items.
           05  ws-equiped-weapon.
               10  ws-equip-weapon-name        pic x(16) value "None".
               10  ws-equip-weapon-atk         pic 999 value 0.
               10  ws-equip-weapon-status      pic x value "0".
                   88  ws-equip-weapon-curse   value "-".
                   88  ws-equip-weapon-normal  value "0".
                   88  ws-equip-weapon-bless   value "+".                   
           05  ws-equiped-armor.
               10  ws-equip-armor-name         pic x(16) value "None".
               10  ws-equip-armor-def          pic 999 value 0.
               10  ws-equip-armor-status       pic x value "0".
                   88  ws-equip-armor-curse    value "-".
                   88  ws-equip-armor-normal   value "0".
                   88  ws-equip-armor-bless    value "+".                   
       01  ws-map-explored-data.
           05  ws-map-explored-y         occurs ws-max-map-height times.
               10  ws-map-explored-x     occurs ws-max-map-width times.
                   15  ws-map-explored        pic a value 'N'.
                       88  ws-is-explored     value 'Y'.
                       88  ws-is-not-explored value 'N'.
       01  ws-temp-damage-delt            pic S999 value 0.
       01  ws-enemy-placed-found        pic a value 'N'.
           88  ws-enemy-found           value 'Y'.
           88  ws-enemy-not-found       value 'N'.
       01  ws-enemy-found-idx           pic 99 comp.
       01  ws-enemy-exp-disp            pic z(7).
       01  ws-enemy-temp-pos.
           05  ws-enemy-temp-y          pic 99.
           05  ws-enemy-temp-x          pic 99.
       01  ws-kb-input                  pic x.
       01  ws-is-quit                   pic a value 'N'.
           88  ws-quit                  value 'Y'.
           88  ws-not-quit              value 'N'.
       01  ws-counter-1                 pic 999 comp.
       01  ws-counter-2                 pic 999 comp.
       01  ws-enemy-idx                 pic 99 comp.
       01  ws-enemy-search-idx          pic 99 comp.
       01  ws-tele-idx                  pic 999 comp.
       01  ws-temp-color                pic 9.
       01  ws-temp-map-pos.
           05  ws-temp-map-pos-y        pic S99.
           05  ws-temp-map-pos-x        pic S99.    
       01  ws-filler                    pic 9(9).
       01  ws-attack-attempt            pic 9(9).
       01  ws-eof                       pic a value 'N'.
           88 ws-is-eof                 value 'Y'.
           88 ws-not-eof                value 'N'.
       01  ws-load-return-code          pic 9.
       01  ws-tile-effect-return-code   pic 99.
       01  ws-player-moved-sw           pic a value 'N'.
           88  ws-player-moved          value 'Y'.
           88  ws-player-not-moved      value 'N'.
       01  ws-teleport-used-sw          pic a value 'N'.
           88  ws-teleport-used         value 'Y'.
           88  ws-teleport-not-used     value 'N'.
       01  ws-frame-rate.
           05  ws-start-frame           pic 9(2).
           05  ws-end-frame             pic 9(2).
           05  ws-frame-diff            pic 9(2).
           05  ws-sleep-time            pic 9(2).
       01 ws-current-date-data.
           05  ws-current-date.
               10  ws-current-year         PIC 9(04).
               10  ws-current-month        PIC 9(02).
               10  ws-current-day          PIC 9(02).
           05  ws-current-time.
               10  ws-current-hour         PIC 9(02).
               10  ws-current-minute       PIC 9(02).
               10  ws-current-second       PIC 9(02).
               10  ws-current-millisecond  PIC 9(02).
       01  ws-command-line-buffer         pic x(2048).
       procedure division.
           set environment "COB_SCREEN_EXCEPTIONS" to 'Y'.
           set environment "COB_SCREEN_ESC" to 'Y'.
           set environment "COB_TIMEOUT_SCALE" to '3'.
       init-setup.                                     
           accept ws-temp-time from time 
           move function random(ws-temp-time) to ws-filler
           accept ws-command-line-buffer from command-line 
           if ws-command-line-buffer not = spaces then 
               call "command-line-parser" using 
                   ws-command-line-buffer
                   ws-map-name
                   ws-map-name-temp 
               end-call 
           end-if 
           display space blank screen                
           .
       load-tile-map.
           move function concatenate("Entering ",
               function trim(ws-map-name), "...")
               to ws-action-history-temp
           call "add-action-history-item" using 
               ws-action-history-temp ws-action-history
           end-call 
           call "load-map-data" using 
               ws-map-files ws-tile-map-table-matrix 
               ws-enemy-data ws-teleport-data
               ws-item-data 
               ws-load-return-code
           end-call 
           if ws-load-return-code > 0 then 
               display space blank screen 
               display 
                   "FATAL ERROR :: Failed to load area data: " at 0101
                   ws-map-name at 0143 
                   "Please make sure related DAT, BGS, TEL files exist"
                   & " in level data directory." at 0201
               end-display 
               stop run 
           end-if
           if ws-teleport-not-used then 
               perform set-player-start-position
           end-if 
           set ws-teleport-not-used to true 
           initialize ws-map-explored-data
           move ws-player-pos to ws-temp-map-pos
           add ws-player-scr-y to ws-temp-map-pos-y
           add ws-player-scr-x to ws-temp-map-pos-x  
           call "set-map-exploration" using 
               ws-map-explored-data, ws-temp-map-pos,
               ws-tile-visibility(
                   ws-temp-map-pos-y, ws-temp-map-pos-x)
           end-call 
           perform draw-playfield 
           .
       main-procedure.
           perform until ws-quit or ws-player-status-dead   
               perform get-input                           
               perform move-player                 
               perform move-enemy                       
               perform draw-playfield 
           end-perform
           if ws-player-status-dead then 
               display 
                   "You died. Game over. Press 'Q' to continue" at 1005
                   foreground-color 7 
                   background-color 0 
               end-display 
               perform  with test after until ws-kb-input = 'Q'
                   accept ws-kb-input  
                       with auto-skip no-echo upper at 1004
                   end-accept 
               end-perform
           end-if 
           call "action-history-log-end" 
           goback.
       draw-playfield.           
           call "draw-dynamic-screen-data" using 
               ws-player ws-tile-map-table-matrix ws-enemy-data
               ws-action-history ws-map-explored-data 
               ws-equiped-items             
           end-call 
           exit paragraph.
       get-input.
           accept ws-kb-input at 2051 
               with auto-skip no-echo 
               time-out after 250
               upper 
           end-accept 
           evaluate ws-crt-status 
               when COB-SCR-KEY-DOWN 
                   add 1 to ws-player-pos-delta-y
               when COB-SCR-KEY-UP
                   subtract 1 from ws-player-pos-delta-y
               when COB-SCR-KEY-LEFT
                   subtract 1 from ws-player-pos-delta-x
               when COB-SCR-KEY-RIGHT
                   add 1 to ws-player-pos-delta-x
               when COB-SCR-ESC
                   set ws-quit to true 
               when COB-SCR-F1
                   call "display-debug" using 
                       ws-player ws-tile-map-table-matrix ws-enemy-data
                       ws-temp-map-pos   
                   end-call                        
               when COB-SCR-F9
                   perform debug-set-full-map-exploration
           end-evaluate
           evaluate true
               when ws-kb-input = 'Q'
                   set ws-quit to true 
               when ws-kb-input = 'S' 
                   add 1 to ws-player-pos-delta-y
               when ws-kb-input = 'W' 
                   subtract 1 from ws-player-pos-delta-y
               when ws-kb-input = 'D'
                   add 1 to ws-player-pos-delta-x
               when ws-kb-input = 'A'
                   subtract 1 from ws-player-pos-delta-x
               when ws-kb-input = 'R'
                   perform restart-level
           end-evaluate
           exit paragraph.
       move-player.
           move ws-player-pos to ws-temp-map-pos
           add ws-player-scr-y to ws-temp-map-pos-y
           add ws-player-scr-x to ws-temp-map-pos-x             
           add ws-player-pos-delta-y to ws-temp-map-pos-y               
           add ws-player-pos-delta-x to ws-temp-map-pos-x               
           if ws-temp-map-pos-y >= ws-max-map-height 
               or ws-temp-map-pos-x >= ws-max-map-width
               or ws-temp-map-pos-y <= 0 or ws-temp-map-pos-x <= 0 
           then
               move zeros to ws-player-pos-delta
               set ws-player-not-moved to true 
               exit paragraph
           end-if 
           move zero to ws-enemy-found-idx
           perform varying ws-enemy-idx 
           from 1 by 1 until ws-enemy-idx > ws-cur-num-enemies
               if ws-enemy-y(ws-enemy-idx) = ws-temp-map-pos-y 
               and ws-enemy-x(ws-enemy-idx) = ws-temp-map-pos-x
               and not ws-enemy-status-dead(ws-enemy-idx) 
               then 
                   move ws-enemy-idx to ws-enemy-found-idx
                   perform player-attack
                   exit perform
               end-if 
           end-perform 
           if ws-enemy-found-idx = 0 and ws-tile-not-blocking(
               ws-temp-map-pos-y, ws-temp-map-pos-x)                
           then                          
               add ws-player-pos-delta-x to ws-player-x
               add ws-player-pos-delta-y to ws-player-y
               call "set-map-exploration" using 
                   ws-map-explored-data, ws-temp-map-pos,
                   ws-tile-visibility(
                       ws-temp-map-pos-y, ws-temp-map-pos-x)
               end-call 
               if ws-player-pos-delta not zeros then
                   set ws-player-moved to true 
               else 
                   set ws-player-not-moved to true   
               end-if
               perform check-tile-effect
           end-if                            
           move zeros to ws-player-pos-delta
           exit paragraph.
       check-tile-effect.
           call "tile-effect-handler" using
               ws-tile-effect-id(ws-temp-map-pos-y, ws-temp-map-pos-x) 
               ws-tile-char(ws-temp-map-pos-y, ws-temp-map-pos-x)
               ws-player ws-temp-map-pos ws-teleport-data ws-map-files
               ws-tile-map-table-matrix
               ws-player-moved-sw 
               ws-action-history
               ws-tile-effect-return-code
           end-call
           evaluate ws-tile-effect-return-code
               when ws-load-map-tele-return-code
                   set ws-teleport-used to true  
                   perform load-tile-map
           end-evaluate
           exit paragraph.
       move-enemy.
           perform varying ws-enemy-idx 
           from 1 by 1 until ws-enemy-idx > ws-cur-num-enemies
               if not ws-enemy-status-dead(ws-enemy-idx) then 
                   add 15 to ws-enemy-current-ticks(ws-enemy-idx)
                   if ws-enemy-current-ticks(ws-enemy-idx) >= 
                   ws-enemy-max-ticks(ws-enemy-idx) then 
                       move 0 to ws-enemy-current-ticks(ws-enemy-idx)
                       if ws-enemy-status-attacked(ws-enemy-idx) 
                       then 
                           set ws-enemy-status-alive(ws-enemy-idx) 
                               to true 
                       end-if 
                       move ws-enemy-pos(ws-enemy-idx) 
                           to ws-enemy-temp-pos 
                       if ws-enemy-y(ws-enemy-idx) not = 
                       ws-player-y + ws-player-scr-y then 
                           if ws-enemy-y(ws-enemy-idx) < 
                           ws-player-y + ws-player-scr-y then                                                          
                               add 1 to ws-enemy-temp-y
                           else 
                               subtract 1 from ws-enemy-temp-y
                           end-if  
                       end-if 
                       if ws-enemy-x(ws-enemy-idx) not = 
                       ws-player-x + ws-player-scr-x then 
                           if ws-enemy-x(ws-enemy-idx) < 
                           ws-player-x + ws-player-scr-x then                
                               add 1 to ws-enemy-temp-x                                                              
                           else                            
                               subtract 1 from ws-enemy-temp-x 
                           end-if 
                       end-if 
                       if ws-enemy-temp-x = 
                       ws-player-x + ws-player-scr-x 
                       and ws-enemy-temp-y = 
                       ws-player-y + ws-player-scr-y then 
                           perform enemy-attack
                       else                        
                           move zero to ws-enemy-found-idx 
                           perform varying ws-enemy-search-idx
                           from 1 by 1 until 
                           ws-enemy-search-idx > ws-cur-num-enemies
                               if ws-enemy-search-idx not = ws-enemy-idx 
                               then 
                                   if ws-enemy-temp-x = 
                                   ws-enemy-x(ws-enemy-search-idx) and
                                   ws-enemy-temp-y = 
                                   ws-enemy-y(ws-enemy-search-idx) then
                                       move ws-enemy-search-idx
                                           to ws-enemy-found-idx 
                                       exit perform 
                                   end-if 
                               end-if
                           end-perform 
                           if ws-enemy-found-idx = 0 then 
                               if ws-tile-not-blocking(
                               ws-enemy-y(ws-enemy-idx), 
                               ws-enemy-temp-x) then 
                                   move ws-enemy-temp-x
                                       to ws-enemy-x(ws-enemy-idx) 
                               end-if 
                               if ws-tile-not-blocking(
                               ws-enemy-temp-y, 
                               ws-enemy-x(ws-enemy-idx)) then 
                                   move ws-enemy-temp-y
                                       to ws-enemy-y(ws-enemy-idx) 
                               end-if 
                           end-if 
                       end-if                        
                   end-if 
               end-if 
           end-perform 
           exit paragraph.
       enemy-attack.
           compute ws-attack-attempt = function random * 100 + 1
           if ws-attack-attempt > 65 then 
               move function concatenate(
                   function trim(ws-enemy-name(ws-enemy-idx)), 
                   " missed ", 
                   function trim(ws-player-name), "."
               ) to ws-action-history-temp
               call "add-action-history-item" using
                   ws-action-history-temp ws-action-history
               end-call 
               exit paragraph
           end-if 
           compute ws-temp-damage-delt = 
               ws-enemy-attack-damage(ws-enemy-idx) - ws-player-def-cur
           end-compute 
           if ws-temp-damage-delt <= 0 then 
               move function concatenate(
                   function trim(ws-enemy-name(ws-enemy-idx)), 
                   " attacks but does no damage to ", 
                   function trim(ws-player-name), "."
               ) to ws-action-history-temp
               call "add-action-history-item" using
                   ws-action-history-temp ws-action-history
               end-call 
               exit paragraph     
           end-if       
           if ws-temp-damage-delt > ws-player-hp-current then 
               set ws-player-status-dead to true 
               move zero to ws-player-hp-current
           else        
               subtract ws-temp-damage-delt from ws-player-hp-current
           end-if 
           move 
               function concatenate(
                   function trim(ws-player-name), 
                   " is attacked by a ",
                   function trim(ws-enemy-name(ws-enemy-idx)),
                   " for ",
                   ws-temp-damage-delt,
                   " damange.")
               to ws-action-history-temp
           call "add-action-history-item" using 
               ws-action-history-temp
               ws-action-history
           end-call 
           exit paragraph.
       player-attack.
           if ws-enemy-hp-current(ws-enemy-idx) > 0
           and not ws-enemy-status-dead(ws-enemy-idx) then
               compute ws-attack-attempt = function random * 100 + 1      
               if ws-attack-attempt > 80 then 
                   move function concatenate(
                       function trim(ws-player-name), " missed ", 
                       function trim(ws-enemy-name(ws-enemy-idx)), "."
                   ) to ws-action-history-temp
                   call "add-action-history-item" using
                       ws-action-history-temp ws-action-history
                   end-call 
                   exit paragraph
               end-if 
               if ws-player-attack-damage <= 
               ws-enemy-hp-current(ws-enemy-idx) then 
                   subtract ws-player-atk-cur 
                       from ws-enemy-hp-current(ws-enemy-idx)
                   end-subtract
                   set ws-enemy-status-attacked(ws-enemy-idx) to true
               else 
                   move zero to ws-enemy-hp-current(ws-enemy-idx)
               end-if 
               move function concatenate(
                   function trim(ws-player-name), " attacks ", 
                   function trim(ws-enemy-name(ws-enemy-idx)), " for ",
                   ws-player-atk-cur, " damage."
               ) to ws-action-history-temp
               call "add-action-history-item" using
                   ws-action-history-temp ws-action-history
               end-call 
               if ws-enemy-hp-current(ws-enemy-idx) <= 0 then                     
                   set ws-enemy-status-dead(ws-enemy-idx) to true 
                   move ws-enemy-exp-worth(ws-enemy-idx) 
                       to ws-enemy-exp-disp
                   move function concatenate(                       
                       function trim(ws-enemy-name(ws-enemy-idx)), 
                       " expires giving ",
                       function trim(ws-enemy-exp-disp), 
                       " experience points."
                   ) to ws-action-history-temp
                   call "add-action-history-item" using
                       ws-action-history-temp ws-action-history
                   end-call 
                   if ws-enemy-exp-worth(ws-enemy-idx) >= 
                   ws-player-exp-next-lvl then 
                       move zero to ws-player-exp-next-lvl
                   else
                       subtract ws-enemy-exp-worth(ws-enemy-idx) from 
                           ws-player-exp-next-lvl
                       end-subtract
                   end-if 
                   add ws-enemy-exp-worth(ws-enemy-idx) 
                       to ws-player-exp-total                   
                   if ws-player-exp-next-lvl = 0 then 
                       compute ws-player-exp-next-lvl = 
                           (ws-player-level * 20) + 75
                       end-compute 
                       add 1 to ws-player-level 
                       compute ws-player-hp-max = 
                           ws-player-hp-max + (ws-player-level * 5)
                       end-compute 
                       move ws-player-hp-max to ws-player-hp-current
                       compute ws-player-atk-base = 
                           ws-player-level * 1.5
                       end-compute 
                       compute ws-player-atk-cur = 
                           ws-player-atk-base + ws-equip-weapon-atk
                       end-compute 
                       compute ws-player-def-base = 
                           ws-player-level * 1.25
                       end-compute 
                       compute ws-player-def-cur = 
                           ws-player-def-base + ws-equip-armor-def
                       end-compute 
                       move function concatenate(                       
                           function trim(ws-player-name), 
                           " has leveled up to level ",
                           ws-player-level, "!"
                       ) to ws-action-history-temp
                       call "add-action-history-item" using
                           ws-action-history-temp ws-action-history
                       end-call 
                   end-if                        
               end-if 
           end-if   
           exit paragraph.
       set-player-start-position.
           compute ws-player-y = 1 - ws-player-scr-y 
           compute ws-player-x = 1 - ws-player-scr-x 
           perform varying ws-counter-1 
           from 1 by 1 until ws-counter-1 > ws-max-map-height
               perform varying ws-counter-2 
               from 1 by 1 until ws-counter-2 > ws-max-map-width
                   if ws-tile-effect-id(ws-counter-1, ws-counter-2)
                   = ws-player-start-effect-id then
                       compute ws-player-y = 
                           ws-counter-1 - ws-player-scr-y
                       end-compute 
                       compute ws-player-x =
                           ws-counter-2 - ws-player-scr-x 
                       end-compute 
                       exit paragraph 
                   end-if 
               end-perform
           end-perform 
           exit paragraph.
       restart-level.
           display "Restart level? [Y/N]: " 
               foreground-color 0
               background-color 7
               at 1010 
           end-display 
           accept ws-kb-input upper at 1032
           if ws-kb-input = 'Y' then 
               perform load-tile-map
           end-if         
           exit paragraph.
       debug-set-full-map-exploration.
           perform varying ws-counter-1 
           from 1 by 1 until ws-counter-1 > ws-max-map-height
               perform varying ws-counter-2 
               from 1 by 1 until ws-counter-2 > ws-max-map-width
                   set ws-is-explored(ws-counter-1, ws-counter-2) 
                       to true    
               end-perform 
           end-perform 
           exit paragraph.
       end program cobol-roguelike-engine.
