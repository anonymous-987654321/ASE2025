       identification division.
       program-id. draw-dynamic-screen-data.
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
       01  ws-unexplored-tile-map-data.
           05  ws-unexplored-tile-fg                 pic 9 value black.   
           05  ws-unexplored-tile-bg                 pic 9 value black.
           05  ws-unexplored-tile-char               pic x value space.
           05  ws-unexplored-tile-highlight          pic a value 'N'.
           05  ws-unexplored-tile-blocking           pic a value 'N'.
           05  ws-unexplored-tile-blinking           pic a value 'N'.
           05  ws-unexplored-tile-effect-id     pic 99 comp value zeros.
           05  ws-unexplored-visiblity          pic 999 comp value zero.       
       local-storage section.
       01  ls-counter-1                 pic 999 comp.
       01  ls-counter-2                 pic 999 comp.
       01  ls-enemy-idx                 pic 99 comp.
       01  ls-scr-draw-pos.
           05  ls-scr-draw-y            pic 99.
           05  ls-scr-draw-x            pic 99.
       01  ls-map-pos.           
           05  ls-map-pos-y             pic S999.
           05  ls-map-pos-x             pic S999.
       01  ls-line-mask                 pic x(80) value spaces. 
       01  ls-enemy-draw-pos        occurs 0 to ws-max-num-enemies times
                                    depending on l-cur-num-enemies.
           05  ls-enemy-draw-y          pic 99.
           05  ls-enemy-draw-x          pic 99.
       01  ls-char-to-draw              pic x value space.      
       01  ls-player-disp-stats.               
           05  ls-player-disp-hp.
               10  ls-player-disp-hp-cur     pic zz9 value 0.
               10  ls-player-disp-hp-max     pic zz9 value 0.
           05  ls-player-disp-attack-dmg.
               10  ls-player-disp-atk-cur    pic zz9 value 0.
               10  ls-player-disp-atk-base   pic zz9 value 0.
           05  ls-player-disp-defense.
               10  ls-player-disp-def-cur    pic zz9 value 0.
               10  ls-player-disp-def-base   pic zz9 value 0.
           05  ls-player-disp-level          pic zz9 value 0.
           05  ls-player-disp-exp.
               10  ls-player-disp-exp-cur    pic z(6)9 value 0.
               10  ls-player-disp-exp-nxt    pic z(6)9 value 0.
       linkage section.
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
       01  l-enemy-data.
           05  l-cur-num-enemies           pic 99 comp.
           05  l-enemy           occurs 0 to unbounded times
                                 depending on l-cur-num-enemies.
               10  l-enemy-name            pic x(16).
               10  l-enemy-hp.
                   15  l-enemy-hp-total    pic 999 comp value 10.
                   15  l-enemy-hp-current  pic 999 comp value 10.
               10  l-enemy-attack-damage   pic 999 comp value 1.
               10  l-enemy-pos.
                   15  l-enemy-y           pic 99.
                   15  l-enemy-x           pic 99.
               10  l-enemy-color           pic 9 value red.                                     
               10  l-enemy-char            pic x.
               10  l-enemy-status              pic 9 comp value 0.
                   88  l-enemy-status-alive    value 0.
                   88  l-enemy-status-dead     value 1.
                   88  l-enemy-status-attacked value 2.
                   88  l-enemy-status-other    value 3.
               10  l-enemy-movement-ticks.
                   15  l-enemy-current-ticks   pic 999 comp.
                   15  l-enemy-max-ticks       pic 999 comp value 3.    
               10  l-enemy-exp-worth           pic 9(4) comp.         
       01  l-action-history.
           05  l-action-history-item     occurs 150 times.
               10  l-action-history-text pic x(75).
       01  l-map-explored-data.
           05  l-map-explored-y         occurs ws-max-map-height times.
               10  l-map-explored-x     occurs ws-max-map-width times.
                   15  l-map-explored        pic a value 'N'.
                       88  l-is-explored     value 'Y'.
                       88  l-is-not-explored value 'N'.
       01  l-equiped-items.
           05  l-equiped-weapon.
               10  l-equip-weapon-name        pic x(16).
               10  l-equip-weapon-atk         pic 999.
               10  l-equip-weapon-status      pic x.
                   88  l-equip-weapon-curse   value "-".
                   88  l-equip-weapon-normal  value "0".
                   88  l-equip-weapon-bless   value "+".                        
           05  l-equiped-armor.
               10  l-equip-armor-name         pic x(16).
               10  l-equip-armor-def          pic 999.
               10  l-equip-armor-status       pic x.
                   88  l-equip-armor-curse    value "-".
                   88  l-equip-armor-normal   value "0".
                   88  l-equip-armor-bless    value "+".      
       procedure division using 
               l-player l-tile-map-table-matrix l-enemy-data
               l-action-history l-map-explored-data l-equiped-items.
       main-procedure.
           perform varying ls-counter-1 
           from 1 by 1 until ls-counter-1 > ws-max-view-height
               perform varying ls-counter-2 
               from 1 by 1 until ls-counter-2 > ws-max-view-width
                   move ls-counter-1 to ls-scr-draw-y
                   move ls-counter-2 to ls-scr-draw-x 
                   compute ls-map-pos-y = l-player-y + ls-counter-1 
                   compute ls-map-pos-x = l-player-x + ls-counter-2 
                   if ls-map-pos-y < ws-max-map-height
                       and ls-map-pos-x < ws-max-map-width
                       and ls-map-pos-y > 0 and ls-map-pos-x > 0 
                       then 
                           if l-is-explored(ls-map-pos-y, ls-map-pos-x)
                           then 
                               move l-tile-char(
                                   ls-map-pos-y, ls-map-pos-x)                            
                                   to ls-char-to-draw                                                    
                               call "draw-tile-character" using
                                   ls-scr-draw-pos, 
                                   l-tile-map-data(
                                          ls-map-pos-y, ls-map-pos-x) 
                                   ls-char-to-draw
                               end-call
                           else 
                               move l-tile-blocking(
                                   ls-map-pos-y, ls-map-pos-x)
                                   to ws-unexplored-tile-blocking
                               call "draw-tile-character" using
                                   ls-scr-draw-pos, 
                                   ws-unexplored-tile-map-data 
                                   ws-unexplored-tile-char
                               end-call
                           end-if   
                   else 
                       display ":"                   
                           at ls-scr-draw-pos
                           background-color black
                           foreground-color red
                       end-display
                   end-if
                   if ls-scr-draw-pos = l-player-scr-pos then
                       display l-player-char 
                           at l-player-scr-pos 
                           background-color 
                           l-tile-bg(ls-map-pos-y, ls-map-pos-x) 
                           foreground-color yellow highlight
                       end-display  
                   end-if   
               end-perform
           end-perform
           if l-cur-num-enemies > 0 then 
               perform varying ls-enemy-idx from 1 by 1 
               until ls-enemy-idx > l-cur-num-enemies
                   if l-enemy-y(ls-enemy-idx) > l-player-y then                    
                       compute ls-enemy-draw-y(ls-enemy-idx) = 
                           l-enemy-y(ls-enemy-idx) - l-player-y
                       end-compute 
                    end-if 
                   if l-enemy-x(ls-enemy-idx) > l-player-x then                    
                       compute ls-enemy-draw-x(ls-enemy-idx) = 
                           l-enemy-x(ls-enemy-idx) - l-player-x
                       end-compute 
                   end-if   
                   if ls-enemy-draw-y(ls-enemy-idx) > 0 and 
                   ls-enemy-draw-y(ls-enemy-idx) <= ws-max-view-height
                   and ls-enemy-draw-x(ls-enemy-idx) > 0 and 
                   ls-enemy-draw-x(ls-enemy-idx) <= ws-max-view-width                   
                   and l-is-explored(l-enemy-y(ls-enemy-idx), 
                   l-enemy-x(ls-enemy-idx)) and 
                   ls-enemy-draw-pos(ls-enemy-idx) 
                   not = l-player-scr-pos 
                   then 
                       move l-enemy-char(ls-enemy-idx) 
                           to ls-char-to-draw
                       if l-enemy-status-attacked(ls-enemy-idx) then 
                           move "#" to ls-char-to-draw
                       end-if 
                       if l-enemy-status-dead(ls-enemy-idx) then 
                           move "X" to ls-char-to-draw
                       end-if 
                       display 
                           ls-char-to-draw
                           at ls-enemy-draw-pos(ls-enemy-idx)
                           foreground-color l-enemy-color(ls-enemy-idx)
                           background-color l-tile-bg(
                               l-enemy-y(ls-enemy-idx), 
                               l-enemy-x(ls-enemy-idx))
                       end-display
                   end-if                   
               end-perform 
           end-if            
           perform display-player-info
           call "display-action-history" using l-action-history
           goback.
       display-player-info.
           move l-player-hp-current to ls-player-disp-hp-cur
           move l-player-hp-max to ls-player-disp-hp-max
           move l-player-atk-cur to ls-player-disp-atk-cur
           move l-player-atk-base to ls-player-disp-atk-base
           move l-player-def-cur to ls-player-disp-def-cur
           move l-player-def-base to ls-player-disp-def-base 
           move l-player-level to ls-player-disp-level
           move l-player-exp-total to ls-player-disp-exp-cur
           move l-player-exp-next-lvl to ls-player-disp-exp-nxt
           display 
               function trim(l-player-name) at 0160 underline highlight           
           end-display 
           display 
               function concatenate(
                   "Level: ", 
                   function trim(ls-player-disp-level)) at 0256
               function concatenate(
                   "HP: ",
                   function trim(ls-player-disp-hp-cur),
                   "/",function trim(ls-player-disp-hp-max)
                   , "    ") at 0359
               function concatenate(
                   "Attack: ",
                   function trim(ls-player-disp-atk-cur), "(",
                   function trim(ls-player-disp-atk-base), ")   ") 
                   at 0455     
               function concatenate(
                   "Defense: ",
                   function trim(ls-player-disp-def-cur), "(",
                   function trim(ls-player-disp-def-base), ")   ") 
                   at 0554                                                 
               function concatenate(
                   "Exp next: ",
                   function trim(ls-player-disp-exp-nxt),
                   "    ") at 0653
               function concatenate(
                   "Total Exp: "
                   function trim(ls-player-disp-exp-cur)) at 0752
           end-display  
           display "Equiped:" at 1060 with highlight underline 
           if not l-equip-weapon-normal then            
               display
                   function concatenate(
                       "Weapon: ",
                       function trim(l-equip-weapon-name), " (",
                       l-equip-weapon-status, ") ") at 1155
               end-display 
           else 
               display
                   function concatenate(
                       "Weapon: ",
                       function trim(l-equip-weapon-name)) at 1155
               end-display 
           end-if 
           if not l-equip-armor-normal then               
               display 
                   function concatenate(
                       " Armor: ",
                       function trim(l-equip-armor-name), " (",
                       l-equip-armor-status, ") ") at 1255
               end-display
           else 
               display 
                   function concatenate(
                       " Armor: ",
                       function trim(l-equip-armor-name)) at 1255
               end-display
           end-if            
           exit paragraph.
       end program draw-dynamic-screen-data.
