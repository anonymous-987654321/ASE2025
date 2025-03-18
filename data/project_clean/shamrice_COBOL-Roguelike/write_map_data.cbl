       identification division.
       program-id. write-map-data.
       environment division.
       configuration section.
       input-output section.
       file-control.
           select optional fd-tile-data 
               assign to dynamic l-map-dat-file 
               organization is record sequential
               file status is ls-map-file-status.
           select optional fd-teleport-data
               assign to dynamic l-map-tel-file
               organization is record sequential
               file status is ls-teleport-file-status.            
           select optional fd-enemy-data
               assign to dynamic l-map-enemy-file
               organization is record sequential
               file status is ls-enemy-file-status.
           select optional fd-item-data
               assign to dynamic l-map-item-file
               organization is record sequential
               file status is ls-item-file-status.
       data division.
       file section.
       fd  fd-tile-data.
       01  f-tile-data-record.
           05  f-tile-fg               pic 9.   
           05  f-tile-bg               pic 9.
           05  f-tile-char             pic x.
           05  f-tile-highlight        pic a.
           05  f-tile-blocking         pic a.
           05  f-tile-blinking         pic a.
           05  f-tile-effect-id        pic 99 comp.
           05  f-tile-visibility       pic 999 comp.
       fd  fd-teleport-data.
       01  f-teleport-data-record.
           05  f-teleport-pos.
               10  f-teleport-y        pic S99.
               10  f-teleport-x        pic S99.
           05  f-teleport-dest-pos.
               10  f-teleport-dest-y   pic S99.
               10  f-teleport-dest-x   pic S99.
           05  f-teleport-dest-map     pic x(15).
       fd  fd-enemy-data.           
       01  f-enemy.
           05  f-enemy-name                 pic x(16).
           05  f-enemy-hp.
               10  f-enemy-hp-total         pic 999 comp.
               10  f-enemy-hp-current       pic 999 comp.
           05  f-enemy-attack-damage        pic 999 comp.
           05  f-enemy-pos.
               10  f-enemy-y                pic 99.
               10  f-enemy-x                pic 99.
           05  f-enemy-color                pic 9. 
           05  f-enemy-char                 pic x. 
           05  f-enemy-status               pic 9 comp.
           05  f-enemy-movement-ticks.
               10  f-enemy-current-ticks    pic 999 comp.
               10  f-enemy-max-ticks        pic 999 comp.
           05  f-enemy-exp-worth            pic 9(4) comp.
       fd  fd-item-data.
       01  f-item-data-record.                               
           05  f-item-name            pic x(16).                                          
           05  f-item-pos.
               10  f-item-y           pic S99.
               10  f-item-x           pic S99.
           05  f-item-taken           pic a.
           05  f-item-effect-id       pic 99.
           05  f-item-worth           pic 999.
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
       01  ws-counter-1                 pic 999 comp.
       01  ws-counter-2                 pic 999 comp.
       local-storage section.
       01  ls-map-file-statuses.
           05  ls-map-file-status      pic xx.
           05  ls-teleport-file-status pic xx.
           05  ls-enemy-file-status    pic xx.
           05  ls-item-file-status     pic xx.
       linkage section.
       01  l-map-files.  
           05  l-map-name             pic x(15).
           05  l-map-name-temp        pic x(15). 
           05  l-map-dat-file         pic x(15).               
           05  l-map-tel-file         pic x(15).
           05  l-map-enemy-file       pic x(15).   
           05  l-map-item-file        pic x(15).
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
       01  l-item-data.
           05  l-cur-num-items            pic 999 comp.
           05  l-item-data-record         occurs 0 to ws-max-num-items
                                          depending on l-cur-num-items.
               10  l-item-name            pic x(16).                                          
               10  l-item-pos.
                   15  l-item-y           pic S99.
                   15  l-item-x           pic S99.
               10  l-item-taken           pic a value 'N'.
                   88  l-item-is-taken    value 'Y'.
                   88  l-item-not-taken   value 'N'.               
               10  l-item-effect-id       pic 99.
               10  l-item-worth           pic 999.
               10  l-item-color           pic 9. 
               10  l-item-char            pic x.               
       01  l-return-code                   pic 9 value 0.         
       procedure division using 
           l-map-files l-tile-map-table-matrix 
           l-enemy-data l-teleport-data l-item-data
           l-return-code. 
       main-procedure.
           move ws-save-status-fail to l-return-code 
           open output fd-tile-data
           perform varying ws-counter-1 
           from 1 by 1 until ws-counter-1 > ws-max-map-height
               perform varying ws-counter-2 
               from 1 by 1 until ws-counter-2 > ws-max-map-width
                   move l-tile-map-data(ws-counter-1, ws-counter-2) 
                       to f-tile-data-record
                   write f-tile-data-record                                                                      
               end-perform
           end-perform
           close fd-tile-data
           open output fd-enemy-data
               perform varying ws-counter-1 
               from 1 by 1 until ws-counter-1 > l-cur-num-enemies
                   move l-enemy(ws-counter-1) to f-enemy
                   write f-enemy 
               end-perform 
           close fd-enemy-data
           open output fd-teleport-data
               perform varying ws-counter-1 
               from 1 by 1 until ws-counter-1 > l-cur-num-teleports
                   move l-teleport-data-record(ws-counter-1) 
                       to f-teleport-data-record
                   write f-teleport-data-record
               end-perform 
           close fd-teleport-data
           open output fd-item-data
               perform varying ws-counter-1 
               from 1 by 1 until ws-counter-1 > l-cur-num-items
                   move l-item-data-record(ws-counter-1) 
                       to f-item-data-record
                   write f-item-data-record
               end-perform 
           close fd-item-data
           move ws-save-status-success to l-return-code
           goback.
       end program write-map-data.
