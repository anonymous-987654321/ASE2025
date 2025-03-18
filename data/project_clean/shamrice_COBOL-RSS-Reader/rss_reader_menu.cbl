       identification division.
       program-id. rss-reader-menu.
       environment division.
       configuration section.
       repository.
           function rss-downloader.
       special-names.
           cursor is ws-cursor-position        
           crt status is ws-crt-status.
       input-output section.
           file-control.                              
               select optional fd-rss-list-file
               assign to dynamic ws-rss-list-file-name
               organization is indexed
               access is dynamic
               record key is f-rss-link
               alternate record key is f-rss-feed-id.               
               select optional fd-rss-last-id-file
               assign to dynamic ws-rss-last-id-file-name
               organization is line sequential.
               select fd-rss-content-file
               assign to dynamic ws-rss-content-file-name
               organization is line sequential.
       data division.
       file section.
           FD fd-rss-list-file.
           01  f-rss-list-record.               
               05 f-rss-feed-id                pic 9(5) value zeros.
               05 f-rss-feed-status            pic 9 value zero.
               05 f-rss-title                  pic x(128) value spaces.               
               05 f-rss-dat-file-name          pic x(128) value spaces.
               05 f-rss-link                   pic x(256) value spaces.
           FD fd-rss-last-id-file.
           01 f-rss-last-id-record               pic 9(5) value zeros.
           FD fd-rss-content-file.
           01  f-rss-content-record.
               05  f-feed-id                  pic 9(5) values zeros.
               05  f-feed-title               pic x(128) value spaces.
               05  f-feed-site-link           pic x(256) value spaces.
               05  f-feed-desc                pic x(256) value spaces.
               05  f-num-items                pic 9(6) value 0.               
               05  f-items                    occurs 0 to 15000 times 
                                              depending on f-num-items.              
                   10  f-item-title          pic x(128) value spaces.
                   10  f-item-link           pic x(256) value spaces.
                   10  f-item-guid           pic x(256) value spaces.
                   10  f-item-pub-date       pic x(128) value spaces.
                   10  f-item-desc           pic x(512) value spaces.
       working-storage section.
       01  ws-rss-list-record.           
           05  ws-rss-feed-id                  pic 9(5) value zeros. 
           05  ws-rss-feed-status              pic 9 value zero.          
           05  ws-rss-title                    pic x(128) value spaces.           
           05  ws-rss-dat-file-name            pic x(128) value spaces.
           05  ws-rss-link                     pic x(256) value spaces.
       01  ws-last-id-record                   pic 9(5) value zeros.
       78  ws-max-rss-items                     value 15000.
       77  ws-num-items-disp                    pic 9(6).
       01  ws-rss-record.
           05  ws-feed-id                       pic 9(5) value zeros.
           05  ws-feed-title                    pic x(128) value spaces.
           05  ws-feed-site-link                pic x(256) value spaces.
           05  ws-feed-desc                     pic x(256) value spaces.
           05  ws-num-items                     pic 9(6) value 0.           
           05  ws-items              occurs 0 to ws-max-rss-items times 
                                     depending on ws-num-items.
               10 ws-item-title                 pic x(128) value spaces.
               10 ws-item-link                  pic x(256) value spaces.
               10 ws-item-guid                  pic x(256) value spaces.
               10 ws-item-pub-date              pic x(128) value spaces.
               10 ws-item-desc                  pic x(512) value spaces.
       01  ws-cursor-position. 
           05  ws-cursor-line                   pic 99. 
           05  ws-cursor-col                    pic 99. 
       01  ws-crt-status. 
           05  ws-key1                          pic x. 
           05  ws-key2                          pic x. 
           05  filler                           pic x. 
           05  filler                           pic x.
       01  ws-mouse-flags                       pic 9(4).       
       01  accept-item1                         pic x value space.
       01  ws-eof-sw                            pic a value 'N'.
           88  ws-eof                           value 'Y'.
           88  ws-not-eof                       value 'N'.     
       01  ws-exit-sw                           pic a value 'N'.
           88  ws-exit-true                     value 'Y'.
           88  ws-exit-false                    value 'N'.
       01  ws-display-text                     occurs 17 times.           
           05  ws-display-rss-id               pic 9(5) value zeros. 
           05  ws-display-list-title           pic x(70) value spaces.
           05  ws-display-text-color           pic 9 
                                               value cob-color-white.
       01  ws-refresh-items-sw                 pic a value 'Y'.
           88  ws-is-refresh-items             value 'Y'.
           88  ws-not-refresh-items            value 'N'.
       01  ws-message-screen-fields.
           05  ws-msg-title                    pic x(70) value spaces.
           05  ws-msg-body                     occurs 2 times.
               10  ws-msg-body-text            pic x(70) value spaces.
           05  ws-msg-input                    pic x value space.
       77  ws-selected-feed-file-name          pic x(255) value spaces.
       77  ws-selected-id                      pic 9(5) value zeros.
       77  ws-counter                          pic 9(5) value 1.
       77  ws-rss-idx                          pic 9(5) value 1.
       77  ws-empty-line                       pic x(80) value spaces. 
       77  ws-download-and-parse-status        pic 9 value zero.
       77  ws-rss-content-file-name          pic x(255) value spaces.
       78  ws-rss-list-file-name             value "./feeds/list.dat".
       78  ws-rss-last-id-file-name          value "./feeds/lastid.dat".
       78  ws-feed-status-success            value 1.
       linkage section.
       01  l-refresh-on-start                  pic a.
       screen section.
       01  s-rss-list-screen           
           blank screen 
           foreground-color 7 
           background-color cob-color-black. 
           05  s-menu-screen-2. 
               10  s-title-line
                   foreground-color cob-color-white background-color 1. 
                   15 line 1 pic x(80) from ws-empty-line.
                   15 line 1 column 32 value "COBOL RSS Reader". 
               10  s-header-line
                   foreground-color cob-color-black background-color 7.
                   15 line 2 pic x(80) from ws-empty-line.                   
                   15 line 2 column 5 value "RSS Feed Name".
               10  line 3  column 2 pic x to accept-item1. 
               10  column 4                          
                   foreground-color ws-display-text-color(1)       
                   pic x(70) from ws-display-list-title(1).
               10  column 4 
                   foreground-color ws-display-text-color(1)
                   pic x(70) from ws-display-list-title(1).
               10  line 4  column 2 pic x to accept-item1. 
               10  column 4 
                   foreground-color ws-display-text-color(2)                   
                   pic x(70) from ws-display-list-title(2).               
               10  line 5  column 2 pic x to accept-item1. 
               10  column 4 foreground-color ws-display-text-color(3)
                   pic x(70) from ws-display-list-title(3).        
               10  line 6  column 2 pic x to accept-item1. 
               10  column 4 foreground-color ws-display-text-color(4)
                   pic x(70) from ws-display-list-title(4).        
               10  line 7  column 2 pic x to accept-item1. 
               10  column 4 foreground-color ws-display-text-color(5)
                   pic x(70) from ws-display-list-title(5).        
               10  line 8  column 2 pic x to accept-item1. 
               10  column 4 foreground-color ws-display-text-color(6)
                   pic x(70) from ws-display-list-title(6).        
               10  line 9  column 2 pic x to accept-item1. 
               10  column 4 foreground-color ws-display-text-color(7)
                   pic x(70) from ws-display-list-title(7).        
               10  line 10  column 2 pic x to accept-item1. 
               10  column 4 foreground-color ws-display-text-color(8)
                   pic x(70) from ws-display-list-title(8).        
               10  line 11  column 2 pic x to accept-item1. 
               10  column 4 foreground-color ws-display-text-color(9)
                   pic x(70) from ws-display-list-title(9).        
               10  line 12  column 2 pic x to accept-item1. 
               10  column 4 foreground-color ws-display-text-color(10)
                   pic x(70) from ws-display-list-title(10).        
               10  line 13  column 2 pic x to accept-item1. 
               10  column 4 foreground-color ws-display-text-color(11)
                   pic x(70) from ws-display-list-title(11).        
               10  line 14  column 2 pic x to accept-item1. 
               10  column 4 foreground-color ws-display-text-color(12)
                   pic x(70) from ws-display-list-title(12).        
               10  line 15  column 2 pic x to accept-item1. 
               10  column 4 foreground-color ws-display-text-color(13)
                   pic x(70) from ws-display-list-title(13).        
               10  line 16  column 2 pic x to accept-item1. 
               10  column 4 foreground-color ws-display-text-color(14)
                   pic x(70) from ws-display-list-title(14).                            
               10  line 17  column 2 pic x to accept-item1. 
               10  column 4 foreground-color ws-display-text-color(15)
                   pic x(70) from ws-display-list-title(15).                            
               10  line 18  column 2 pic x to accept-item1. 
               10  column 4 foreground-color ws-display-text-color(16)
                   pic x(70) from ws-display-list-title(16).                            
               10  s-help-line-1
                   foreground-color cob-color-black background-color 7.
                   15  line 20 pic x(80) from ws-empty-line.                   
                   15  line 20 column 8
                       value 
            "Arrow Keys or Tab to move between feeds. Enter to select.".
               10  s-help-text-2.
                   15  foreground-color cob-color-black 
                   background-color cob-color-white line 21 column 2
                   value "F1".
                   15  foreground-color cob-color-white 
                   background-color cob-color-black line 21 column 5
                   value "Help".
                   15  foreground-color cob-color-black 
                   background-color cob-color-white line 21 column 10
                   value "F3".
                   15  foreground-color cob-color-white 
                   background-color cob-color-black line 21 column 13
                   value "Add Feed".
                   15  foreground-color cob-color-black 
                   background-color cob-color-white line 21 column 22
                   value "F4".
                   15  foreground-color cob-color-white 
                   background-color cob-color-black line 21 column 25
                   value "Delete Feed".
                   15  foreground-color cob-color-black 
                   background-color cob-color-white line 21 column 37
                   value "F5".
                   15  foreground-color cob-color-white 
                   background-color cob-color-black line 21 column 40
                   value "Refresh".
                   15  foreground-color cob-color-black 
                   background-color cob-color-white line 21 column 48
                   value "F8".
                   15  foreground-color cob-color-white 
                   background-color cob-color-black line 21 column 51
                   value "Export".
                   15  foreground-color cob-color-black
                   background-color cob-color-white line 21 column 58
                   value "F9".
                   15  foreground-color cob-color-white
                   background-color cob-color-black line 21 column 61
                   value "Config".
                   15  foreground-color cob-color-black
                   background-color cob-color-white line 21 column 68
                   value "F10".
                   15  foreground-color cob-color-white 
                   background-color cob-color-black line 21 column 72
                   value "Exit".
       01  s-blank-screen.
           05 blank screen.
       01  s-message-screen           
           blank screen 
           foreground-color 7 
           background-color cob-color-black. 
           05 s-message-screen-2. 
               10  s-title-line
                   foreground-color cob-color-white background-color 1. 
                   15  line 11 column 10 pic x(60) from ws-empty-line.
                   15  line 11 column 12 pic x(50) from ws-msg-title. 
               10  s-spacer-line
                   foreground-color cob-color-black background-color 7.
                   15  line 12 column 10 pic x(60) from ws-empty-line.                   
               10  s-message-line-1
                   foreground-color cob-color-black background-color 7.
                   15  line 13 column 10 pic x(60) from ws-empty-line.                   
                   15  line 13 column 12 
                       pic x(50) from ws-msg-body-text(1). 
               10  s-message-line-2
                   foreground-color cob-color-black background-color 7.
                   15  line 14 column 10 pic x(60) from ws-empty-line.                   
                   15  line 14 column 12 
                       pic x(50) from ws-msg-body-text(2). 
               10  s-spacer-line
                   foreground-color cob-color-black background-color 7.
                   15  line 15 column 10 pic x(60) from ws-empty-line.    
               10  s-input-line
                   foreground-color 7 background-color 7.
                   15  line 15  column 10 pic x to ws-msg-input. 
       procedure division using l-refresh-on-start.
           set environment 'COB_SCREEN_EXCEPTIONS' to 'Y'.
           set environment 'COB_SCREEN_ESC'        to 'Y'.
           set environment "COB_EXIT_WAIT"         to "NO".
       mouse-setup.
           compute ws-mouse-flags = COB-AUTO-MOUSE-HANDLING 
               + COB-ALLOW-LEFT-DOWN
               + COB-ALLOW-LEFT-UP
               + COB-ALLOW-MOUSE-MOVE
           set environment "COB_MOUSE_FLAGS" to ws-mouse-flags.
       main-procedure.
           call "logger" using "In RSS reader."
           move "Loading..." to ws-msg-title
           if l-refresh-on-start = 'Y' then 
               move "Loading and refreshing RSS feeds..." 
                   to ws-msg-body-text(1)
               set ws-is-refresh-items to true 
           else 
               move "Loading RSS feeds..." to ws-msg-body-text(1)
               set ws-not-refresh-items to true 
           end-if
           display s-message-screen
           perform set-rss-menu-items  
           call "logger" using "done loading rss menu items." 
           move 0 to ws-cursor-line, ws-cursor-col              
           perform until ws-exit-true
               move 0 to ws-selected-id
               move spaces to ws-crt-status
               move spaces to ws-selected-feed-file-name
               display s-blank-screen        
               accept s-rss-list-screen
               evaluate true 
                   when ws-key1 = COB-SCR-OK
                      perform open-selected-in-reader-view-feed
                   when ws-crt-status = COB-SCR-F1
                       perform open-help
                   when ws-crt-status = COB-SCR-F3
                       perform open-add-feed 
                   when ws-crt-status = COB-SCR-F4
                       compute ws-selected-id = 
                           ws-display-rss-id(ws-cursor-line - 2)
                       end-compute
                       perform open-delete-feed
                   when ws-crt-status = COB-SCR-F5
                       perform refresh-feeds                       
                   when ws-crt-status = COB-SCR-F8
                       compute ws-selected-id = 
                           ws-display-rss-id(ws-cursor-line - 2)
                       end-compute
                       perform open-export-feed  
                   when ws-crt-status = COB-SCR-F9
                       perform open-configuration            
                   when ws-crt-status = COB-SCR-F10
                       set ws-exit-true to true 
                   when ws-crt-status = COB-SCR-LEFT-RELEASED
                       perform handle-mouse-click                   
               end-evaluate
           end-perform       
           display s-blank-screen    
           goback.
       handle-mouse-click.
           call "logger" using function concatenate(
               "Mouse clicked at: ", ws-cursor-position)
           end-call 
           if ws-cursor-line = 21 then 
               evaluate true 
                   when ws-cursor-col >= 2 and ws-cursor-col < 9 
                       perform open-help
                   when ws-cursor-col >= 10 and ws-cursor-col < 21
                       perform open-add-feed
                   when ws-cursor-col >= 22 and ws-cursor-col < 36                       
                       display "Enter RSS feed id to delete: " 
                           with blank line 
                           at 2101
                       end-display 
                       accept ws-selected-id at 2130
                       call "logger" using function concatenate(
                           "selected id from delete set: " 
                           ws-selected-id)
                       end-call
                       perform open-delete-feed
                   when ws-cursor-col >= 37 and ws-cursor-col < 47
                       perform refresh-feeds
                   when ws-cursor-col >= 48 and ws-cursor-col < 57                       
                       display "Enter RSS feed id to export: "                            
                           with blank line 
                           at 2101
                       end-display 
                       accept ws-selected-id at 2130
                       call "logger" using function concatenate(
                           "selected id for export set: " 
                           ws-selected-id)
                       end-call                       
                       perform open-export-feed
                   when ws-cursor-col >= 58 and ws-cursor-col < 68
                       perform open-configuration
                   when ws-cursor-col >= 69 and ws-cursor-col < 76 
                       set ws-exit-true to true 
               end-evaluate
           end-if 
           if ws-cursor-line < 20 then 
               perform open-selected-in-reader-view-feed
           end-if 
           exit paragraph.
       open-help.
           call "rss-reader-help"
           cancel "rss-reader-help"
           exit paragraph.
       open-add-feed.
           call "rss-reader-add-feed"
           cancel "rss-reader-add-feed"
           set ws-not-refresh-items to true
           perform set-rss-menu-items        
           exit paragraph.
       open-delete-feed.
           if ws-selected-id <= ws-last-id-record then
               call "rss-reader-delete-feed" using ws-selected-id
               cancel "rss-reader-delete-feed"                                                          
               set ws-not-refresh-items to true 
               perform set-rss-menu-items
           end-if  
           exit paragraph.
       refresh-feeds.
           move "Loading and refreshing RSS feeds..." 
               to ws-msg-body-text(1)
           display s-message-screen
           set ws-is-refresh-items to true 
           perform set-rss-menu-items         
           exit paragraph.
       open-export-feed.
           if ws-selected-id <= ws-last-id-record then
               call "rss-reader-export-feed" using ws-selected-id
               cancel "rss-reader-export-feed"
               set ws-not-refresh-items to true 
               perform set-rss-menu-items 
           end-if            
           exit paragraph.
       open-configuration.
           call "rss-reader-configuration"
           cancel "rss-reader-configuration"
           exit paragraph.
       open-selected-in-reader-view-feed.
           compute ws-selected-id = 
               ws-display-rss-id(ws-cursor-line - 2)
           end-compute
           if ws-selected-id <= ws-last-id-record then
               perform set-selected-feed-file-name
               if ws-selected-feed-file-name not = spaces then
                   call "rss-reader-view-feed" using 
                       by content ws-selected-feed-file-name
                   end-call
                   cancel "rss-reader-view-feed"
               end-if
           end-if  
           exit paragraph.
       load-highest-rss-record.
           set ws-not-eof to true 
           open extend fd-rss-last-id-file close fd-rss-last-id-file
           open input fd-rss-last-id-file
               perform until ws-eof
                   read fd-rss-last-id-file into ws-last-id-record
                       at end set ws-eof to true                    
                   end-read
               end-perform
           close fd-rss-last-id-file
           call "logger" using function concatenate(
               "Highest record found: ", ws-last-id-record)
           end-call 
           exit paragraph.
       set-rss-menu-items.
           perform varying ws-counter from 1 by 1 until ws-counter > 17
               initialize ws-display-text(ws-counter)
           end-perform
           perform load-highest-rss-record
           if ws-last-id-record is zeros then 
               call "logger" using 
                   "No max RSS id found. No items to set. Skipping..."
               end-call 
               exit paragraph
           end-if 
           move 1 to ws-counter
           open extend fd-rss-list-file close fd-rss-list-file
           open input fd-rss-list-file
               perform varying ws-rss-idx 
                   from 1 by 1 until ws-rss-idx > ws-last-id-record
                   if ws-counter > 17 then 
                       call "logger" using function concatenate(
                           "Max feeds displayed on current page. Last",
                           "RSS idx: ", ws-last-id-record, 
                           " : line number: ", ws-counter, 
                           " :: done setting items.")
                       end-call 
                       close fd-rss-list-file
                       exit paragraph
                   end-if 
                   call "logger" using function concatenate(
                       "Checking RSS Feed ID: ", ws-rss-idx)
                   end-call                      
                   move ws-rss-idx to f-rss-feed-id
                   read fd-rss-list-file into ws-rss-list-record
                       key is f-rss-feed-id
                       invalid key 
                           call "logger" using function concatenate(
                               "Unable to find feed with id: ", 
                               f-rss-feed-id, " : Skipping.")
                           end-call 
                       not invalid key 
                           call "logger" using function concatenate(
                               "FOUND :: Title=", ws-rss-title)
                           end-call                           
                           move f-rss-feed-id 
                           to ws-display-rss-id(ws-counter)
                           move ws-rss-title
                           to ws-display-list-title(ws-counter)    
                           move f-rss-feed-status 
                               to ws-download-and-parse-status                       
                           if ws-is-refresh-items then 
                               call "logger" using function concatenate(
                                   "Refreshing feed: ", ws-rss-link)
                               end-call
                               move function rss-downloader(ws-rss-link)
                                   to ws-download-and-parse-status   
                               display s-message-screen                             
                           end-if
                           if ws-download-and-parse-status 
                           = ws-feed-status-success then                                
                               move cob-color-white
                               to ws-display-text-color(ws-counter)
                           else 
                               move cob-color-red
                               to ws-display-text-color(ws-counter)
                           end-if
                           add 1 to ws-counter 
                   end-read       
               end-perform
           close fd-rss-list-file      
           call "logger" using "Done setting rss menu items"
           exit paragraph.
       set-selected-feed-file-name.
           if ws-selected-id > 0 then 
               call "logger" using function concatenate( 
                   "Getting file name for Feed ID: ", ws-selected-id)
               end-call
           else
               call "logger" using "Selected Id=0. No File name to set."
               move spaces to ws-selected-feed-file-name
               exit paragraph
           end-if                      
           open input fd-rss-list-file
               move ws-selected-id to f-rss-feed-id
               read fd-rss-list-file into ws-rss-list-record
                   key is f-rss-feed-id
                   invalid key 
                       move spaces 
                       to ws-selected-feed-file-name
                   not invalid key 
                       call "logger" using function concatenate(
                           "FOUND: ", ws-rss-dat-file-name)
                       end-call                           
                       move ws-rss-dat-file-name
                       to ws-selected-feed-file-name                             
               end-read       
           close fd-rss-list-file 
           exit paragraph.
       end program rss-reader-menu.
