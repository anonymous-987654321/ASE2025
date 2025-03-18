       identification division.
       program-id. rss-reader-view-feed.
       environment division.
       configuration section.
       special-names.
           cursor is ws-cursor-position        
           crt status is ws-crt-status.
       input-output section.
           file-control.               
               select fd-rss-content-file
               assign to dynamic ws-rss-content-file-name
               organization is line sequential.
       data division.
       file section.
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
           05  ws-cursor-line                        pic 99. 
           05  ws-cursor-col                         pic 99. 
       01  ws-crt-status. 
           05  ws-key1                               pic x. 
           05  ws-key2                               pic x. 
           05  filler                                pic x. 
           05  filler                                pic x.
       01  ws-accept-item                            pic x value space.
       01  ws-eof-sw                                 pic a value 'N'.
           88  ws-eof                                value 'Y'.
           88  ws-not-eof                            value 'N'.
       01  ws-exit-sw                                pic a value 'N'.
           88  ws-exit-true                          value 'Y'.
           88  ws-exit-false                         value 'N'.
       77  ws-empty-line                             pic x(80) 
                                                     value spaces. 
       77  ws-selected-id                       pic 9(5) value zeros.    
       77  ws-rss-content-file-name                  pic x(255) 
                                                     value spaces.
       77  ws-idx                                    pic 9(6) comp.       
       local-storage section. 
       01  ls-display-item-title                pic x(128) value spaces
                                                occurs 15 times.
       linkage section.
           01  l-rss-content-file-name               pic x(255).
       screen section.
       01  s-rss-info-screen           
           blank screen 
           foreground-color 7 
           background-color cob-color-black. 
           05  s-menu-screen-2. 
               10  s-title-line
                   foreground-color cob-color-white background-color 1. 
                   15  line 1 pic x(80) from ws-empty-line.
                   15  line 1 column 25 
                       value "COBOL RSS Reader - View Feed". 
               10  s-header-line
                   foreground-color cob-color-black background-color 7.
                   15 line 2 pic x(80) from ws-empty-line.                   
                   15 line 2 column 2 pic x(70) from ws-feed-title.
               10  s-sub-header-line-1
                   foreground-color cob-color-black background-color 7.
                   15 line 3 pic x(80) from ws-empty-line.                   
                   15 line 3 column 2 pic x(70) from ws-feed-site-link. 
               10  s-sub-header-line-2
                   foreground-color cob-color-black background-color 7.
                   15 line 4 pic x(80) from ws-empty-line.                   
                   15 line 4 column 2 pic x(70) from ws-feed-desc. 
               10  line 5  column 2 
                   pic x to ws-accept-item. 
               10  column 4 pic x(70) from ls-display-item-title(1).        
               10  line 6  column 2 
                   pic x to ws-accept-item. 
               10  column 4 pic x(70) from ls-display-item-title(2).        
               10  line 7  column 2 
                   pic x to ws-accept-item. 
               10  column 4 pic x(70) from ls-display-item-title(3).        
               10  line 8  column 2 
                   pic x to ws-accept-item. 
               10  column 4 pic x(70) from ls-display-item-title(4).        
               10  line 9  column 2 
                   pic x to ws-accept-item. 
               10  column 4 pic x(70) from ls-display-item-title(5).        
               10  line 10  column 2 
                   pic x to ws-accept-item. 
               10  column 4 pic x(70) from ls-display-item-title(6).        
               10  line 11  column 2 
                   pic x to ws-accept-item. 
               10  column 4 pic x(70) from ls-display-item-title(7).        
               10  line 12  column 2 
                   pic x to ws-accept-item. 
               10  column 4 pic x(70) from ls-display-item-title(8).        
               10  line 13  column 2 
                   pic x to ws-accept-item. 
               10  column 4 pic x(70) from ls-display-item-title(9).        
               10  line 14  column 2 
                   pic x to ws-accept-item. 
               10  column 4 pic x(70) from ls-display-item-title(10).                            
               10  line 15  column 2 
                   pic x to ws-accept-item. 
               10  column 4 pic x(70) from ls-display-item-title(11).                            
               10  line 16  column 2 
                   pic x to ws-accept-item. 
               10  column 4 pic x(70) from ls-display-item-title(12).                            
               10  line 17  column 2 
                   pic x to ws-accept-item. 
               10  column 4 pic x(70) from ls-display-item-title(13). 
               10  line 18  column 2 
                   pic x to ws-accept-item. 
               10  column 4 pic x(70) from ls-display-item-title(14). 
               10  line 19  column 2 
                   pic x to ws-accept-item. 
               10  column 4 pic x(70) from ls-display-item-title(15). 
               10  s-help-text-1.
                   15  foreground-color cob-color-black 
                   background-color cob-color-white line 21 column 8
                   value " Enter ".
                   15  foreground-color cob-color-white 
                   background-color cob-color-black line 21 column 16
                   value "View Feed Item".
                   15  foreground-color cob-color-black 
                   background-color cob-color-white line 21 column 35
                   value " ESC ".
                   15  foreground-color cob-color-white 
                   background-color cob-color-black line 21 column 41
                   value "Return to RSS List".
       01  s-blank-screen.
           05 blank screen.
       procedure division using l-rss-content-file-name.
       set environment 'COB_SCREEN_EXCEPTIONS' TO 'Y'.
       set environment 'COB_SCREEN_ESC'        TO 'Y'.
       main-procedure.
           display s-blank-screen 
           call "logger" using function concatenate(
               "viewing: ", function trim(l-rss-content-file-name))
           end-call
           if l-rss-content-file-name not = spaces then 
               move l-rss-content-file-name to ws-rss-content-file-name
               perform load-feed-data
               perform handle-user-input
           else 
               call "logger" using 
                   "ERROR: No feed file passed to feed viewer."
               end-call
               move spaces to ws-rss-record
               move "File name passed was empty" to ws-feed-title
               accept s-rss-info-screen
           end-if
           display s-blank-screen 
           goback.
       handle-user-input.
           perform until ws-exit-true
               accept s-rss-info-screen
               evaluate true 
                   when ws-key1 = COB-SCR-OK
                       perform view-selected-feed-item
                   when ws-crt-status = COB-SCR-ESC
                       set ws-exit-true to true 
                   when ws-crt-status = COB-SCR-LEFT-RELEASED
                       perform handle-mouse-click     
               end-evaluate
           end-perform
           exit paragraph.
       handle-mouse-click.
           if ws-cursor-line = 21 and ws-cursor-col >= 35 
           and ws-cursor-col < 59 then 
               set ws-exit-true to true                
           end-if 
           if ws-cursor-line < 20 then                 
               perform view-selected-feed-item
           end-if 
           exit paragraph.
       view-selected-feed-item.
           if ws-cursor-line not > 4 then 
               exit paragraph
           end-if 
           compute ws-selected-id = ws-cursor-line - 4
           move ws-num-items to ws-num-items-disp
           call "logger" using function concatenate(
                   "Selected item ID to view is: ", 
                   ws-selected-id " max: " ws-max-rss-items 
                   " num items: " ws-num-items-disp)
           end-call  
           if ws-selected-id > 0 and ws-selected-id <= ws-max-rss-items 
           and ws-selected-id <= ws-num-items then
               call "logger" using function concatenate(
                   "Selected item ID to view is: ", 
                   ws-selected-id, " Item: ", ws-items(ws-selected-id))
               end-call
               call "rss-reader-view-item" using by content 
                   ws-feed-title,
                   ws-feed-site-link,
                   ws-items(ws-selected-id)
               end-call
               cancel "rss-reader-view-item"
           end-if
           exit paragraph.
       load-feed-data.
           open input fd-rss-content-file
               perform until ws-eof
                   read fd-rss-content-file into ws-rss-record
                       at end set ws-eof to true 
                   not at end
                       call "logger" using function concatenate(
                           "Viewing feed data items for feed: ",
                           function trim(ws-feed-title))
                       end-call                      
                   end-read
               end-perform
           close fd-rss-content-file
           if ws-num-items > 0 then 
               perform varying ws-idx from 1 by 1 
               until ws-idx > ws-num-items or ws-idx > 15
                   move ws-item-title(ws-idx) 
                       to ls-display-item-title(ws-idx)
               end-perform 
           end-if
           exit paragraph.
       end program rss-reader-view-feed.
