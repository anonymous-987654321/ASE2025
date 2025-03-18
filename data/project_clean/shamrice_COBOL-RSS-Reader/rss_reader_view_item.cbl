       identification division.
       program-id. rss-reader-view-item.
       environment division.
       configuration section.
       repository.
           function get-config.
       special-names.
           cursor is ws-cursor-position      
           crt status is ws-crt-status.
       input-output section.
       data division.
       file section.
       working-storage section.
       01  ws-cursor-position. 
           05  ws-cursor-line                pic 99. 
           05  ws-cursor-col                 pic 99. 
       01  ws-crt-status. 
           05  ws-key1                       pic x. 
           05  ws-key2                       pic x. 
           05  filler                        pic x. 
           05  filler                        pic x.
       01  ws-accept-item                    pic x value space.
       01  ws-feed-header-lines.
           05  ws-feed-title                 pic x(128).
           05  ws-feed-site-link             pic x(256).
       01  ws-item-header-lines.
           05  ws-item-title                 pic x(128) value spaces.
           05  ws-item-link                  pic x(256) value spaces.
           05  ws-item-guid                  pic x(256) value spaces.
           05  ws-item-pub-date              pic x(128) value spaces.       
       01  ws-item-desc-lines.
           05  ws-desc-line                  pic x(70) value spaces                               
                                             occurs 8 times.
       01  ws-browser-key-text               pic x(7) value spaces. 
       01  ws-browser-text                   pic x(25) value spaces.
       01  ws-browser-enabled-sw             pic x value 'N'.
           88  ws-browser-enabled            value 'Y'.
           88  ws-browser-disabled           value 'N'.
       01  ws-exit-sw                        pic a value 'N'.
           88  ws-exit-true                  value 'Y'.
           88  ws-exit-false                 value 'N'.
       77  ws-empty-line                     pic x(80) value spaces. 
       01  ws-browser-key-fore-color        pic 9 value cob-color-white.
       01  ws-browser-key-back-color        pic 9 value cob-color-black.
       local-storage section.
       01  ls-config-val-temp                pic x(32) value spaces.
       linkage section.
       01  l-feed-title                      pic x any length.
       01  l-feed-site-link                  pic x any length.
       01  l-feed-item.           
           05  l-item-title                  pic x(128) value spaces.
           05  l-item-link                   pic x(256) value spaces.
           05  l-item-guid                   pic x(256) value spaces.
           05  l-item-pub-date               pic x(128) value spaces.
           05  l-item-desc                   pic x(512) value spaces.
       screen section.
       01  s-rss-item-screen           
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
               10  s-sub-header-line
                   foreground-color cob-color-black background-color 7.
                   15 line 3 pic x(80) from ws-empty-line.                   
                   15 line 3 column 2 pic x(70) from ws-feed-site-link. 
               10  line 4  column 2 
                   pic x to ws-accept-item. 
               10  s-item-title.
                   15 line 4 column 2 value "Item Title:".
                   15 line 4 column 14 pic x(65) from ws-item-title. 
               10  s-item-link.
                   15 line 5 column 2 value "Item Link:".
                   15 line 5 column 14 pic x(65) from ws-item-link.
               10  s-item-guid.
                   15 line 6 column 2 value "Item Guid:".
                   15 line 6 column 14 pic x(65) from ws-item-guid.
               10  s-item-pub-date.
                   15 line 7 column 2 value "Item Pub Date:". 
                   15 line 7 column 20 pic x(60) from ws-item-pub-date.
               10  s-item-description.
                   15 line 9 column 2 value "Item Description:".
                   15 line 10 column 2 value "-----------------".
                   15 line 11 column 2 
                      pic x(70) from ws-desc-line(1).
               10  s-item-description-2.
                   15 line 12 column 2 
                      pic x(70) from ws-desc-line(2).
               10  s-item-description-3.
                   15 line 13 column 2 
                      pic x(70) from ws-desc-line(3).
               10  s-item-description-4.
                   15 line 14 column 2 
                      pic x(70) from ws-desc-line(4).
               10  s-item-description-5.
                   15 line 15 column 2
                      pic x(70) from ws-desc-line(5).
               10  s-item-description-6.
                   15 line 16 column 2 
                      pic x(70) from ws-desc-line(6).
               10  s-item-description-7.
                   15 line 17 column 2 
                      pic x(70) from ws-desc-line(7).
               10  s-item-description-8.
                   15 line 18 column 2 
                      pic x(70) from ws-desc-line(8).
               10  s-help-text-1.
                   15  foreground-color ws-browser-key-fore-color 
                       background-color ws-browser-key-back-color 
                       line 21 column 3
                       pic x(7) from ws-browser-key-text.                   
                   15  foreground-color cob-color-white 
                       background-color cob-color-black 
                       line 21 column 11
                       pic x(25) from ws-browser-text.                   
                   15  foreground-color cob-color-black 
                       background-color cob-color-white 
                       line 21 column 35
                       value " ESC ".
                   15  foreground-color cob-color-white 
                       background-color cob-color-black 
                       line 21 column 41
                       value "Return to Item List".
       01  s-blank-screen.
           05 blank screen.
       procedure division using 
           l-feed-title, l-feed-site-link, l-feed-item.
       set environment 'COB_SCREEN_EXCEPTIONS' TO 'Y'.
       set environment 'COB_SCREEN_ESC'        TO 'Y'.
       main-procedure.
           display space blank screen 
           call "logger" using function concatenate(
               "Viewing feed item: ", l-item-desc)
           end-call 
           move l-feed-title to ws-feed-title
           move l-feed-site-link to ws-feed-site-link
           move l-item-title to ws-item-title
           move l-item-link to ws-item-link
           move l-item-guid to ws-item-guid
           move l-item-pub-date to ws-item-pub-date
           move l-item-desc to ws-item-desc-lines
           move function get-config("browser") to ls-config-val-temp           
           if ls-config-val-temp = "NOT-SET" then 
               set ws-browser-disabled to true
               move spaces to ws-browser-text
               move spaces to ws-browser-key-text
               move cob-color-black to ws-browser-key-fore-color
               move cob-color-black to ws-browser-key-back-color
           else 
               set ws-browser-enabled to true 
               move " Enter " to ws-browser-key-text
               move cob-color-black to ws-browser-key-fore-color
               move cob-color-white to ws-browser-key-back-color
               evaluate ls-config-val-temp
                   when "lynx" 
                       move "Open In Lynx Browser" to ws-browser-text
                   when "links" 
                       move "Open In Links Browser" to ws-browser-text
                   when other 
                       move "Open In Browser" to ws-browser-text
               end-evaluate
           end-if 
           perform handle-user-input
           display space blank screen 
           goback.
       handle-user-input.
           perform until ws-exit-true
               accept s-rss-item-screen
               evaluate true 
                   when ws-key1 = COB-SCR-OK
                       if ws-browser-enabled then 
                           call "browser-launcher" using by content 
                               ws-item-link
                           end-call 
                           cancel "browser-launcher"
                       end-if 
                   when ws-crt-status = COB-SCR-ESC
                       set ws-exit-true to true
                   when ws-crt-status = COB-SCR-LEFT-RELEASED
                       perform handle-mouse-click    
               end-evaluate
           end-perform
           exit paragraph.
       handle-mouse-click.
           if ws-cursor-line = 21 then 
               evaluate true
                   when ws-cursor-col >= 3 and ws-cursor-col < 33
                       if ws-browser-enabled then 
                           call "browser-launcher" using by content 
                               ws-item-link
                           end-call 
                           cancel "browser-launcher"    
                       end-if 
                   when ws-cursor-col >= 35 and ws-cursor-col < 61 
                       set ws-exit-true to true                                              
               end-evaluate
           end-if 
           exit paragraph.
       end program rss-reader-view-item.
