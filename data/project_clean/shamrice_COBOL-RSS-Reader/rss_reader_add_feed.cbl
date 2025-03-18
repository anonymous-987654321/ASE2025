       identification division.
       program-id. rss-reader-add-feed.
       environment division.
       configuration section.
       repository.
           function rss-downloader.
       special-names.
           crt status is ws-crt-status.
       input-output section.
       data division.
       file section.
       working-storage section.
       01  ws-crt-status. 
           05  ws-key1                       pic x. 
           05  ws-key2                       pic x. 
           05  filler                        pic x. 
           05  filler                        pic x.
       01  ws-rss-url                        pic x(256) value space.
       01  ws-add-feed-status                pic 9 value zero.
       01  ws-message-screen-fields.
           05  ws-msg-title                  pic x(70) value spaces.
           05  ws-msg-body                   occurs 2 times.
               10  ws-msg-body-text          pic x(70) value spaces.
           05  ws-msg-input                  pic x value space.
       01  ws-exit-sw                        pic a value 'N'.
           88  ws-exit-true                  value 'Y'.
           88  ws-exit-false                 value 'N'.
       77  ws-empty-line                     pic x(80) value spaces. 
       linkage section.
       screen section.
       01  s-blank-screen.
           05 blank screen.
       01  s-rss-add-feed-screen           
           blank screen 
           foreground-color 7 
           background-color cob-color-black. 
           05 s-rss-add-feed-screen-2. 
               10  s-title-line
                   foreground-color cob-color-white background-color 1. 
                   15  line 4 column 1 pic x(80) from ws-empty-line.
                   15  line 4 column 35 value "Add RSS Feed". 
               10  s-spacer-line
                   foreground-color cob-color-black background-color 7.
                   15  line 5 column 1 pic x(80) from ws-empty-line.                 
               10  s-message-line
                   foreground-color cob-color-black background-color 7.
                   15  line 6 column 1 pic x(80) from ws-empty-line.
                   15  line 6 column 2 
                   value "Enter RSS Feed URL to Add or ESC to Cancel:".   
               10  s-spacer-line
                   foreground-color cob-color-black background-color 7.
                   15  line 7 column 1 pic x(80) from ws-empty-line.                 
               10  s-input-line
                   foreground-color cob-color-black background-color 7.
                   15  line 8 column 1 pic x(80) from ws-empty-line.                   
                   15  line 8 column 2 pic x(78) to ws-rss-url. 
               10  s-spacer-line
                   foreground-color cob-color-black background-color 7.
                   15  line 9 column 1 pic x(80) from ws-empty-line.                   
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
       procedure division.
       set environment 'COB_SCREEN_EXCEPTIONS' TO 'Y'.
       set environment 'COB_SCREEN_ESC'        TO 'Y'.
       main-procedure.
           move "Add Feed Status" to ws-msg-title
           perform handle-user-input
           display s-blank-screen 
           goback.
       handle-user-input.
           perform until ws-exit-true
               accept s-rss-add-feed-screen 
               evaluate true 
                   when ws-key1 = COB-SCR-OK
                       call "logger" using ws-rss-url
                       move function rss-downloader(ws-rss-url)
                           to ws-add-feed-status
                       if ws-add-feed-status = 1 then 
                           move "New RSS feed added successfully." to
                               ws-msg-body-text(1)
                       else 
                           move function concatenate(
                               "Downloading and parsing RSS feed ",
                               "failed.")
                               to ws-msg-body-text(1)
                           move function concatenate(
                               "Please check logs. Status: ", 
                               ws-add-feed-status)
                               to ws-msg-body-text(2)
                       end-if    
                       accept s-message-screen
                       set ws-exit-true to true 
                   when ws-crt-status = COB-SCR-ESC
                       set ws-exit-true to true 
               end-evaluate
           end-perform
           exit paragraph.
       end program rss-reader-add-feed.
