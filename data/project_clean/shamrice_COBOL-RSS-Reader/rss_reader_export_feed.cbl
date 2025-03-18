       identification division.
       program-id. rss-reader-export-feed.
       environment division.
       configuration section.
       repository.
           function rss-report-writer.
       special-names.
           crt status is ws-crt-status.
       input-output section.
           file-control.                              
               select optional fd-rss-list-file
               assign to dynamic ws-rss-list-file-name
               organization is indexed
               access is dynamic
               record key is f-rss-link
               alternate record key is f-rss-feed-id.               
       data division.
       file section.
           FD fd-rss-list-file.
           01  f-rss-list-record.               
               05 f-rss-feed-id                pic 9(5) value zeros.
               05 f-rss-feed-status            pic 9 value zero.
               05 f-rss-title                  pic x(128) value spaces.               
               05 f-rss-dat-file-name          pic x(128) value spaces.
               05 f-rss-link                   pic x(256) value spaces.
       working-storage section.
       01  ws-rss-list-record.           
           05  ws-rss-feed-id                  pic 9(5) value zeros. 
           05  ws-rss-feed-status              pic 9 value zero.          
           05  ws-rss-title                    pic x(128) value spaces.           
           05  ws-rss-dat-file-name            pic x(128) value spaces.
           05  ws-rss-link                     pic x(256) value spaces.
       01  ws-crt-status. 
           05  ws-key1                       pic x. 
           05  ws-key2                       pic x. 
           05  filler                        pic x. 
           05  filler                        pic x.
       01  ws-accept                         pic x value zero.
       01  ws-export-name                    pic x(70) value spaces.
       01  ws-export-feed-status             pic 9 value zero.
       01  ws-export-msg.
           05  ws-export-line-1              pic x(70) value spaces.
           05  ws-export-line-2              pic x(70) value spaces.
       01  ws-message-screen-fields.
           05  ws-msg-title                  pic x(70) value spaces.
           05  ws-msg-body                   occurs 2 times.
               10  ws-msg-body-text          pic x(70) value spaces.
           05  ws-msg-input                  pic x value space.
       01  ws-exit-sw                        pic a value 'N'.
           88  ws-exit-true                  value 'Y'.
           88  ws-exit-false                 value 'N'.
       77  ws-empty-line                     pic x(80) value spaces. 
       78  ws-rss-list-file-name             value "./feeds/list.dat".
       linkage section.
       01  l-rss-feed-id                     pic 9(5).
       screen section.
       01  s-blank-screen.
           05 blank screen.
       01  s-rss-export-feed-screen           
           blank screen 
           foreground-color 7 
           background-color cob-color-black. 
           05 s-rss-export-feed-screen-2. 
               10  s-title-line
                   foreground-color cob-color-white background-color 1. 
                   15  line 4 column 1 pic x(80) from ws-empty-line.
                   15  line 4 column 35 value "Export RSS Feed". 
               10  s-spacer-line
                   foreground-color cob-color-black background-color 7.
                   15  line 5 column 1 pic x(80) from ws-empty-line.                 
               10  s-rss-export-msg-line-1
                   foreground-color cob-color-black background-color 7.
                   15  line 6 column 1 pic x(80) from ws-empty-line.
                   15  line 6 column 2 pic x(70) from ws-export-line-1.   
               10  s-rss-export-msg-line-2
                   foreground-color cob-color-black background-color 7.
                   15  line 7 column 1 pic x(80) from ws-empty-line.
                   15  line 7 column 2 pic x(70) from ws-export-line-2.   
               10  s-spacer-line
                   foreground-color cob-color-black background-color 7.
                   15  line 8 column 1 pic x(80) from ws-empty-line.                   
               10  s-input-line
                   foreground-color cob-color-black background-color 7.
                   15  line 9 column 1 pic x(80) from ws-empty-line.                   
                   15  line 9 column 2 pic x(78) to ws-export-name. 
                10  s-spacer-line
                   foreground-color cob-color-black background-color 7.
                   15  line 10 column 1 pic x(80) from ws-empty-line.                   
               10  s-message-line
                   foreground-color cob-color-black background-color 7.
                   15  line 11 column 1 pic x(80) from ws-empty-line.
                   15  line 11 column 2 
               value "Press Enter to Export RSS Feed or ESC to Cancel.".   
               10  s-spacer-line
                   foreground-color cob-color-black background-color 7.
                   15  line 12 column 1 pic x(80) from ws-empty-line.                   
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
       procedure division using l-rss-feed-id.
       set environment 'COB_SCREEN_EXCEPTIONS' TO 'Y'.
       set environment 'COB_SCREEN_ESC'        TO 'Y'.
       main-procedure.
           if l-rss-feed-id is zeros then 
               call "logger" using function concatenate(
                   "Cannot export RSS feed with ID ", l-rss-feed-id,
                   ". Ignoring export request and returning.")
               end-call
               goback 
           end-if
           move "Export Feed Status" to ws-msg-title
           move l-rss-feed-id to f-rss-feed-id   
           perform load-feed-to-export
           move function concatenate("Please specify file name for ", 
               "export of feed ", function trim(ws-rss-title)) 
               to ws-export-msg
           perform handle-user-input
           display s-blank-screen 
           goback.
       handle-user-input.
           perform until ws-exit-true
               accept s-rss-export-feed-screen 
               evaluate true 
                   when ws-key1 = COB-SCR-OK
                       perform export-rss-record
                       accept s-message-screen
                       set ws-exit-true to true 
                   when ws-crt-status = COB-SCR-ESC
                       set ws-exit-true to true
               end-evaluate
           end-perform
           exit paragraph.
       load-feed-to-export.
           open input fd-rss-list-file
               read fd-rss-list-file into ws-rss-list-record
               key is f-rss-feed-id
                   invalid key 
                       call "logger" using function concatenate( 
                           "Export RSS feed: Unable to load feed by ",
                           "rss list id. Invalid key: ", f-rss-feed-id)
                       end-call
                   not invalid key                            
                       call "logger" using function concatenate( 
                           "Found to export :: ID: ", f-rss-feed-id, 
                           " :: Title: ", ws-rss-title)
                       end-call     
               end-read       
           close fd-rss-list-file      
           exit paragraph.
       export-rss-record.
           if ws-export-name = spaces then 
               call "logger" using function concatenate( 
                   "No export file name specified. Not calling report ",
                   "writer.")
               end-call 
               move "No file name specified. Not exporting feed."
                   to ws-msg-body(1) 
               exit paragraph
           end-if     
           call "logger" using function concatenate(
               "Exporting RSS id: ", f-rss-feed-id)
           end-call 
           move function rss-report-writer(f-rss-link, ws-export-name) 
               to ws-export-feed-status
           if ws-export-feed-status = 1 then 
               call "logger" using function concatenate( 
                   "RSS Record " f-rss-feed-id " exported to ", 
                   ws-export-name) 
               end-call 
               move "Successfully exported RSS Feed from list."
                   to ws-msg-body-text(1)
           else
               move "Unable to export RSS feed from list."
                   to ws-msg-body-text(1)
               move function concatenate(
                   "Export status code: ", ws-export-feed-status)
                   to ws-msg-body-text(2)
           end-if
           exit paragraph.
       end program rss-reader-export-feed.
