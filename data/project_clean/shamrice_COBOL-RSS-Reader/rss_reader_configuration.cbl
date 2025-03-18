       identification division.
       program-id. rss-reader-configuration.
       environment division.
       configuration section.
       repository.
           function get-config
           function save-config.
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
       01  ws-enabled-configs.
           05  ws-wget-enabled-config        pic x.       
           05  ws-curl-enabled-config        pic x.    
           05  ws-lynx-enabled-config        pic x.
           05  ws-links-enabled-config       pic x.
           05  ws-no-browser-enabled-config  pic x.
           05  ws-xterm-enabled-config       pic x.
           05  ws-xterm-disabled-config      pic x.
           05  ws-xmllint-enabled-config     pic x.
           05  ws-xmllint-disabled-config    pic x.
       01  ws-exit-sw                        pic a value 'N'.
           88  ws-exit-true                  value 'Y'.
           88  ws-exit-false                 value 'N'.
       77  ws-empty-line                     pic x(80) value spaces. 
       01  ws-message-screen-fields.
           05  ws-msg-title                  pic x(70) value spaces.
           05  ws-msg-body                   occurs 2 times.
               10  ws-msg-body-text          pic x(70) value spaces.
           05  ws-msg-input                  pic x value space.
       01  ws-option-colors.
           05  ws-option-wget-fg-color       pic 9 value 7.
           05  ws-option-curl-fg-color       pic 9 value 7.    
           05  ws-option-lynx-fg-color       pic 9 value 7.
           05  ws-option-links-fg-color      pic 9 value 7.
           05  ws-option-no-browser-fg-color pic 9 value 7.
           05  ws-option-xterm-fg-color      pic 9 value 7.
           05  ws-option-no-xterm-fg-color   pic 9 value 7.
           05  ws-option-xmllint-fg-color    pic 9 value 7.
           05  ws-option-no-xmllint-fg-color pic 9 value 7.
       local-storage section.
       01  ls-config-val-temp                pic x(32) value spaces.
       01  ls-auto-config-return-val         pic 9.
       01  ls-browser-options-selected       pic 9.
       01  ls-input-valid-sw                 pic a value 'N'.
           88  ls-input-valid                value 'Y'.
           88  ls-input-invalid              value 'N'.
       linkage section.
       screen section.
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
       01  s-rss-config-screen           
           blank screen 
           foreground-color 7 
           background-color cob-color-black. 
           05  s-menu-screen-2. 
               10  s-title-line
                   foreground-color cob-color-white background-color 1. 
                   15  line 1 pic x(80) from ws-empty-line.
                   15  line 1 column 25 
                       value "COBOL RSS Reader Configuration". 
               10  s-header-line
                   foreground-color cob-color-black background-color 7.
                   15 line 2 pic x(80) from ws-empty-line.                                                       
               10  line 3 column 2 value 
           "Here you can manually set the configuration values for the".
               10  line 4 column 2 value 
           "application or choose to use the auto-configuration.".
               10  line 6 column 2 value 
           "Options that are currently enabled are in green.".
               10  line 8 column 2 value 
           "Place an 'x' next to the configs you want to set.".
               10  line 10 column 18 
                   value "RSS Download Command:".
               10  line 10 column 40 
                   pic x to ws-wget-enabled-config.
               10  line 10 column 42 
                   foreground-color ws-option-wget-fg-color
                   value "[wget]".
               10  line 11 column 40 
                   pic x to ws-curl-enabled-config.
               10  line 11 column 42
                   foreground-color ws-option-curl-fg-color
                   value "[curl]".
               10  line 13 column 27 
                   value "Web Browser:".
               10  line 13 column 40 
                   pic x to ws-lynx-enabled-config.
               10  line 13 column 42 
                   foreground-color ws-option-lynx-fg-color
                   value "[lynx]".
               10  line 14 column 40 
                   pic x to ws-links-enabled-config.
               10  line 14 column 42 
                   foreground-color ws-option-links-fg-color
                   value "[links]".
               10  line 15 column 40 
                   pic x to ws-no-browser-enabled-config.
               10  line 15 column 42 
                   foreground-color ws-option-no-browser-fg-color
                   value "[none]".
               10  line 17 column 8
                   value "Open items in new Xterm window?".
               10  line 17 column 40 
                   pic x to ws-xterm-enabled-config.
               10  line 17 column 42 
                   foreground-color ws-option-xterm-fg-color
                   value "[Yes]".
               10  line 17 column 48 
                   pic x to ws-xterm-disabled-config.
               10  line 17 column 50 
                   foreground-color ws-option-no-xterm-fg-color
                   value "[No]".
               10  line 19 column 7
                   value "Retry feed parsing with xmllint?".
               10  line 19 column 40 
                   pic x to ws-xmllint-enabled-config.
               10  line 19 column 42 
                   foreground-color ws-option-xmllint-fg-color
                   value "[Yes]".
               10  line 19 column 48 
                   pic x to ws-xmllint-disabled-config.
               10  line 19 column 50 
                   foreground-color ws-option-no-xmllint-fg-color
                   value "[No]".
               10  s-help-text-1.
                   15  foreground-color cob-color-black  
                       background-color cob-color-white
                       line 21 column 3
                       value " F5 ".
                   15  foreground-color cob-color-white 
                       background-color cob-color-black 
                       line 21 column 8                       
                       value "Run Auto Configuration".
                   15  foreground-color cob-color-black
                       background-color cob-color-white 
                       line 21 column 31 
                       value " Enter ".
                   15  foreground-color cob-color-white
                       background-color cob-color-black
                       line 21 column 39
                       value "Save Changes".
                   15  foreground-color cob-color-black 
                       background-color cob-color-white 
                       line 21 column 52
                       value " ESC ".
                   15  foreground-color cob-color-white 
                       background-color cob-color-black 
                       line 21 column 58
                       value "Return to RSS List".
       procedure division.
       main-procedure.
           display space blank screen 
           call "logger" using "Viewing configuration screen."
           end-call 
           perform handle-user-input
           display space blank screen 
           goback.
       handle-user-input.
           perform until ws-exit-true
               perform refresh-screen-config-values
               accept s-rss-config-screen
               evaluate true 
                   when ws-key1 = COB-SCR-OK 
                       perform validate-and-save-configuration
                   when ws-key1 = COB-SCR-F5
                       perform run-auto-configuration
                   when ws-crt-status = COB-SCR-ESC
                       set ws-exit-true to true
                   when ws-crt-status = COB-SCR-LEFT-RELEASED
                       perform handle-mouse-click    
               end-evaluate
           end-perform
           exit paragraph.
       handle-mouse-click.
           if ws-cursor-line = 10 
           and (ws-cursor-col >= 42 and ws-cursor-col <= 48) 
           then 
               perform save-download-command-wget
               perform save-auto-config-disabled
           end-if 
           if ws-cursor-line = 11 
           and (ws-cursor-col >= 42 and ws-cursor-col <= 48) 
           then
               perform save-download-command-curl
               perform save-auto-config-disabled
           end-if 
           if ws-cursor-line = 13 
           and (ws-cursor-col >= 42 and ws-cursor-col <= 48) 
           then 
               perform save-browser-command-lynx
               perform save-auto-config-disabled
           end-if 
           if ws-cursor-line = 14 
           and (ws-cursor-col >= 42 and ws-cursor-col <= 49) 
           then
               if ws-option-no-xterm-fg-color = cob-color-green then 
                   move "Error" to ws-msg-title
                   move "Xterm is required to use the Links browser." 
                       to ws-msg-body-text(1)   
                   move "Press enter to continue." 
                       to ws-msg-body-text(2)
                   accept s-message-screen   
                   display space blank screen 
               else 
                   perform save-browser-command-links
                   perform save-auto-config-disabled
               end-if 
           end-if 
           if ws-cursor-line = 15 
           and (ws-cursor-col >= 42 and ws-cursor-col <= 48) 
           then 
               perform save-browser-command-no-browser
               perform save-auto-config-disabled
           end-if 
           if ws-cursor-line = 17 
           and (ws-cursor-col >= 42 and ws-cursor-col <= 47) 
           then
               perform save-xterm-command-enabled
               perform save-auto-config-disabled
           end-if 
           if ws-cursor-line = 17 
           and (ws-cursor-col >= 50 and ws-cursor-col <= 54) 
           then
             if ws-option-links-fg-color = cob-color-green then 
                   move "Error" to ws-msg-title
                   move "Links requires that Xterm is enabled." 
                       to ws-msg-body-text(1)   
                   move "Press enter to continue." 
                       to ws-msg-body-text(2)
                   accept s-message-screen   
                   display space blank screen 
               else            
                   perform save-xterm-command-disabled
                   perform save-auto-config-disabled
               end-if 
           end-if 
           if ws-cursor-line = 19 
           and (ws-cursor-col >= 42 and ws-cursor-col <= 47) 
           then
               perform save-xmllint-command-enabled
               perform save-auto-config-disabled
           end-if 
           if ws-cursor-line = 19
           and (ws-cursor-col >= 50 and ws-cursor-col <= 54) 
           then
               perform save-xmllint-command-disabled
               perform save-auto-config-disabled
           end-if 
           if ws-cursor-line = 21 then 
               evaluate true
                   when ws-cursor-col >= 3 and ws-cursor-col < 30
                       perform run-auto-configuration                      
                   when ws-cursor-col >= 31 and ws-cursor-col < 51
                      perform validate-and-save-configuration                       
                   when ws-cursor-col >= 52 and ws-cursor-col < 77 
                       set ws-exit-true to true                                              
               end-evaluate
           end-if 
           exit paragraph.
       run-auto-configuration.
           call "auto-configure" using    
               ls-auto-config-return-val
           end-call           
           display space blank screen 
           display s-rss-config-screen
           move "Auto Configuration" to ws-msg-title
           if ls-auto-config-return-val = 0 then 
               perform save-auto-config-enabled
               move "Auto configuration success." 
                   to ws-msg-body-text(1)   
           else 
               perform save-auto-config-disabled
               move function concatenate(
                   "Auto configuration failure. Status: ", 
                   ls-auto-config-return-val)
                   to ws-msg-body-text(1)                              
           end-if
           move "Press enter to continue." to ws-msg-body-text(2)
           accept s-message-screen   
           display space blank screen 
           exit paragraph.
       refresh-screen-config-values.
           move cob-color-white to ws-option-wget-fg-color
           move cob-color-white to ws-option-curl-fg-color
           move function get-config("down_cmd") to ls-config-val-temp           
           evaluate ls-config-val-temp(1:4)
               when "wget"
                   move cob-color-green to ws-option-wget-fg-color                   
               when "curl" 
                   move cob-color-green to ws-option-curl-fg-color
           end-evaluate
           move cob-color-white to ws-option-lynx-fg-color
           move cob-color-white to ws-option-links-fg-color
           move cob-color-white to ws-option-no-browser-fg-color
           move function get-config("browser") to ls-config-val-temp
           evaluate ls-config-val-temp(1:4)
               when "lynx"
                   move cob-color-green to ws-option-lynx-fg-color
               when "link"
                   move cob-color-green to ws-option-links-fg-color
               when other 
                   move cob-color-green to ws-option-no-browser-fg-color
           end-evaluate
           move cob-color-white to ws-option-xterm-fg-color
           move cob-color-white to ws-option-no-xterm-fg-color
           move function get-config("newwin") to ls-config-val-temp
           if ls-config-val-temp = "true" then 
               move cob-color-green to ws-option-xterm-fg-color
           else 
               move cob-color-green to ws-option-no-xterm-fg-color
           end-if 
           move cob-color-white to ws-option-xmllint-fg-color
           move cob-color-white to ws-option-no-xmllint-fg-color
           move function get-config("xmllint") to ls-config-val-temp
           if ls-config-val-temp(1:7) = "xmllint" then 
               move cob-color-green to ws-option-xmllint-fg-color
           else 
               move cob-color-green to ws-option-no-xmllint-fg-color
           end-if 
           exit paragraph.
       validate-and-save-configuration.
           perform validate-input
           if ls-input-valid then 
               perform save-changes
                   set ls-input-invalid to true 
               else 
                   accept s-message-screen   
                   display space blank screen                              
           end-if  
           exit paragraph.
       validate-input.
           call "logger" using function concatenate(
               "Validating screen input: wget: " ws-wget-enabled-config
               " curl: " ws-curl-enabled-config
               " lynx: " ws-lynx-enabled-config
               " links: " ws-links-enabled-config
               " no-browser: " ws-no-browser-enabled-config
               " xterm: " ws-xterm-enabled-config
               " no-xterm: " ws-xterm-disabled-config
               " xmllint: " ws-xmllint-enabled-config
               " no-xmllint: " ws-xmllint-disabled-config
           ) end-call       
           move "Saving Configuration" to ws-msg-title 
           move "Please correct these errors and try again."
               to ws-msg-body-text(2)
           if (ws-wget-enabled-config not = space and 'x')
           or (ws-curl-enabled-config not = space and 'x') 
           or (ws-lynx-enabled-config not = space and 'x') 
           or (ws-links-enabled-config not = space and 'x') 
           or (ws-no-browser-enabled-config not = space and 'x') 
           or (ws-xterm-enabled-config not = space and 'x') 
           or (ws-xterm-disabled-config not = space and 'x') 
           or (ws-xmllint-enabled-config not = space and 'x') 
           or (ws-xmllint-disabled-config not = space and 'x') 
           then 
               move "Selected configs must be marked with an 'x'."
                   to ws-msg-body-text(1)
               exit paragraph
           end-if 
           if ws-wget-enabled-config not = space 
           and ws-curl-enabled-config not = space then 
               move "Only one download option is allowed."
                   to ws-msg-body-text(1)
               exit paragraph
           end-if
           move zero to ls-browser-options-selected
           if ws-lynx-enabled-config not = space then                
               add 1 to ls-browser-options-selected
           end-if 
           if ws-links-enabled-config not = space then                
               add 1 to ls-browser-options-selected
           end-if 
           if ws-no-browser-enabled-config not = space then                
               add 1 to ls-browser-options-selected
           end-if 
           if ls-browser-options-selected > 1 then 
               move "Only one browser option is allowed."
                   to ws-msg-body-text(1)
               exit paragraph
           end-if
           if ws-xterm-enabled-config not = space 
           and ws-xterm-disabled-config not = space then 
               move "Please select yes or no to use xterm."
                   to ws-msg-body-text(1)
               exit paragraph
           end-if
           if ws-xterm-disabled-config not = space 
           and (ws-links-enabled-config not = space 
           or ws-option-links-fg-color = cob-color-green) 
           and ws-lynx-enabled-config = space 
           and ws-no-browser-enabled-config = space 
           then 
               move "Xterm is required to use the Links browser."
                   to ws-msg-body-text(1) 
               exit paragraph
               move spaces to ws-links-enabled-config
               move 'x' to ws-no-browser-enabled-config
           end-if 
           if ws-xmllint-enabled-config not = space 
           and ws-xmllint-disabled-config not = space then 
               move "Please select yes or no to use xmllint."
                   to ws-msg-body-text(1)
               exit paragraph
           end-if
           set ls-input-valid to true 
           exit paragraph.
       save-changes.
           call "logger" using "Saving config changes."           
           if ws-wget-enabled-config not = space then 
               perform save-download-command-wget
           end-if 
           if ws-curl-enabled-config not = space then 
               perform save-download-command-curl
           end-if 
           if ws-lynx-enabled-config not = space then 
               perform save-browser-command-lynx
           end-if 
           if ws-links-enabled-config not = space then 
               perform save-browser-command-links
           end-if 
           if ws-no-browser-enabled-config not = space then 
               perform save-browser-command-no-browser
           end-if                
           if ws-xterm-enabled-config not = space then 
               perform save-xterm-command-enabled
           end-if
           if ws-xterm-disabled-config not = space then 
               perform save-xterm-command-disabled
           end-if 
           if ws-xmllint-enabled-config not = space then 
               perform save-xmllint-command-enabled
           end-if 
           if ws-xmllint-disabled-config not = space then 
               perform save-xmllint-command-disabled
           end-if 
           perform save-auto-config-disabled
           move "Saving Configuration" to ws-msg-title                      
           move "Configuration changes saved successfully." 
               to ws-msg-body-text(1)                       
           move "Press enter to continue." to ws-msg-body-text(2)  
           accept s-message-screen   
           display space blank screen   
           exit paragraph.
       save-download-command-wget.
           call "save-config" using "down_cmd" "wget -q -O "
           exit paragraph.
       save-download-command-curl.
           call "save-config" using "down_cmd" "curl -s -o "
           exit paragraph.
       save-browser-command-lynx.
           call "save-config" using "browser" "lynx "
           exit paragraph.
       save-browser-command-links.
           call "save-config" using "browser" "links "
           exit paragraph.
       save-browser-command-no-browser.
           call "save-config" using "browser" "NOT-SET"
           exit paragraph.
       save-xterm-command-enabled.
           call "save-config" using "newwin" "true"
           call "save-config" using "newwin_s" 'xterm -hold -e "'
           call "save-config" using "newwin_e" '" & '
           exit paragraph.
       save-xterm-command-disabled.
           call "save-config" using "newwin" "false"
           exit paragraph.
       save-xmllint-command-enabled.
           call "save-config" using "xmllint" "xmllint --format "
           exit paragraph.
       save-xmllint-command-disabled.
           call "save-config" using "xmllint" "NOT-SET"
           exit paragraph.
       save-auto-config-enabled.
           call "save-config" using "autoconf" "true"
           exit paragraph.
       save-auto-config-disabled.
           call "save-config" using "autoconf" "false"
           exit paragraph.
       end program rss-reader-configuration.
