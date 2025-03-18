       identification division.
       program-id. auto-configure.
       environment division.
       configuration section.
       repository.
           function get-config 
           function pipe-open
           function pipe-close.
       input-output section.
       data division.
       file section.
       working-storage section.
       78  ws-config-not-set               value "NOT-SET".
       78  ws-wget-check-cmd   value "wget --version > /dev/null 2>&1".
       78  ws-curl-check-cmd   value "curl --version > /dev/null 2>&1".
       78  ws-lynx-check-cmd   value "lynx --version > /dev/null 2>&1".
       78  ws-links-check-cmd  value "links -version > /dev/null 2>&1".
       78  ws-xmllint-check-cmd            
                            value "xmllint --version > /dev/null 2>&1".
       78  ws-xterm-check-cmd  value "xterm -version > /dev/null 2>&1".
       78  ws-down-cmd-key                 value "down_cmd".
       78  ws-wget-cmd-value               value "wget -q -O ".
       78  ws-curl-cmd-value               value "curl -s -o ".
       78  ws-browser-cmd-key              value "browser".
       78  ws-lynx-cmd-value               value "lynx ".
       78  ws-links-cmd-value              value "links ".
       78  ws-no-browser-value             value ws-config-not-set.
       78  ws-xmllint-cmd-key              value "xmllint".
       78  ws-xmllint-exists-value         value "xmllint --format ".
       78  ws-xmllint-not-exists-value     value ws-config-not-set.
       78  ws-new-window-cmd-key           value "newwin".
       78  ws-new-window-start-cmd-key     value "newwin_s".
       78  ws-new-window-end-cmd-key       value "newwin_e".
       78  ws-new-window-start-cmd-value   value 'xterm -hold -e "'.
       78  ws-new-window-end-cmd-value     value '" & '.
       78  ws-use-autoconf-key             value "autoconf".
       78  ws-use-autoconf-value           value "true".
       local-storage section.
       01  ls-command-to-test              pic x(128).
       01  ls-config-set.
           05  ls-config-name              pic x(8) value spaces.
           05  ls-config-value             pic x(32) value spaces.
           05  ls-dl-cmd-config-value      redefines ls-config-value  
                                           pic x(32).
               88  ls-wget-value           value ws-wget-cmd-value.
               88  ls-curl-value           value ws-curl-cmd-value.
           05  ls-new-window-cmd-config-value redefines ls-config-value
                                           pic x(32).
               88  ls-no-new-win-value     value "false".
               88  ls-use-new-win-value    value "true".
           05  ls-browser-cmd-config-value redefines ls-config-value  
                                           pic x(32).
               88  ls-lynx-value           value ws-lynx-cmd-value.
               88  ls-links-value          value ws-links-cmd-value.               
               88  ls-no-browser-value     value ws-no-browser-value.
           05  ls-xmllint-cmd-config-value redefines ls-config-value  
                                           pic x(32).
               88  ls-xmllint-found-value  
                                      value ws-xmllint-exists-value.
               88  ls-xmllint-not-found-value  
                                      value ws-xmllint-not-exists-value.               
       01  ls-command-exists-sw            pic x value 'N'.
           88  ls-command-not-exists       value 'N'.
           88  ls-command-exists           value 'Y'.
       01  ls-pipe-record.
           05  ls-pipe-pointer             usage pointer.
           05  ls-pipe-return              usage binary-long.
       77  ls-launch-status                pic 9 value 9.
       linkage section.
       01  l-return-status                 pic 9 value 0.
           88  l-return-status-success     value 0.
           88  l-return-status-failure     value 1.
       procedure division using by reference l-return-status.           
       main-procedure.
           call "logger" using "Running auto-configuration..." end-call           
           perform configure-download-command
           perform configure-browser-in-new-window-command
           perform configure-browser-command
           perform configure-xmllint-command
           if l-return-status-success then 
               call "save-config" using 
                   ws-use-autoconf-key ws-use-autoconf-value
               end-call 
           end-if 
           goback.
       check-program-exists.
           move 9 to ls-launch-status
           move 255 to ls-pipe-return 
           set ls-command-not-exists to true
           move pipe-open(ls-command-to-test, "w") to ls-pipe-record
           call "logger" using "pipe open called..."
           if ls-pipe-return not equal 255 then
               call "logger" using "pipe return value check."
               move pipe-close(ls-pipe-record) to ls-launch-status
               if ls-launch-status is zero then
                   call "logger" using function concatenate(
                       "Launch success. Status=", ls-launch-status)
                   end-call 
                   set ls-command-exists to true
               else
                   call "logger" using function concatenate(
                       "Error launching ", 
                       function trim(ls-command-to-test), 
                       ".. Status=", ls-launch-status)
                   end-call
               end-if
           end-if
           exit paragraph.
       configure-download-command.
           call "logger" using "Checking if wget exists"
           move ws-wget-check-cmd to ls-command-to-test
           perform check-program-exists
           if ls-command-exists then 
               call "logger" using "Command exists! wget will be used."
               set ls-wget-value to true
           else 
               call "logger" using "wget not found. Checking for curl."
               move ws-curl-check-cmd to ls-command-to-test
               perform check-program-exists
               if ls-command-exists then 
                   call "logger" using "Using curl for downloading." 
                   set ls-curl-value to true                   
               else 
                   call "logger" using function concatenate(
                       "Failed to find application to download feeds. "
                       "Auto-configuration has failed.")
                   end-call 
                   set l-return-status-failure to true 
                   goback 
               end-if 
           end-if 
           call "save-config" using ws-down-cmd-key ls-config-value 
           exit paragraph.
       configure-browser-in-new-window-command.
           call "logger" using "Checking if xterm exists"
           move ws-xterm-check-cmd to ls-command-to-test
           perform check-program-exists
           if ls-command-exists then 
               call "logger" using "Cmd exists! xterm will be used."
               set ls-use-new-win-value to true
           else 
               call "logger" using function concatenate(
                   "Failed to find xterm installation. RSS items will "
                   "attempt to be opened in current terminal window.")
               end-call 
               set ls-no-new-win-value to true                
           end-if 
           call "save-config" using 
               ws-new-window-cmd-key ls-config-value 
           end-call 
           if ls-use-new-win-value then 
               move ws-new-window-start-cmd-value to ls-config-value
               call "save-config" using 
                   ws-new-window-start-cmd-key ls-config-value 
               end-call 
               move ws-new-window-end-cmd-value to ls-config-value
               call "save-config" using 
                   ws-new-window-end-cmd-key ls-config-value 
               end-call 
           else 
               move ws-config-not-set to ls-config-value
               call "save-config" using 
                   ws-new-window-start-cmd-key ls-config-value
               end-call                
               call "save-config" using 
                   ws-new-window-end-cmd-key ls-config-value 
               end-call 
           end-if 
           exit paragraph.  
       configure-browser-command.
           call "logger" using "Checking if lynx exists"
           move ws-lynx-check-cmd to ls-command-to-test
           perform check-program-exists
           if ls-command-exists then 
               call "logger" using "Command exists! lynx will be used."
               set ls-lynx-value to true
           else 
               move function get-config(ws-new-window-cmd-key)  
                   to ls-config-value
               if ls-config-value = "false" then 
                   call "logger" using function concatenate(
                       "Xterm was not found so browser cannot be opened"
                       " in a new window. Lynx is the only compatible "
                       "browser for this configuration and is not "
                       "present. Setting browser value to NOT-SET")
                   end-call 
                   set ls-no-browser-value to true 
                   call "save-config" using 
                       ws-browser-cmd-key ls-config-value
                   end-call 
                   exit paragraph 
               end-if 
               call "logger" using "lynx not found. Checking for links."
               move ws-links-check-cmd to ls-command-to-test
               perform check-program-exists
               if ls-command-exists then 
                   call "logger" using "Using links for browser." 
                   set ls-links-value to true    
               else 
                   call "logger" using function concatenate(
                       "Failed to find application to browse feeds. "
                       "Open feed in browser option will be disabled.")
                   end-call 
                   set ls-no-browser-value to true   
               end-if 
           end-if 
           call "save-config" using ws-browser-cmd-key ls-config-value 
           exit paragraph.   
       configure-xmllint-command.
           call "logger" using "Checking if xmllint exists"
           move ws-xmllint-check-cmd to ls-command-to-test
           perform check-program-exists
           if ls-command-exists then 
               call "logger" using "Cmd exists! xmllint will be used."
               set ls-xmllint-found-value to true
           else 
               call "logger" using function concatenate(
                   "Failed to find xmllint installation. Retrying of "
                   "formatted feeds will be disabled. ")
               end-call 
               set ls-xmllint-not-found-value to true                
           end-if 
           call "save-config" using ws-xmllint-cmd-key ls-config-value 
           exit paragraph.                    
       end program auto-configure.
