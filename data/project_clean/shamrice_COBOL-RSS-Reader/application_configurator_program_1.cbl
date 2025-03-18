       identification division.
       function-id. get-config.
       environment division.
       configuration section.
       input-output section.
           file-control.
               select optional fd-config-file
               assign to dynamic ws-file-name
               organization is indexed
               access is dynamic
               record key is f-config-name.           
       data division.
       file section.
           FD  fd-config-file.
           01  f-config-set.
               05  f-config-name           pic x(8).
               05  f-config-value          pic x(32).    
       working-storage section.
       77  ws-file-name                    pic x(18) value "crssr.conf".
       local-storage section.
       01  ls-config-set.
           05  ls-config-name              pic x(8) value spaces.
           05  ls-config-value             pic x(32) value spaces.
       linkage section.
       01  l-config-name                   pic x any length.
       01  l-config-value                  pic x(32).
       procedure division 
           using l-config-name
           returning l-config-value.
       main-procedure.
           call "logger" using function concatenate(
               "Getting value for configuration: ", l-config-name)
           end-call 
           move l-config-name to ls-config-name
           move spaces to l-config-value 
           open extend fd-config-file
           close fd-config-file
           open input fd-config-file
               move ls-config-name to f-config-name
               read fd-config-file into ls-config-set
                   key is f-config-name
                   invalid key 
                       call "logger" using function concatenate(
                          "Unable to find config with name: ", 
                          f-config-name, " : Returning spaces.")
                       end-call 
                   not invalid key          
                       call "logger" using function concatenate(
                           "Config found :: name: ", ls-config-name, 
                           " : value: ", ls-config-value)
                       end-call                          
                       move ls-config-value to l-config-value 
               end-read     
           close fd-config-file      
           goback.
       end function get-config.
