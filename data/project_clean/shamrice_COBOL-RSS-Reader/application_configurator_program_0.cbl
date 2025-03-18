       identification division.
       program-id. save-config.
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
           FD fd-config-file.
           01  f-config-set.
               05  f-config-name           pic x(8).
               05  f-config-value          pic x(32).    
       working-storage section.
       77  ws-file-name                    pic x(18) value "crssr.conf".
       local-storage section.
       01  ls-config-set.
           05  ls-config-name              pic x(8) value spaces.
           05  ls-config-value             pic x(32) value spaces.
       01  ls-record-exists-sw             pic x value 'N'.
           88  ls-record-exists            value 'Y'.
           88  ls-record-not-exists        value 'N'. 
       linkage section.
       01  l-config-name                   pic x any length.
       01  l-config-value                  pic x any length.
       procedure division 
           using l-config-name, l-config-value.
       main-procedure.
           move l-config-name to ls-config-name
           move l-config-value to ls-config-value
           call "logger" using function concatenate(
               "Saving configuration: ", ls-config-name, 
               " with value: ", ls-config-value)
           end-call 
           open extend fd-config-file
           close fd-config-file
           open i-o fd-config-file
               write f-config-set from ls-config-set
                   invalid key 
                       call "logger" using 
                           "Config key already exists in list."
                       end-call 
                       set ls-record-exists to true 
                   not invalid key 
                       call "logger" using 
                           "Saved new Config to config file"
                       end-call 
                       set ls-record-not-exists to true 
               end-write
               if ls-record-exists then 
                   rewrite f-config-set from ls-config-set
                       invalid key
                           call "logger" using function concatenate(
                               "Config record exists but rewrite ",
                               "failed on invalid key for config set: ", 
                               ls-config-set)
                           end-call
                       not invalid key
                           call "logger" using function concatenate(
                               "Config record updated with new value: ",
                               ls-config-set)
                           end-call
                   end-rewrite
               end-if
           close fd-config-file
           goback.
       end program save-config.
