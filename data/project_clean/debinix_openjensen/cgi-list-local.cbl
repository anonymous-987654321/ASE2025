       IDENTIFICATION DIVISION.
       program-id. cgi-list-local.
       DATA DIVISION.
       working-storage section.
       01   switches.
            03  is-db-connected-switch      PIC X   VALUE 'N'.
                88  is-db-connected                 VALUE 'Y'.
            03  is-valid-init-switch        PIC X   VALUE 'N'.
                88  is-valid-init                   VALUE 'Y'.
            03  is-real-locals-switch     PIC X   VALUE 'N'.
                88  is-real-locals                VALUE 'Y'.                
       01  wn-rtn-code             PIC  S99   VALUE ZERO.
       01  wc-post-name            PIC X(40)  VALUE SPACE.
       01  wc-post-value           PIC X(40)  VALUE SPACE.
       01  wc-printscr-string      PIC X(40)  VALUE SPACE.   
       01  wc-pagetitle            PIC X(20) VALUE 'Lista lokaler'.
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  wc-database              PIC  X(30).
       01  wc-passwd                PIC  X(10).       
       01  wc-username              PIC  X(30).
       EXEC SQL END DECLARE SECTION END-EXEC.       
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  jlocal-rec-vars.       
           05  jlokal-lokal-id      PIC  9(4).
           05  jlokal-lokalnamn     PIC  X(40).
           05  jlokal-vaningsplan   PIC  X(40).
           05  jlokal-maxdeltagare  PIC  X(40).
       EXEC SQL END DECLARE SECTION END-EXEC.
       01  wr-rec-vars.
           05  wn-lokal-id         PIC  9(4) VALUE ZERO.          
           05  wc-lokalnamn        PIC  X(40) VALUE SPACE.
           05  wc-vaningsplan      PIC  X(40) VALUE SPACE.
           05  wc-maxdeltagare     PIC  X(40) VALUE SPACE.     
       EXEC SQL INCLUDE SQLCA END-EXEC.
       PROCEDURE DIVISION.
       0000-main.
           COPY setupenv_openjensen. 
           PERFORM A0100-init
           IF is-valid-init
                PERFORM B0100-connect
                IF is-db-connected
                    PERFORM B0200-list-locals
                    PERFORM B0300-disconnect
                END-IF
           END-IF
           PERFORM C0100-closedown
           GOBACK
           .
       A0100-init.       
           CALL 'wui-print-header' USING wn-rtn-code  
           CALL 'wui-start-html' USING wc-pagetitle
           CALL 'write-post-string' USING wn-rtn-code
           IF wn-rtn-code = ZERO
               SET is-valid-init TO TRUE
               MOVE ZERO TO wn-rtn-code
               MOVE SPACE TO wc-post-value
               MOVE 'jensen-only' TO wc-post-name
               CALL 'get-post-value' USING wn-rtn-code
                                           wc-post-name wc-post-value
               IF wc-post-value = 'on'
                   SET is-real-locals TO TRUE
               END-IF
           END-IF
           .
       B0100-connect.
           MOVE  "openjensen"    TO   wc-database.
           MOVE  "jensen"        TO   wc-username.
           MOVE  SPACE           TO   wc-passwd.
           EXEC SQL
               CONNECT :wc-username IDENTIFIED BY :wc-passwd
                                                 USING :wc-database 
           END-EXEC
           IF  SQLSTATE NOT = ZERO
                PERFORM Z0100-error-routine
           ELSE
                SET is-db-connected TO TRUE
           END-IF  
           .       
       B0200-list-locals.
           IF is-real-locals
               PERFORM B0210-list-real-locals
           ELSE
               PERFORM B0220-list-all-locals 
           END-IF
           .
       B0210-list-real-locals.
           EXEC SQL 
               DECLARE curslocal CURSOR FOR
               SELECT Lokal_id, Lokalnamn, Vaningsplan, Maxdeltagare
                      FROM T_JLOKAL
                      WHERE Vaningsplan IS NOT NULL
           END-EXEC
           EXEC SQL
               OPEN curslocal
           END-EXEC
           EXEC SQL 
               FETCH curslocal INTO :jlokal-lokal-id,:jlokal-lokalnamn,
                          :jlokal-vaningsplan,:jlokal-maxdeltagare
           END-EXEC
           PERFORM UNTIL SQLCODE NOT = ZERO
              MOVE  jlokal-lokal-id      TO    wn-lokal-id
              MOVE  jlokal-lokalnamn     TO    wc-lokalnamn
              MOVE  jlokal-vaningsplan   TO    wc-vaningsplan
              MOVE  jlokal-maxdeltagare  TO    wc-maxdeltagare
              PERFORM Z0200-display-row
              INITIALIZE jlocal-rec-vars
               EXEC SQL 
                    FETCH curslocal INTO :jlokal-lokal-id,
                                :jlokal-lokalnamn,:jlokal-vaningsplan,
                                :jlokal-maxdeltagare
               END-EXEC
           END-PERFORM
           IF  SQLSTATE NOT = '02000'
                PERFORM Z0100-error-routine
           END-IF              
           EXEC SQL 
               CLOSE curslocal 
           END-EXEC 
           .       
       B0220-list-all-locals.
           EXEC SQL 
               DECLARE cursall CURSOR FOR
               SELECT Lokal_id, Lokalnamn, Vaningsplan, Maxdeltagare
                      FROM T_JLOKAL
                      ORDER BY Lokal_id
           END-EXEC
           EXEC SQL
               OPEN cursall
           END-EXEC
           EXEC SQL 
               FETCH cursall INTO :jlokal-lokal-id,:jlokal-lokalnamn,
                          :jlokal-vaningsplan,:jlokal-maxdeltagare
           END-EXEC
           PERFORM UNTIL SQLCODE NOT = ZERO
              MOVE  jlokal-lokal-id      TO    wn-lokal-id
              MOVE  jlokal-lokalnamn     TO    wc-lokalnamn
              MOVE  jlokal-vaningsplan   TO    wc-vaningsplan
              MOVE  jlokal-maxdeltagare  TO    wc-maxdeltagare
              PERFORM Z0200-display-row
              INITIALIZE jlocal-rec-vars
               EXEC SQL 
                    FETCH cursall INTO :jlokal-lokal-id,
                                :jlokal-lokalnamn,:jlokal-vaningsplan,
                                :jlokal-maxdeltagare
               END-EXEC
           END-PERFORM
           IF  SQLSTATE NOT = '02000'
                PERFORM Z0100-error-routine
           END-IF              
           EXEC SQL 
               CLOSE cursall 
           END-EXEC 
           .
       B0300-disconnect. 
           EXEC SQL
               DISCONNECT ALL
           END-EXEC
           .
       C0100-closedown.
           CALL 'wui-end-html' USING wn-rtn-code 
           .
       Z0100-error-routine.
           COPY z0100-error-routine.
           .
       Z0200-display-row.            
           DISPLAY
                "<br>|" wn-lokal-id "|" wc-lokalnamn "|"
                          wc-vaningsplan "|" wc-maxdeltagare "|"
           END-DISPLAY
           .            
