       IDENTIFICATION DIVISION.
       program-id. cgi-add-local.
       DATA DIVISION.
       working-storage section.
       01   switches-add.
            03  is-db-connected-switch              PIC X   VALUE 'N'.
                88  is-db-connected                         VALUE 'Y'.
            03  is-valid-init-switch                PIC X   VALUE 'N'.
                88  is-valid-init                           VALUE 'Y'.
            03  name-is-in-table-switch             PIC X   VALUE 'N'.
                88  name-is-in-table                        VALUE 'Y'.
            03  is-valid-table-position-switch      PIC X   VALUE 'N'.
                88  is-valid-table-position                 VALUE 'Y'.
       01  wn-rtn-code             PIC  S99   VALUE ZERO.
       01  wc-post-name            PIC X(40)  VALUE SPACE.
       01  wc-post-value           PIC X(40)  VALUE SPACE.
       01  wc-printscr-string      PIC X(40)  VALUE SPACE. 
       01  wc-pagetitle            PIC X(20) VALUE 'Addera lokal'.
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
           SET ENVIRONMENT "OJ_DBG" TO "1"
           SET ENVIRONMENT "OJ_LOG" TO "1"           
           PERFORM A0100-init
           IF is-valid-init
                PERFORM B0100-connect
                IF is-db-connected
                    PERFORM B0200-add-local
                    PERFORM Z0200-disconnect
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
               PERFORM A0110-init-add-action
           END-IF
           . 
       A0110-init-add-action.
           MOVE ZERO TO wn-rtn-code
           MOVE SPACE TO wc-post-value
           MOVE 'local-sign-name' TO wc-post-name
           CALL 'get-post-value' USING wn-rtn-code
                                       wc-post-name wc-post-value                           
           MOVE wc-post-value TO wc-lokalnamn
           IF wc-post-value = SPACE
              MOVE ZERO TO wn-rtn-code
              MOVE SPACE TO wc-post-value
              MOVE 'local-alt-name' TO wc-post-name
              CALL 'get-post-value' USING wn-rtn-code
                                  wc-post-name wc-post-value
              MOVE wc-post-value TO wc-lokalnamn
           END-IF
           IF wc-lokalnamn = SPACE
               MOVE 'Saknar namn på lokal' TO wc-printscr-string
               CALL 'stop-printscr' USING wc-printscr-string
           ELSE
               SET is-valid-init TO TRUE
           END-IF
           MOVE ZERO TO wn-rtn-code
           MOVE SPACE TO wc-post-value
           MOVE 'plan' TO wc-post-name
           CALL 'get-post-value' USING wn-rtn-code wc-post-name
                                       wc-post-value                                     
           MOVE wc-post-value TO wc-vaningsplan
           MOVE ZERO TO wn-rtn-code
           MOVE SPACE TO wc-post-value
           MOVE 'local-max' TO wc-post-name
           CALL 'get-post-value' USING wn-rtn-code
                                       wc-post-name wc-post-value               
           MOVE wc-post-value TO wc-maxdeltagare       
           . 
       B0100-connect.
           MOVE  "openjensen"    TO   wc-database
           MOVE  "jensen"        TO   wc-username
           MOVE  SPACE           TO   wc-passwd
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
       B0200-add-local.
           PERFORM B0210-does-local-name-exist
           IF NOT name-is-in-table
               PERFORM B0220-get-new-row-number
               IF is-valid-table-position
                   PERFORM B0230-add-local-to-table
               END-IF
           ELSE
               MOVE 'Denna lokal finns redan upplagd'
                    TO wc-printscr-string
               CALL 'stop-printscr' USING wc-printscr-string
           END-IF
           .
       B0210-does-local-name-exist.
           EXEC SQL
             DECLARE cursaddlocal CURSOR FOR
                 SELECT Lokal_id, Lokalnamn
                 FROM T_JLOKAL
           END-EXEC      
           EXEC SQL
                OPEN cursaddlocal
           END-EXEC
           MOVE wc-lokalnamn TO jlokal-lokalnamn
           EXEC SQL
               FETCH cursaddlocal
                   INTO :jlokal-lokal-id, :jlokal-lokalnamn
           END-EXEC
           PERFORM UNTIL SQLCODE NOT = ZERO
               IF FUNCTION UPPER-CASE (wc-lokalnamn) =
                  FUNCTION UPPER-CASE (jlokal-lokalnamn)
                        SET name-is-in-table TO TRUE
               END-IF
               EXEC SQL
                   FETCH cursaddlocal
                       INTO :jlokal-lokal-id, :jlokal-lokalnamn
               END-EXEC
           END-PERFORM
           IF  SQLSTATE NOT = '02000'
                PERFORM Z0100-error-routine
           END-IF                 
           EXEC SQL 
               CLOSE cursaddlocal 
           END-EXEC 
           .       
       B0220-get-new-row-number.
           EXEC SQL
             DECLARE cursaddid CURSOR FOR
                 SELECT Lokal_id
                 FROM T_JLOKAL
                 ORDER BY Lokal_id DESC
           END-EXEC   
           EXEC SQL
                OPEN cursaddid
           END-EXEC
           EXEC SQL
               FETCH cursaddid
                   INTO :jlokal-lokal-id
           END-EXEC       
           IF  SQLCODE NOT = ZERO
                PERFORM Z0100-error-routine
           ELSE
               SET is-valid-table-position TO TRUE
               COMPUTE wn-lokal-id = jlokal-lokal-id + 1             
           END-IF
           EXEC SQL 
               CLOSE cursaddid 
           END-EXEC            
           .
       B0230-add-local-to-table.
           MOVE wn-lokal-id TO jlokal-lokal-id
           MOVE wc-lokalnamn TO jlokal-lokalnamn
           MOVE wc-vaningsplan TO jlokal-vaningsplan
           MOVE wc-maxdeltagare TO jlokal-maxdeltagare
           EXEC SQL
               INSERT INTO T_JLOKAL
               VALUES (:jlokal-lokal-id, :jlokal-lokalnamn,
                       :jlokal-vaningsplan, :jlokal-maxdeltagare)
           END-EXEC 
           IF  SQLCODE NOT = ZERO
                PERFORM Z0100-error-routine
           ELSE
                PERFORM B0240-commit-work
                MOVE 'Lokal adderad' TO wc-printscr-string
                CALL 'ok-printscr' USING wc-printscr-string
           END-IF     
           .
       B0240-commit-work.
           EXEC SQL 
               COMMIT WORK
           END-EXEC
           .           
       C0100-closedown.
           CALL 'wui-end-html' USING wn-rtn-code 
           .
       Z0100-error-routine.
           EVALUATE SQLSTATE
               WHEN  "02000"
                   MOVE 'Data återfinns ej i databasen'
                       TO wc-printscr-string
                   CALL 'stop-printscr' USING wc-printscr-string 
              WHEN  "08003"
              WHEN  "08001"
                   MOVE 'Anslutning till databas misslyckades'
                       TO wc-printscr-string
                   CALL 'stop-printscr' USING wc-printscr-string 
              WHEN  "23503"
                   MOVE 'Kan ej ta bort data - pga tabellberoenden'
                       TO wc-printscr-string
                   CALL 'stop-printscr' USING wc-printscr-string                              
              WHEN  SPACE
                   MOVE 'Obekant fel - kontakta leverantören'
                       TO wc-printscr-string
                   CALL 'stop-printscr' USING wc-printscr-string  
              WHEN  OTHER
                   CALL 'error-printscr' USING SQLSTATE SQLERRMC
           END-EVALUATE
           .
       Z0200-disconnect. 
           EXEC SQL
               DISCONNECT ALL
           END-EXEC
           .
