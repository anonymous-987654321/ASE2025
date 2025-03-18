       IDENTIFICATION DIVISION.
       program-id. cgi-remove-local.
       DATA DIVISION.
       working-storage section.
       01   switches.
            03  is-valid-post-switch        PIC X   VALUE 'N'.
                88  is-valid-post                   VALUE 'Y'.
            03  is-valid-transaction-switch PIC X   VALUE 'N'.
                88  is-valid-transaction            VALUE 'Y'.
            03  is-db-connected-switch      PIC X   VALUE 'N'.
                88  is-db-connected                 VALUE 'Y'.
            03  is-valid-init-switch        PIC X   VALUE 'N'.
                88  is-valid-init                   VALUE 'Y'.
            03  is-lokal-id-found-switch    PIC X   VALUE 'N'.
                88  is-lokal-id-found               VALUE 'Y'.                              
       01  wn-rtn-code             PIC  S99   VALUE ZERO.
       01  wc-post-name            PIC X(40)  VALUE SPACE.
       01  wc-post-value           PIC X(40)  VALUE SPACE.
       01  wc-printscr-string      PIC X(40)  VALUE SPACE.          
       01  wc-pagetitle            PIC X(20)  VALUE 'Tag bort lokal'.
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
           IF is-valid-post AND is-valid-init
                PERFORM B0100-connect
                IF is-db-connected
                    PERFORM B0200-cgi-delete-row
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
               MOVE 'local-id' TO wc-post-name               
               CALL 'get-post-value' USING wn-rtn-code
                                           wc-post-name wc-post-value
               MOVE FUNCTION NUMVAL(wc-post-value) TO wn-lokal-id                                                               
           END-IF                                            
           IF wn-lokal-id = 0
                MOVE 'Saknar lokalens identifikation'
                    TO wc-printscr-string
                CALL 'stop-printscr' USING wc-printscr-string
           ELSE
                SET is-valid-post TO TRUE
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
           IF  SQLCODE NOT = ZERO
                PERFORM Z0100-error-routine
           ELSE
                SET is-db-connected TO TRUE
           END-IF  
           .
       B0200-cgi-delete-row.      
           IF wn-lokal-id NOT = 0  
                MOVE wn-lokal-id TO jlokal-lokal-id
                PERFORM B0210-is-lokal-id-data-found
                IF is-lokal-id-found
                     EXEC SQL 
                         DELETE FROM T_JLOKAL
                                  WHERE Lokal_id = :jlokal-lokal-id
                     END-EXEC
                END-IF
                IF  SQLCODE = ZERO
                    MOVE 'Lokal bortagen'
                    TO wc-printscr-string
                    CALL 'ok-printscr' USING wc-printscr-string        
                ELSE
                    PERFORM Z0100-error-routine
                END-IF
           END-IF
           PERFORM B0300-commit-work
           PERFORM B0310-disconnect
           .
       B0210-is-lokal-id-data-found.
           EXEC SQL
             DECLARE curs1 CURSOR FOR
                 SELECT Lokal_id
                 FROM T_JLOKAL
                     WHERE Lokal_id = :jlokal-lokal-id
           END-EXEC.      
           EXEC SQL
                OPEN curs1
           END-EXEC
           EXEC SQL
               FETCH curs1
                   INTO :wn-lokal-id
           END-EXEC
           IF SQLCODE = ZERO
               SET is-lokal-id-found TO TRUE
           END-IF
           . 
       B0300-commit-work.
           EXEC SQL 
               COMMIT WORK
           END-EXEC
           .           
       B0310-disconnect.
           EXEC SQL
               DISCONNECT ALL
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
