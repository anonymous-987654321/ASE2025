       PROCESS CICS,NODYNAM,NSYMBOL(NATIONAL),TRUNC(STD)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. WEBHOME.
       AUTHOR. GOHILPR.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.  
       1 ACCOUNT-NUMBER-IN.
         2 CUST-NO-IN             PIC X(4).
       1 RETURN-DATA.
         2 CUSTOMER-NAME          PIC X(65) VALUE ' '.
         2 CUSTOMER-LOAN-RATE     PIC X(8)  VALUE ' '.
         2 CUSTOMER-ACCOUNTS.
          3 CURRENT-ACCOUNTS.
           4  NUMBER-OF-ACCOUNTS  PIC S9(4) COMP-5 SYNC VALUE 9.
           4  ACCOUNT-DETAILS OCCURS 5 TIMES.
            5  ACCT-NUMBER        PIC X(8) VALUE ' '.
            5  BALANCE            PIC X(8) VALUE ' '.
            5  OVERDRAFT          PIC X(8) VALUE ' '.
          3 PARTNER-ACCOUNTS.
           4  NUMBER-OF-ACCOUNTS  PIC S9(4) COMP-5 SYNC VALUE 9.
           4  ACCOUNT-DETAILS OCCURS 5 TIMES.
            5  ACCT-NUMBER        PIC X(8) VALUE ' '.
            5  BALANCE            PIC X(8) VALUE ' '.
            5  OVERDRAFT          PIC X(8) VALUE ' '.
       1 TERMINAL-STATUS.
         2 PARENT-PROGRAM         PIC X(8)  VALUE 'WEBHOME'.
         2 FILLER                 PIC X(5)  VALUE ' ACC#'.
         2 ACCOUNT-NUM            PIC X(4)  VALUE '    '.
         2 FILLER                 PIC X(9)  VALUE ' STATUS( '.
         2 CURRENT-STATUS         PIC X(8)  VALUE 'RUNNING '.
         2 FILLER                 PIC X(2)  VALUE ' )'.
       1 STATUS-MSG.
         2 MSG-TIME.
           3 MSG-HOUR            PIC X(2).
           3 FILLER              PIC X(1)  VALUE ':'.
           3 MSG-MIN             PIC X(2).
           3 FILLER              PIC X(1)  VALUE '.'.
           3 MSG-SEC             PIC X(2).
           3 FILLER              PIC X(1)  VALUE SPACES.
         2 MSG-TEXT              PIC X(61) VALUE ' '.
       1 READ-INPUT.
         2 TRANID                PIC X(4) VALUE '    '.
         2 FILLER                PIC X(1).
         2 INPUTACCNUM           PIC X(4) VALUE '    '.
       1 READ-INPUT-LENGTH       PIC S9(4) COMP-5 SYNC VALUE 9.
       1 CONTAINER-NAMES.
         2 INPUT-CONTAINER       PIC X(16) VALUE 'INPUTCONTAINER  '.
         2 GETNAME-CONTAINER     PIC X(16) VALUE 'GETNAMECONTAINER'.
         2 ACCTCURR-CONTAINER    PIC X(16) VALUE 'ACCTCURRCONT    '.
         2 ACCTPTNR-CONTAINER    PIC X(16) VALUE 'ACCTPTNRCONT    '.
         2 GETLOAN-CONTAINER     PIC X(16) VALUE 'GETLOANCONTAINER'.
         2 ACCOUNTS-CONTAINER    PIC X(16) VALUE 'ALLCUSTACCOUNTS '.
       1 MYCHANNEL               PIC X(16) VALUE 'MYCHANNEL       '.
       1 PROGRAM-NAMES.
         2 GET-NAME              PIC X(8) VALUE 'GETNAME '.
         2 ACCTCURR              PIC X(8) VALUE 'ACCTCURR'.
         2 ACCTPTNR              PIC X(8) VALUE 'ACCTPTNR'.
         2 GETLOAN               PIC X(8) VALUE 'GETLOAN '.
       1 TRANSIDS.
         2 GET-NAME-TRAN         PIC X(4) VALUE 'GETN'.
         2 ACCTCURR-TRAN         PIC X(4) VALUE 'ACUR'.
         2 ACCTPTNR-TRAN         PIC X(4) VALUE 'PTNR'.
         2 GETLOAN-TRAN          PIC X(4) VALUE 'GETL'.
       1 CHILD-TOKENS.
         2 ANY-CHILD-TKN         PIC X(16).
         2 GET-NAME-TKN          PIC X(16).
         2 ACCTCURR-TKN          PIC X(16).
         2 ACCTPTNR-TKN          PIC X(16).
         2 GET-LOAN-TKN          PIC X(16).
       1 RETURN-CHANNELS.
         2 ANY-CHILD-CHAN        PIC X(16).
         2 GET-NAME-CHAN         PIC X(16).
         2 ACCTCURR-CHAN         PIC X(16).
         2 ACCTPTNR-CHAN         PIC X(16).
         2 GET-LOAN-CHAN         PIC X(16).
       1 CHILD-RETURN-STATUS     PIC S9(8) USAGE BINARY.
       1 CHILD-RETURN-ABCODE     PIC X(4).
       1 COMMAND-RESP            PIC S9(8) COMP.
       1 COMMAND-RESP2           PIC S9(8) COMP.
       1 TIMEOUT-TSQ.
         2 TSQ-NAME              PIC X(8) VALUE 'LTIMEOUT'.
         2 TSQ-TIMEOUT           PIC X(8) VALUE '        '.
         2 TIMEOUT-LEN           PIC S9(4) USAGE BINARY.
       1 LOAN-RATE-TIMEOUT       PIC S9(8) USAGE BINARY VALUE 0.
       1 COUNTER                 PIC S9(4) COMP-5 SYNC VALUE 9.
       LINKAGE SECTION.
       PROCEDURE DIVISION.
       MAINLINE SECTION.
           INITIALIZE STATUS-MSG
           MOVE 'Started Web banking log-on data retrieval' TO MSG-TEXT
           PERFORM PRINT-STATUS-MESSAGE
           PERFORM GET-INPUT-ACCOUNT-NUMBER
           EXEC CICS PUT CONTAINER ( INPUT-CONTAINER )
                           FROM    ( ACCOUNT-NUMBER-IN )
                           CHANNEL ( MYCHANNEL)
                           RESP    ( COMMAND-RESP )
                           RESP2   ( COMMAND-RESP2 )
           END-EXEC
           PERFORM CHECK-COMMAND
           EXEC CICS RUN TRANSID ( ACCTPTNR-TRAN )
                         CHANNEL ( MYCHANNEL )
                         CHILD   ( ACCTPTNR-TKN )
                         RESP    ( COMMAND-RESP )
                         RESP2   ( COMMAND-RESP2 )
           END-EXEC
           PERFORM CHECK-COMMAND
           EXEC CICS RUN TRANSID ( GET-NAME-TRAN )
                         CHANNEL ( MYCHANNEL )
                         CHILD   ( GET-NAME-TKN )
                         RESP    ( COMMAND-RESP )
                         RESP2   ( COMMAND-RESP2 )
           END-EXEC
           PERFORM CHECK-COMMAND
           EXEC CICS RUN TRANSID ( ACCTCURR-TRAN )
                         CHANNEL ( MYCHANNEL )
                         CHILD   ( ACCTCURR-TKN )
                         RESP    ( COMMAND-RESP )
                         RESP2   ( COMMAND-RESP2 )
           END-EXEC
           PERFORM CHECK-COMMAND
           PERFORM 3 TIMES
             EXEC CICS FETCH ANY        ( ANY-CHILD-TKN )
                             CHANNEL    ( ANY-CHILD-CHAN )
                             COMPSTATUS ( CHILD-RETURN-STATUS )
                             ABCODE     ( CHILD-RETURN-ABCODE )
                             RESP       ( COMMAND-RESP )
                             RESP2      ( COMMAND-RESP2 )
             END-EXEC
             PERFORM CHECK-COMMAND
             PERFORM CHECK-CHILD
             EVALUATE ANY-CHILD-TKN
               WHEN GET-NAME-TKN
                 MOVE ANY-CHILD-CHAN TO GET-NAME-CHAN
                 EXEC CICS GET CONTAINER ( GETNAME-CONTAINER )
                                 CHANNEL ( GET-NAME-CHAN )
                                 INTO    ( CUSTOMER-NAME )
                                 RESP    ( COMMAND-RESP )
                                 RESP2   ( COMMAND-RESP2 )
                 END-EXEC
                 PERFORM CHECK-COMMAND
                 INITIALIZE STATUS-MSG
                 STRING 'Welcome '
                        DELIMITED BY SIZE
                        CUSTOMER-NAME
                        DELIMITED BY SIZE
                      INTO MSG-TEXT
                 PERFORM PRINT-STATUS-MESSAGE
               WHEN ACCTCURR-TKN
                 MOVE ANY-CHILD-CHAN TO ACCTCURR-CHAN
                 EXEC CICS GET CONTAINER ( ACCTCURR-CONTAINER )
                                 CHANNEL ( ACCTCURR-CHAN )
                                 INTO    ( CURRENT-ACCOUNTS )
                                 RESP    ( COMMAND-RESP )
                                 RESP2   ( COMMAND-RESP2 )
                 END-EXEC
                 PERFORM CHECK-COMMAND
                 PERFORM PRINT-CURRENT-ACCOUNTS-DETAILS
               WHEN ACCTPTNR-TKN
                 MOVE ANY-CHILD-CHAN TO ACCTPTNR-CHAN
                 EXEC CICS GET CONTAINER ( ACCTPTNR-CONTAINER )
                               CHANNEL   ( ACCTPTNR-CHAN )
                               INTO      ( PARTNER-ACCOUNTS )
                               RESP      ( COMMAND-RESP )
                               RESP2     ( COMMAND-RESP2 )
                 END-EXEC
                 PERFORM CHECK-COMMAND
                 PERFORM PRINT-PARTNER-ACCOUNTS-DETAILS
               WHEN OTHER
                 INITIALIZE STATUS-MSG
                 STRING '*** Unknown child token: '
                        DELIMITED BY SIZE
                        ANY-CHILD-TKN
                        DELIMITED BY SIZE
                      INTO MSG-TEXT
                 PERFORM PRINT-STATUS-MESSAGE
                 PERFORM WEBHOME-ERROR
             END-EVALUATE
           END-PERFORM
           EXEC CICS PUT CONTAINER ( ACCOUNTS-CONTAINER )
                           FROM    ( CUSTOMER-ACCOUNTS )
                           CHANNEL ( MYCHANNEL)
                           RESP    ( COMMAND-RESP )
                           RESP2   ( COMMAND-RESP2 )
           END-EXEC
           PERFORM CHECK-COMMAND
           EXEC CICS RUN TRANSID ( GETLOAN-TRAN )
                         CHANNEL ( MYCHANNEL )
                         CHILD   ( GET-LOAN-TKN )
                         RESP    ( COMMAND-RESP )
                         RESP2   ( COMMAND-RESP2 )
           END-EXEC
           PERFORM CHECK-COMMAND
           MOVE 8 TO TIMEOUT-LEN
           EXEC CICS READQ TS QUEUE  ( TSQ-NAME )
                              ITEM   ( 1 )
                              INTO   ( TSQ-TIMEOUT )
                              LENGTH ( TIMEOUT-LEN )
                              RESP   ( COMMAND-RESP )
                              RESP2  ( COMMAND-RESP2 )
           END-EXEC
           IF COMMAND-RESP = DFHRESP(NORMAL)
           THEN
             MOVE TSQ-TIMEOUT(1:TIMEOUT-LEN) TO LOAN-RATE-TIMEOUT
             INITIALIZE STATUS-MSG
             STRING 'Timeout of '
                      DELIMITED BY SIZE
                      TSQ-TIMEOUT
                      DELIMITED BY SPACE
                      ' milliseconds to get loan rate quote.'
                      DELIMITED BY SIZE
                    INTO MSG-TEXT
             PERFORM PRINT-STATUS-MESSAGE 
           ELSE
             MOVE 0 TO LOAN-RATE-TIMEOUT
             INITIALIZE STATUS-MSG
             MOVE 'Timeout not set for loan rate quote.' TO MSG-TEXT
             PERFORM PRINT-STATUS-MESSAGE
           END-IF
           EXEC CICS FETCH CHILD      ( GET-LOAN-TKN )
                           TIMEOUT    ( LOAN-RATE-TIMEOUT )
                           CHANNEL    ( GET-LOAN-CHAN )
                           COMPSTATUS ( CHILD-RETURN-STATUS )
                           ABCODE     ( CHILD-RETURN-ABCODE )
                           RESP       ( COMMAND-RESP )
                           RESP2      ( COMMAND-RESP2 )
           END-EXEC
           IF COMMAND-RESP = DFHRESP(NOTFINISHED) AND COMMAND-RESP2 = 53
           THEN
             INITIALIZE STATUS-MSG
             MOVE
              'Abandoned loan quote because it took too long!'
              TO MSG-TEXT
             PERFORM PRINT-STATUS-MESSAGE
           ELSE
             PERFORM CHECK-COMMAND
             PERFORM CHECK-CHILD
             EXEC CICS GET CONTAINER ( GETLOAN-CONTAINER )
                           CHANNEL   ( GET-LOAN-CHAN )
                           INTO      ( CUSTOMER-LOAN-RATE )
                           RESP      ( COMMAND-RESP )
                           RESP2     ( COMMAND-RESP2 )
             END-EXEC
             PERFORM CHECK-COMMAND
             INITIALIZE STATUS-MSG
             STRING 'Personalised Loan Rate: '
                    DELIMITED BY SIZE
                    CUSTOMER-LOAN-RATE
                    DELIMITED BY SPACE
                    ' %'
                    DELIMITED BY SIZE
                  INTO MSG-TEXT
             PERFORM PRINT-STATUS-MESSAGE
           END-IF
           MOVE 'COMPLETE' TO CURRENT-STATUS
           PERFORM PRINT-TEXT-TO-SCREEN
           INITIALIZE STATUS-MSG
           MOVE 'Ended Web banking log-on data retrieval' TO MSG-TEXT
           PERFORM PRINT-STATUS-MESSAGE
           EXEC CICS RETURN
           END-EXEC
           .
       GET-INPUT-ACCOUNT-NUMBER.
           EXEC CICS RECEIVE INTO       ( READ-INPUT )
                             LENGTH     ( READ-INPUT-LENGTH )
                             NOTRUNCATE
                             RESP       ( COMMAND-RESP )
                             RESP2      ( COMMAND-RESP2 )
           END-EXEC
           IF INPUTACCNUM = '    '
           THEN 
             MOVE '9999' TO CUST-NO-IN
             MOVE '9999' TO ACCOUNT-NUM
           ELSE
             MOVE INPUTACCNUM TO CUST-NO-IN
             MOVE INPUTACCNUM TO ACCOUNT-NUM
           END-IF
           PERFORM PRINT-TEXT-TO-SCREEN
           .
       PRINT-CURRENT-ACCOUNTS-DETAILS.
           IF NUMBER-OF-ACCOUNTS OF CURRENT-ACCOUNTS > 0 THEN
             MOVE 1 TO COUNTER
             PERFORM UNTIL COUNTER > 
                       NUMBER-OF-ACCOUNTS OF CURRENT-ACCOUNTS
               INITIALIZE STATUS-MSG
               STRING 'Acc: '
                      DELIMITED BY SIZE
                      ACCT-NUMBER OF CURRENT-ACCOUNTS (COUNTER)
                      DELIMITED BY SPACE
                      ' Bal: $'
                      DELIMITED BY SIZE
                      BALANCE OF CURRENT-ACCOUNTS (COUNTER)
                      DELIMITED BY SIZE
                      ' Overdraft: $'
                      DELIMITED BY SIZE
                      OVERDRAFT OF CURRENT-ACCOUNTS (COUNTER)
                      DELIMITED BY SIZE
                    INTO MSG-TEXT
               PERFORM PRINT-STATUS-MESSAGE
               ADD 1 TO COUNTER
             END-PERFORM
           END-IF
           .
       PRINT-PARTNER-ACCOUNTS-DETAILS.
           IF NUMBER-OF-ACCOUNTS OF PARTNER-ACCOUNTS > 0 THEN
             MOVE 1 TO COUNTER
             PERFORM UNTIL COUNTER >
                       NUMBER-OF-ACCOUNTS OF PARTNER-ACCOUNTS
               INITIALIZE STATUS-MSG
               STRING 'Acc: '
                      DELIMITED BY SIZE
                      ACCT-NUMBER OF PARTNER-ACCOUNTS (COUNTER)
                      DELIMITED BY SPACE
                      ' Bal: $'
                      DELIMITED BY SIZE
                      BALANCE OF PARTNER-ACCOUNTS (COUNTER)
                      DELIMITED BY SIZE
                      ' Overdraft: $'
                      DELIMITED BY SIZE
                      OVERDRAFT OF PARTNER-ACCOUNTS (COUNTER)
                      DELIMITED BY SIZE
                    INTO MSG-TEXT
               PERFORM PRINT-STATUS-MESSAGE
               ADD 1 TO COUNTER
             END-PERFORM
           END-IF
           .
       PRINT-STATUS-MESSAGE.
           MOVE FUNCTION CURRENT-DATE(13:2) TO MSG-SEC
           MOVE FUNCTION CURRENT-DATE(11:2) TO MSG-MIN
           MOVE FUNCTION CURRENT-DATE(9:2)  TO MSG-HOUR
           DISPLAY STATUS-MSG
           .
       PRINT-TEXT-TO-SCREEN.
           EXEC CICS SEND TEXT FROM ( TERMINAL-STATUS )
                     TERMINAL WAIT
                     FREEKB
                     ERASE
           END-EXEC
           .
       CHECK-COMMAND.
           IF COMMAND-RESP NOT = DFHRESP(NORMAL)
           THEN
             PERFORM WEBHOME-ERROR
           END-IF
           .
       CHECK-CHILD.
           IF CHILD-RETURN-STATUS NOT = DFHVALUE(NORMAL)
           THEN
             PERFORM WEBHOME-ERROR
           END-IF
           .
       WEBHOME-ERROR.
           INITIALIZE STATUS-MSG
           MOVE '*** Error occurred in WEBHOME.' TO MSG-TEXT
           PERFORM PRINT-STATUS-MESSAGE
           MOVE 'FAILED' TO CURRENT-STATUS
           PERFORM PRINT-TEXT-TO-SCREEN
           EXEC CICS ABEND ABCODE('WEBH') NODUMP END-EXEC
           .
       END PROGRAM 'WEBHOME'.