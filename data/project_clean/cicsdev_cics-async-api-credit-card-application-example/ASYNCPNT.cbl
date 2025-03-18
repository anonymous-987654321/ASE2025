       PROCESS CICS,NODYNAM,NSYMBOL(NATIONAL),TRUNC(STD)
       IDENTIFICATION DIVISION.
        PROGRAM-ID. ASYNCPNT.
        AUTHOR. GOHILPR.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
        WORKING-STORAGE SECTION.
       1 ACCOUNT-NUMBER-IN.
         2 CUST-NO-IN PIC X(4).
       1 RETURN-DATA.
         2 CREDIT-CHECK-RESULT    PIC X(3)  VALUE ' '.
         2 CUSTOMER-NAME          PIC X(80) VALUE ' '.
         2 CUSTOMER-ADDRESS       PIC X(80) VALUE ' '.
         2 CUSTOMER-POSTCODE      PIC X(8)  VALUE ' '.
         2 CUSTOMER-IMPORTANCE    PIC X(8)  VALUE ' '.
         2 APPLICATION-RESULT     PIC X(7)  VALUE ' '.
       1 APPLICATION-SUCCESS  PIC X(7) VALUE 'SUCCESS'.
       1 APPLICATION-FAILED   PIC X(7) VALUE 'FAILED '.
       1 READ-INPUT.
         2 TRANID                 PIC X(4).
         2 FILLER                 PIC X(1).
         2 INPUTACCNUM            PIC X(4).
       1 READ-INPUT-LENGTH        PIC S9(4) COMP-5 SYNC VALUE 9.
       1 PRINT-LINE.
         2 PARENT-PROGRAM         PIC X(8)  VALUE 'ASYNCPNT'.
         2 FILLER                 PIC X(5)  VALUE ' ACC#'.
         2 ACCOUNT-NUM            PIC X(4)  VALUE '    '.
         2 FILLER                 PIC X(1)  VALUE ' '.
         2 TRANSACTION-1          PIC X(4)  VALUE 'ICCK'.
         2 FILLER                 PIC X(1)  VALUE '('.
         2 TRAN1-STATUS           PIC X(1)  VALUE ' '.
         2 FILLER                 PIC X(2)  VALUE ') '.
         2 TRANSACTION-2          PIC X(4)  VALUE 'GETN'.
         2 FILLER                 PIC X(1)  VALUE '('.
         2 TRAN2-STATUS           PIC X(1)  VALUE ' '.
         2 FILLER                 PIC X(2)  VALUE ') '.
         2 TRANSACTION-3          PIC X(4)  VALUE 'GETA'.
         2 FILLER                 PIC X(1)  VALUE '('.
         2 TRAN3-STATUS           PIC X(1)  VALUE ' '.
         2 FILLER                 PIC X(2)  VALUE ') '.
         2 TRANSACTION-4          PIC X(4)  VALUE 'STUS'.
         2 FILLER                 PIC X(1)  VALUE '('.
         2 TRAN4-STATUS           PIC X(1)  VALUE ' '.
         2 FILLER                 PIC X(2)  VALUE ') '.
         2 TRANSACTION-5          PIC X(4)  VALUE 'UPDB'.
         2 FILLER                 PIC X(1)  VALUE '('.
         2 TRAN5-STATUS           PIC X(1)  VALUE ' '.
         2 FILLER                 PIC X(9)  VALUE ') RESULT-'.
         2 RESULT-TEXT            PIC X(7)  VALUE '       '.
        LOCAL-STORAGE SECTION.
       1 IS-TERMINAL-BASED    PIC X(1) VALUE 'N'.
       1 START-CODE           PIC X(2).
       1 CONTAINER-NAMES.
         2 INPUT-CONTAINER    PIC X(16) VALUE 'INPUTCONTAINER  '.
         2 CRDTCHK-CONTAINER  PIC X(16) VALUE 'CREDITCHECKCONT '.
         2 UPDCSDB-CONTAINER  PIC X(16) VALUE 'UPDATEDB2       '.
         2 GETNAME-CONTAINER  PIC X(16) VALUE 'GETNAMECONTAINER'.
         2 GETADDR-CONTAINER  PIC X(16) VALUE 'GETADDRCONTAINER'.
         2 GETPOST-CONTAINER  PIC X(16) VALUE 'GETPOSTCODE     '.
         2 CSSTATUS-CONTAINER PIC X(16) VALUE 'GETVIPSTATUS    '.
       1 MYCHANNEL            PIC X(16) VALUE 'MYCHANNEL       '.
       1 PROG-NAMES.
         2 CREDIT-CHECK       PIC X(8) VALUE 'CRDTCHK '.
         2 DB-CACHE           PIC X(8) VALUE 'UPDCSDB '.
         2 GET-NAME           PIC X(8) VALUE 'GETNAME '.
         2 GET-ADDR           PIC X(8) VALUE 'GETADDR '.
         2 CSSTATUS           PIC X(8) VALUE 'CSSTATS2'.
       1 COMMAND-RESP  PIC S9(8) COMP.
       1 COMMAND-RESP2 PIC S9(8) COMP.
       1 TRANSIDS.
         2 CREDIT-CHECK-TRAN  PIC X(4) VALUE 'ICCK'.
         2 DB-CACHE-TRAN      PIC X(4) VALUE 'UPDB'.
         2 GET-NAME-TRAN      PIC X(4) VALUE 'GETN'.
         2 GET-ADDR-TRAN      PIC X(4) VALUE 'GETA'.
         2 CSSTATUS-TRAN      PIC X(4) VALUE 'STUS'.
         2 GET-POLICY-TRAN    PIC X(4) VALUE 'GETP'.
         2 GET-SPEND-TRAN     PIC X(4) VALUE 'SPND'.
       1 CHILD-TOKENS.
         2 CREDIT-CHECK-TKN   PIC X(16).
         2 DB-CACHE-TKN       PIC X(16).
         2 GET-NAME-TKN       PIC X(16).
         2 GET-ADDR-TKN       PIC X(16).
         2 CSSTATUS-TKN       PIC X(16).
         2 GET-POLICY-TKN     PIC X(16).
         2 GET-SPEND-TKN      PIC X(16).
       1 RETURN-CHANNELS.
         2 CREDIT-CHECK-CHAN   PIC X(16).
         2 DB-CACHE-CHAN       PIC X(16).
         2 GET-NAME-CHAN       PIC X(16).
         2 GET-ADDR-CHAN       PIC X(16).
         2 CSSTATUS-CHAN       PIC X(16).
         2 GET-POLICY-CHAN     PIC X(16).
         2 GET-SPEND-CHAN      PIC X(16).
       1 CHILD-RETURN-STATUS   PIC S9(8) USAGE BINARY.
       1 CHILD-RETURN-ABCODE   PIC X(4).
        LINKAGE SECTION.
       PROCEDURE DIVISION .
       MAINLINE SECTION.
           PERFORM GET-INPUT-ACCOUNT-NUMBER
           EXEC CICS PUT CONTAINER ( INPUT-CONTAINER )
                           FROM    ( ACCOUNT-NUMBER-IN )
                           CHANNEL ( MYCHANNEL)
                           RESP    ( COMMAND-RESP )
                           RESP2   ( COMMAND-RESP2 )
           END-EXEC
           MOVE '.' TO TRAN1-STATUS
           PERFORM PRINT-TEXT-TO-SCREEN
           EXEC CICS RUN TRANSID      (CREDIT-CHECK-TRAN)
                         CHANNEL      (MYCHANNEL)
                         CHILD        (CREDIT-CHECK-TKN)
           END-EXEC
           MOVE '.' TO TRAN3-STATUS
           PERFORM PRINT-TEXT-TO-SCREEN
           EXEC CICS RUN TRANSID      (GET-ADDR-TRAN)
                         CHANNEL      (MYCHANNEL)
                         CHILD        (GET-ADDR-TKN)
           END-EXEC
           MOVE '.' TO TRAN4-STATUS
           PERFORM PRINT-TEXT-TO-SCREEN
           EXEC CICS RUN TRANSID      (CSSTATUS-TRAN)
                         CHANNEL      (MYCHANNEL)
                         CHILD        (CSSTATUS-TKN)
           END-EXEC
           MOVE '.' TO TRAN2-STATUS
           PERFORM PRINT-TEXT-TO-SCREEN
           EXEC CICS RUN TRANSID      (GET-NAME-TRAN)
                         CHANNEL      (MYCHANNEL)
                         CHILD        (GET-NAME-TKN)
           END-EXEC
           EXEC CICS FETCH CHILD       (CREDIT-CHECK-TKN)
                           CHANNEL     (CREDIT-CHECK-CHAN)
                           COMPSTATUS  (CHILD-RETURN-STATUS)
                           ABCODE      (CHILD-RETURN-ABCODE)
           END-EXEC
           MOVE 'Y' TO TRAN1-STATUS
           PERFORM PRINT-TEXT-TO-SCREEN
           MOVE '.' TO TRAN5-STATUS
           PERFORM PRINT-TEXT-TO-SCREEN
           EXEC CICS RUN TRANSID      (DB-CACHE-TRAN)
                         CHANNEL      (MYCHANNEL)
                         CHILD        (DB-CACHE-TKN)
           END-EXEC
           EXEC CICS GET CONTAINER (CRDTCHK-CONTAINER)
                           INTO    (CREDIT-CHECK-RESULT)
                           CHANNEL (CREDIT-CHECK-CHAN)
                           RESP    (COMMAND-RESP)
                           RESP2   (COMMAND-RESP2)
           END-EXEC
           EXEC CICS FETCH CHILD       (GET-NAME-TKN)
                           CHANNEL     (GET-NAME-CHAN)
                           COMPSTATUS  (CHILD-RETURN-STATUS)
                           ABCODE      (CHILD-RETURN-ABCODE)
           END-EXEC
           MOVE 'Y' TO TRAN2-STATUS
           PERFORM PRINT-TEXT-TO-SCREEN
           EXEC CICS GET CONTAINER (GETNAME-CONTAINER)
                           CHANNEL (GET-NAME-CHAN)
                           INTO    (CUSTOMER-NAME)
                           RESP    (COMMAND-RESP)
                           RESP2   (COMMAND-RESP2)
           END-EXEC
           EXEC CICS FETCH CHILD       (CSSTATUS-TKN)
                           CHANNEL     (CSSTATUS-CHAN)
                           COMPSTATUS  (CHILD-RETURN-STATUS)
                           ABCODE      (CHILD-RETURN-ABCODE)
           END-EXEC
           MOVE 'Y' TO TRAN4-STATUS
           PERFORM PRINT-TEXT-TO-SCREEN
           EXEC CICS GET CONTAINER (CSSTATUS-CONTAINER)
                           CHANNEL (CSSTATUS-CHAN)
                           INTO    (CUSTOMER-IMPORTANCE)
                           RESP    (COMMAND-RESP)
                           RESP2   (COMMAND-RESP2)
           END-EXEC
           EXEC CICS FETCH CHILD       (GET-ADDR-TKN)
                           CHANNEL     (GET-ADDR-CHAN)
                           COMPSTATUS  (CHILD-RETURN-STATUS)
                           ABCODE      (CHILD-RETURN-ABCODE)
           END-EXEC
           MOVE 'Y' TO TRAN3-STATUS
           PERFORM PRINT-TEXT-TO-SCREEN
           EXEC CICS GET CONTAINER (GETADDR-CONTAINER)
                           CHANNEL (GET-ADDR-CHAN)
                           INTO    (CUSTOMER-ADDRESS)
                           RESP    (COMMAND-RESP)
                           RESP2   (COMMAND-RESP2)
           END-EXEC
           EXEC CICS GET CONTAINER (GETPOST-CONTAINER)
                           CHANNEL (GET-ADDR-CHAN)
                           INTO    (CUSTOMER-POSTCODE)
                           RESP    (COMMAND-RESP)
                           RESP2   (COMMAND-RESP2)
           END-EXEC
           EXEC CICS FETCH CHILD       (DB-CACHE-TKN)
                           COMPSTATUS  (CHILD-RETURN-STATUS)
                           ABCODE      (CHILD-RETURN-ABCODE)
           END-EXEC
           MOVE 'Y' TO TRAN5-STATUS
           PERFORM PRINT-TEXT-TO-SCREEN
           MOVE APPLICATION-SUCCESS TO APPLICATION-RESULT
           MOVE APPLICATION-SUCCESS TO RESULT-TEXT
           PERFORM PRINT-TEXT-TO-SCREEN
           EXEC CICS PUT CONTAINER ('ASYNCPNT' )
                           FROM    ( RETURN-DATA )
                           RESP    ( COMMAND-RESP )
                           RESP2   ( COMMAND-RESP2 )
           END-EXEC
           EXEC CICS RETURN
           END-EXEC
           .
       GET-INPUT-ACCOUNT-NUMBER.
           EXEC CICS ASSIGN STARTCODE( START-CODE )
           END-EXEC
           IF START-CODE = 'TD'
           THEN
             MOVE 'Y' TO IS-TERMINAL-BASED
             EXEC CICS RECEIVE INTO     ( READ-INPUT )
                             LENGTH     ( READ-INPUT-LENGTH )
                             NOTRUNCATE
                             RESP       ( COMMAND-RESP )
                             RESP2      ( COMMAND-RESP2 )
             END-EXEC
             MOVE INPUTACCNUM TO CUST-NO-IN
             MOVE INPUTACCNUM TO ACCOUNT-NUM
             PERFORM PRINT-TEXT-TO-SCREEN
           ELSE
             EXEC CICS GET CONTAINER ('ASYNCPNT' )
                             INTO    ( ACCOUNT-NUMBER-IN )
                             RESP    ( COMMAND-RESP )
                             RESP2   ( COMMAND-RESP2 )
             END-EXEC
           END-IF
           .
       PRINT-TEXT-TO-SCREEN.
           IF IS-TERMINAL-BASED = 'Y' THEN
             EXEC CICS SEND TEXT FROM ( PRINT-LINE )
                       TERMINAL WAIT
                       FREEKB
                       ERASE
             END-EXEC
           END-IF
           .
       END PROGRAM 'ASYNCPNT'.
