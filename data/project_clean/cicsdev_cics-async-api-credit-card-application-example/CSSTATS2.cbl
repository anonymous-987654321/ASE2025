       PROCESS CICS,NODYNAM,NSYMBOL(NATIONAL),TRUNC(STD)
       IDENTIFICATION DIVISION.
        PROGRAM-ID. CSSTATS2.
        AUTHOR. GOHILPR.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
        WORKING-STORAGE SECTION.
       1 ACCOUNT-NUMBER-IN.
         2 CUST-NO-IN PIC X(4).
       1 RETURN-DATA.
         2 CUSTOMER-IMPORTANCE    PIC X(8)  VALUE '        '.
        LOCAL-STORAGE SECTION.
       1 CONTAINER-NAMES.
         2 INPUT-CONTAINER    PIC X(16) VALUE 'INPUTCONTAINER  '.
         2 CSSTATS2-CONTAINER PIC X(16) VALUE 'GETVIPSTATUS    '.
       1 PROG-NAMES.
         2 GETPOL             PIC X(8) VALUE 'GETPOL  '.
         2 GETSPND            PIC X(8) VALUE 'GETSPND '.
       1 COMMAND-RESP  PIC S9(8) COMP.
       1 COMMAND-RESP2 PIC S9(8) COMP.
       1 TRANSIDS.
         2 GET-POLICY-TRAN    PIC X(4) VALUE 'GETP'.
         2 GET-SPEND-TRAN     PIC X(4) VALUE 'SPND'.
       1 CHILD-TOKENS.
         2 GET-POLICY-TKN     PIC X(16).
         2 GET-SPEND-TKN      PIC X(16).
        LINKAGE SECTION.
       PROCEDURE DIVISION .
       MAINLINE SECTION.
           EXEC CICS GET CONTAINER ( INPUT-CONTAINER )
                           INTO    ( ACCOUNT-NUMBER-IN )
                           RESP    ( COMMAND-RESP )
                           RESP2   ( COMMAND-RESP2 )
           END-EXEC
           EXEC CICS RUN TRANSID      (GET-POLICY-TRAN)
                         CHILD        (GET-POLICY-TKN)
           END-EXEC
           EXEC CICS RUN TRANSID      (GET-SPEND-TRAN)
                         CHILD        (GET-SPEND-TKN)
           END-EXEC
           IF ACCOUNT-NUMBER-IN = '0001'
           THEN
             MOVE 'VERY VIP' TO CUSTOMER-IMPORTANCE
           ELSE
             MOVE 'REGULAR ' TO CUSTOMER-IMPORTANCE
           END-IF
           EXEC CICS PUT CONTAINER ( CSSTATS2-CONTAINER )
                           FROM    ( CUSTOMER-IMPORTANCE )
                           RESP    ( COMMAND-RESP )
                           RESP2   ( COMMAND-RESP2 )
           END-EXEC
           EXEC CICS RETURN
           END-EXEC.
       END PROGRAM 'CSSTATS2'.
