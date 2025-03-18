       PROCESS CICS,NODYNAM,NSYMBOL(NATIONAL),TRUNC(STD)
       IDENTIFICATION DIVISION.
        PROGRAM-ID. ACCTPTNR.
        AUTHOR. GOHILPR.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
        WORKING-STORAGE SECTION.
       1 ACCOUNT-NUMBER-IN.
         2 CUST-NO-IN PIC X(4).
       1 RETURN-DATA.
         2 NUMBER-OF-ACCOUNTS     PIC S9(4) COMP-5 SYNC VALUE 9. 
         2 ACCOUNT-DETAILS OCCURS 5 TIMES.
           3 ACCT-NUMBER     PIC X(8) VALUE ' '.
           3 BALANCE         PIC X(8) VALUE ' '.
           3 OVERDRAFT       PIC X(8) VALUE ' '.
       1 CONTAINER-NAMES.
         2 INPUT-CONTAINER    PIC X(16) VALUE 'INPUTCONTAINER  '.
         2 ACCTPTNR-CONTAINER PIC X(16) VALUE 'ACCTPTNRCONT    '.
       1 COMMAND-RESP  PIC S9(8) COMP.
       1 COMMAND-RESP2 PIC S9(8) COMP.
        LINKAGE SECTION.
       PROCEDURE DIVISION .
       MAINLINE SECTION.
           EXEC CICS DELAY FOR SECONDS(4) END-EXEC
           EXEC CICS GET CONTAINER (INPUT-CONTAINER)
                           INTO    ( ACCOUNT-NUMBER-IN )
                           RESP    ( COMMAND-RESP )
                           RESP2   ( COMMAND-RESP2 )
           END-EXEC
           IF ACCOUNT-NUMBER-IN = '0001'
           THEN
             MOVE 3 TO NUMBER-OF-ACCOUNTS
             MOVE '62837456' TO ACCT-NUMBER(1)
             MOVE '234.56  ' TO BALANCE(1)
             MOVE '0.00       ' TO OVERDRAFT(1)
             MOVE '64620987' TO ACCT-NUMBER(2)
             MOVE '3092.60 ' TO BALANCE(2)
             MOVE '1000.00 ' TO OVERDRAFT(2)
             MOVE '64563923' TO ACCT-NUMBER(3)
             MOVE '10123.98' TO BALANCE(3)
             MOVE '0.00    ' TO OVERDRAFT(3)
           ELSE
             MOVE 2 TO NUMBER-OF-ACCOUNTS
             MOVE '67849321' TO ACCT-NUMBER(1)
             MOVE '3.50    ' TO BALANCE(1)
             MOVE '0.00    ' TO OVERDRAFT(1)
             MOVE '63298568' TO ACCT-NUMBER(2)
             MOVE '52.48   ' TO BALANCE(2)
             MOVE '3000.00 ' TO OVERDRAFT(2)
           END-IF
           EXEC CICS PUT CONTAINER ( ACCTPTNR-CONTAINER )
                           FROM    ( RETURN-DATA )
                           RESP    ( COMMAND-RESP )
                           RESP2   ( COMMAND-RESP2 )
           END-EXEC
           EXEC CICS RETURN
           END-EXEC.
       END PROGRAM 'ACCTPTNR'.