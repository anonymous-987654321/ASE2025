       PROCESS CICS,NODYNAM,NSYMBOL(NATIONAL),TRUNC(STD)
       IDENTIFICATION DIVISION.
        PROGRAM-ID. GETNAME.
        AUTHOR. GOHILPR.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
        WORKING-STORAGE SECTION.
       1 ACCOUNT-NUMBER-IN.
         2 CUST-NO-IN PIC X(4).
       1 RETURN-DATA.
         2 CUSTOMER-NAME          PIC X(65) VALUE ' '.
       1 CONTAINER-NAMES.
         2 INPUT-CONTAINER    PIC X(16) VALUE 'INPUTCONTAINER  '.
         2 GETNAME-CONTAINER  PIC X(16) VALUE 'GETNAMECONTAINER'.
       1 COMMAND-RESP  PIC S9(8) COMP.
       1 COMMAND-RESP2 PIC S9(8) COMP.
        LINKAGE SECTION.
       PROCEDURE DIVISION .
       MAINLINE SECTION.
           EXEC CICS DELAY FOR SECONDS(3) END-EXEC
           EXEC CICS GET CONTAINER (INPUT-CONTAINER)
                           INTO    ( ACCOUNT-NUMBER-IN )
                           RESP    ( COMMAND-RESP )
                           RESP2   ( COMMAND-RESP2 )
           END-EXEC
           EVALUATE ACCOUNT-NUMBER-IN
             WHEN '0001'
               MOVE 'Pradeep Gohil'     TO CUSTOMER-NAME
             WHEN '0002'
               MOVE 'Chris Poole'       TO CUSTOMER-NAME
             WHEN '0003'
               MOVE 'Jenny He'          TO CUSTOMER-NAME
             WHEN '0004'
               MOVE 'Julian Horn'       TO CUSTOMER-NAME
             WHEN '0005'
               MOVE 'Amy Reeve'         TO CUSTOMER-NAME
             WHEN '0006'
               MOVE 'Greg Lubel'        TO CUSTOMER-NAME
             WHEN '0007'
               MOVE 'Tony Papageorgiou' TO CUSTOMER-NAME
             WHEN OTHER
               MOVE 'Simon Rachman'     TO CUSTOMER-NAME
           END-EVALUATE
           EXEC CICS PUT CONTAINER ( GETNAME-CONTAINER )
                           FROM    ( CUSTOMER-NAME )
                           RESP    ( COMMAND-RESP )
                           RESP2   ( COMMAND-RESP2 )
           END-EXEC
           EXEC CICS RETURN
           END-EXEC.
       END PROGRAM 'GETNAME'.