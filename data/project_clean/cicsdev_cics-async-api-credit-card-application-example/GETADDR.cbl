       PROCESS CICS,NODYNAM,NSYMBOL(NATIONAL),TRUNC(STD)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GETADDR.
       AUTHOR. GOHILPR.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       1 ACCOUNT-NUMBER-IN.
         2 CUST-NO-IN PIC X(4).
       1 RETURN-DATA.
         2 CUSTOMER-ADDRESS       PIC X(80) VALUE ' '.
         2 CUSTOMER-POSTCODE      PIC X(8)  VALUE ' '.
       LOCAL-STORAGE SECTION.
       1 CONTAINER-NAMES.
         2 INPUT-CONTAINER    PIC X(16) VALUE 'INPUTCONTAINER  '.
         2 GETADDR-CONTAINER  PIC X(16) VALUE 'GETADDRCONTAINER'.
         2 GETPOST-CONTAINER  PIC X(16) VALUE 'GETPOSTCODE     '.
       1 PROG-NAMES.
         2 GET-ADDR           PIC X(8) VALUE 'GETADDR '.
       1 COMMAND-RESP  PIC S9(8) COMP.
       1 COMMAND-RESP2 PIC S9(8) COMP.
       LINKAGE SECTION.
       PROCEDURE DIVISION .
       MAINLINE SECTION.
           EXEC CICS GET CONTAINER (INPUT-CONTAINER)
                           INTO    ( ACCOUNT-NUMBER-IN )
                           RESP    ( COMMAND-RESP )
                           RESP2   ( COMMAND-RESP2 )
           END-EXEC
           IF ACCOUNT-NUMBER-IN = '0001'
           THEN
             MOVE '1 HURSLEY PARK, WINCHESTER, UK' TO CUSTOMER-ADDRESS
             MOVE 'SO21 2JN'                       TO CUSTOMER-POSTCODE
           ELSE
             MOVE '123 HIGH STREET, LONDON, UK'    TO CUSTOMER-ADDRESS
             MOVE 'S14 4WG'                        TO CUSTOMER-POSTCODE
           END-IF
           EXEC CICS DELAY FOR SECONDS(5)
           END-EXEC
           EXEC CICS PUT CONTAINER ( GETADDR-CONTAINER )
                           FROM    ( CUSTOMER-ADDRESS )
                           RESP    ( COMMAND-RESP )
                           RESP2   ( COMMAND-RESP2 )
           END-EXEC
           EXEC CICS PUT CONTAINER ( GETPOST-CONTAINER )
                           FROM    ( CUSTOMER-POSTCODE )
                           RESP    ( COMMAND-RESP )
                           RESP2   ( COMMAND-RESP2 )
           END-EXEC
           EXEC CICS RETURN
           END-EXEC.
       END PROGRAM 'GETADDR'.
