       PROCESS CICS,NODYNAM,NSYMBOL(NATIONAL),TRUNC(STD)
       CBL CICS('SP,EDF,DLI')
       IDENTIFICATION DIVISION.
       PROGRAM-ID. UPDCUST.
       AUTHOR. Jon Collett.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-370.
       OBJECT-COMPUTER.  IBM-370.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SORTCODE.
       77 SYSIDERR-RETRY               PIC 999.
       01 WS-CICS-WORK-AREA.
          03 WS-CICS-RESP              PIC S9(8) COMP.
          03 WS-CICS-RESP2             PIC S9(8) COMP.
       LOCAL-STORAGE SECTION.
       01 DB2-DATE-REFORMAT.
          03 DB2-DATE-REF-YR           PIC 9(4).
          03 FILLER                    PIC X.
          03 DB2-DATE-REF-MNTH         PIC 99.
          03 FILLER                    PIC X.
          03 DB2-DATE-REF-DAY          PIC 99.
       01 WS-CUST-DATA.
          COPY CUSTOMER.
       01 WS-EIBTASKN12                PIC 9(12) VALUE 0.
       01 WS-SQLCODE-DISP              PIC 9(9)  VALUE 0.
       01 DESIRED-CUST-KEY.
          03 DESIRED-SORT-CODE         PIC 9(6).
          03 DESIRED-CUSTNO            PIC 9(10).
       01 WS-CUST-REC-LEN              PIC S9(4) COMP VALUE 0.
       01 WS-U-TIME                    PIC S9(15) COMP-3.
       01 WS-ORIG-DATE                 PIC X(10).
       01 WS-ORIG-DATE-GRP REDEFINES WS-ORIG-DATE.
          03 WS-ORIG-DATE-DD           PIC 99.
          03 FILLER                    PIC X.
          03 WS-ORIG-DATE-MM           PIC 99.
          03 FILLER                    PIC X.
          03 WS-ORIG-DATE-YYYY         PIC 9999.
       01 WS-ORIG-DATE-GRP-X.
          03 WS-ORIG-DATE-DD-X         PIC XX.
          03 FILLER                    PIC X VALUE '.'.
          03 WS-ORIG-DATE-MM-X         PIC XX.
          03 FILLER                    PIC X VALUE '.'.
          03 WS-ORIG-DATE-YYYY-X       PIC X(4).
       01 REJ-REASON                   PIC XX VALUE SPACES.
       01 WS-PASSED-DATA.
          02 WS-TEST-KEY               PIC X(4).
          02 WS-SORT-CODE              PIC 9(6).
          02 WS-CUSTOMER-RANGE.
             07 WS-CUSTOMER-RANGE-TOP     PIC X.
             07 WS-CUSTOMER-RANGE-MIDDLE  PIC X.
             07 WS-CUSTOMER-RANGE-BOTTOM  PIC X.
       01 WS-SORT-DIV.
          03 WS-SORT-DIV1              PIC XX.
          03 WS-SORT-DIV2              PIC XX.
          03 WS-SORT-DIV3              PIC XX.
       01 CUSTOMER-KY.
          03 REQUIRED-SORT-CODE        PIC 9(6)  VALUE 0.
          03 REQUIRED-ACC-NUM          PIC 9(8)  VALUE 0.
       01 STORM-DRAIN-CONDITION        PIC X(20).
       01 WS-UNSTR-TITLE               PIC X(9)  VALUE ' '.
       01 WS-TITLE-VALID               PIC X.
       01 WS-TIME-DATA.
           03 WS-TIME-NOW              PIC 9(6).
           03 WS-TIME-NOW-GRP REDEFINES WS-TIME-NOW.
              05 WS-TIME-NOW-GRP-HH    PIC 99.
              05 WS-TIME-NOW-GRP-MM    PIC 99.
              05 WS-TIME-NOW-GRP-SS    PIC 99.
       01 WS-ABEND-PGM                 PIC X(8) VALUE 'ABNDPROC'.
       01 ABNDINFO-REC.
           COPY ABNDINFO.
       LINKAGE SECTION.
       01 DFHCOMMAREA.
           COPY UPDCUST.
       PROCEDURE DIVISION.
       PREMIERE SECTION.
       A010.
           MOVE SORTCODE TO COMM-SCODE
                            DESIRED-SORT-CODE.
           MOVE SPACES TO WS-UNSTR-TITLE.
           UNSTRING COMM-NAME DELIMITED BY SPACE
              INTO WS-UNSTR-TITLE.
           MOVE ' ' TO WS-TITLE-VALID.
           EVALUATE WS-UNSTR-TITLE
              WHEN 'Professor'
                 MOVE 'Y' TO WS-TITLE-VALID
              WHEN 'Mr       '
                 MOVE 'Y' TO WS-TITLE-VALID
              WHEN 'Mrs      '
                 MOVE 'Y' TO WS-TITLE-VALID
              WHEN 'Miss     '
                 MOVE 'Y' TO WS-TITLE-VALID
              WHEN 'Ms       '
                 MOVE 'Y' TO WS-TITLE-VALID
              WHEN 'Dr       '
                 MOVE 'Y' TO WS-TITLE-VALID
              WHEN 'Drs      '
                 MOVE 'Y' TO WS-TITLE-VALID
              WHEN 'Lord     '
                 MOVE 'Y' TO WS-TITLE-VALID
              WHEN 'Sir      '
                 MOVE 'Y' TO WS-TITLE-VALID
              WHEN 'Lady     '
                 MOVE 'Y' TO WS-TITLE-VALID
              WHEN '         '
                 MOVE 'Y' TO WS-TITLE-VALID
              WHEN OTHER
                 MOVE 'N' TO WS-TITLE-VALID
           END-EVALUATE.
           IF WS-TITLE-VALID = 'N'
             MOVE 'N' TO COMM-UPD-SUCCESS
             MOVE 'T' TO COMM-UPD-FAIL-CD
             GOBACK
           END-IF
           PERFORM UPDATE-CUSTOMER-VSAM
           PERFORM GET-ME-OUT-OF-HERE.
       A999.
           EXIT.
       UPDATE-CUSTOMER-VSAM SECTION.
       UCV010.
           MOVE COMM-CUSTNO TO DESIRED-CUSTNO.
           EXEC CICS READ FILE('CUSTOMER')
                RIDFLD(DESIRED-CUST-KEY)
                INTO(WS-CUST-DATA)
                UPDATE
                RESP(WS-CICS-RESP)
                RESP2(WS-CICS-RESP2)
           END-EXEC.
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
              MOVE 'N' TO COMM-UPD-SUCCESS
              IF WS-CICS-RESP = DFHRESP(NOTFND)
                 MOVE '1' TO COMM-UPD-FAIL-CD
              ELSE
                 MOVE '2' TO COMM-UPD-FAIL-CD
              END-IF
              GO TO UCV999
           END-IF.
           IF (COMM-NAME = SPACES OR COMM-NAME(1:1) = ' ') AND
           (COMM-ADDR = SPACES OR COMM-ADDR(1:1) = ' ')
              MOVE 'N' TO COMM-UPD-SUCCESS
              MOVE '4' TO COMM-UPD-FAIL-CD
              GO TO UCV999
           END-IF.
           IF (COMM-NAME = SPACES OR COMM-NAME(1:1) = ' ') AND
           (COMM-ADDR NOT = SPACES OR COMM-ADDR(1:1) NOT = ' ')
              MOVE COMM-ADDR TO CUSTOMER-ADDRESS OF WS-CUST-DATA
           END-IF.
           IF (COMM-ADDR = SPACES OR COMM-ADDR(1:1) = ' ') AND
           (COMM-NAME NOT = SPACES OR COMM-NAME(1:1) NOT = ' ')
              MOVE COMM-NAME TO CUSTOMER-NAME OF WS-CUST-DATA
           END-IF.
           IF COMM-ADDR(1:1) NOT = ' ' AND COMM-NAME(1:1) NOT = ' '
              MOVE COMM-ADDR TO CUSTOMER-ADDRESS OF WS-CUST-DATA
              MOVE COMM-NAME TO CUSTOMER-NAME OF WS-CUST-DATA
           END-IF.
           COMPUTE WS-CUST-REC-LEN = LENGTH OF WS-CUST-DATA.
           EXEC CICS REWRITE
              FILE ('CUSTOMER')
              FROM (WS-CUST-DATA)
              LENGTH(WS-CUST-REC-LEN)
              RESP(WS-CICS-RESP)
              RESP2(WS-CICS-RESP2)
           END-EXEC.
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
              MOVE 'N' TO COMM-UPD-SUCCESS
              MOVE '3' TO COMM-UPD-FAIL-CD
              GO TO UCV999
           END-IF.
           MOVE CUSTOMER-EYECATCHER OF WS-CUST-DATA
              TO COMM-EYE.
           MOVE CUSTOMER-SORTCODE OF WS-CUST-DATA
              TO COMM-SCODE.
           MOVE CUSTOMER-NUMBER OF WS-CUST-DATA
              TO COMM-CUSTNO.
           MOVE CUSTOMER-NAME OF WS-CUST-DATA
              TO COMM-NAME.
           MOVE CUSTOMER-ADDRESS OF WS-CUST-DATA
              TO COMM-ADDR.
           MOVE CUSTOMER-DATE-OF-BIRTH OF WS-CUST-DATA
              TO COMM-DOB.
           MOVE CUSTOMER-CREDIT-SCORE OF WS-CUST-DATA
              TO COMM-CREDIT-SCORE.
           MOVE CUSTOMER-CS-REVIEW-DATE OF WS-CUST-DATA
              TO COMM-CS-REVIEW-DATE.
           MOVE 'Y' TO COMM-UPD-SUCCESS.
       UCV999.
           EXIT.
       GET-ME-OUT-OF-HERE SECTION.
       GMOOH010.
           EXEC CICS RETURN
           END-EXEC.
       GMOOH999.
           EXIT.
       POPULATE-TIME-DATE SECTION.
       PTD010.
           EXEC CICS ASKTIME
              ABSTIME(WS-U-TIME)
           END-EXEC.
           EXEC CICS FORMATTIME
                     ABSTIME(WS-U-TIME)
                     DDMMYYYY(WS-ORIG-DATE)
                     TIME(WS-TIME-NOW)
                     DATESEP
           END-EXEC.
       PTD999.
           EXIT.
