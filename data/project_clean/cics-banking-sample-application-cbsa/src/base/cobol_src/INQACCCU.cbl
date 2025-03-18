       CBL CICS('SP,EDF,DLI')
       CBL SQL
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INQACCCU.
       AUTHOR. James O'Grady.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-370.
       OBJECT-COMPUTER.  IBM-370.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 SORTCODE           PIC 9(6) VALUE 987654.
           EXEC SQL
              INCLUDE ACCDB2
           END-EXEC.
       01 HOST-ACCOUNT-ROW.
          03 HV-ACCOUNT-EYECATCHER        PIC X(4).
          03 HV-ACCOUNT-CUST-NO           PIC X(10).
          03 HV-ACCOUNT-SORTCODE          PIC X(6).
          03 HV-ACCOUNT-ACC-NO            PIC X(8).
          03 HV-ACCOUNT-ACC-TYPE          PIC X(8).
          03 HV-ACCOUNT-INT-RATE          PIC S9(4)V99 COMP-3.
          03 HV-ACCOUNT-OPENED            PIC X(10).
          03 HV-ACCOUNT-OVERDRAFT-LIM     PIC S9(9) COMP.
          03 HV-ACCOUNT-LAST-STMT         PIC X(10).
          03 HV-ACCOUNT-NEXT-STMT         PIC X(10).
          03 HV-ACCOUNT-AVAIL-BAL         PIC S9(10)V99 COMP-3.
          03 HV-ACCOUNT-ACTUAL-BAL        PIC S9(10)V99 COMP-3.
       01 EIBRCODE-NICE.
          03 EIBRCODE-FIRST               PIC X.
          03 EIBRCODE-SECOND              PIC X.
          03 EIBRCODE-THIRD               PIC X.
          03 EIBRCODE-FOURTH              PIC X.
          03 EIBRCODE-FIFTH               PIC X.
          03 EIBRCODE-SIXTH               PIC X.
       EXEC SQL
          INCLUDE SQLCA
       END-EXEC.
           EXEC SQL DECLARE ACC-CURSOR CURSOR FOR
              SELECT ACCOUNT_EYECATCHER,
                     ACCOUNT_CUSTOMER_NUMBER,
                     ACCOUNT_SORTCODE,
                     ACCOUNT_NUMBER,
                     ACCOUNT_TYPE,
                     ACCOUNT_INTEREST_RATE,
                     ACCOUNT_OPENED,
                     ACCOUNT_OVERDRAFT_LIMIT,
                     ACCOUNT_LAST_STATEMENT,
                     ACCOUNT_NEXT_STATEMENT,
                     ACCOUNT_AVAILABLE_BALANCE,
                     ACCOUNT_ACTUAL_BALANCE
                     FROM ACCOUNT
                     WHERE ACCOUNT_CUSTOMER_NUMBER =
                        :HV-ACCOUNT-CUST-NO
                      AND ACCOUNT_SORTCODE =
                      :HV-ACCOUNT-SORTCODE
                     FOR FETCH ONLY
           END-EXEC.
       01 WS-CICS-WORK-AREA.
          03 WS-CICS-RESP                 PIC S9(8) COMP.
          03 WS-CICS-RESP2                PIC S9(8) COMP.
          03 WS-CICS-RESP-DISPLAY         PIC +9(8) DISPLAY.
       01 EXIT-BROWSE-LOOP                PIC X VALUE 'N'.
       01 OUTPUT-DATA.
              03 ACCOUNT-DATA.
                 05 ACCOUNT-EYE-CATCHER        PIC X(4).
                 88 ACCOUNT-EYECATCHER-VALUE        VALUE 'ACCT'.
                 05 ACCOUNT-CUST-NO            PIC 9(10).
                 05 ACCOUNT-KEY.
                    07 ACCOUNT-SORT-CODE       PIC 9(6).
                    07 ACCOUNT-NUMBER          PIC 9(8).
                 05 ACCOUNT-TYPE               PIC X(8).
                 05 ACCOUNT-INTEREST-RATE      PIC 9(4)V99.
                 05 ACCOUNT-OPENED             PIC 9(8).
                 05 ACCOUNT-OPENED-GROUP REDEFINES ACCOUNT-OPENED.
                    07 ACCOUNT-OPENED-DAY       PIC 99.
                    07 ACCOUNT-OPENED-MONTH     PIC 99.
                    07 ACCOUNT-OPENED-YEAR      PIC 9999.
                 05 ACCOUNT-OVERDRAFT-LIMIT    PIC 9(8).
                 05 ACCOUNT-LAST-STMT-DATE     PIC 9(8).
                 05 ACCOUNT-LAST-STMT-GROUP
                    REDEFINES ACCOUNT-LAST-STMT-DATE.
                    07 ACCOUNT-LAST-STMT-DAY   PIC 99.
                    07 ACCOUNT-LAST-STMT-MONTH PIC 99.
                    07 ACCOUNT-LAST-STMT-YEAR  PIC 9999.
                 05 ACCOUNT-NEXT-STMT-DATE     PIC 9(8).
                 05 ACCOUNT-NEXT-STMT-GROUP
                   REDEFINES ACCOUNT-NEXT-STMT-DATE.
                    07 ACCOUNT-NEXT-STMT-DAY   PIC 99.
                    07 ACCOUNT-NEXT-STMT-MONTH PIC 99.
                    07 ACCOUNT-NEXT-STMT-YEAR  PIC 9999.
                 05 ACCOUNT-AVAILABLE-BALANCE  PIC S9(10)V99.
                 05 ACCOUNT-ACTUAL-BALANCE     PIC S9(10)V99.
       01 RETURNED-DATA.
           03 RETURNED-EYE-CATCHER        PIC X(4).
           03 RETURNED-CUST-NO            PIC 9(10).
           03 RETURNED-KEY.
              05 RETURNED-SORT-CODE       PIC 9(6).
              05 RETURNED-NUMBER          PIC 9(8).
           03 RETURNED-TYPE               PIC X(8).
           03 RETURNED-INTEREST-RATE      PIC 9(4)V99.
           03 RETURNED-OPENED             PIC 9(8).
           03 RETURNED-OVERDRAFT-LIMIT    PIC 9(8).
           03 RETURNED-LAST-STMT-DATE     PIC 9(8).
           03 RETURNED-NEXT-STMT-DATE     PIC 9(8).
           03 RETURNED-AVAILABLE-BALANCE  PIC S9(10)V99.
           03 RETURNED-ACTUAL-BALANCE     PIC S9(10)V99.
       01 DESIRED-KEY.
           03 DESIRED-KEY-CUSTOMER        PIC 9(10).
           03 DESIRED-KEY-SORTCODE        PIC 9(6).
       01 DB2-DATE-REFORMAT.
          03 DB2-DATE-REF-YR              PIC 9(4).
          03 FILLER                       PIC X.
          03 DB2-DATE-REF-MNTH            PIC 99.
          03 FILLER                       PIC X.
          03 DB2-DATE-REF-DAY             PIC 99.
       01 DATA-STORE-TYPE                 PIC X.
          88 DATASTORE-TYPE-DB2              VALUE '2'.
          88 DATASTORE-TYPE-VSAM             VALUE 'V'.
       01 DB2-EXIT-LOOP                   PIC X.
       01 FETCH-DATA-CNT                  PIC 9(4) COMP.
       01 WS-CUST-ALT-KEY-LEN             PIC S9(4) COMP VALUE +10.
       01 CUSTOMER-KY.
          03 REQUIRED-SORT-CODE           PIC 9(6)  VALUE 0.
          03 REQUIRED-ACC-NUM             PIC 9(8)  VALUE 0.
       01 SQLCODE-DISPLAY                 PIC S9(8) DISPLAY
                                          SIGN LEADING SEPARATE.
       01 MY-ABEND-CODE                   PIC XXXX.
       01 WS-STORM-DRAIN                  PIC X VALUE 'N'.
       01 STORM-DRAIN-CONDITION           PIC X(20).
       01 CUSTOMER-AREA.
           03 CUSTOMER-RECORD.
              05 CUSTOMER-EYECATCHER                 PIC X(4).
                 88 CUSTOMER-EYECATCHER-VALUE        VALUE 'CUST'.
              05 CUSTOMER-KEY.
                 07 CUSTOMER-SORTCODE                PIC 9(6) DISPLAY.
                 07 CUSTOMER-NUMBER                  PIC 9(10) DISPLAY.
              05 CUSTOMER-NAME                       PIC X(60).
              05 CUSTOMER-ADDRESS                    PIC X(160).
              05 CUSTOMER-DATE-OF-BIRTH              PIC 9(8).
              05 CUSTOMER-DOB-GROUP REDEFINES CUSTOMER-DATE-OF-BIRTH.
                 07 CUSTOMER-BIRTH-DAY               PIC 99.
                 07 CUSTOMER-BIRTH-MONTH             PIC 99.
                 07 CUSTOMER-BIRTH-YEAR              PIC 9999.
              05 CUSTOMER-CREDIT-SCORE               PIC 999.
              05 CUSTOMER-CS-REVIEW-DATE             PIC 9(8).
              05 CUSTOMER-CS-GROUP
                 REDEFINES CUSTOMER-CS-REVIEW-DATE.
                 07 CUSTOMER-CS-REVIEW-DAY           PIC 99.
                 07 CUSTOMER-CS-REVIEW-MONTH         PIC 99.
                 07 CUSTOMER-CS-REVIEW-YEAR          PIC 9999.
       01 INQCUST-COMMAREA.
          03 INQCUST-EYE                  PIC X(4).
          03 INQCUST-SCODE                PIC X(6).
          03 INQCUST-CUSTNO               PIC 9(10).
          03 INQCUST-NAME                 PIC X(60).
          03 INQCUST-ADDR                 PIC X(160).
          03 INQCUST-DOB.
            05 INQCUST-DOB-DD             PIC 99.
            05 INQCUST-DOB-MM             PIC 99.
            05 INQCUST-DOB-YYYY           PIC 9999.
          03 INQCUST-CREDIT-SCORE         PIC 999.
          03 INQCUST-CS-REVIEW-DT.
            05 INQCUST-CS-REVIEW-DD       PIC 99.
            05 INQCUST-CS-REVIEW-MM       PIC 99.
            05 INQCUST-CS-REVIEW-YYYY     PIC 9999.
          03 INQCUST-INQ-SUCCESS          PIC X.
          03 INQCUST-INQ-FAIL-CD          PIC X.
          03 INQCUST-PCB-POINTER          POINTER.
       01 WS-U-TIME                       PIC S9(15) COMP-3.
       01 WS-ORIG-DATE                    PIC X(10).
       01 WS-ORIG-DATE-GRP REDEFINES WS-ORIG-DATE.
          03 WS-ORIG-DATE-DD                 PIC 99.
          03 FILLER                          PIC X.
          03 WS-ORIG-DATE-MM                 PIC 99.
          03 FILLER                          PIC X.
          03 WS-ORIG-DATE-YYYY               PIC 9999.
       01 WS-ORIG-DATE-GRP-X.
          03 WS-ORIG-DATE-DD-X            PIC XX.
          03 FILLER                       PIC X VALUE '.'.
          03 WS-ORIG-DATE-MM-X            PIC XX.
          03 FILLER                       PIC X VALUE '.'.
          03 WS-ORIG-DATE-YYYY-X          PIC X(4).
       01 WS-TIME-DATA.
           03 WS-TIME-NOW                 PIC 9(6).
           03 WS-TIME-NOW-GRP REDEFINES WS-TIME-NOW.
              05 WS-TIME-NOW-GRP-HH          PIC 99.
              05 WS-TIME-NOW-GRP-MM          PIC 99.
              05 WS-TIME-NOW-GRP-SS          PIC 99.
       01 WS-ABEND-PGM                       PIC X(8) VALUE 'ABNDPROC'.
       01 ABNDINFO-REC.
           03 ABND-VSAM-KEY.
              05 ABND-UTIME-KEY                  PIC S9(15) COMP-3.
              05 ABND-TASKNO-KEY                 PIC 9(4).
           03 ABND-APPLID                        PIC X(8).
           03 ABND-TRANID                        PIC X(4).
           03 ABND-DATE                          PIC X(10).
           03 ABND-TIME                          PIC X(8).
           03 ABND-CODE                          PIC X(4).
           03 ABND-PROGRAM                       PIC X(8).
           03 ABND-RESPCODE                      PIC S9(8) DISPLAY
                  SIGN LEADING SEPARATE.
           03 ABND-RESP2CODE                     PIC S9(8) DISPLAY
                  SIGN LEADING SEPARATE.
           03 ABND-SQLCODE                       PIC S9(8) DISPLAY
                  SIGN LEADING SEPARATE.
           03 ABND-FREEFORM                      PIC X(600).
       LINKAGE SECTION.
       01 DFHCOMMAREA.
          03 NUMBER-OF-ACCOUNTS        PIC S9(8) BINARY.
          03 CUSTOMER-NUMBER           PIC 9(10).
          03 COMM-SUCCESS              PIC X.
          03 COMM-FAIL-CODE            PIC X.
          03 CUSTOMER-FOUND            PIC X.
          03 COMM-PCB-POINTER          POINTER.
          03 ACCOUNT-DETAILS OCCURS 1 TO 20 DEPENDING ON
              NUMBER-OF-ACCOUNTS.
            05 COMM-EYE                  PIC X(4).
            05 COMM-CUSTNO               PIC X(10).
            05 COMM-SCODE                PIC X(6).
            05 COMM-ACCNO                PIC 9(8).
            05 COMM-ACC-TYPE             PIC X(8).
            05 COMM-INT-RATE             PIC 9(4)V99.
            05 COMM-OPENED               PIC 9(8).
            05 COMM-OPENED-GROUP REDEFINES COMM-OPENED.
              07 COMM-OPENED-DAY PIC 99.
              07 COMM-OPENED-MONTH PIC 99.
              07 COMM-OPENED-YEAR PIC 9999.
            05 COMM-OVERDRAFT            PIC 9(8).
            05 COMM-LAST-STMT-DT         PIC 9(8).
            05 COMM-LAST-STMT-GROUP REDEFINES COMM-LAST-STMT-DT.
              07 COMM-LAST-STMT-DAY PIC 99.
              07 COMM-LAST-STMT-MONTH PIC 99.
              07 COMM-LAST-STMT-YEAR PIC 9999.
            05 COMM-NEXT-STMT-DT         PIC 9(8).
            05 COMM-NEXT-STMT-GROUP REDEFINES COMM-NEXT-STMT-DT.
              07 COMM-NEXT-STMT-DAY PIC 99.
              07 COMM-NEXT-STMT-MONTH PIC 99.
              07 COMM-NEXT-STMT-YEAR PIC 9999.
            05 COMM-AVAIL-BAL            PIC S9(10)V99.
            05 COMM-ACTUAL-BAL           PIC S9(10)V99.
       PROCEDURE DIVISION USING DFHCOMMAREA.
       PREMIERE SECTION.
       A010.
           MOVE 'N' TO COMM-SUCCESS
           MOVE '0' TO COMM-FAIL-CODE
           EXEC CICS HANDLE ABEND
              LABEL(ABEND-HANDLING)
           END-EXEC.
           MOVE SORTCODE TO REQUIRED-SORT-CODE OF CUSTOMER-KY.
           PERFORM CUSTOMER-CHECK.
           IF CUSTOMER-FOUND = 'N'
              MOVE 'N' TO COMM-SUCCESS
              MOVE '1' TO COMM-FAIL-CODE
              PERFORM GET-ME-OUT-OF-HERE
           END-IF
           PERFORM READ-ACCOUNT-DB2
           PERFORM GET-ME-OUT-OF-HERE.
       A999.
           EXIT.
       READ-ACCOUNT-DB2 SECTION.
       RAD010.
           MOVE CUSTOMER-NUMBER IN DFHCOMMAREA TO HV-ACCOUNT-CUST-NO.
           MOVE  SORTCODE TO HV-ACCOUNT-SORTCODE.
           EXEC SQL OPEN
              ACC-CURSOR
           END-EXEC.
           MOVE SQLCODE TO SQLCODE-DISPLAY.
           IF SQLCODE NOT = 0
              MOVE SQLCODE TO SQLCODE-DISPLAY
              PERFORM CHECK-FOR-STORM-DRAIN-DB2
              MOVE 'N'  TO COMM-SUCCESS
              MOVE 'N'  TO CUSTOMER-FOUND
              MOVE '2'  TO COMM-FAIL-CODE
              MOVE ZERO TO NUMBER-OF-ACCOUNTS
              EXEC CICS SYNCPOINT ROLLBACK
                RESP(WS-CICS-RESP)
                RESP2(WS-CICS-RESP2)
              END-EXEC
              IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
                 INITIALIZE ABNDINFO-REC
                 MOVE EIBRESP    TO ABND-RESPCODE
                 MOVE EIBRESP2   TO ABND-RESP2CODE
                 EXEC CICS ASSIGN APPLID(ABND-APPLID)
                 END-EXEC
                 MOVE EIBTASKN   TO ABND-TASKNO-KEY
                 MOVE EIBTRNID   TO ABND-TRANID
                 PERFORM POPULATE-TIME-DATE
                 MOVE WS-ORIG-DATE TO ABND-DATE
                 STRING WS-TIME-NOW-GRP-HH DELIMITED BY SIZE,
                       ':' DELIMITED BY SIZE,
                        WS-TIME-NOW-GRP-MM DELIMITED BY SIZE,
                        ':' DELIMITED BY SIZE,
                        WS-TIME-NOW-GRP-MM DELIMITED BY SIZE
                        INTO ABND-TIME
                 END-STRING
                 MOVE WS-U-TIME   TO ABND-UTIME-KEY
                 MOVE 'HROL'      TO ABND-CODE
                 EXEC CICS ASSIGN PROGRAM(ABND-PROGRAM)
                 END-EXEC
                 MOVE ZEROS      TO ABND-SQLCODE
                 STRING 'RAD010 - Unable to perform Synpoint Rollback'
                       DELIMITED BY SIZE,
                       '. Possible Integrity issue following DB2 '
                       DELIMITED BY SIZE,
                       'CURSOR OPEN' DELIMITED BY SIZE,
                       'EIBRESP=' DELIMITED BY SIZE,
                       ABND-RESPCODE DELIMITED BY SIZE,
                       ' RESP2=' DELIMITED BY SIZE,
                       ABND-RESP2CODE DELIMITED BY SIZE
                       INTO ABND-FREEFORM
                 END-STRING
                 EXEC CICS LINK PROGRAM(WS-ABEND-PGM)
                           COMMAREA(ABNDINFO-REC)
                 END-EXEC
                 DISPLAY 'INQACCCU: Unable to perform Synpoint Rollback'
                 '. Possible Integrity issue following DB2 CURSOR OPEN'
                 ' SQLCODE=' SQLCODE-DISPLAY
                 ',RESP=' WS-CICS-RESP
                 ',RESP2=' WS-CICS-RESP2
                 EXEC CICS ABEND
                    ABCODE ('HROL')
                    CANCEL
                 END-EXEC
              END-IF
              GO TO RAD999
           END-IF.
           PERFORM FETCH-DATA.
           EXEC SQL CLOSE
                          ACC-CURSOR
           END-EXEC.
           IF SQLCODE NOT = 0
              MOVE SQLCODE TO SQLCODE-DISPLAY
              DISPLAY 'Failure when attempting to close the DB2 CURSOR'
                  ' ACC-CURSOR. With SQL code='
                  SQLCODE-DISPLAY
              PERFORM CHECK-FOR-STORM-DRAIN-DB2
              MOVE 'N' TO COMM-SUCCESS
              MOVE 'N' TO CUSTOMER-FOUND
              MOVE '4' TO COMM-FAIL-CODE
              EXEC CICS SYNCPOINT
                 ROLLBACK
                 RESP(WS-CICS-RESP)
                 RESP2(WS-CICS-RESP2)
              END-EXEC
              IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
                 INITIALIZE ABNDINFO-REC
                 MOVE EIBRESP    TO ABND-RESPCODE
                 MOVE EIBRESP2   TO ABND-RESP2CODE
                 EXEC CICS ASSIGN APPLID(ABND-APPLID)
                 END-EXEC
                 MOVE EIBTASKN   TO ABND-TASKNO-KEY
                 MOVE EIBTRNID   TO ABND-TRANID
                 PERFORM POPULATE-TIME-DATE
                 MOVE WS-ORIG-DATE TO ABND-DATE
                 STRING WS-TIME-NOW-GRP-HH DELIMITED BY SIZE,
                       ':' DELIMITED BY SIZE,
                        WS-TIME-NOW-GRP-MM DELIMITED BY SIZE,
                        ':' DELIMITED BY SIZE,
                        WS-TIME-NOW-GRP-MM DELIMITED BY SIZE
                        INTO ABND-TIME
                 END-STRING
                 MOVE WS-U-TIME   TO ABND-UTIME-KEY
                 MOVE 'HROL'      TO ABND-CODE
                 EXEC CICS ASSIGN PROGRAM(ABND-PROGRAM)
                 END-EXEC
                 MOVE ZEROS      TO ABND-SQLCODE
                 STRING 'RAD010(2)- Unable to perform Syncpoint'
                       DELIMITED BY SIZE,
                       'Rollback. Possible Integrity issue following'
                       DELIMITED BY SIZE,
                       'CURSOR CLOSE' DELIMITED BY SIZE,
                       'EIBRESP=' DELIMITED BY SIZE,
                       ABND-RESPCODE DELIMITED BY SIZE,
                       ' RESP2=' DELIMITED BY SIZE,
                       ABND-RESP2CODE DELIMITED BY SIZE
                       INTO ABND-FREEFORM
                 END-STRING
                 EXEC CICS LINK PROGRAM(WS-ABEND-PGM)
                           COMMAREA(ABNDINFO-REC)
                 END-EXEC
                 DISPLAY 'INQACCCU:Unable to perform Synpoint ROLLBACK'
                         '. Possible Integrity issue following '
                         'DB2 CURSOR OPEN SQLCODE=' SQLCODE-DISPLAY
                         ',RESP=' WS-CICS-RESP
                         ',RESP2=' WS-CICS-RESP2
                 EXEC CICS ABEND
                    ABCODE ('HROL')
                    CANCEL
                 END-EXEC
              END-IF
              GO TO RAD999
           END-IF.
           MOVE 'Y' TO COMM-SUCCESS.
       RAD999.
           EXIT.
       FETCH-DATA SECTION.
       FD010.
           MOVE ZERO TO NUMBER-OF-ACCOUNTS.
           PERFORM UNTIL SQLCODE NOT = 0 OR
           NUMBER-OF-ACCOUNTS = 20
              EXEC SQL FETCH FROM ACC-CURSOR
              INTO :HV-ACCOUNT-EYECATCHER,
                   :HV-ACCOUNT-CUST-NO,
                   :HV-ACCOUNT-SORTCODE,
                   :HV-ACCOUNT-ACC-NO,
                   :HV-ACCOUNT-ACC-TYPE,
                   :HV-ACCOUNT-INT-RATE,
                   :HV-ACCOUNT-OPENED,
                   :HV-ACCOUNT-OVERDRAFT-LIM,
                   :HV-ACCOUNT-LAST-STMT,
                   :HV-ACCOUNT-NEXT-STMT,
                   :HV-ACCOUNT-AVAIL-BAL,
                   :HV-ACCOUNT-ACTUAL-BAL
              END-EXEC
              IF SQLCODE = +100
                  MOVE 'Y' TO COMM-SUCCESS
                  GO TO FD999
              END-IF
              IF SQLCODE NOT = 0
                 MOVE SQLCODE TO SQLCODE-DISPLAY
                 DISPLAY 'Failure when attempting to FETCH from the'
                    ' DB2 CURSOR ACC-CURSOR. With SQL code='
                    SQLCODE-DISPLAY
                 PERFORM CHECK-FOR-STORM-DRAIN-DB2
                 MOVE 'N'  TO COMM-SUCCESS
                 MOVE 'N'  TO CUSTOMER-FOUND
                 MOVE ZERO TO NUMBER-OF-ACCOUNTS
                 MOVE '3' TO COMM-FAIL-CODE
                 EXEC CICS SYNCPOINT
                    ROLLBACK
                    RESP(WS-CICS-RESP)
                    RESP2(WS-CICS-RESP2)
                 END-EXEC
                 IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
                    INITIALIZE ABNDINFO-REC
                    MOVE EIBRESP    TO ABND-RESPCODE
                    MOVE EIBRESP2   TO ABND-RESP2CODE
                    EXEC CICS ASSIGN APPLID(ABND-APPLID)
                    END-EXEC
                    MOVE EIBTASKN   TO ABND-TASKNO-KEY
                    MOVE EIBTRNID   TO ABND-TRANID
                    PERFORM POPULATE-TIME-DATE
                    MOVE WS-ORIG-DATE TO ABND-DATE
                    STRING WS-TIME-NOW-GRP-HH DELIMITED BY SIZE,
                       ':' DELIMITED BY SIZE,
                       WS-TIME-NOW-GRP-MM DELIMITED BY SIZE,
                       ':' DELIMITED BY SIZE,
                       WS-TIME-NOW-GRP-MM DELIMITED BY SIZE
                       INTO ABND-TIME
                    END-STRING
                    MOVE WS-U-TIME   TO ABND-UTIME-KEY
                    MOVE 'HROL'      TO ABND-CODE
                    EXEC CICS ASSIGN PROGRAM(ABND-PROGRAM)
                    END-EXEC
                    MOVE ZEROS      TO ABND-SQLCODE
                    STRING 'FD010 - Unable to perform Syncpoint'
                       DELIMITED BY SIZE,
                       'Rollback. Possible Integrity issue following'
                       DELIMITED BY SIZE,
                       'DB2 FETCH' DELIMITED BY SIZE,
                       'EIBRESP=' DELIMITED BY SIZE,
                       ABND-RESPCODE DELIMITED BY SIZE,
                       ' RESP2=' DELIMITED BY SIZE,
                       ABND-RESP2CODE DELIMITED BY SIZE
                       INTO ABND-FREEFORM
                    END-STRING
                    EXEC CICS LINK PROGRAM(WS-ABEND-PGM)
                           COMMAREA(ABNDINFO-REC)
                    END-EXEC
                    DISPLAY 'INQACCCU: Unable to perform Syncpoint '
                       'ROLLBACK. Possible Integrity issue following'
                       ' DB2 FETCH. SQLCODE=' SQLCODE-DISPLAY
                       ',RESP=' WS-CICS-RESP
                       ',RESP2=' WS-CICS-RESP2
                    EXEC CICS ABEND
                       ABCODE ('HROL')
                       CANCEL
                    END-EXEC
                 END-IF
                 GO TO FD999
              END-IF
              ADD 1 TO NUMBER-OF-ACCOUNTS GIVING NUMBER-OF-ACCOUNTS
              MOVE HV-ACCOUNT-EYECATCHER
                 TO COMM-EYE(NUMBER-OF-ACCOUNTS)
              MOVE HV-ACCOUNT-CUST-NO
                  TO COMM-CUSTNO(NUMBER-OF-ACCOUNTS)
              MOVE HV-ACCOUNT-SORTCODE
                  TO COMM-SCODE(NUMBER-OF-ACCOUNTS)
              MOVE HV-ACCOUNT-ACC-NO
                  TO COMM-ACCNO(NUMBER-OF-ACCOUNTS)
              MOVE HV-ACCOUNT-ACC-TYPE
                  TO COMM-ACC-TYPE(NUMBER-OF-ACCOUNTS)
              MOVE HV-ACCOUNT-INT-RATE
                  TO COMM-INT-RATE(NUMBER-OF-ACCOUNTS)
              MOVE HV-ACCOUNT-OPENED TO DB2-DATE-REFORMAT
              STRING DB2-DATE-REF-DAY
                DB2-DATE-REF-MNTH
                DB2-DATE-REF-YR
                DELIMITED BY SIZE
                INTO COMM-OPENED(NUMBER-OF-ACCOUNTS)
              END-STRING
              MOVE HV-ACCOUNT-OVERDRAFT-LIM
                 TO COMM-OVERDRAFT(NUMBER-OF-ACCOUNTS)
              MOVE HV-ACCOUNT-LAST-STMT TO DB2-DATE-REFORMAT
              STRING DB2-DATE-REF-DAY
                 DB2-DATE-REF-MNTH
                 DB2-DATE-REF-YR
                 DELIMITED BY SIZE
                 INTO COMM-LAST-STMT-DT(NUMBER-OF-ACCOUNTS)
              END-STRING
              MOVE HV-ACCOUNT-NEXT-STMT TO DB2-DATE-REFORMAT
              STRING DB2-DATE-REF-DAY
                 DB2-DATE-REF-MNTH
                 DB2-DATE-REF-YR
                 DELIMITED BY SIZE
                 INTO COMM-NEXT-STMT-DT(NUMBER-OF-ACCOUNTS)
              END-STRING
              MOVE HV-ACCOUNT-ACTUAL-BAL
                 TO COMM-ACTUAL-BAL(NUMBER-OF-ACCOUNTS)
              MOVE HV-ACCOUNT-AVAIL-BAL
                 TO COMM-AVAIL-BAL(NUMBER-OF-ACCOUNTS)
           END-PERFORM.
       FD999.
           EXIT.
       GET-ME-OUT-OF-HERE SECTION.
       GMOFH010.
           EXEC CICS RETURN
           END-EXEC.
           GOBACK.
       GMOFH999.
           EXIT.
       CHECK-FOR-STORM-DRAIN-DB2 SECTION.
       CFSDD010.
           EVALUATE SQLCODE
              WHEN 923
                 MOVE 'DB2 Connection lost ' TO STORM-DRAIN-CONDITION
              WHEN OTHER
                 MOVE 'Not Storm Drain     ' TO STORM-DRAIN-CONDITION
           END-EVALUATE.
           IF STORM-DRAIN-CONDITION NOT EQUAL 'Not Storm Drain     '
              DISPLAY 'INQACCCU: Check-For-Storm-Drain-DB2: Storm '
                      'Drain condition (' STORM-DRAIN-CONDITION ') '
                      'has been met (' SQLCODE-DISPLAY ').'
           ELSE
              CONTINUE
           END-IF.
       CFSDD999.
           EXIT.
       ABEND-HANDLING SECTION.
       AH010.
           EXEC CICS ASSIGN
              ABCODE(MY-ABEND-CODE)
           END-EXEC.
           EVALUATE MY-ABEND-CODE
              WHEN 'AD2Z'
                 MOVE SQLCODE TO SQLCODE-DISPLAY
                 DISPLAY 'DB2 DEADLOCK DETECTED IN INQACCCU, SQLCODE='
                    SQLCODE-DISPLAY
                 DISPLAY 'DB2 DEADLOCK FOR ACCOUNT '
                    HV-ACCOUNT-ACC-NO
                 DISPLAY  'SQLSTATE=' SQLSTATE
                          ',SQLERRMC=' sqlerrmc(1:sqlerrmL)
                          ',sqlerrd(1)=' sqlerrd(1)
                          ',sqlerrd(2)=' sqlerrd(2)
                          ',sqlerrd(3)=' sqlerrd(3)
                          ',sqlerrd(4)=' sqlerrd(4)
                          ',sqlerrd(5)=' sqlerrd(5)
                          ',sqlerrd(6)=' sqlerrd(6)
              WHEN 'AFCR'
              WHEN 'AFCS'
              WHEN 'AFCT'
                 MOVE 'Y' TO WS-STORM-DRAIN
                 DISPLAY 'INQACCCU: Check-For-Storm-Drain-VSAM: Storm '
                        'Drain condition (Abend ' MY-ABEND-CODE ') '
                        'has been met.'
                 EXEC CICS SYNCPOINT ROLLBACK
                    RESP(WS-CICS-RESP)
                    RESP2(WS-CICS-RESP2)
                 END-EXEC
                 IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
                    INITIALIZE ABNDINFO-REC
                    MOVE EIBRESP    TO ABND-RESPCODE
                    MOVE EIBRESP2   TO ABND-RESP2CODE
                    EXEC CICS ASSIGN APPLID(ABND-APPLID)
                    END-EXEC
                    MOVE EIBTASKN   TO ABND-TASKNO-KEY
                    MOVE EIBTRNID   TO ABND-TRANID
                    PERFORM POPULATE-TIME-DATE
                    MOVE WS-ORIG-DATE TO ABND-DATE
                    STRING WS-TIME-NOW-GRP-HH DELIMITED BY SIZE,
                          ':' DELIMITED BY SIZE,
                           WS-TIME-NOW-GRP-MM DELIMITED BY SIZE,
                          ':' DELIMITED BY SIZE,
                          WS-TIME-NOW-GRP-MM DELIMITED BY SIZE
                          INTO ABND-TIME
                    END-STRING
                    MOVE WS-U-TIME   TO ABND-UTIME-KEY
                    MOVE 'HROL'      TO ABND-CODE
                    EXEC CICS ASSIGN PROGRAM(ABND-PROGRAM)
                    END-EXEC
                    MOVE 0 TO ABND-SQLCODE
                    STRING 'AH010 -Unable to perform SYNCPOINT '
                       DELIMITED BY SIZE,
                       'ROLLBACK' DELIMITED BY SIZE,
                       ' Possible integrity issue following VSAM RLS '
                       DELIMITED BY SIZE,
                       ' abend.' DELIMITED BY SIZE,
                       'EIBRESP=' DELIMITED BY SIZE,
                       ABND-RESPCODE DELIMITED BY SIZE,
                       ' RESP2=' DELIMITED BY SIZE,
                       ABND-RESP2CODE DELIMITED BY SIZE
                       INTO ABND-FREEFORM
                    END-STRING
                    EXEC CICS LINK PROGRAM(WS-ABEND-PGM)
                           COMMAREA(ABNDINFO-REC)
                    END-EXEC
                    DISPLAY 'INQACCC: Unable to perform Syncpoint.'
                       ' Rollback. Possible Integrity issue following '
                       ' VSAM RLS abend RESP CODE=' WS-CICS-RESP
                       ' RESP2 CODE=' WS-CICS-RESP2
                    EXEC CICS ABEND
                       ABCODE ('HROL')
                       CANCEL
                    END-EXEC
                 END-IF
                 MOVE 'N' TO COMM-SUCCESS
                 EXEC CICS RETURN
                 END-EXEC
           END-EVALUATE.
           IF WS-STORM-DRAIN = 'N'
              EXEC CICS ABEND
                 ABCODE( MY-ABEND-CODE)
                 NODUMP
                 CANCEL
              END-EXEC
           END-IF.
       AH999.
           EXIT.
       CUSTOMER-CHECK SECTION.
       CC010.
           IF CUSTOMER-NUMBER IN DFHCOMMAREA = ZERO
              MOVE 'N' TO CUSTOMER-FOUND
              MOVE ZERO TO NUMBER-OF-ACCOUNTS
              GO TO CC999
           END-IF.
           IF CUSTOMER-NUMBER IN DFHCOMMAREA = '9999999999'
              MOVE 'N' TO CUSTOMER-FOUND
              MOVE ZERO TO NUMBER-OF-ACCOUNTS
              GO TO CC999
           END-IF.
           INITIALIZE INQCUST-COMMAREA.
           MOVE CUSTOMER-NUMBER IN DFHCOMMAREA TO INQCUST-CUSTNO.
           EXEC CICS LINK PROGRAM('INQCUST ')
              COMMAREA(INQCUST-COMMAREA)
           END-EXEC.
           IF INQCUST-INQ-SUCCESS = 'Y'
              MOVE 'Y' TO CUSTOMER-FOUND
           ELSE
              MOVE 'N' TO CUSTOMER-FOUND
              MOVE ZERO TO NUMBER-OF-ACCOUNTS
           END-IF.
       CC999.
           EXIT.
       POPULATE-TIME-DATE SECTION.
       PTD010.
           DISPLAY 'POPULATE-TIME-DATE SECTION'.
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