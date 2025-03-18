       CBL CICS('SP,EDF,DLI')
       CBL SQL
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INQACC.
       AUTHOR. Jon Collett.
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
          03 HV-ACCOUNT-EYECATCHER     PIC X(4).
          03 HV-ACCOUNT-CUST-NO        PIC X(10).
          03 HV-ACCOUNT-SORTCODE       PIC X(6).
          03 HV-ACCOUNT-ACC-NO         PIC X(8).
          03 HV-ACCOUNT-ACC-TYPE       PIC X(8).
          03 HV-ACCOUNT-INT-RATE       PIC S9(4)V99 COMP-3.
          03 HV-ACCOUNT-OPENED         PIC X(10).
          03 HV-ACCOUNT-OVERDRAFT-LIM  PIC S9(9) COMP.
          03 HV-ACCOUNT-LAST-STMT      PIC X(10).
          03 HV-ACCOUNT-NEXT-STMT      PIC X(10).
          03 HV-ACCOUNT-AVAIL-BAL      PIC S9(10)V99 COMP-3.
          03 HV-ACCOUNT-ACTUAL-BAL     PIC S9(10)V99 COMP-3.
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
                     WHERE ACCOUNT_SORTCODE =
                        :HV-ACCOUNT-SORTCODE AND
                        ACCOUNT_NUMBER =
                        :HV-ACCOUNT-ACC-NO
                     FOR FETCH ONLY
           END-EXEC.
       01 WS-CICS-WORK-AREA.
          05 WS-CICS-RESP              PIC S9(8) COMP.
          05 WS-CICS-RESP2             PIC S9(8) COMP.
       01 EXIT-BROWSE-LOOP             PIC X VALUE 'N'.
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
           03 RETURNED-EYE-CATCHER     PIC X(4).
           03 RETURNED-CUST-NO         PIC 9(10).
           03 RETURNED-KEY.
              05 RETURNED-SORT-CODE       PIC 9(6).
              05 RETURNED-NUMBER          PIC 9(8).
           03 RETURNED-TYPE            PIC X(8).
           03 RETURNED-INTEREST-RATE   PIC 9(4)V99.
           03 RETURNED-OPENED          PIC 9(8).
           03 RETURNED-OVERDRAFT-LIMIT PIC 9(8).
           03 RETURNED-LAST-STMT-DATE  PIC 9(8).
           03 RETURNED-NEXT-STMT-DATE  PIC 9(8).
           03 RETURNED-AVAILABLE-BALANCE  PIC S9(10)V99.
           03 RETURNED-ACTUAL-BALANCE     PIC S9(10)V99.
       01 DESIRED-KEY                  PIC 9(10) BINARY.
       01 DB2-DATE-REFORMAT.
          03 DB2-DATE-REF-YR           PIC 9(4).
          03 FILLER                    PIC X.
          03 DB2-DATE-REF-MNTH         PIC 99.
          03 FILLER                    PIC X.
          03 DB2-DATE-REF-DAY          PIC 99.
       01 DATA-STORE-TYPE              PIC X.
          88 DATASTORE-TYPE-DB2           VALUE '2'.
          88 DATASTORE-TYPE-VSAM          VALUE 'V'.
       01 DB2-EXIT-LOOP                PIC X.
       01 FETCH-DATA-CNT               PIC 9(4) COMP.
       01 WS-CUST-ALT-KEY-LEN          PIC S9(4) COMP VALUE +10.
       01 ACCOUNT-KY.
          03 REQUIRED-SORT-CODE        PIC 9(6)  VALUE 0.
          03 REQUIRED-ACC-NUM          PIC 9(8)  VALUE 0.
       01 MY-ABEND-CODE                PIC XXXX.
       01 WS-STORM-DRAIN               PIC X VALUE 'N'.
       01 STORM-DRAIN-CONDITION        PIC X(20).
       01 SQLCODE-DISPLAY              PIC S9(8) DISPLAY
           SIGN LEADING SEPARATE.
       01 NCS-ACC-NO-STUFF.
          03 NCS-ACC-NO-NAME.
             05 NCS-ACC-NO-ACT-NAME    PIC X(8)
                                          VALUE 'HBNKACCT'.
             05 NCS-ACC-NO-TEST-SORT   PIC X(6)
                                          VALUE '      '.
             05 NCS-ACC-NO-FILL        PIC XX
                                          VALUE '  '.
          03 NCS-ACC-NO-INC            PIC 9(16) COMP
                                          VALUE 0.
          03 NCS-ACC-NO-VALUE          PIC 9(16) COMP
                                          VALUE 0.
          03 NCS-ACC-NO-RESP           PIC XX VALUE '00'.
       01 WS-DISP-ACC-NO-VAL           PIC S9(18) DISPLAY.
       01 ACCOUNT-KY2.
          03 REQUIRED-SORT-CODE2       PIC 9(6) VALUE 0.
          03 REQUIRED-ACC-NUMBER2      PIC 9(8) VALUE 0.
       01 WS-POINTER USAGE POINTER.
       01 WS-POINTER-BYTES  REDEFINES WS-POINTER  PIC X(8).
       01 WS-POINTER-NUMBER  REDEFINES WS-POINTER PIC 9(8) BINARY.
       01 WS-POINTER-NUMBER-DISPLAY    PIC 9(8) DISPLAY.
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
       01 WS-TIME-DATA.
           03 WS-TIME-NOW              PIC 9(6).
           03 WS-TIME-NOW-GRP REDEFINES WS-TIME-NOW.
              05 WS-TIME-NOW-GRP-HH       PIC 99.
              05 WS-TIME-NOW-GRP-MM       PIC 99.
              05 WS-TIME-NOW-GRP-SS       PIC 99.
       01 WS-ABEND-PGM                 PIC X(8) VALUE 'ABNDPROC'.
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
          03 INQACC-EYE                  PIC X(4).
          03 INQACC-CUSTNO               PIC 9(10).
          03 INQACC-SCODE                PIC 9(6).
          03 INQACC-ACCNO                PIC 9(8).
          03 INQACC-ACC-TYPE             PIC X(8).
          03 INQACC-INT-RATE             PIC 9(4)V99.
          03 INQACC-OPENED               PIC 9(8).
          03 INQACC-OPENED-GROUP REDEFINES INQACC-OPENED.
            05 INQACC-OPENED-DAY         PIC 99.
            05 INQACC-OPENED-MONTH         PIC 99.
            05 INQACC-OPENED-YEAR         PIC 9999.
          03 INQACC-OVERDRAFT            PIC 9(8).
          03 INQACC-LAST-STMT-DT         PIC 9(8).
          03 INQACC-LAST-STMT-GROUP REDEFINES INQACC-LAST-STMT-DT.
            05 INQACC-LAST-STMT-DAY         PIC 99.
            05 INQACC-LAST-STMT-MONTH         PIC 99.
            05 INQACC-LAST-STMT-YEAR         PIC 9999.
          03 INQACC-NEXT-STMT-DT         PIC 9(8).
          03 INQACC-NEXT-STMT-GROUP REDEFINES INQACC-NEXT-STMT-DT.
            05 INQACC-NEXT-STMT-DAY         PIC 99.
            05 INQACC-NEXT-STMT-MONTH         PIC 99.
            05 INQACC-NEXT-STMT-YEAR         PIC 9999.
          03 INQACC-AVAIL-BAL            PIC S9(10)V99.
          03 INQACC-ACTUAL-BAL           PIC S9(10)V99.
          03 INQACC-SUCCESS              PIC X.
          03 INQACC-PCB1-POINTER         POINTER.
       PROCEDURE DIVISION USING DFHCOMMAREA.
       PREMIERE SECTION.
       A010.
           INITIALIZE OUTPUT-DATA.
           EXEC CICS HANDLE
              ABEND LABEL(ABEND-HANDLING)
           END-EXEC.
           MOVE SORTCODE TO REQUIRED-SORT-CODE OF ACCOUNT-KY.
           IF INQACC-ACCNO = 99999999
             PERFORM READ-ACCOUNT-LAST
           ELSE
             PERFORM READ-ACCOUNT-DB2
           END-IF
           IF ACCOUNT-TYPE = SPACES OR LOW-VALUES
              MOVE 'N' TO INQACC-SUCCESS
           ELSE
              MOVE ACCOUNT-EYE-CATCHER       TO INQACC-EYE
              MOVE ACCOUNT-CUST-NO           TO INQACC-CUSTNO
              MOVE ACCOUNT-SORT-CODE         TO INQACC-SCODE
              MOVE ACCOUNT-NUMBER            TO INQACC-ACCNO
              MOVE ACCOUNT-TYPE              TO INQACC-ACC-TYPE
              MOVE ACCOUNT-INTEREST-RATE     TO INQACC-INT-RATE
              MOVE ACCOUNT-OPENED            TO INQACC-OPENED
              MOVE ACCOUNT-OVERDRAFT-LIMIT   TO INQACC-OVERDRAFT
              MOVE ACCOUNT-LAST-STMT-DATE    TO INQACC-LAST-STMT-DT
              MOVE ACCOUNT-NEXT-STMT-DATE    TO INQACC-NEXT-STMT-DT
              MOVE ACCOUNT-AVAILABLE-BALANCE TO INQACC-AVAIL-BAL
              MOVE ACCOUNT-ACTUAL-BALANCE    TO INQACC-ACTUAL-BAL
              MOVE 'Y'                       TO INQACC-SUCCESS
           END-IF.
           PERFORM GET-ME-OUT-OF-HERE.
       A999.
           EXIT.
       READ-ACCOUNT-DB2 SECTION.
       RAD010.
           MOVE INQACC-ACCNO
              TO HV-ACCOUNT-ACC-NO.
           MOVE SORTCODE TO HV-ACCOUNT-SORTCODE.
           EXEC SQL OPEN ACC-CURSOR
           END-EXEC.
           IF SQLCODE NOT = 0
              MOVE SQLCODE TO SQLCODE-DISPLAY
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
              MOVE 'HRAC'      TO ABND-CODE
              EXEC CICS ASSIGN PROGRAM(ABND-PROGRAM)
              END-EXEC
              MOVE SQLCODE-DISPLAY TO ABND-SQLCODE
              STRING 'RAD010 -Failure when attempting to OPEN DB2 '
                    DELIMITED BY SIZE,
                    'CURSOR. Check SQLCODE. '
                    DELIMITED BY SIZE,
                    'SQLCODE=' DELIMITED BY SIZE,
                    SQLCODE-DISPLAY DELIMITED BY SIZE,
                    INTO ABND-FREEFORM
              END-STRING
              EXEC CICS LINK PROGRAM(WS-ABEND-PGM)
                        COMMAREA(ABNDINFO-REC)
              END-EXEC
              DISPLAY 'Failure when attempting to open DB2 CURSOR '
                  'ACC-CURSOR. With SQL code='
                  SQLCODE-DISPLAY
              PERFORM CHECK-FOR-STORM-DRAIN-DB2
              EXEC CICS ABEND ABCODE('HRAC')
                 CANCEL
                 NODUMP
              END-EXEC
           END-IF.
           PERFORM FETCH-DATA.
           EXEC SQL CLOSE ACC-CURSOR
           END-EXEC.
           IF SQLCODE NOT = 0
              MOVE SQLCODE TO SQLCODE-DISPLAY
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
              MOVE 'HRAC'      TO ABND-CODE
              EXEC CICS ASSIGN PROGRAM(ABND-PROGRAM)
              END-EXEC
              MOVE SQLCODE-DISPLAY TO ABND-SQLCODE
              STRING 'RAD010 -Failure when attempting to CLOSE DB2 '
                    DELIMITED BY SIZE,
                    'CURSOR (ACC-CUSOR). Check SQLCODE'
                    DELIMITED BY SIZE,
                    'SQLCODE=' DELIMITED BY SIZE,
                    SQLCODE-DISPLAY DELIMITED BY SIZE,
                    INTO ABND-FREEFORM
              END-STRING
              EXEC CICS LINK PROGRAM(WS-ABEND-PGM)
                        COMMAREA(ABNDINFO-REC)
              END-EXEC
              DISPLAY 'Failure when attempting to close the DB2 CURSOR'
                  ' ACC-CURSOR. With SQL code='
                  SQLCODE-DISPLAY
              PERFORM CHECK-FOR-STORM-DRAIN-DB2
              EXEC CICS ABEND ABCODE('HRAC')
                 CANCEL
                 NODUMP
              END-EXEC
           END-IF.
       RAD999.
           EXIT.
       FETCH-DATA SECTION.
       FD010.
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
           END-EXEC.
           IF SQLCODE = +100
             INITIALIZE OUTPUT-DATA
             MOVE SORTCODE TO ACCOUNT-SORT-CODE OF OUTPUT-DATA
             MOVE INQACC-ACCNO TO ACCOUNT-NUMBER OF OUTPUT-DATA
             GO TO FD999
           END-IF.
           IF SQLCODE NOT = 0
              PERFORM CHECK-FOR-STORM-DRAIN-DB2
              MOVE SQLCODE TO SQLCODE-DISPLAY
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
              MOVE 'HRAC'      TO ABND-CODE
              EXEC CICS ASSIGN PROGRAM(ABND-PROGRAM)
              END-EXEC
              MOVE SQLCODE-DISPLAY TO ABND-SQLCODE
              STRING 'FD010 -Failure when attempting to FETCH from '
                    DELIMITED BY SIZE,
                    'DB2 CURSOR (ACC-CURSOR). Check SQLCODE'
                    DELIMITED BY SIZE,
                    'SQLCODE=' DELIMITED BY SIZE,
                    SQLCODE-DISPLAY DELIMITED BY SIZE,
                    INTO ABND-FREEFORM
              END-STRING
              EXEC CICS LINK PROGRAM(WS-ABEND-PGM)
                        COMMAREA(ABNDINFO-REC)
              END-EXEC
              DISPLAY 'Failure when attempting to FETCH from the DB2 '
                  'CURSOR ACC-CURSOR. With SQL code='
                  SQLCODE-DISPLAY
              EXEC CICS ABEND ABCODE('HRAC')
                 CANCEL
                 NODUMP
              END-EXEC
           END-IF.
           MOVE HV-ACCOUNT-EYECATCHER TO
              ACCOUNT-EYE-CATCHER OF OUTPUT-DATA.
           MOVE HV-ACCOUNT-CUST-NO TO
              ACCOUNT-CUST-NO OF OUTPUT-DATA.
           MOVE HV-ACCOUNT-SORTCODE TO
              ACCOUNT-SORT-CODE OF OUTPUT-DATA.
           MOVE HV-ACCOUNT-ACC-NO TO
              ACCOUNT-NUMBER OF OUTPUT-DATA.
           MOVE HV-ACCOUNT-ACC-TYPE TO
              ACCOUNT-TYPE OF OUTPUT-DATA.
           MOVE HV-ACCOUNT-INT-RATE TO
              ACCOUNT-INTEREST-RATE OF OUTPUT-DATA.
           MOVE HV-ACCOUNT-OPENED TO DB2-DATE-REFORMAT.
           MOVE DB2-DATE-REF-DAY TO
              ACCOUNT-OPENED-DAY OF OUTPUT-DATA.
           MOVE DB2-DATE-REF-MNTH TO
              ACCOUNT-OPENED-MONTH OF OUTPUT-DATA.
           MOVE DB2-DATE-REF-YR TO
              ACCOUNT-OPENED-YEAR OF OUTPUT-DATA.
           MOVE HV-ACCOUNT-OVERDRAFT-LIM TO
              ACCOUNT-OVERDRAFT-LIMIT OF OUTPUT-DATA.
           MOVE HV-ACCOUNT-LAST-STMT TO DB2-DATE-REFORMAT.
           MOVE DB2-DATE-REF-DAY TO
              ACCOUNT-LAST-STMT-DAY OF OUTPUT-DATA.
           MOVE DB2-DATE-REF-MNTH TO
              ACCOUNT-LAST-STMT-MONTH OF OUTPUT-DATA.
           MOVE DB2-DATE-REF-YR TO
              ACCOUNT-LAST-STMT-YEAR OF OUTPUT-DATA.
           MOVE HV-ACCOUNT-NEXT-STMT TO DB2-DATE-REFORMAT
           MOVE DB2-DATE-REF-DAY TO
              ACCOUNT-NEXT-STMT-DAY OF OUTPUT-DATA.
           MOVE DB2-DATE-REF-MNTH TO
              ACCOUNT-NEXT-STMT-MONTH OF OUTPUT-DATA.
           MOVE DB2-DATE-REF-YR TO
              ACCOUNT-NEXT-STMT-YEAR OF OUTPUT-DATA.
           MOVE HV-ACCOUNT-AVAIL-BAL TO
              ACCOUNT-AVAILABLE-BALANCE OF OUTPUT-DATA.
           MOVE HV-ACCOUNT-ACTUAL-BAL TO
              ACCOUNT-ACTUAL-BALANCE OF OUTPUT-DATA.
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
       CFSDCD010.
           EVALUATE SQLCODE
              WHEN 923
                 MOVE 'DB2 Connection lost ' TO STORM-DRAIN-CONDITION
              WHEN OTHER
                 MOVE 'Not Storm Drain     ' TO STORM-DRAIN-CONDITION
           END-EVALUATE.
           MOVE SQLCODE TO SQLCODE-DISPLAY.
           IF STORM-DRAIN-CONDITION NOT EQUAL 'Not Storm Drain     '
              DISPLAY 'INQACC: Check-For-Storm-Drain-DB2: Storm '
                      'Drain condition (' STORM-DRAIN-CONDITION ') '
                      'has been met (' SQLCODE-DISPLAY ').'
           ELSE
              CONTINUE
           END-IF.
       CFSDCD999.
           EXIT.
       ABEND-HANDLING SECTION.
       AH010.
           EXEC CICS ASSIGN ABCODE(MY-ABEND-CODE)
           END-EXEC.
           EVALUATE MY-ABEND-CODE
              WHEN 'AD2Z'
                 MOVE SQLCODE TO SQLCODE-DISPLAY
                 DISPLAY 'DB2 DEADLOCK DETECTED IN INQACC, SQLCODE='
                    SQLCODE-DISPLAY
                 DISPLAY 'DB2 DEADLOCK FOR ACCOUNT '
                    HV-ACCOUNT-ACC-NO
                 DISPLAY  'SQLSTATE=' SQLSTATE
                        ',SQLERRMC=' sqlerrmc(1:sqlerrmL)
                        ',SQLERRD(1)=' SQLERRD(1)
                        ',SQLERRD(2)=' SQLERRD(2)
                        ',SQLERRD(3)=' SQLERRD(3)
                        ',SQLERRD(4)=' SQLERRD(4)
                        ',SQLERRD(5)=' SQLERRD(5)
                        ',SQLERRD(6)=' SQLERRD(6)
              WHEN 'AFCR'
              WHEN 'AFCS'
              WHEN 'AFCT'
                 MOVE 'Y' TO WS-STORM-DRAIN
                 DISPLAY 'INQACC: Check-For-Storm-Drain-VSAM: Storm '
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
                   STRING 'AH010 -Unable to perform SYNCPOINT ROLLBACK.'
                       DELIMITED BY SIZE,
                       ' Possible integrity issue following VSAM RLS '
                       DELIMITED BY SIZE,
                       ' abend.' DELIMITED BY SIZE,
                       ' EIBRESP=' DELIMITED BY SIZE,
                       ABND-RESPCODE DELIMITED BY SIZE,
                       ' RESP2=' DELIMITED BY SIZE,
                       ABND-RESP2CODE DELIMITED BY SIZE
                       INTO ABND-FREEFORM
                   END-STRING
                   EXEC CICS LINK PROGRAM(WS-ABEND-PGM)
                           COMMAREA(ABNDINFO-REC)
                   END-EXEC
                   DISPLAY 'INQACC: Unable to perform Syncpoint '
                           'Rollback. Possible Integrity issue '
                           ' following VSAM RLS abend. '
                           ' RESP CODE=' WS-CICS-RESP
                           ' RESP2 CODE=' WS-CICS-RESP2
                   EXEC CICS ABEND
                      ABCODE ('HROL')
                      NODUMP
                       CANCEL
                   END-EXEC
                END-IF
                MOVE 'N' TO INQACC-SUCCESS
                EXEC CICS RETURN
                END-EXEC
           END-EVALUATE.
           IF WS-STORM-DRAIN = 'N'
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
              MOVE MY-ABEND-CODE TO ABND-CODE
              EXEC CICS ASSIGN PROGRAM(ABND-PROGRAM)
              END-EXEC
              MOVE ZEROS      TO ABND-SQLCODE
              STRING 'AH010 -WVS-STORM-DRAIN=N'
                    DELIMITED BY SIZE,
                    ' EIBRESP=' DELIMITED BY SIZE,
                    ABND-RESPCODE DELIMITED BY SIZE,
                    ' RESP2=' DELIMITED BY SIZE,
                    ABND-RESP2CODE DELIMITED BY SIZE
                    INTO ABND-FREEFORM
              END-STRING
              EXEC CICS LINK PROGRAM(WS-ABEND-PGM)
                 COMMAREA(ABNDINFO-REC)
              END-EXEC
              EXEC CICS ABEND ABCODE( MY-ABEND-CODE)
              NODUMP
              CANCEL
              END-EXEC
           END-IF.
       AH999.
           EXIT.
       READ-ACCOUNT-LAST SECTION.
       RAN010.
            PERFORM GET-LAST-ACCOUNT-DB2.
            MOVE REQUIRED-ACC-NUMBER2 TO NCS-ACC-NO-VALUE.
       RAN999.
           EXIT.
       GET-LAST-ACCOUNT-DB2 SECTION.
       GLAD010.
           INITIALIZE OUTPUT-DATA.
           MOVE REQUIRED-ACC-NUMBER2 TO HV-ACCOUNT-ACC-NO.
           MOVE REQUIRED-SORT-CODE TO HV-ACCOUNT-SORTCODE.
           MOVE SORTCODE TO HV-ACCOUNT-SORTCODE.
           EXEC SQL
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
              FROM ACCOUNT
              WHERE ACCOUNT_SORTCODE = :HV-ACCOUNT-SORTCODE
              ORDER BY ACCOUNT_NUMBER DESC
              FETCH FIRST 1 ROWS ONLY
           END-EXEC.
           IF SQLCODE IS NOT EQUAL TO ZERO
              MOVE SQLCODE TO SQLCODE-DISPLAY
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
              MOVE 'HNCS'      TO ABND-CODE
              EXEC CICS ASSIGN PROGRAM(ABND-PROGRAM)
              END-EXEC
              MOVE SQLCODE-DISPLAY TO ABND-SQLCODE
              STRING 'GLAD010 -ACCOUNT NCS '
                   DELIMITED BY SIZE,
                   NCS-ACC-NO-NAME   DELIMITED BY SIZE,
                   ' CANNOT be accessed and DB2 ' DELIMITED BY SIZE,
                   ' SELECT failed. SQLCODE='
                   DELIMITED BY SIZE,
                   SQLCODE-DISPLAY DELIMITED BY SIZE
                   INTO ABND-FREEFORM
              END-STRING
              EXEC CICS LINK PROGRAM(WS-ABEND-PGM)
                       COMMAREA(ABNDINFO-REC)
              END-EXEC
              DISPLAY 'INQACC - ACCOUNT NCS ' NCS-ACC-NO-NAME
                 ' CANNOT BE ACCESSED AND DB2 SELECT FAILED. SQLCODE='
                 SQLCODE-DISPLAY
              EXEC CICS ABEND
                       ABCODE('HNCS')
                       NODUMP
                       CANCEL
              END-EXEC
           ELSE
             MOVE HV-ACCOUNT-EYECATCHER TO
                ACCOUNT-EYE-CATCHER OF OUTPUT-DATA
             MOVE HV-ACCOUNT-CUST-NO TO
                ACCOUNT-CUST-NO OF OUTPUT-DATA
             MOVE HV-ACCOUNT-SORTCODE TO
                ACCOUNT-SORT-CODE OF OUTPUT-DATA
             MOVE HV-ACCOUNT-ACC-NO TO
                ACCOUNT-NUMBER OF OUTPUT-DATA
             MOVE HV-ACCOUNT-ACC-TYPE TO
                ACCOUNT-TYPE OF OUTPUT-DATA
             MOVE HV-ACCOUNT-INT-RATE TO
                ACCOUNT-INTEREST-RATE OF OUTPUT-DATA
             MOVE HV-ACCOUNT-OPENED TO DB2-DATE-REFORMAT
             MOVE DB2-DATE-REF-DAY TO
                ACCOUNT-OPENED-DAY OF OUTPUT-DATA
             MOVE DB2-DATE-REF-MNTH TO
                ACCOUNT-OPENED-MONTH OF OUTPUT-DATA
             MOVE DB2-DATE-REF-YR TO
                ACCOUNT-OPENED-YEAR OF OUTPUT-DATA
             MOVE HV-ACCOUNT-OVERDRAFT-LIM TO
                ACCOUNT-OVERDRAFT-LIMIT OF OUTPUT-DATA
             MOVE HV-ACCOUNT-LAST-STMT TO DB2-DATE-REFORMAT
             MOVE DB2-DATE-REF-DAY TO
                ACCOUNT-LAST-STMT-DAY OF OUTPUT-DATA
             MOVE DB2-DATE-REF-MNTH TO
                ACCOUNT-LAST-STMT-MONTH OF OUTPUT-DATA
             MOVE DB2-DATE-REF-YR TO
                ACCOUNT-LAST-STMT-YEAR OF OUTPUT-DATA
             MOVE HV-ACCOUNT-NEXT-STMT TO DB2-DATE-REFORMAT
             MOVE DB2-DATE-REF-DAY TO
                ACCOUNT-NEXT-STMT-DAY OF OUTPUT-DATA
             MOVE DB2-DATE-REF-MNTH TO
                ACCOUNT-NEXT-STMT-MONTH OF OUTPUT-DATA
             MOVE DB2-DATE-REF-YR TO
                ACCOUNT-NEXT-STMT-YEAR OF OUTPUT-DATA
             MOVE HV-ACCOUNT-AVAIL-BAL TO
                ACCOUNT-AVAILABLE-BALANCE OF OUTPUT-DATA
             MOVE HV-ACCOUNT-ACTUAL-BAL TO
                ACCOUNT-ACTUAL-BALANCE OF OUTPUT-DATA
           END-IF.
       GLAD999.
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
