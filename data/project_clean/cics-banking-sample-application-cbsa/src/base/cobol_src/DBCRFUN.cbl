       PROCESS CICS,NODYNAM,NSYMBOL(NATIONAL),TRUNC(STD)
       CBL CICS('SP,EDF')
       CBL SQL
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DBCRFUN.
       AUTHOR. Jon Collett.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-370.
       OBJECT-COMPUTER.  IBM-370.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 SORTCODE           PIC 9(6) VALUE 987654.
       77 SYSIDERR-RETRY                PIC 999.
           EXEC SQL
              INCLUDE ACCDB2
           END-EXEC.
       01 HOST-ACCOUNT-ROW.
          03 HV-ACCOUNT-EYECATCHER      PIC X(4).
          03 HV-ACCOUNT-CUST-NO         PIC X(10).
          03 HV-ACCOUNT-KEY.
             05 HV-ACCOUNT-SORTCODE     PIC X(6).
             05 HV-ACCOUNT-ACC-NO       PIC X(8).
          03 HV-ACCOUNT-ACC-TYPE        PIC X(8).
          03 HV-ACCOUNT-INT-RATE        PIC S9(4)V99 COMP-3.
          03 HV-ACCOUNT-OPENED          PIC X(10).
          03 HV-ACCOUNT-OVERDRAFT-LIM   PIC S9(9) COMP.
          03 HV-ACCOUNT-LAST-STMT       PIC X(10).
          03 HV-ACCOUNT-NEXT-STMT       PIC X(10).
          03 HV-ACCOUNT-AVAIL-BAL       PIC S9(10)V99 COMP-3.
          03 HV-ACCOUNT-ACTUAL-BAL      PIC S9(10)V99 COMP-3.
          EXEC SQL
             INCLUDE PROCDB2
          END-EXEC.
       01 HOST-PROCTRAN-ROW.
          03 HV-PROCTRAN-EYECATCHER     PIC X(4).
          03 HV-PROCTRAN-SORT-CODE      PIC X(6).
          03 HV-PROCTRAN-ACC-NUMBER     PIC X(8).
          03 HV-PROCTRAN-DATE           PIC X(10).
          03 HV-PROCTRAN-TIME           PIC X(6).
          03 HV-PROCTRAN-REF            PIC X(12).
          03 HV-PROCTRAN-TYPE           PIC X(3).
          03 HV-PROCTRAN-DESC           PIC X(40).
          03 HV-PROCTRAN-AMOUNT         PIC S9(10)V99 COMP-3.
       EXEC SQL
          INCLUDE SQLCA
       END-EXEC.
       01 WS-CICS-WORK-AREA.
          05 WS-CICS-RESP               PIC S9(8) COMP.
          05 WS-CICS-RESP2              PIC S9(8) COMP.
       LOCAL-STORAGE SECTION.
       01 FILE-RETRY                    PIC 999.
       01 WS-EXIT-RETRY-LOOP            PIC X         VALUE ' '.
       01 DB2-DATE-REFORMAT.
          03 DB2-DATE-REF-YR            PIC 9(4).
          03 FILLER                     PIC X.
          03 DB2-DATE-REF-MNTH          PIC 99.
          03 FILLER                     PIC X.
          03 DB2-DATE-REF-DAY           PIC 99.
       01 DATA-STORE-TYPE               PIC X.
          88 DATASTORE-TYPE-DLI                       VALUE '1'.
          88 DATASTORE-TYPE-DB2                       VALUE '2'.
          88 DATASTORE-TYPE-VSAM                      VALUE 'V'.
       01 WS-ACC-DATA.
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
       01 WS-EIBTASKN12                 PIC 9(12)     VALUE 0.
       01 WS-SQLCODE-DISP               PIC 9(9)      VALUE 0.
       01 DESIRED-ACC-KEY.
          03 DESIRED-SORT-CODE          PIC 9(6).
          03 DESIRED-ACC-NO             PIC 9(8).
       01 NEW-ACCOUNT-AVAILABLE-BALANCE PIC S9(10)V99 VALUE 0.
       01 NEW-ACCOUNT-ACTUAL-BALANCE    PIC S9(10)V99 VALUE 0.
       01 WS-ACC-REC-LEN                PIC S9(4) COMP
                                                      VALUE 0.
       01 WS-U-TIME                     PIC S9(15) COMP-3.
       01 WS-ORIG-DATE                  PIC X(10).
       01 WS-ORIG-DATE-GRP REDEFINES WS-ORIG-DATE.
          03 WS-ORIG-DATE-DD            PIC 99.
          03 FILLER                     PIC X.
          03 WS-ORIG-DATE-MM            PIC 99.
          03 FILLER                     PIC X.
          03 WS-ORIG-DATE-YYYY          PIC 9999.
       01 WS-ORIG-DATE-GRP-X.
          03 WS-ORIG-DATE-DD-X          PIC XX.
          03 FILLER                     PIC X         VALUE '.'.
          03 WS-ORIG-DATE-MM-X          PIC XX.
          03 FILLER                     PIC X         VALUE '.'.
          03 WS-ORIG-DATE-YYYY-X        PIC X(4).
       01 PROCTRAN-AREA.
           03 PROC-TRAN-DATA.
              05 PROC-TRAN-EYE-CATCHER        PIC X(4).
              88 PROC-TRAN-VALID VALUE 'PRTR'.
              05 PROC-TRAN-LOGICAL-DELETE-AREA REDEFINES
                 PROC-TRAN-EYE-CATCHER.
                 07 PROC-TRAN-LOGICAL-DELETE-FLAG PIC X.
                 88 PROC-TRAN-LOGICALLY-DELETED VALUE X'FF'.
                 07 FILLER PIC X(3).
              05 PROC-TRAN-ID.
                 07 PROC-TRAN-SORT-CODE       PIC 9(6).
                 07 PROC-TRAN-NUMBER          PIC 9(8).
              05 PROC-TRAN-DATE               PIC 9(8).
              05 PROC-TRAN-DATE-GRP REDEFINES PROC-TRAN-DATE.
                 07 PROC-TRAN-DATE-GRP-YYYY   PIC 9999.
                 07 PROC-TRAN-DATE-GRP-MM     PIC 99.
                 07 PROC-TRAN-DATE-GRP-DD     PIC 99.
              05 PROC-TRAN-TIME               PIC 9(6).
              05 PROC-TRAN-TIME-GRP REDEFINES PROC-TRAN-TIME.
                 07 PROC-TRAN-TIME-GRP-HH     PIC 99.
                 07 PROC-TRAN-TIME-GRP-MM     PIC 99.
                 07 PROC-TRAN-TIME-GRP-SS     PIC 99.
              05 PROC-TRAN-REF                PIC 9(12).
              05 PROC-TRAN-TYPE               PIC X(3).
              88 PROC-TY-CHEQUE-ACKNOWLEDGED      VALUE 'CHA'.
              88 PROC-TY-CHEQUE-FAILURE           VALUE 'CHF'.
              88 PROC-TY-CHEQUE-PAID-IN           VALUE 'CHI'.
              88 PROC-TY-CHEQUE-PAID-OUT          VALUE 'CHO'.
              88 PROC-TY-CREDIT                   VALUE 'CRE'.
              88 PROC-TY-DEBIT                    VALUE 'DEB'.
              88 PROC-TY-WEB-CREATE-ACCOUNT       VALUE 'ICA'.
              88 PROC-TY-WEB-CREATE-CUSTOMER      VALUE 'ICC'.
              88 PROC-TY-WEB-DELETE-ACCOUNT       VALUE 'IDA'.
              88 PROC-TY-WEB-DELETE-CUSTOMER      VALUE 'IDC'.
              88 PROC-TY-BRANCH-CREATE-ACCOUNT    VALUE 'OCA'.
              88 PROC-TY-BRANCH-CREATE-CUSTOMER   VALUE 'OCC'.
              88 PROC-TY-BRANCH-DELETE-ACCOUNT    VALUE 'ODA'.
              88 PROC-TY-BRANCH-DELETE-CUSTOMER   VALUE 'ODC'.
              88 PROC-TY-CREATE-SODD              VALUE 'OCS'.
              88 PROC-TY-PAYMENT-CREDIT           VALUE 'PCR'.
              88 PROC-TY-PAYMENT-DEBIT            VALUE 'PDR'.
              88 PROC-TY-TRANSFER                 VALUE 'TFR'.
              05 PROC-TRAN-DESC               PIC X(40).
              05 PROC-TRAN-DESC-XFR REDEFINES PROC-TRAN-DESC.
                07 PROC-TRAN-DESC-XFR-HEADER PIC X(26).
                88 PROC-TRAN-DESC-XFR-FLAG
                   VALUE 'TRANSFER'.
                07 PROC-TRAN-DESC-XFR-SORTCODE
                   PIC 9(6).
                07 PROC-TRAN-DESC-XFR-ACCOUNT
                   PIC 9(8).
              05 PROC-TRAN-DESC-DELACC REDEFINES PROC-TRAN-DESC.
                07 PROC-DESC-DELACC-CUSTOMER PIC 9(10).
                07 PROC-DESC-DELACC-ACCTYPE PIC X(8).
                07 PROC-DESC-DELACC-LAST-DD PIC 99.
                07 PROC-DESC-DELACC-LAST-MM PIC 99.
                07 PROC-DESC-DELACC-LAST-YYYY PIC 9999.
                07 PROC-DESC-DELACC-NEXT-DD PIC 99.
                07 PROC-DESC-DELACC-NEXT-MM PIC 99.
                07 PROC-DESC-DELACC-NEXT-YYYY PIC 9999.
                07 PROC-DESC-DELACC-FOOTER PIC X(6).
                88 PROC-DESC-DELACC-FLAG
                   VALUE 'DELETE'.
              05 PROC-TRAN-DESC-CREACC REDEFINES PROC-TRAN-DESC.
                07 PROC-DESC-CREACC-CUSTOMER PIC 9(10).
                07 PROC-DESC-CREACC-ACCTYPE PIC X(8).
                07 PROC-DESC-CREACC-LAST-DD PIC 99.
                07 PROC-DESC-CREACC-LAST-MM PIC 99.
                07 PROC-DESC-CREACC-LAST-YYYY PIC 9999.
                07 PROC-DESC-CREACC-NEXT-DD PIC 99.
                07 PROC-DESC-CREACC-NEXT-MM PIC 99.
                07 PROC-DESC-CREACC-NEXT-YYYY PIC 9999.
                07 PROC-DESC-CREACC-FOOTER PIC X(6).
                88 PROC-DESC-CREACC-FLAG
                   VALUE 'CREATE'.
              05 PROC-TRAN-DESC-DELCUS REDEFINES PROC-TRAN-DESC.
                07 PROC-DESC-DELCUS-SORTCODE PIC 9(6).
                07 PROC-DESC-DELCUS-CUSTOMER PIC 9(10).
                07 PROC-DESC-DELCUS-NAME     PIC X(14).
                07 PROC-DESC-DELCUS-DOB-YYYY  PIC 9999.
                07 PROC-DESC-DELCUS-FILLER    PIC X.
                88 PROC-DESC-DELCUS-FILLER-SET VALUE '-'.
                07 PROC-DESC-DELCUS-DOB-MM    PIC 99.
                07 PROC-DESC-DELCUS-FILLER2   PIC X.
                88 PROC-DESC-DELCUS-FILLER2-SET VALUE '-'.
                07 PROC-DESC-DELCUS-DOB-DD    PIC 99.
              05 PROC-TRAN-DESC-CRECUS REDEFINES PROC-TRAN-DESC.
                07 PROC-DESC-CRECUS-SORTCODE PIC 9(6).
                07 PROC-DESC-CRECUS-CUSTOMER PIC 9(10).
                07 PROC-DESC-CRECUS-NAME     PIC X(14).
                07 PROC-DESC-CRECUS-DOB-YYYY  PIC 9999.
                07 PROC-DESC-CRECUS-FILLER    PIC X.
                88 PROC-DESC-CRECUS-FILLER-SET VALUE '-'.
                07 PROC-DESC-CRECUS-DOB-MM    PIC 99.
                07 PROC-DESC-CRECUS-FILLER2   PIC X.
                88 PROC-DESC-CRECUS-FILLER2-SET VALUE '-'.
                07 PROC-DESC-CRECUS-DOB-DD    PIC 99.
              05 PROC-TRAN-AMOUNT             PIC S9(10)V99.
       01 WS-PASSED-DATA.
          02 WS-TEST-KEY                PIC X(4).
          02 WS-SORT-CODE               PIC 9(6).
          02 WS-CUSTOMER-RANGE.
             07 WS-CUSTOMER-RANGE-TOP   PIC X.
             07 WS-CUSTOMER-RANGE-MIDDLE
                                        PIC X.
             07 WS-CUSTOMER-RANGE-BOTTOM
                                        PIC X.
       01 PROCTRAN-RIDFLD               PIC S9(8) COMP.
       01 SQLCODE-DISPLAY               PIC S9(8) DISPLAY
             SIGN LEADING SEPARATE.
       01 MY-ABEND-CODE                 PIC XXXX.
       01 WS-STORM-DRAIN                PIC X         VALUE 'N'.
       01 STORM-DRAIN-CONDITION         PIC X(20).
       01 NUMERIC-AMOUNT-DISPLAY        PIC +9(10).99.
       01 WS-TIME-DATA.
          03 WS-TIME-NOW                PIC 9(6).
          03 WS-TIME-NOW-GRP REDEFINES WS-TIME-NOW.
             05 WS-TIME-NOW-GRP-HH      PIC 99.
             05 WS-TIME-NOW-GRP-MM      PIC 99.
             05 WS-TIME-NOW-GRP-SS      PIC 99.
       01 WS-ABEND-PGM                  PIC X(8)      VALUE 'ABNDPROC'.
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
       01 WS-SUFFICIENT-FUNDS           PIC X VALUE 'N'.
       01 WS-DIFFERENCE                 PIC S9(10)V99.
       LINKAGE SECTION.
       01 DFHCOMMAREA.
          03 COMM-ACCNO               PIC X(8).
          03 COMM-AMT                 PIC S9(10)V99.
          03 COMM-SORTC               PIC 9(6).
          03 COMM-AV-BAL              PIC S9(10)V99.
          03 COMM-ACT-BAL             PIC S9(10)V99.
          03 COMM-ORIGIN.
               05 COMM-APPLID           PIC X(8).
               05 COMM-USERID           PIC X(8).
               05 COMM-FACILITY-NAME    PIC X(8).
               05 COMM-NETWRK-ID        PIC X(8).
               05 COMM-FACILTYPE        PIC S9(8) COMP.
               05 FILLER                PIC X(4).
          03 COMM-SUCCESS             PIC X.
          03 COMM-FAIL-CODE           PIC X.
       PROCEDURE DIVISION USING DFHCOMMAREA.
       PREMIERE SECTION.
       A010.
           MOVE 'N' TO COMM-SUCCESS
           MOVE '0' TO COMM-FAIL-CODE
           EXEC CICS HANDLE ABEND
              LABEL(ABEND-HANDLING)
           END-EXEC.
           MOVE SORTCODE TO COMM-SORTC.
           MOVE SORTCODE TO DESIRED-SORT-CODE.
            PERFORM UPDATE-ACCOUNT-DB2.
           PERFORM GET-ME-OUT-OF-HERE.
       A999.
           EXIT.
       UPDATE-ACCOUNT-DB2 SECTION.
       UAD010.
           MOVE COMM-ACCNO TO DESIRED-ACC-NO.
           MOVE DESIRED-SORT-CODE TO HV-ACCOUNT-SORTCODE.
           MOVE DESIRED-ACC-NO TO HV-ACCOUNT-ACC-NO.
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
              INTO  :HV-ACCOUNT-EYECATCHER,
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
              WHERE  (ACCOUNT_SORTCODE = :HV-ACCOUNT-SORTCODE AND
                      ACCOUNT_NUMBER = :HV-ACCOUNT-ACC-NO)
           END-EXEC.
           IF SQLCODE NOT = 0
              MOVE 'N' TO COMM-SUCCESS
              IF SQLCODE = +100
                 MOVE '1' TO COMM-FAIL-CODE
              ELSE
                 MOVE '2' TO COMM-FAIL-CODE
              END-IF
              PERFORM CHECK-FOR-STORM-DRAIN-DB2
              GO TO UAD999
           END-IF.
           IF COMM-AMT < 0
              IF (HV-ACCOUNT-ACC-TYPE = 'MORTGAGE'
              AND COMM-FACILTYPE = 496)
              OR (HV-ACCOUNT-ACC-TYPE = 'LOAN    '
              AND COMM-FACILTYPE = 496)
                 MOVE 'N' TO COMM-SUCCESS
                 MOVE '4' TO COMM-FAIL-CODE
                 GO TO UAD999
              END-IF
              MOVE 0 TO WS-DIFFERENCE
              COMPUTE WS-DIFFERENCE = HV-ACCOUNT-AVAIL-BAL
                 + COMM-AMT
              IF WS-DIFFERENCE < 0 AND COMM-FACILTYPE = 496
                 MOVE 'N' TO COMM-SUCCESS
                 MOVE '3' TO COMM-FAIL-CODE
                 GO TO UAD999
              END-IF
           END-IF.
           IF (HV-ACCOUNT-ACC-TYPE = 'MORTGAGE' AND
           COMM-FACILTYPE = 496)
           OR (HV-ACCOUNT-ACC-TYPE = 'LOAN    '
           AND COMM-FACILTYPE = 496)
              MOVE 'N' TO COMM-SUCCESS
              MOVE '4' TO COMM-FAIL-CODE
              GO TO UAD999
           END-IF.
           COMPUTE HV-ACCOUNT-AVAIL-BAL =
              HV-ACCOUNT-AVAIL-BAL + COMM-AMT.
           COMPUTE HV-ACCOUNT-ACTUAL-BAL =
              HV-ACCOUNT-ACTUAL-BAL + COMM-AMT.
           EXEC SQL
              UPDATE ACCOUNT
              SET ACCOUNT_EYECATCHER = :HV-ACCOUNT-EYECATCHER,
                  ACCOUNT_CUSTOMER_NUMBER = :HV-ACCOUNT-CUST-NO,
                  ACCOUNT_SORTCODE = :HV-ACCOUNT-SORTCODE,
                  ACCOUNT_NUMBER = :HV-ACCOUNT-ACC-NO,
                  ACCOUNT_TYPE = :HV-ACCOUNT-ACC-TYPE,
                  ACCOUNT_INTEREST_RATE = :HV-ACCOUNT-INT-RATE,
                  ACCOUNT_OPENED = :HV-ACCOUNT-OPENED,
                  ACCOUNT_OVERDRAFT_LIMIT = :HV-ACCOUNT-OVERDRAFT-LIM,
                  ACCOUNT_LAST_STATEMENT = :HV-ACCOUNT-LAST-STMT,
                  ACCOUNT_NEXT_STATEMENT = :HV-ACCOUNT-NEXT-STMT,
                  ACCOUNT_AVAILABLE_BALANCE = :HV-ACCOUNT-AVAIL-BAL,
                  ACCOUNT_ACTUAL_BALANCE = :HV-ACCOUNT-ACTUAL-BAL
              WHERE (ACCOUNT_SORTCODE = :HV-ACCOUNT-SORTCODE AND
                     ACCOUNT_NUMBER = :HV-ACCOUNT-ACC-NO)
           END-EXEC.
           MOVE SQLCODE TO SQLCODE-DISPLAY.
           MOVE HV-ACCOUNT-AVAIL-BAL TO
              COMM-AV-BAL.
           MOVE HV-ACCOUNT-ACTUAL-BAL TO
              COMM-ACT-BAL.
           IF SQLCODE NOT = 0
              MOVE 'N' TO COMM-SUCCESS
              MOVE '2' TO COMM-FAIL-CODE
              PERFORM CHECK-FOR-STORM-DRAIN-DB2
              GO TO UAD999
           END-IF.
           PERFORM WRITE-TO-PROCTRAN.
       UAD999.
           EXIT.
       WRITE-TO-PROCTRAN SECTION.
       WTP010.
            PERFORM WRITE-TO-PROCTRAN-DB2.
       WTP999.
           EXIT.
       WRITE-TO-PROCTRAN-DB2 SECTION.
       WTPD010.
           INITIALIZE HOST-PROCTRAN-ROW.
           INITIALIZE WS-EIBTASKN12.
           MOVE 'PRTR' TO HV-PROCTRAN-EYECATCHER.
           MOVE COMM-SORTC TO HV-PROCTRAN-SORT-CODE.
           MOVE COMM-ACCNO TO HV-PROCTRAN-ACC-NUMBER.
           MOVE EIBTASKN TO WS-EIBTASKN12.
           MOVE WS-EIBTASKN12 TO HV-PROCTRAN-REF.
           EXEC CICS ASKTIME
              ABSTIME(WS-U-TIME)
           END-EXEC.
           EXEC CICS FORMATTIME
                     ABSTIME(WS-U-TIME)
                     DDMMYYYY(WS-ORIG-DATE)
                     TIME(HV-PROCTRAN-TIME)
                     DATESEP('.')
           END-EXEC.
           MOVE WS-ORIG-DATE TO WS-ORIG-DATE-GRP-X.
           MOVE WS-ORIG-DATE-GRP-X TO HV-PROCTRAN-DATE.
           MOVE SPACES TO HV-PROCTRAN-DESC.
           IF COMM-AMT < 0
              MOVE 'DEB' TO HV-PROCTRAN-TYPE
              MOVE 'COUNTER WTHDRW' TO HV-PROCTRAN-DESC
              IF COMM-FACILTYPE = 496
                 MOVE 'PDR' TO HV-PROCTRAN-TYPE
                 MOVE COMM-ORIGIN(1:14) TO
                    HV-PROCTRAN-DESC
              END-IF
           ELSE
              MOVE 'CRE' TO HV-PROCTRAN-TYPE
              MOVE 'COUNTER RECVED' TO HV-PROCTRAN-DESC
              IF COMM-FACILTYPE = 496
                 MOVE 'PCR' TO HV-PROCTRAN-TYPE
                 MOVE COMM-ORIGIN(1:14) TO
                    HV-PROCTRAN-DESC
              END-IF
           END-IF.
           MOVE COMM-AMT TO HV-PROCTRAN-AMOUNT.
           EXEC SQL
              INSERT INTO PROCTRAN
                     (
                      PROCTRAN_EYECATCHER,
                      PROCTRAN_SORTCODE,
                      PROCTRAN_NUMBER,
                      PROCTRAN_DATE,
                      PROCTRAN_TIME,
                      PROCTRAN_REF,
                      PROCTRAN_TYPE,
                      PROCTRAN_DESC,
                      PROCTRAN_AMOUNT
                     )
              VALUES
                     (
                      :HV-PROCTRAN-EYECATCHER,
                      :HV-PROCTRAN-SORT-CODE,
                      :HV-PROCTRAN-ACC-NUMBER,
                      :HV-PROCTRAN-DATE,
                      :HV-PROCTRAN-TIME,
                      :HV-PROCTRAN-REF,
                      :HV-PROCTRAN-TYPE,
                      :HV-PROCTRAN-DESC,
                      :HV-PROCTRAN-AMOUNT
                     )
           END-EXEC.
           IF SQLCODE NOT = 0
              MOVE SQLCODE TO SQLCODE-DISPLAY
              DISPLAY 'UNABLE TO WRITE TO PROCTRAN DB2 DATASTORE'
              ' SQLCODE=' SQLCODE-DISPLAY
              'WITH THE FOLLOWING DATA:' HOST-PROCTRAN-ROW
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
                 STRING 'WTPD010 - COULD NOT ROLL BACK, POSSIBLE DATA'
                       DELIMITED BY SIZE,
                       'INTEGRITY ISSUE BETWEEN ACCOUNT AND PROCTRAN '
                       DELIMITED BY SIZE,
                       'FOR ROW:' DELIMITED BY SIZE,
                       HOST-PROCTRAN-ROW DELIMITED BY SIZE,
                       'EIBRESP=' DELIMITED BY SIZE,
                       ABND-RESPCODE DELIMITED BY SIZE,
                       ' RESP2=' DELIMITED BY SIZE,
                       ABND-RESP2CODE DELIMITED BY SIZE
                       INTO ABND-FREEFORM
                 END-STRING
                 EXEC CICS LINK PROGRAM(WS-ABEND-PGM)
                           COMMAREA(ABNDINFO-REC)
                 END-EXEC
                 DISPLAY ' COULD NOT ROLL BACK, POSSIBLE DATA '
                    'INTEGRITY ISSUE BETWEEN ACCOUNT AND PROCTRAN '
                    'FOR ROW:' HOST-PROCTRAN-ROW
                    'RESP CODE=' WS-CICS-RESP ' RESP2 CODE='
                    WS-CICS-RESP2
                 EXEC CICS ABEND
                    ABCODE ('HROL')
                    CANCEL
                 END-EXEC
              END-IF
              MOVE 'N' TO COMM-SUCCESS
              MOVE '02' TO COMM-FAIL-CODE
              PERFORM CHECK-FOR-STORM-DRAIN-DB2
           ELSE
              MOVE 'Y' TO COMM-SUCCESS
              MOVE '0' TO COMM-FAIL-CODE
           END-IF.
       WTPD999.
           EXIT.
       GET-ME-OUT-OF-HERE SECTION.
       GMOOH010.
           EXEC CICS RETURN
           END-EXEC.
           GOBACK.
       GMOOH999.
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
              DISPLAY 'DBCRFUN: Check-For-Storm-Drain-DB2: Storm '
                      'Drain condition (' STORM-DRAIN-CONDITION ') '
                      'has been met (' SQLCODE-DISPLAY ').'
           ELSE
              CONTINUE
           END-IF.
       CFSDD999.
           EXIT.
       ABEND-HANDLING SECTION.
       AH010.
           EXEC CICS ASSIGN ABCODE(MY-ABEND-CODE)
           END-EXEC.
           EVALUATE MY-ABEND-CODE
              WHEN 'AD2Z'
                 MOVE SQLCODE TO SQLCODE-DISPLAY
                 DISPLAY 'DB2 DEADLOCK DETECTED IN DBCRFUN, SQLCODE='
                    SQLCODE-DISPLAY
                 DISPLAY 'DB2 DEADLOCK FOR ACCOUNT '
                   HV-ACCOUNT-ACC-NO
                 DISPLAY  'SQLSTATE=' SQLSTATE
                        ',SQLERRMC=' SQLERRMC(1:SQLERRML)
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
                 DISPLAY 'DBCRFUN: Check-For-Storm-Drain-VSAM: Storm '
                        'Drain condition (Abend ' MY-ABEND-CODE ') '
                        'has been met.'
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
                    STRING 'AH010 - Unable to perform Synpoint '
                       DELIMITED BY SIZE,
                       'Rollback.' DELIMITED BY SIZE,
                       ' Possible Integrity issue following VSAM RLS '
                       DELIMITED BY SIZE,
                       'abend.' DELIMITED BY SIZE,
                       'EIBRESP=' DELIMITED BY SIZE,
                       ABND-RESPCODE DELIMITED BY SIZE,
                       ' RESP2=' DELIMITED BY SIZE,
                       ABND-RESP2CODE DELIMITED BY SIZE
                       INTO ABND-FREEFORM
                    END-STRING
                    EXEC CICS LINK PROGRAM(WS-ABEND-PGM)
                           COMMAREA(ABNDINFO-REC)
                    END-EXEC
                    DISPLAY 'DBCRFUN: Unable to perform Synpoint '
                       'Rollback.'
                       ' Possible Integrity issue following VSAM RLS'
                       ' abend'
                       ' RESP CODE=' WS-CICS-RESP
                       ' RESP2 CODE=' WS-CICS-RESP2
                    EXEC CICS ABEND
                       ABCODE ('HROL')
                       CANCEL
                    END-EXEC
                 END-IF
                 MOVE 'N' TO COMM-SUCCESS
                 MOVE '2' TO COMM-FAIL-CODE
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
       POPULATE-TIME-DATE SECTION.
       PTD10.
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
