       PROCESS CICS,NODYNAM,NSYMBOL(NATIONAL),TRUNC(STD)
       CBL CICS('SP,EDF,DLI')
       CBL SQL
       IDENTIFICATION DIVISION.
       PROGRAM-ID. UPDACC.
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
          INCLUDE SQLCA
       END-EXEC.
       01 SQLCODE-DISPLAY             PIC S9(8) DISPLAY
            SIGN LEADING SEPARATE.
       01 WS-CICS-WORK-AREA.
          05 WS-CICS-RESP               PIC S9(8) COMP.
          05 WS-CICS-RESP2              PIC S9(8) COMP.
       LOCAL-STORAGE SECTION.
       01 DB2-DATE-REFORMAT.
          03 DB2-DATE-REF-YR            PIC 9(4).
          03 FILLER                     PIC X.
          03 DB2-DATE-REF-MNTH          PIC 99.
          03 FILLER                     PIC X.
          03 DB2-DATE-REF-DAY           PIC 99.
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
       01 REJ-REASON                    PIC XX        VALUE SPACES.
       01 CUSTOMER-KY.
          03 REQUIRED-SORT-CODE         PIC 9(6)      VALUE 0.
          03 REQUIRED-ACC-NUM           PIC 9(8)      VALUE 0.
       01 STORM-DRAIN-CONDITION         PIC X(20).
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
       LINKAGE SECTION.
       01 DFHCOMMAREA.
          03 COMM-EYE                  PIC X(4).
          03 COMM-CUSTNO               PIC X(10).
          03 COMM-SCODE                PIC X(6).
          03 COMM-ACCNO                PIC 9(8).
          03 COMM-ACC-TYPE             PIC X(8).
          03 COMM-INT-RATE             PIC 9(4)V99.
          03 COMM-OPENED               PIC 9(8).
          03 COMM-OPENED-GROUP REDEFINES COMM-OPENED.
             05 COMM-OPENED-DAY              PIC 99.
             05 COMM-OPENED-MONTH            PIC 99.
             05 COMM-OPENED-YEAR             PIC 9999.
          03 COMM-OVERDRAFT            PIC 9(8).
          03 COMM-LAST-STMT-DT         PIC 9(8).
          03 COMM-LAST-STMNT-GROUP REDEFINES COMM-LAST-STMT-DT.
             05 COMM-LASTST-DAY               PIC 99.
             05 COMM-LASTST-MONTH             PIC 99.
             05 COMM-LASTST-YEAR              PIC 9999.
          03 COMM-NEXT-STMT-DT         PIC 9(8).
          03 COMM-NEXT-STMNT-GROUP REDEFINES COMM-NEXT-STMT-DT.
             05 COMM-NEXTST-DAY               PIC 99.
             05 COMM-NEXTST-MONTH             PIC 99.
             05 COMM-NEXTST-YEAR              PIC 9999.
          03 COMM-AVAIL-BAL            PIC S9(10)V99.
          03 COMM-ACTUAL-BAL           PIC S9(10)V99.
          03 COMM-SUCCESS              PIC X.
       PROCEDURE DIVISION.
       PREMIERE SECTION.
       A010.
           MOVE SORTCODE TO COMM-SCODE.
           MOVE SORTCODE TO DESIRED-SORT-CODE.
           PERFORM UPDATE-ACCOUNT-DB2
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
              MOVE SQLCODE TO SQLCODE-DISPLAY
              DISPLAY 'ERROR: UPDACC returned ' SQLCODE-DISPLAY
              ' on SELECT'
              GO TO UAD999
           END-IF.
           IF (COMM-ACC-TYPE = SPACES OR COMM-ACC-TYPE(1:1) = ' ')
              MOVE 'N' TO COMM-SUCCESS
              DISPLAY 'ERROR: UPDACC has invalid account-type'
              GO TO UAD999
           END-IF.
           MOVE COMM-ACC-TYPE  TO HV-ACCOUNT-ACC-TYPE.
           MOVE COMM-OVERDRAFT TO HV-ACCOUNT-OVERDRAFT-LIM.
           MOVE COMM-INT-RATE  TO HV-ACCOUNT-INT-RATE.
           EXEC SQL
              UPDATE ACCOUNT
              SET ACCOUNT_TYPE = :HV-ACCOUNT-ACC-TYPE,
                  ACCOUNT_INTEREST_RATE = :HV-ACCOUNT-INT-RATE,
                  ACCOUNT_OVERDRAFT_LIMIT = :HV-ACCOUNT-OVERDRAFT-LIM
              WHERE (ACCOUNT_SORTCODE = :HV-ACCOUNT-SORTCODE AND
                     ACCOUNT_NUMBER = :HV-ACCOUNT-ACC-NO)
           END-EXEC.
           IF SQLCODE NOT = 0
              MOVE 'N' TO COMM-SUCCESS
              MOVE SQLCODE TO SQLCODE-DISPLAY
              DISPLAY 'ERROR: UPDACC returned ' SQLCODE-DISPLAY
              ' on UPDATE'
              GO TO UAD999
           END-IF.
           MOVE HV-ACCOUNT-EYECATCHER TO COMM-EYE.
           MOVE HV-ACCOUNT-CUST-NO    TO COMM-CUSTNO.
           MOVE HV-ACCOUNT-SORTCODE   TO COMM-SCODE.
           MOVE HV-ACCOUNT-ACC-NO     TO COMM-ACCNO.
           MOVE HV-ACCOUNT-ACC-TYPE   TO COMM-ACC-TYPE.
           MOVE HV-ACCOUNT-INT-RATE   TO COMM-INT-RATE.
           INITIALIZE DB2-DATE-REFORMAT.
           MOVE HV-ACCOUNT-OPENED     TO DB2-DATE-REFORMAT.
           MOVE DB2-DATE-REF-YR       TO COMM-OPENED-YEAR.
           MOVE DB2-DATE-REF-MNTH     TO COMM-OPENED-MONTH.
           MOVE DB2-DATE-REF-DAY      TO COMM-OPENED-DAY.
           MOVE HV-ACCOUNT-OVERDRAFT-LIM TO COMM-OVERDRAFT.
           INITIALIZE DB2-DATE-REFORMAT.
           MOVE HV-ACCOUNT-LAST-STMT  TO DB2-DATE-REFORMAT.
           MOVE DB2-DATE-REF-YR       TO COMM-LASTST-YEAR.
           MOVE DB2-DATE-REF-MNTH     TO COMM-LASTST-MONTH.
           MOVE DB2-DATE-REF-DAY      TO COMM-LASTST-DAY.
           INITIALIZE DB2-DATE-REFORMAT.
           MOVE HV-ACCOUNT-NEXT-STMT  TO DB2-DATE-REFORMAT.
           MOVE DB2-DATE-REF-YR       TO COMM-NEXTST-YEAR.
           MOVE DB2-DATE-REF-MNTH     TO COMM-NEXTST-MONTH.
           MOVE DB2-DATE-REF-DAY      TO COMM-NEXTST-DAY.
           MOVE HV-ACCOUNT-AVAIL-BAL  TO COMM-AVAIL-BAL.
           MOVE HV-ACCOUNT-ACTUAL-BAL TO COMM-ACTUAL-BAL.
           MOVE 'Y' TO COMM-SUCCESS.
       UAD999.
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
