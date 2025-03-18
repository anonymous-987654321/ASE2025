       CBL CICS('SP,EDF')
       CBL SQL
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DELCUS.
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
       01 SYSIDERR-RETRY               PIC 999.
       01 FILE-RETRY                   PIC 999.
       01 WS-EXIT-RETRY-LOOP           PIC X VALUE ' '.
           EXEC SQL
              INCLUDE PROCDB2
           END-EXEC.
       01 HOST-PROCTRAN-ROW.
           03 HV-PROCTRAN-EYECATCHER   PIC X(4).
           03 HV-PROCTRAN-SORT-CODE    PIC X(6).
           03 HV-PROCTRAN-ACC-NUMBER   PIC X(8).
           03 HV-PROCTRAN-DATE         PIC X(10).
           03 HV-PROCTRAN-TIME         PIC X(6).
           03 HV-PROCTRAN-REF          PIC X(12).
           03 HV-PROCTRAN-TYPE         PIC X(3).
           03 HV-PROCTRAN-DESC         PIC X(40).
           03 HV-PROCTRAN-AMOUNT       PIC S9(10)V99 COMP-3.
       EXEC SQL
          INCLUDE SQLCA
       END-EXEC.
       01 WS-CICS-WORK-AREA.
           05 WS-CICS-RESP             PIC S9(8) COMP.
           05 WS-CICS-RESP2            PIC S9(8) COMP.
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
       01 OUTPUT-CUST-DATA.
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
       01 PROCTRAN-RIDFLD                 PIC S9(8) COMP.
       77 PROCTRAN-RETRY                  PIC 999.
       01 ACCOUNT-ACT-BAL-STORE           PIC S9(10)V99 VALUE 0.
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
       01 ACCTCUST-DESIRED-KEY PIC 9(10) BINARY.
       01 DB2-DATE-REFORMAT.
          03 DB2-DATE-REF-YR           PIC 9(4).
          03 FILLER                    PIC X.
          03 DB2-DATE-REF-MNTH         PIC 99.
          03 FILLER                    PIC X.
          03 DB2-DATE-REF-DAY          PIC 99.
       01 DB2-EXIT-LOOP                PIC X.
       01 FETCH-DATA-CNT               PIC 9(4) COMP.
       01 WS-CUST-ALT-KEY-LEN          PIC S9(4) COMP VALUE +10.
       01 WS-EIBTASKN12                PIC 9(12) VALUE 0.
       01 WS-CNT                       PIC S9(4) COMP VALUE 0.
       01 CUSTOMER-KY.
          03 REQUIRED-SORT-CODE        PIC 9(6)  VALUE 0.
       01 WS-ACC-KEY-LEN               PIC S9(4) COMP VALUE +14.
       01 WS-ACC-NUM                   PIC S9(4) COMP VALUE 0.
       01 WS-CUST-KEY-LEN              PIC S9(4) COMP VALUE +16.
       01 WS-CUST-NUM                  PIC S9(4) COMP VALUE 0.
       01 DESIRED-KEY-ACCTCUST.
           03 DESIRED-KEY-CUSTOMER-ACCTCUST   PIC 9(10).
           03 DESIRED-KEY-SORTCODE-ACCTCUST   PIC 9(6).
       01 DESIRED-KEY.
           03 DESIRED-KEY-SORTCODE     PIC 9(6).
           03 DESIRED-KEY-CUSTOMER     PIC 9(10).
       01 WS-U-TIME                    PIC S9(15) COMP-3.
       01 WS-ORIG-DATE                 PIC X(10).
       01 WS-ORIG-DATE-GRP REDEFINES WS-ORIG-DATE.
          03 WS-ORIG-DATE-DD              PIC 99.
          03 FILLER                       PIC X.
          03 WS-ORIG-DATE-MM              PIC 99.
          03 FILLER                       PIC X.
          03 WS-ORIG-DATE-YYYY            PIC 9999.
       01 WS-ORIG-DATE-GRP-X.
          03 WS-ORIG-DATE-DD-X          PIC XX.
          03 FILLER                     PIC X VALUE '.'.
          03 WS-ORIG-DATE-MM-X          PIC XX.
          03 FILLER                     PIC X VALUE '.'.
          03 WS-ORIG-DATE-YYYY-X        PIC X(4).
       01 REMIX-STMT-DATE              PIC 9(8).
       01 WS-APPLID                    PIC X(8).
       01 VAR-REMIX.
          03 REMIX-SCODE               PIC 9(6).
          03 REMIX-CHAR REDEFINES REMIX-SCODE.
             05 REMIX-SCODE-CHAR           PIC X(6).
       01 WS-STOREDC-CUSTOMER.
          03 WS-STOREDC-EYECATCHER           PIC X(4).
          03 WS-STOREDC-SORTCODE             PIC 9(6).
          03 WS-STOREDC-NUMBER               PIC 9(10).
          03 WS-STOREDC-NAME                 PIC X(60).
          03 WS-STOREDC-ADDRESS              PIC X(160).
          03 WS-STOREDC-DATE-OF-BIRTH        PIC X(10).
          03 WS-STOREDC-CREDIT-SCORE         PIC 9(3).
          03 WS-STOREDC-CS-REVIEW-DATE       PIC X(10).
       01 WS-NONE-LEFT                 PIC X VALUE 'N'.
       01 WS-EXIT-FETCH                PIC X VALUE 'N'.
       01 SQLCODE-DISPLAY              PIC S9(8) DISPLAY
                                         SIGN LEADING SEPARATE.
       01 DELACC-COMMAREA.
          03 DELACC-COMM-EYE           PIC X(4).
          03 DELACC-COMM-CUSTNO        PIC X(10).
          03 DELACC-COMM-SCODE         PIC X(6).
          03 DELACC-COMM-ACCNO         PIC 9(8).
          03 DELACC-COMM-ACC-TYPE      PIC X(8).
          03 DELACC-COMM-INT-RATE      PIC 9(4)V99.
          03 DELACC-COMM-OPENED        PIC 9(8).
          03 DELACC-COMM-OVERDRAFT     PIC 9(8).
          03 DELACC-COMM-LAST-STMT-DT  PIC 9(8).
          03 DELACC-COMM-NEXT-STMT-DT  PIC 9(8).
          03 DELACC-COMM-AVAIL-BAL     PIC S9(10)V99.
          03 DELACC-COMM-ACTUAL-BAL    PIC S9(10)V99.
          03 DELACC-COMM-SUCCESS       PIC X.
          03 DELACC-COMM-FAIL-CD       PIC X.
          03 DELACC-COMM-DEL-SUCCESS   PIC X.
          03 DELACC-COMM-DEL-FAIL-CD   PIC X.
          03 DELACC-COMM-APPLID        PIC X(8).
          03 DELACC-COMM-PCB1          POINTER.
          03 DELACC-COMM-PCB2          POINTER.
       01 WS-TOKEN                     PIC S9(8) BINARY.
       01 WS-INDEX                     PIC S9(8) BINARY.
       01 INQACCCU-PROGRAM         PIC X(8) VALUE 'INQACCCU'.
       01 INQACCCU-COMMAREA.
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
       01 STORM-DRAIN-CONDITION       PIC X(20).
       01 INQCUST-PROGRAM          PIC X(8) VALUE 'INQCUST '.
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
       01 WS-TIME-DATA.
           03 WS-TIME-NOW                  PIC 9(6).
           03 WS-TIME-NOW-GRP REDEFINES WS-TIME-NOW.
              05 WS-TIME-NOW-GRP-HH     PIC 99.
              05 WS-TIME-NOW-GRP-MM     PIC 99.
              05 WS-TIME-NOW-GRP-SS     PIC 99.
       01 WS-ABEND-PGM                  PIC X(8) VALUE 'ABNDPROC'.
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
          03 COMM-SCODE                PIC X(6).
          03 COMM-CUSTNO               PIC X(10).
          03 COMM-NAME                 PIC X(60).
          03 COMM-ADDR                 PIC X(160).
          03 COMM-DOB                  PIC 9(8).
          03 COMM-DOB-GROUP REDEFINES COMM-DOB.
             05 COMM-BIRTH-DAY               PIC 99.
             05 COMM-BIRTH-MONTH             PIC 99.
             05 COMM-BIRTH-YEAR              PIC 9999.
          03 COMM-CREDIT-SCORE         PIC 9(3).
          03 COMM-CS-REVIEW-DATE       PIC 9(8).
          03 COMM-CS-REVIEW-GROUP REDEFINES COMM-CS-REVIEW-DATE.
             05 COMM-CS-REVIEW-DD            PIC 99.
             05 COMM-CS-REVIEW-MM            PIC 99.
             05 COMM-CS-REVIEW-YYYY          PIC 9999.
          03 COMM-DEL-SUCCESS          PIC X.
          03 COMM-DEL-FAIL-CD          PIC X.
       PROCEDURE DIVISION USING DFHCOMMAREA.
       PREMIERE SECTION.
       A010.
           MOVE SORTCODE TO REQUIRED-SORT-CODE
                            REQUIRED-SORT-CODE OF CUSTOMER-KY
                            DESIRED-KEY-SORTCODE.
           MOVE COMM-CUSTNO OF DFHCOMMAREA
             TO DESIRED-KEY-CUSTOMER.
           INITIALIZE INQCUST-COMMAREA.
           MOVE COMM-CUSTNO OF DFHCOMMAREA TO
              INQCUST-CUSTNO.
           EXEC CICS LINK PROGRAM(INQCUST-PROGRAM)
                     COMMAREA(INQCUST-COMMAREA)
           END-EXEC.
           IF INQCUST-INQ-SUCCESS = 'N'
             MOVE 'N' TO COMM-DEL-SUCCESS
             MOVE INQCUST-INQ-FAIL-CD TO COMM-DEL-FAIL-CD
             EXEC CICS RETURN
             END-EXEC
           END-IF.
           PERFORM GET-ACCOUNTS
           IF NUMBER-OF-ACCOUNTS > 0
             PERFORM DELETE-ACCOUNTS
           END-IF
           PERFORM DEL-CUST-VSAM
           MOVE 'Y' TO COMM-DEL-SUCCESS.
           MOVE ' ' TO COMM-DEL-FAIL-CD.
           PERFORM GET-ME-OUT-OF-HERE.
       A999.
           EXIT.
       DELETE-ACCOUNTS SECTION.
       DA010.
           PERFORM VARYING WS-INDEX FROM 1 BY 1
           UNTIL WS-INDEX > NUMBER-OF-ACCOUNTS
              INITIALIZE DELACC-COMMAREA
              MOVE WS-APPLID TO DELACC-COMM-APPLID
              MOVE COMM-ACCNO(WS-INDEX) TO DELACC-COMM-ACCNO
              EXEC CICS LINK PROGRAM('DELACC  ')
                       COMMAREA(DELACC-COMMAREA)
              END-EXEC
           END-PERFORM.
       DA999.
           EXIT.
       GET-ACCOUNTS SECTION.
       GAC010.
           MOVE COMM-CUSTNO OF DFHCOMMAREA
              TO CUSTOMER-NUMBER OF INQACCCU-COMMAREA.
           MOVE 20 TO NUMBER-OF-ACCOUNTS IN INQACCCU-COMMAREA.
           SET COMM-PCB-POINTER OF INQACCCU-COMMAREA
              TO DELACC-COMM-PCB1
           EXEC CICS LINK PROGRAM('INQACCCU')
                     COMMAREA(INQACCCU-COMMAREA)
                     SYNCONRETURN
           END-EXEC.
       GAC999.
           EXIT.
       DEL-CUST-VSAM SECTION.
       DCV010.
           INITIALIZE OUTPUT-CUST-DATA.
           EXEC CICS READ FILE('CUSTOMER')
                RIDFLD(DESIRED-KEY)
                INTO(OUTPUT-CUST-DATA)
                UPDATE
                TOKEN(WS-TOKEN)
                RESP(WS-CICS-RESP)
                RESP2(WS-CICS-RESP2)
           END-EXEC.
           IF WS-CICS-RESP = DFHRESP(SYSIDERR)
              PERFORM VARYING SYSIDERR-RETRY FROM 1 BY 1
              UNTIL SYSIDERR-RETRY > 100
              OR WS-CICS-RESP = DFHRESP(NORMAL)
              OR WS-CICS-RESP IS NOT EQUAL TO DFHRESP(SYSIDERR)
                 EXEC CICS DELAY FOR SECONDS(3)
                 END-EXEC
                 EXEC CICS READ FILE('CUSTOMER')
                    RIDFLD(DESIRED-KEY)
                    INTO(OUTPUT-CUST-DATA)
                    UPDATE
                    TOKEN(WS-TOKEN)
                    RESP(WS-CICS-RESP)
                    RESP2(WS-CICS-RESP2)
                 END-EXEC
              END-PERFORM
           END-IF
           IF WS-CICS-RESP = DFHRESP(NOTFND)
              GO TO DCV999
           END-IF
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
              MOVE 'WPV6'      TO ABND-CODE
              EXEC CICS ASSIGN PROGRAM(ABND-PROGRAM)
              END-EXEC
              MOVE ZEROS    TO ABND-SQLCODE
              STRING 'DCV010 - Unable to READ CUSTOMER VSAM rec '
                    DELIMITED BY SIZE,
                    'for key:' DESIRED-KEY DELIMITED BY SIZE,
                    ' EIBRESP=' DELIMITED BY SIZE,
                    ABND-RESPCODE DELIMITED BY SIZE,
                    ' RESP2=' DELIMITED BY SIZE,
                    ABND-RESP2CODE DELIMITED BY SIZE
                    INTO ABND-FREEFORM
              END-STRING
              EXEC CICS LINK PROGRAM(WS-ABEND-PGM)
                        COMMAREA(ABNDINFO-REC)
              END-EXEC
              DISPLAY 'In DELCUS (DCV010) '
              'UNABLE TO READ CUSTOMER VSAM REC'
              ' RESP CODE=' WS-CICS-RESP, ' RESP2=' WS-CICS-RESP2
              'FOR KEY=' DESIRED-KEY
              EXEC CICS ABEND
                 ABCODE ('WPV6')
              END-EXEC
           END-IF.
           MOVE CUSTOMER-EYECATCHER TO WS-STOREDC-EYECATCHER
                                        COMM-EYE IN DFHCOMMAREA.
           MOVE CUSTOMER-SORTCODE   TO WS-STOREDC-SORTCODE
                                       COMM-SCODE IN DFHCOMMAREA.
           MOVE CUSTOMER-NUMBER OF CUSTOMER-RECORD
              TO WS-STOREDC-NUMBER COMM-CUSTNO IN DFHCOMMAREA.
           MOVE CUSTOMER-NAME       TO WS-STOREDC-NAME
                                       COMM-NAME IN DFHCOMMAREA.
           MOVE CUSTOMER-ADDRESS    TO WS-STOREDC-ADDRESS
                                       COMM-ADDR IN DFHCOMMAREA.
           MOVE CUSTOMER-DATE-OF-BIRTH(1:2)
              TO WS-STOREDC-DATE-OF-BIRTH(1:2)
                 COMM-BIRTH-DAY IN DFHCOMMAREA.
           MOVE '/'                 TO WS-STOREDC-DATE-OF-BIRTH(3:1).
           MOVE CUSTOMER-DATE-OF-BIRTH(3:2)
              TO WS-STOREDC-DATE-OF-BIRTH(4:2)
                 COMM-BIRTH-MONTH IN DFHCOMMAREA.
           MOVE '/'                 TO WS-STOREDC-DATE-OF-BIRTH(6:1).
           MOVE CUSTOMER-DATE-OF-BIRTH(5:4)
              TO WS-STOREDC-DATE-OF-BIRTH(7:4)
                 COMM-BIRTH-YEAR IN DFHCOMMAREA.
           MOVE CUSTOMER-CREDIT-SCORE TO WS-STOREDC-CREDIT-SCORE
                                         COMM-CREDIT-SCORE.
           MOVE CUSTOMER-CS-REVIEW-DATE(1:2)
             TO WS-STOREDC-CS-REVIEW-DATE(1:2)
                COMM-CS-REVIEW-DD IN DFHCOMMAREA.
           MOVE '/'                 TO WS-STOREDC-CS-REVIEW-DATE(3:1).
           MOVE CUSTOMER-CS-REVIEW-DATE(3:2)
             TO WS-STOREDC-CS-REVIEW-DATE(4:2)
                COMM-CS-REVIEW-MM IN DFHCOMMAREA.
           MOVE '/'                 TO WS-STOREDC-CS-REVIEW-DATE(6:1).
           MOVE CUSTOMER-CS-REVIEW-DATE(5:4)
             TO WS-STOREDC-CS-REVIEW-DATE(7:4)
                COMM-CS-REVIEW-YYYY IN DFHCOMMAREA.
           EXEC CICS
              DELETE FILE ('CUSTOMER')
              TOKEN(WS-TOKEN)
              RESP(WS-CICS-RESP)
              RESP2(WS-CICS-RESP2)
           END-EXEC.
           IF WS-CICS-RESP = DFHRESP(SYSIDERR)
              PERFORM VARYING SYSIDERR-RETRY FROM 1 BY 1
              UNTIL SYSIDERR-RETRY > 100
              OR WS-CICS-RESP = DFHRESP(NORMAL)
              OR WS-CICS-RESP IS NOT EQUAL TO DFHRESP(SYSIDERR)
                 EXEC CICS DELAY FOR SECONDS(3)
                 END-EXEC
                 EXEC CICS DELETE FILE ('CUSTOMER')
                    TOKEN(WS-TOKEN)
                    RESP(WS-CICS-RESP)
                    RESP2(WS-CICS-RESP2)
                 END-EXEC
              END-PERFORM
           END-IF
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
              MOVE 'WPV7'      TO ABND-CODE
              EXEC CICS ASSIGN PROGRAM(ABND-PROGRAM)
              END-EXEC
              MOVE ZEROS    TO ABND-SQLCODE
              STRING 'DCV010(2) - Unbale to DELETE CUSTOMER VSAM rec '
                    DELIMITED BY SIZE,
                    'for key:' DESIRED-KEY DELIMITED BY SIZE,
                    ' EIBRESP=' DELIMITED BY SIZE,
                    ABND-RESPCODE DELIMITED BY SIZE,
                    ' RESP2=' DELIMITED BY SIZE,
                    ABND-RESP2CODE DELIMITED BY SIZE
                    INTO ABND-FREEFORM
              END-STRING
              EXEC CICS LINK PROGRAM(WS-ABEND-PGM)
                        COMMAREA(ABNDINFO-REC)
              END-EXEC
              DISPLAY 'In DELCUS (DCV010) '
              'UNABLE TO DELETE CUSTOMER VSAM REC'
              ' RESP CODE=' WS-CICS-RESP, ' RESP2=' WS-CICS-RESP2
              'FOR KEY=' DESIRED-KEY
              EXEC CICS ABEND
                 ABCODE ('WPV7')
              END-EXEC
           END-IF.
           PERFORM WRITE-PROCTRAN-CUST.
       DCV999.
           EXIT.
       WRITE-PROCTRAN-CUST SECTION.
       WPC010.
              PERFORM WRITE-PROCTRAN-CUST-DB2.
       WPC999.
           EXIT.
       WRITE-PROCTRAN-CUST-DB2 SECTION.
       WPCD010.
           INITIALIZE HOST-PROCTRAN-ROW.
           INITIALIZE WS-EIBTASKN12.
           MOVE 'PRTR' TO HV-PROCTRAN-EYECATCHER.
           MOVE WS-STOREDC-SORTCODE
              TO HV-PROCTRAN-SORT-CODE.
           MOVE ZEROS TO HV-PROCTRAN-ACC-NUMBER.
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
           MOVE WS-STOREDC-SORTCODE      TO HV-PROCTRAN-DESC(1:6).
           MOVE WS-STOREDC-NUMBER        TO HV-PROCTRAN-DESC(7:10).
           MOVE WS-STOREDC-NAME          TO HV-PROCTRAN-DESC(17:14).
           MOVE WS-STOREDC-DATE-OF-BIRTH TO HV-PROCTRAN-DESC(31:10).
           MOVE 'ODC'         TO HV-PROCTRAN-TYPE.
           MOVE ZEROS         TO HV-PROCTRAN-AMOUNT.
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
              MOVE 'HWPT'      TO ABND-CODE
              EXEC CICS ASSIGN PROGRAM(ABND-PROGRAM)
              END-EXEC
              MOVE SQLCODE-DISPLAY   TO ABND-SQLCODE
              STRING 'WPCD010 - Unable to WRITE to PROCTRAN DB2 '
                    DELIMITED BY SIZE,
                    'datastore with the following data:'
                    DELIMITED BY SIZE,
                    HOST-PROCTRAN-ROW DELIMITED BY SIZE,
                    ' EIBRESP=' DELIMITED BY SIZE,
                    ABND-RESPCODE DELIMITED BY SIZE,
                    ' RESP2=' DELIMITED BY SIZE,
                    ABND-RESP2CODE DELIMITED BY SIZE
                    INTO ABND-FREEFORM
              END-STRING
              EXEC CICS LINK PROGRAM(WS-ABEND-PGM)
                        COMMAREA(ABNDINFO-REC)
              END-EXEC
              DISPLAY 'In DELCUS(WPCD010) '
              'UNABLE TO WRITE TO PROCTRAN DB2 DATASTORE'
              ' SQLCODE=' SQLCODE-DISPLAY
              'WITH THE FOLLOWING DATA:' HOST-PROCTRAN-ROW
              EXEC CICS ABEND
                 ABCODE('HWPT')
                 NODUMP
              END-EXEC
           END-IF.
       WPCD999.
           EXIT.
       GET-ME-OUT-OF-HERE SECTION.
       GMOFH010.
           GOBACK.
       GMOFH999.
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
