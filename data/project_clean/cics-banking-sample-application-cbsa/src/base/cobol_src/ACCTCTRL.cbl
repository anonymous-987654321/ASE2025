       CBL CICS('SP,EDF')
       CBL SQL
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCTCTRL.
       AUTHOR. OGRADYJ.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-370.
       OBJECT-COMPUTER.  IBM-370.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 SYSIDERR-RETRY PIC 999.
       LOCAL-STORAGE SECTION.
       01 DATA-STORE-TYPE      PIC X.
          88 DATASTORE-TYPE-DLI     VALUE '1'.
          88 DATASTORE-TYPE-DB2     VALUE '2'.
          88 DATASTORE-TYPE-VSAM    VALUE 'V'.
       01 ACCOUNT-KY.
          03 REQUIRED-SORT-CODE   PIC 9(6) VALUE 0.
          03 REQUIRED-ACCT-NUMBER PIC 9(8) VALUE 0.
       01 EXIT-DB2-READ        PIC X VALUE 'N'.
       01 WS-V-RETRIED         PIC X VALUE 'N'.
       01 WS-D-RETRIED         PIC X VALUE 'N'.
       01 SQLCODE-DISPLAY                 PIC S9(8) DISPLAY
           SIGN LEADING SEPARATE.
       01 WS-PASSED-DATA.
          02 WS-TEST-KEY                             PIC X(4).
          02 WS-SORT-CODE                            PIC 9(6).
          02 WS-ACCOUNT-RANGE.
             07 WS-ACCOUNT-RANGE-TOP                PIC X.
             07 WS-ACCOUNT-RANGE-MIDDLE             PIC X.
             07 WS-ACCOUNT-RANGE-BOTTOM             PIC X.
       01 WS-SORT-DIV.
          03 WS-SORT-DIV1                    PIC XX.
          03 WS-SORT-DIV2                    PIC XX.
          03 WS-SORT-DIV3                    PIC XX.
       01 WS-DISP-ACCT-NO-VAL                PIC S9(18) DISPLAY.
       01 WS-ACCT-REC-LEN                    PIC S9(4) COMP VALUE 0.
       01 NCS-UPDATED                        PIC X VALUE 'N'.
       01 WS-EIBTASKN12                  PIC 9(12) VALUE 0.
       77 PROCTRAN-RETRY PIC 999.
       01 ACCOUNT-KY2.
          03 REQUIRED-SORT-CODE2   PIC 9(6) VALUE 0.
          03 REQUIRED-ACCT-NUMBER2 PIC 9(10) VALUE 0.
       01 ACCOUNT-KY2-BYTES REDEFINES ACCOUNT-KY2 PIC X(16).
       01 HIGHEST-ACCT-NUMBER  PIC 9(10) VALUE 0.
       01 WS-CICS-RESP PIC S9(8) BINARY.
       01 WS-CICS-RESP2 PIC S9(8) BINARY.
       EXEC SQL
          INCLUDE SQLCA
       END-EXEC.
       01 HV-NUMBER-OF-ACCOUNTS PIC S9(8) BINARY.
       01 HV-ACCOUNT-SORTCODE PIC X(6).
       01 WS-U-TIME                      PIC S9(15) COMP-3.
       01 WS-ORIG-DATE                   PIC X(10).
       01 WS-ORIG-DATE-GRP REDEFINES WS-ORIG-DATE.
          03 WS-ORIG-DATE-DD             PIC 99.
          03 FILLER                      PIC X.
          03 WS-ORIG-DATE-MM             PIC 99.
          03 FILLER                      PIC X.
          03 WS-ORIG-DATE-YYYY           PIC 9999.
       01 WS-ORIG-DATE-GRP-X.
          03 WS-ORIG-DATE-DD-X           PIC XX.
          03 FILLER                      PIC X VALUE '.'.
          03 WS-ORIG-DATE-MM-X           PIC XX.
          03 FILLER                      PIC X VALUE '.'.
          03 WS-ORIG-DATE-YYYY-X         PIC X(4).
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
       77 SORTCODE           PIC 9(6) VALUE 987654.
       LINKAGE SECTION.
       01 DFHCOMMAREA.
              03 ACCOUNT-CONTROL-RECORD.
                 05 ACCOUNT-CONTROL-EYE-CATCHER        PIC X(4).
                 88 ACCOUNT-CONTROL-EYECATCHER-V   VALUE 'CTRL'.
                 05 FILLER                     PIC 9(10).
                 05 ACCOUNT-CONTROL-KEY.
                    07 ACCOUNT-CONTROL-SORT-CODE       PIC 9(6).
                    07 ACCOUNT-CONTROL-NUMBER          PIC 9(8).
                 05 NUMBER-OF-ACCOUNTS                 PIC 9(8).
                 05 LAST-ACCOUNT-NUMBER                PIC 9(8).
                 05 ACCOUNT-CONTROL-SUCCESS-FLAG       PIC X.
                 88 ACCOUNT-CONTROL-SUCCESS VALUE 'Y'.
                 05 ACCOUNT-CONTROL-FAIL-CODE PIC X.
                 05 FILLER                     PIC 9(4)V99.
                 05 FILLER                     PIC 9(8).
                 05 FILLER                     PIC 9(8).
                 05 FILLER                     PIC 9(8).
                 05 FILLER                     PIC 9(8).
                 05 FILLER                     PIC S9(10)V99.
                 05 FILLER                     PIC X(2).
       PROCEDURE DIVISION USING DFHCOMMAREA.
       PREMIERE SECTION.
       P010.
           MOVE SORTCODE TO
              REQUIRED-SORT-CODE.
           PERFORM GET-NUMBER-OF-ACCOUNTS-DB2
           PERFORM GET-ME-OUT-OF-HERE.
       P999.
           EXIT.
       GET-NUMBER-OF-ACCOUNTS-DB2 SECTION.
       WCD010.
           INITIALIZE DFHCOMMAREA.
           MOVE REQUIRED-SORT-CODE TO HV-ACCOUNT-SORTCODE
           EXEC SQL
              SELECT COUNT(*)
              INTO  :HV-NUMBER-OF-ACCOUNTS
              FROM ACCOUNT
              WHERE ACCOUNT_SORTCODE = :HV-ACCOUNT-SORTCODE
           END-EXEC.
           IF SQLCODE = ZERO
             MOVE 'Y' TO ACCOUNT-CONTROL-SUCCESS-FLAG
             MOVE HV-NUMBER-OF-ACCOUNTS TO NUMBER-OF-ACCOUNTS
           ELSE
             MOVE 'N' TO ACCOUNT-CONTROL-SUCCESS-FLAG
             MOVE SQLCODE TO SQLCODE-DISPLAY
           END-IF.
       WCD999.
           EXIT.
      /
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
       GET-ME-OUT-OF-HERE SECTION.
       GMOFH010.
           EXEC CICS RETURN
           END-EXEC.
       GMOFH999.
           EXIT.
