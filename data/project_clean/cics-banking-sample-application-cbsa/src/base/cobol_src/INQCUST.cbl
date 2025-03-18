       CBL CICS('SP,EDF')
       IDENTIFICATION DIVISION.
       PROGRAM-ID. INQCUST.
       AUTHOR. Jon Collett.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-370.
       OBJECT-COMPUTER.  IBM-370.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 SYSIDERR-RETRY               PIC 999.
       77 INQCUST-RETRY                PIC 9999.
       01 WS-CICS-WORK-AREA.
          03 WS-CICS-RESP              PIC S9(8) COMP.
          03 WS-CICS-RESP2             PIC S9(8) COMP.
       LOCAL-STORAGE SECTION.
       77 SORTCODE           PIC 9(6) VALUE 987654.
       01 OUTPUT-DATA.
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
       01 CUSTOMER-KY.
          03 REQUIRED-SORT-CODE        PIC 9(6) VALUE 0.
          03 REQUIRED-CUST-NUMBER      PIC 9(10) VALUE 0.
       01 CUSTOMER-KY2.
          03 REQUIRED-SORT-CODE2       PIC 9(6) VALUE 0.
          03 REQUIRED-CUST-NUMBER2     PIC 9(10) VALUE 0.
       01 RANDOM-CUSTOMER              PIC 9(10) VALUE 0.
       01 HIGHEST-CUST-NUMBER          PIC 9(10) VALUE 0.
       01 EXIT-VSAM-READ               PIC X VALUE 'N'.
       01 EXIT-DB2-READ                PIC X VALUE 'N'.
       01 EXIT-IMS-READ                PIC X VALUE 'N'.
       01 WS-V-RETRIED                 PIC X VALUE 'N'.
       01 WS-D-RETRIED                 PIC X VALUE 'N'.
       01 WS-PROGRAM                   PIC X(8) VALUE SPACES.
       01 NCS-CUST-NO-STUFF.
          03 NCS-CUST-NO-NAME.
             05 NCS-CUST-NO-ACT-NAME   PIC X(8) VALUE 'HBNKCUST'.
             05 NCS-CUST-NO-TEST-SORT  PIC X(6) VALUE '      '.
             05 NCS-CUST-NO-FILL       PIC XX VALUE '  '.
          03 NCS-CUST-NO-INC           PIC 9(16) COMP VALUE 0.
          03 NCS-CUST-NO-VALUE         PIC 9(16) COMP VALUE 0.
          03 NCS-CUST-NO-RESP          PIC XX VALUE '00'.
       01 WS-PASSED-DATA.
          02 WS-TEST-KEY               PIC X(4).
          02 WS-SORT-CODE              PIC 9(6).
          02 WS-CUSTOMER-RANGE.
             07 WS-CUSTOMER-RANGE-TOP             PIC X.
             07 WS-CUSTOMER-RANGE-MIDDLE          PIC X.
             07 WS-CUSTOMER-RANGE-BOTTOM          PIC X.
       01 WS-SORT-DIV.
          03 WS-SORT-DIV1              PIC XX.
          03 WS-SORT-DIV2              PIC XX.
          03 WS-SORT-DIV3              PIC XX.
       01 WS-DISP-CUST-NO-VAL          PIC S9(18) DISPLAY.
       01 VAR-REMIX.
          03 REMIX-SCODE               PIC X(6).
          03 REMIX-NUM REDEFINES REMIX-SCODE.
             05 REMIX-SCODE-NUM        PIC 9(6).
       01 VAR-REMIX2.
          03 REMIX2-CREDIT-SCR         PIC X(3).
          03 REMIX2-NUM REDEFINES REMIX2-CREDIT-SCR.
             05 REMIX2-CREDIT-SCR-NUM  PIC 9(3).
       01 MY-ABEND-CODE                PIC XXXX.
       01 WS-STORM-DRAIN               PIC X VALUE 'N'.
       01 STORM-DRAIN-CONDITION        PIC X(20).
       01 SQLCODE-DISPLAY              PIC S9(8) DISPLAY
           SIGN LEADING SEPARATE.
       01 WS-INVOKING-PROGRAM         PIC X(8).
       01 WS-POINTER USAGE POINTER.
       01 WS-POINTER-BYTES   REDEFINES WS-POINTER PIC X(8).
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
              05 WS-TIME-NOW-GRP-HH    PIC 99.
              05 WS-TIME-NOW-GRP-MM    PIC 99.
              05 WS-TIME-NOW-GRP-SS    PIC 99.
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
       PROCEDURE DIVISION USING DFHCOMMAREA.
       PREMIERE SECTION.
       P010.
           EXEC CICS HANDLE ABEND
              LABEL(ABEND-HANDLING)
           END-EXEC.
           MOVE 'N' TO INQCUST-INQ-SUCCESS
           MOVE '0' TO INQCUST-INQ-FAIL-CD
           MOVE SORTCODE TO REQUIRED-SORT-CODE.
           MOVE INQCUST-CUSTNO TO REQUIRED-CUST-NUMBER.
           IF INQCUST-CUSTNO = 0000000000 OR INQCUST-CUSTNO = 9999999999
              PERFORM READ-CUSTOMER-NCS
              IF INQCUST-INQ-SUCCESS = 'Y'
                MOVE NCS-CUST-NO-VALUE TO REQUIRED-CUST-NUMBER
              ELSE
                PERFORM GET-ME-OUT-OF-HERE
              END-IF
           END-IF.
           IF INQCUST-CUSTNO = 0000000000
              PERFORM GENERATE-RANDOM-CUSTOMER
              MOVE RANDOM-CUSTOMER TO REQUIRED-CUST-NUMBER
           END-IF.
           MOVE 'N' TO EXIT-VSAM-READ.
           MOVE 'N' TO EXIT-DB2-READ.
           MOVE 'N' TO WS-D-RETRIED.
           MOVE 'N' TO WS-V-RETRIED.
           PERFORM READ-CUSTOMER-VSAM
             UNTIL EXIT-VSAM-READ = 'Y'.
           IF INQCUST-INQ-SUCCESS = 'Y'
             MOVE '0' TO INQCUST-INQ-FAIL-CD
             MOVE CUSTOMER-EYECATCHER OF OUTPUT-DATA
                TO INQCUST-EYE
             MOVE CUSTOMER-SORTCODE OF OUTPUT-DATA
                TO INQCUST-SCODE
             MOVE CUSTOMER-NUMBER OF OUTPUT-DATA
                TO INQCUST-CUSTNO
             MOVE CUSTOMER-NAME OF OUTPUT-DATA
                TO INQCUST-NAME
             MOVE CUSTOMER-ADDRESS OF OUTPUT-DATA
                TO INQCUST-ADDR
             MOVE CUSTOMER-DATE-OF-BIRTH OF OUTPUT-DATA
                TO INQCUST-DOB
             MOVE CUSTOMER-CREDIT-SCORE OF OUTPUT-DATA
                TO INQCUST-CREDIT-SCORE
             MOVE CUSTOMER-CS-REVIEW-DATE OF OUTPUT-DATA
                TO INQCUST-CS-REVIEW-DT
           END-IF.
           PERFORM GET-ME-OUT-OF-HERE.
       P999.
           EXIT.
       READ-CUSTOMER-NCS SECTION.
       RCN010.
           PERFORM GET-LAST-CUSTOMER-VSAM
           IF INQCUST-INQ-SUCCESS = 'Y'
             MOVE REQUIRED-CUST-NUMBER2 TO NCS-CUST-NO-VALUE
           END-IF.
       RCN999.
           EXIT.
       READ-CUSTOMER-VSAM SECTION.
       RCV010.
           INITIALIZE OUTPUT-DATA.
           EXEC CICS READ FILE('CUSTOMER')
                RIDFLD(CUSTOMER-KY)
                INTO(OUTPUT-DATA)
                RESP(WS-CICS-RESP)
                RESP2(WS-CICS-RESP2)
           END-EXEC.
           IF WS-CICS-RESP = DFHRESP(NORMAL)
              MOVE 'Y' TO EXIT-VSAM-READ
              MOVE 'Y' TO INQCUST-INQ-SUCCESS
              GO TO RCV999
           END-IF.
           IF WS-CICS-RESP = DFHRESP(SYSIDERR)
              PERFORM VARYING SYSIDERR-RETRY FROM 1 BY 1
              UNTIL SYSIDERR-RETRY > 100
              OR WS-CICS-RESP IS NOT EQUAL TO DFHRESP(SYSIDERR)
                 EXEC CICS DELAY FOR SECONDS(3)
                 END-EXEC
                 EXEC CICS READ FILE('CUSTOMER')
                    RIDFLD(CUSTOMER-KY)
                    INTO(OUTPUT-DATA)
                    RESP(WS-CICS-RESP)
                    RESP2(WS-CICS-RESP2)
                  END-EXEC
                  IF WS-CICS-RESP = DFHRESP(NORMAL)
                     MOVE 'Y' TO EXIT-VSAM-READ
                     MOVE 'Y' TO INQCUST-INQ-SUCCESS
                     GO TO RCV999
                  END-IF
              END-PERFORM
           END-IF.
           IF WS-CICS-RESP = DFHRESP(NOTFND) AND
              INQCUST-CUSTNO = 0000000000
            IF INQCUST-RETRY < 1000
                PERFORM GENERATE-RANDOM-CUSTOMER-AGAIN
                MOVE RANDOM-CUSTOMER TO REQUIRED-CUST-NUMBER
                GO TO RCV999
              ELSE
                MOVE 'Y' TO EXIT-VSAM-READ
                MOVE 'N' TO INQCUST-INQ-SUCCESS
                MOVE '1' TO INQCUST-INQ-FAIL-CD
                GO TO RCV999
              END-IF
           END-IF.
           IF WS-CICS-RESP = DFHRESP(NOTFND) AND
           INQCUST-CUSTNO = 9999999999 AND
           WS-V-RETRIED = 'N'
              PERFORM GET-LAST-CUSTOMER-VSAM
              MOVE NCS-CUST-NO-VALUE TO REQUIRED-CUST-NUMBER
              MOVE 'Y' TO WS-V-RETRIED
              GO TO RCV999
           END-IF.
           IF (WS-CICS-RESP = DFHRESP(NOTFND)
                AND INQCUST-CUSTNO NOT = 9999999999)
                AND (WS-CICS-RESP = DFHRESP(NOTFND)
                AND INQCUST-CUSTNO NOT = 0000000000)
              MOVE REQUIRED-CUST-NUMBER TO CUSTOMER-NUMBER
                                           OF OUTPUT-DATA
              MOVE 'Y' TO EXIT-VSAM-READ
              MOVE 'N' TO INQCUST-INQ-SUCCESS
              MOVE '1' TO INQCUST-INQ-FAIL-CD
              MOVE SPACES TO INQCUST-ADDR
              MOVE SPACES TO INQCUST-NAME
              GO TO RCV999
           END-IF.
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
              MOVE 'CVR1'      TO ABND-CODE
              EXEC CICS ASSIGN PROGRAM(ABND-PROGRAM)
              END-EXEC
              MOVE ZEROS      TO ABND-SQLCODE
              STRING 'RCV010 - CUSTOMER VSAM RECORD KEY='
                    DELIMITED BY SIZE,
                    CUSTOMER-KY DELIMITED SIZE,
                    ' GAVE VSAM RC=' DELIMITED BY SIZE,
                    ' EIBRESP=' DELIMITED BY SIZE,
                    ABND-RESPCODE DELIMITED BY SIZE,
                    ' RESP2=' DELIMITED BY SIZE,
                    ABND-RESP2CODE DELIMITED BY SIZE
                    INTO ABND-FREEFORM
              END-STRING
              EXEC CICS LINK PROGRAM(WS-ABEND-PGM)
                        COMMAREA(ABNDINFO-REC)
              END-EXEC
              DISPLAY 'CUSTOMER VSAM RECORD KEY='
                  CUSTOMER-KY ' GAVE VSAM RC='
                  WS-CICS-RESP
              IF WS-V-RETRIED = 'Y'
                 DISPLAY 'ON A RETRY'
              END-IF
              EXEC CICS ABEND ABCODE('CVR1')
                 CANCEL
              END-EXEC
           END-IF.
       RCV999.
           EXIT.
       GET-ME-OUT-OF-HERE SECTION.
       GMOFH010.
           EXEC CICS RETURN
           END-EXEC.
       GMOFH999.
           EXIT.
       ABEND-HANDLING SECTION.
       AH010.
           EXEC CICS ASSIGN
              ABCODE(MY-ABEND-CODE)
           END-EXEC.
           EVALUATE MY-ABEND-CODE
             WHEN 'AFCR'
             WHEN 'AFCS'
             WHEN 'AFCT'
               MOVE 'Y' TO WS-STORM-DRAIN
               DISPLAY 'INQCUST: Check-For-Storm-Drain-VSAM: Storm '
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
                 DISPLAY 'INQCUST: Unable to perform Synpoint Rollback.'
                 ' Possible Integrity issue following VSAM RLS abend'
                 ' RESP CODE=' WS-CICS-RESP ' RESP2 CODE=' WS-CICS-RESP2
                  EXEC CICS ABEND
                     ABCODE ('HROL')
                     CANCEL
                  END-EXEC
               END-IF
               MOVE 'N' TO INQCUST-INQ-SUCCESS
               MOVE '2' TO INQCUST-INQ-FAIL-CD
               EXEC CICS RETURN
               END-EXEC
           END-EVALUATE.
           IF WS-STORM-DRAIN = 'N'
              EXEC CICS ABEND ABCODE( MY-ABEND-CODE)
              NODUMP
              END-EXEC
           END-IF.
       AH999.
           EXIT.
       GET-LAST-CUSTOMER-VSAM SECTION.
       GLCV010.
           INITIALIZE OUTPUT-DATA.
           MOVE HIGH-VALUES TO CUSTOMER-KY2.
           EXEC CICS STARTBR FILE('CUSTOMER')
                RIDFLD(CUSTOMER-KY2)
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
                 EXEC CICS STARTBR FILE('CUSTOMER')
                    RIDFLD(CUSTOMER-KY2)
                    RESP(WS-CICS-RESP)
                    RESP2(WS-CICS-RESP2)
                 END-EXEC
              END-PERFORM
           END-IF.
           IF WS-CICS-RESP IS NOT EQUAL TO DFHRESP(NORMAL)
               MOVE 'N' TO INQCUST-INQ-SUCCESS
               MOVE '9' TO INQCUST-INQ-FAIL-CD
               GO TO GLCVE999
           END-IF.
           EXEC CICS READPREV FILE('CUSTOMER')
                RIDFLD(CUSTOMER-KY2)
                INTO(OUTPUT-DATA)
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
                 EXEC CICS READPREV FILE('CUSTOMER')
                      RIDFLD(CUSTOMER-KY2)
                      INTO(OUTPUT-DATA)
                      RESP(WS-CICS-RESP)
                      RESP2(WS-CICS-RESP2)
                 END-EXEC
              END-PERFORM
           END-IF.
           IF WS-CICS-RESP IS NOT EQUAL TO DFHRESP(NORMAL)
              MOVE 'N' TO INQCUST-INQ-SUCCESS
              MOVE '9' TO INQCUST-INQ-FAIL-CD
              GO TO GLCVE999
           END-IF.
           EXEC CICS ENDBR FILE('CUSTOMER')
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
                 EXEC CICS ENDBR FILE('CUSTOMER')
                      RESP(WS-CICS-RESP)
                      RESP2(WS-CICS-RESP2)
                 END-EXEC
              END-PERFORM
           IF WS-CICS-RESP IS NOT EQUAL TO DFHRESP(NORMAL)
              MOVE 'N' TO INQCUST-INQ-SUCCESS
              MOVE '9' TO INQCUST-INQ-FAIL-CD
              GO TO GLCVE999
           END-IF.
           MOVE 'Y' TO INQCUST-INQ-SUCCESS.
       GLCVE999.
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
       GENERATE-RANDOM-CUSTOMER SECTION.
       GRC010.
           MOVE ZERO TO INQCUST-RETRY.
           COMPUTE RANDOM-CUSTOMER = ((NCS-CUST-NO-VALUE - 1)
       GRC999.
           EXIT.
       GENERATE-RANDOM-CUSTOMER-AGAIN SECTION.
       GRCA10.
           ADD 1 TO INQCUST-RETRY GIVING INQCUST-RETRY.
           COMPUTE RANDOM-CUSTOMER = ((NCS-CUST-NO-VALUE - 1)
       GRCA99.
            EXIT.
