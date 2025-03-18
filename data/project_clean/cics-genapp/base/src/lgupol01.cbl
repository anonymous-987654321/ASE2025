       IDENTIFICATION DIVISION.
       PROGRAM-ID. LGUPOL01.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-HEADER.
           03 WS-EYECATCHER            PIC X(16)
                                        VALUE 'LGUPOL01------WS'.
           03 WS-TRANSID               PIC X(4).
           03 WS-TERMID                PIC X(4).
           03 WS-TASKNUM               PIC 9(7).
           03 WS-FILLER                PIC X.
           03 WS-ADDR-DFHCOMMAREA      USAGE is POINTER.
           03 WS-CALEN                 PIC S9(4) COMP.
       01  WS-ABSTIME                  PIC S9(8) COMP VALUE +0.
       01  WS-TIME                     PIC X(8)  VALUE SPACES.
       01  WS-DATE                     PIC X(10) VALUE SPACES.
       01  WS-POLICY-LENGTHS.
           03 WS-FULL-ENDOW-LEN        PIC S9(4) COMP VALUE +124.
           03 WS-FULL-HOUSE-LEN        PIC S9(4) COMP VALUE +130.
           03 WS-FULL-MOTOR-LEN        PIC S9(4) COMP VALUE +137.
       01  ERROR-MSG.
           03 EM-DATE                  PIC X(8)  VALUE SPACES.
           03 FILLER                   PIC X     VALUE SPACES.
           03 EM-TIME                  PIC X(6)  VALUE SPACES.
           03 FILLER                   PIC X(9)  VALUE ' LGUPOL01'.
           03 EM-VARIABLE              PIC X(21) VALUE SPACES.
       01  CA-ERROR-MSG.
           03 FILLER                   PIC X(9)  VALUE 'COMMAREA='.
           03 CA-DATA                  PIC X(90) VALUE SPACES.
       01  WS-COMMAREA-LENGTHS.
           03 WS-CA-HEADER-LEN         PIC S9(4) COMP VALUE +28.
           03 WS-REQUIRED-CA-LEN       PIC S9(4) COMP VALUE +0.
       01 LGUPDB01                     PIC X(8) VALUE 'LGUPDB01'.
       LINKAGE SECTION.
       01  DFHCOMMAREA.
           03 CA-REQUEST-ID            PIC X(6).
           03 CA-RETURN-CODE           PIC 9(2).
           03 CA-CUSTOMER-NUM          PIC 9(10).
           03 CA-REQUEST-SPECIFIC      PIC X(32482).
           03 CA-CUSTOMER-REQUEST REDEFINES CA-REQUEST-SPECIFIC.
              05 CA-FIRST-NAME         PIC X(10).
              05 CA-LAST-NAME          PIC X(20).
              05 CA-DOB                PIC X(10).
              05 CA-HOUSE-NAME         PIC X(20).
              05 CA-HOUSE-NUM          PIC X(4).
              05 CA-POSTCODE           PIC X(8).
              05 CA-NUM-POLICIES       PIC 9(3).
              05 CA-PHONE-MOBILE       PIC X(20).
              05 CA-PHONE-HOME         PIC X(20).
              05 CA-EMAIL-ADDRESS      PIC X(100).
              05 CA-POLICY-DATA        PIC X(32267).
           03 CA-CUSTSECR-REQUEST REDEFINES CA-REQUEST-SPECIFIC.
              05 CA-CUSTSECR-PASS      PIC X(32).
              05 CA-CUSTSECR-COUNT     PIC X(4).
              05 CA-CUSTSECR-STATE     PIC X.
              05 CA-CUSTSECR-DATA      PIC X(32445).
           03 CA-POLICY-REQUEST REDEFINES CA-REQUEST-SPECIFIC.
              05 CA-POLICY-NUM         PIC 9(10).
              05 CA-POLICY-COMMON.
                 07 CA-ISSUE-DATE      PIC X(10).
                 07 CA-EXPIRY-DATE     PIC X(10).
                 07 CA-LASTCHANGED     PIC X(26).
                 07 CA-BROKERID        PIC 9(10).
                 07 CA-BROKERSREF      PIC X(10).
                 07 CA-PAYMENT         PIC 9(6).
              05 CA-POLICY-SPECIFIC    PIC X(32400).
              05 CA-ENDOWMENT REDEFINES CA-POLICY-SPECIFIC.
                 07 CA-E-WITH-PROFITS    PIC X.
                 07 CA-E-EQUITIES        PIC X.
                 07 CA-E-MANAGED-FUND    PIC X.
                 07 CA-E-FUND-NAME       PIC X(10).
                 07 CA-E-TERM            PIC 99.
                 07 CA-E-SUM-ASSURED     PIC 9(6).
                 07 CA-E-LIFE-ASSURED    PIC X(31).
                 07 CA-E-PADDING-DATA    PIC X(32348).
              05 CA-HOUSE REDEFINES CA-POLICY-SPECIFIC.
                 07 CA-H-PROPERTY-TYPE   PIC X(15).
                 07 CA-H-BEDROOMS        PIC 9(3).
                 07 CA-H-VALUE           PIC 9(8).
                 07 CA-H-HOUSE-NAME      PIC X(20).
                 07 CA-H-HOUSE-NUMBER    PIC X(4).
                 07 CA-H-POSTCODE        PIC X(8).
                 07 CA-H-FILLER          PIC X(32342).
              05 CA-MOTOR REDEFINES CA-POLICY-SPECIFIC.
                 07 CA-M-MAKE            PIC X(15).
                 07 CA-M-MODEL           PIC X(15).
                 07 CA-M-VALUE           PIC 9(6).
                 07 CA-M-REGNUMBER       PIC X(7).
                 07 CA-M-COLOUR          PIC X(8).
                 07 CA-M-CC              PIC 9(4).
                 07 CA-M-MANUFACTURED    PIC X(10).
                 07 CA-M-PREMIUM         PIC 9(6).
                 07 CA-M-ACCIDENTS       PIC 9(6).
                 07 CA-M-FILLER          PIC X(32323).
              05 CA-COMMERCIAL REDEFINES CA-POLICY-SPECIFIC.
                 07 CA-B-Address         PIC X(255).
                 07 CA-B-Postcode        PIC X(8).
                 07 CA-B-Latitude        PIC X(11).
                 07 CA-B-Longitude       PIC X(11).
                 07 CA-B-Customer        PIC X(255).
                 07 CA-B-PropType        PIC X(255).
                 07 CA-B-FirePeril       PIC 9(4).
                 07 CA-B-FirePremium     PIC 9(8).
                 07 CA-B-CrimePeril      PIC 9(4).
                 07 CA-B-CrimePremium    PIC 9(8).
                 07 CA-B-FloodPeril      PIC 9(4).
                 07 CA-B-FloodPremium    PIC 9(8).
                 07 CA-B-WeatherPeril    PIC 9(4).
                 07 CA-B-WeatherPremium  PIC 9(8).
                 07 CA-B-Status          PIC 9(4).
                 07 CA-B-RejectReason    PIC X(255).
                 07 CA-B-FILLER          PIC X(31298).
              05 CA-CLAIM      REDEFINES CA-POLICY-SPECIFIC.
                 07 CA-C-Num             PIC 9(10).
                 07 CA-C-Date            PIC X(10).
                 07 CA-C-Paid            PIC 9(8).
                 07 CA-C-Value           PIC 9(8).
                 07 CA-C-Cause           PIC X(255).
                 07 CA-C-Observations    PIC X(255).
                 07 CA-C-FILLER          PIC X(31854).
       PROCEDURE DIVISION.
       MAINLINE SECTION.
           INITIALIZE WS-HEADER.
           MOVE EIBTRNID TO WS-TRANSID.
           MOVE EIBTRMID TO WS-TERMID.
           MOVE EIBTASKN TO WS-TASKNUM.
           IF EIBCALEN IS EQUAL TO ZERO
               MOVE ' NO COMMAREA RECEIVED' TO EM-VARIABLE
               PERFORM WRITE-ERROR-MESSAGE
               EXEC CICS ABEND ABCODE('LGCA') NODUMP END-EXEC
           END-IF
           MOVE '00' TO CA-RETURN-CODE
           MOVE EIBCALEN TO WS-CALEN.
           SET WS-ADDR-DFHCOMMAREA TO ADDRESS OF DFHCOMMAREA.
           EVALUATE CA-REQUEST-ID
             WHEN '01UEND'
               ADD WS-CA-HEADER-LEN  TO WS-REQUIRED-CA-LEN
               ADD WS-FULL-ENDOW-LEN TO WS-REQUIRED-CA-LEN
               IF EIBCALEN IS LESS THAN WS-REQUIRED-CA-LEN
                 MOVE '98' TO CA-RETURN-CODE
                 EXEC CICS RETURN END-EXEC
               END-IF
             WHEN '01UHOU'
               ADD WS-CA-HEADER-LEN  TO WS-REQUIRED-CA-LEN
               ADD WS-FULL-HOUSE-LEN TO WS-REQUIRED-CA-LEN
               IF EIBCALEN IS LESS THAN WS-REQUIRED-CA-LEN
                 MOVE '98' TO CA-RETURN-CODE
                 EXEC CICS RETURN END-EXEC
               END-IF
             WHEN '01UMOT'
               ADD WS-CA-HEADER-LEN  TO WS-REQUIRED-CA-LEN
               ADD WS-FULL-MOTOR-LEN TO WS-REQUIRED-CA-LEN
               IF EIBCALEN IS LESS THAN WS-REQUIRED-CA-LEN
                 MOVE '98' TO CA-RETURN-CODE
                 EXEC CICS RETURN END-EXEC
               END-IF
             WHEN OTHER
               MOVE '99' TO CA-RETURN-CODE
           END-EVALUATE
           PERFORM UPDATE-POLICY-DB2-INFO.
       END-PROGRAM.
           EXEC CICS RETURN END-EXEC.
       MAINLINE-EXIT.
           EXIT.
       UPDATE-POLICY-DB2-INFO.
           EXEC CICS LINK Program(LGUPDB01)
                Commarea(DFHCOMMAREA)
                LENGTH(32500)
           END-EXEC.
           EXIT.
       WRITE-ERROR-MESSAGE.
           EXEC CICS ASKTIME ABSTIME(WS-ABSTIME)
           END-EXEC
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                     MMDDYYYY(WS-DATE)
                     TIME(WS-TIME)
           END-EXEC
           MOVE WS-DATE TO EM-DATE
           MOVE WS-TIME TO EM-TIME
           EXEC CICS LINK PROGRAM('LGSTSQ')
                     COMMAREA(ERROR-MSG)
                     LENGTH(LENGTH OF ERROR-MSG)
           END-EXEC.
           IF EIBCALEN > 0 THEN
             IF EIBCALEN < 91 THEN
               MOVE DFHCOMMAREA(1:EIBCALEN) TO CA-DATA
               EXEC CICS LINK PROGRAM('LGSTSQ')
                         COMMAREA(CA-ERROR-MSG)
                         LENGTH(LENGTH OF CA-ERROR-MSG)
               END-EXEC
             ELSE
               MOVE DFHCOMMAREA(1:90) TO CA-DATA
               EXEC CICS LINK PROGRAM('LGSTSQ')
                         COMMAREA(CA-ERROR-MSG)
                         LENGTH(LENGTH OF CA-ERROR-MSG)
               END-EXEC
             END-IF
           END-IF.
           EXIT.
