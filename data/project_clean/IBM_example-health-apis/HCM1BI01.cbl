       IDENTIFICATION DIVISION.
       PROGRAM-ID. HCM1BI01.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-HEADER.
           03 WS-EYECATCHER            PIC X(16)
                                        VALUE 'HCM1BI01------WS'.
           03 WS-TRANSID               PIC X(4).
           03 WS-TERMID                PIC X(4).
           03 WS-TASKNUM               PIC 9(7).
           03 WS-FILLER                PIC X.
           03 WS-ADDR-DFHCOMMAREA      USAGE is POINTER.
           03 WS-CALEN                 PIC S9(4) COMP.
       01 HCIMDB01                  PIC x(8) Value 'HCIMDB01'.
       01  WS-COMMAREA-LENGTHS.
           03 WS-CA-HEADERTRAILER-LEN  PIC S9(4) COMP VALUE +18.
           03 WS-REQUIRED-CA-LEN       PIC S9(4)      VALUE +0.
           03 WS-COUNTER               PIC S9(4)      VALUE +0.
       01  WS-Resp                     PIC S9(8) Comp.
       01 HCAZERRS           PIC x(8) Value 'HCAZERRS'.
       01  WS-ABSTIME                  PIC S9(8) COMP VALUE +0.
       01  WS-TIME                     PIC X(8)  VALUE SPACES.
       01  WS-DATE                     PIC X(10) VALUE SPACES.
       01  ERROR-MSG.
           03 EM-DATE                  PIC X(8)  VALUE SPACES.
           03 FILLER                   PIC X     VALUE SPACES.
           03 EM-TIME                  PIC X(6)  VALUE SPACES.
           03 FILLER                   PIC X(9)  VALUE ' HCP1BI01'.
           03 EM-VARIABLE.
             05 FILLER                 PIC X(6)  VALUE ' PNUM='.
             05 EM-PATNUM              PIC X(10)  VALUE SPACES.
             05 FILLER                 PIC X(6)  VALUE ' MNUM='.
             05 EM-MEDNUM              PIC X(10)  VALUE SPACES.
             05 EM-SQLREQ              PIC X(16) VALUE SPACES.
             05 FILLER                 PIC X(9)  VALUE ' SQLCODE='.
             05 EM-SQLRC               PIC +9(5) USAGE DISPLAY.
       01 CA-ERROR-MSG.
           03 FILLER                PIC X(9)  VALUE 'COMMAREA='.
           03 CA-DATA               PIC X(90) VALUE SPACES.
       LINKAGE SECTION.
       01  DFHCOMMAREA.
           03 CA-REQUEST-ID            PIC X(6).
           03 CA-RETURN-CODE           PIC 9(2).
           03 CA-PATIENT-ID            PIC 9(10).
           03 CA-LIST-MEDICATION-REQUEST.
              05 CA-NUM-MEDICATIONS    PIC 99 COMP-3.
              05 CA-MEDICATIONS OCCURS 0 to 50 times
                 depending on CA-NUM-MEDICATIONS.
                 10 CA-MEDICATION-ID      PIC 9(10).
                 10 CA-DRUG-NAME          PIC X(50).
                 10 CA-STRENGTH           PIC X(20).
                 10 CA-AMOUNT             PIC 9(03).
                 10 CA-ROUTE              PIC X(20).
                 10 CA-FREQUENCY          PIC X(20).
                 10 CA-IDENTIFIER         PIC X(20).
                 10 CA-TYPE               PIC X(2).
.
       PROCEDURE DIVISION.
       MAINLINE SECTION.
           INITIALIZE WS-HEADER.
           MOVE EIBTRNID TO WS-TRANSID.
           MOVE EIBTRMID TO WS-TERMID.
           MOVE EIBTASKN TO WS-TASKNUM.
           IF EIBCALEN IS EQUAL TO ZERO
               MOVE ' NO COMMAREA RECEIVED' TO EM-VARIABLE
               PERFORM WRITE-ERROR-MESSAGE
               EXEC CICS ABEND ABCODE('HCCA') NODUMP END-EXEC
           END-IF
           MOVE '00' TO CA-RETURN-CODE
           MOVE EIBCALEN TO WS-CALEN.
           SET WS-ADDR-DFHCOMMAREA TO ADDRESS OF DFHCOMMAREA.
           ADD WS-CA-HEADERTRAILER-LEN TO WS-REQUIRED-CA-LEN
           IF EIBCALEN IS LESS THAN WS-REQUIRED-CA-LEN
             MOVE '98' TO CA-RETURN-CODE
             EXEC CICS RETURN END-EXEC
           END-IF
           MOVE CA-PATIENT-ID TO EM-PATNUM
           PERFORM GET-MEDICATIONS.
           IF CA-NUM-MEDICATIONS > 0
              MOVE ZERO To ws-counter
              PERFORM CA-NUM-MEDICATIONS TIMES
               ADD +1 to ws-counter
               EVALUATE CA-FREQUENCY(ws-counter)
                  WHEN 1
                    MOVE 'every 24 hours' TO  CA-FREQUENCY(ws-counter)
                  WHEN 2
                    MOVE 'every 12 hours' TO  CA-FREQUENCY(ws-counter)
                  WHEN 3
                    MOVE 'every 8 hours' TO  CA-FREQUENCY(ws-counter)
                  WHEN 4
                    MOVE 'every 6 hours' TO  CA-FREQUENCY(ws-counter)
               END-EVALUATE
              END-PERFORM.
       MAINLINE-END.
           EXEC CICS RETURN END-EXEC.
       MAINLINE-EXIT.
           EXIT.
       GET-MEDICATIONS.
             EXEC CICS LINK Program(HCIMDB01)
                 Commarea(DFHCOMMAREA)
                 LENGTH(LENGTH OF DFHCOMMAREA)
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
           EXEC CICS LINK PROGRAM(HCAZERRS)
                     COMMAREA(ERROR-MSG)
                     LENGTH(LENGTH OF ERROR-MSG)
           END-EXEC.
           IF EIBCALEN > 0 THEN
             IF EIBCALEN < 91 THEN
               MOVE DFHCOMMAREA(1:EIBCALEN) TO CA-DATA
               EXEC CICS LINK PROGRAM(HCAZERRS)
                         COMMAREA(CA-ERROR-MSG)
                         LENGTH(LENGTH OF CA-ERROR-MSG)
               END-EXEC
             ELSE
               MOVE DFHCOMMAREA(1:90) TO CA-DATA
               EXEC CICS LINK PROGRAM(HCAZERRS)
                         COMMAREA(CA-ERROR-MSG)
                         LENGTH(LENGTH OF CA-ERROR-MSG)
               END-EXEC
             END-IF
           END-IF.
           EXIT.
