       IDENTIFICATION DIVISION.
       PROGRAM-ID. HCIVDB01.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-HEADER.
           03 WS-EYECATCHER            PIC X(16)
                                        VALUE 'HCIVDB01------WS'.
           03 WS-TRANSID               PIC X(4).
           03 WS-TERMID                PIC X(4).
           03 WS-TASKNUM               PIC 9(7).
           03 WS-FILLER                PIC X.
           03 WS-ADDR-DFHCOMMAREA      USAGE is POINTER.
           03 WS-CALEN                 PIC S9(4) COMP.
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
       01  WS-COMMAREA-LENGTHS.
           03 WS-CA-HEADERTRAILER-LEN  PIC S9(4) COMP VALUE +18.
           03 WS-REQUIRED-CA-LEN       PIC S9(4)      VALUE +0.
       01  DB2-IN.
           03 DB2-PATIENT-ID           PIC S9(9) COMP.
           03 DB2-TIMESTAMP            PIC X(26).
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
       LINKAGE SECTION.
       01  DFHCOMMAREA.
           EXEC SQL
             INCLUDE HCCMAREA
           END-EXEC.
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
           MOVE CA-PATIENT-ID TO DB2-PATIENT-ID
           MOVE CA-PATIENT-ID TO EM-PATNUM
           MOVE CA-VISIT-DATE   TO DB2-TIMESTAMP(1:10)
           MOVE SPACE           TO DB2-TIMESTAMP (11:1)
           IF CA-VISIT-TIME(10:) EQUAL SPACE
              MOVE '.0' TO CA-VISIT-TIME(9:2)
           END-IF
           MOVE CA-VISIT-TIME   TO DB2-TIMESTAMP(12:10)
           MOVE '00000'         TO DB2-TIMESTAMP(22:5)
           PERFORM GET-BLOODPRESSURE.
           PERFORM GET-HEARTRATE.
       MAINLINE-END.
           EXEC CICS RETURN END-EXEC.
       MAINLINE-EXIT.
           EXIT.
       GET-BLOODPRESSURE.
           EXEC SQL
               SELECT BLOODPRESSURE
               INTO  :CA-BLOOD-PRESSURE
               FROM BLOODPRESSURE
               WHERE PATIENTID = :DB2-PATIENT-ID AND
                     BPDATETIME = :DB2-TIMESTAMP
               END-EXEC.
           Evaluate SQLCODE
             When 0
               MOVE '00' TO CA-RETURN-CODE
             When 100
               MOVE '01' TO CA-RETURN-CODE
             When -913
               MOVE '01' TO CA-RETURN-CODE
             When Other
               MOVE '90' TO CA-RETURN-CODE
               PERFORM WRITE-ERROR-MESSAGE
               EXEC CICS RETURN END-EXEC
           END-Evaluate.
           EXIT.
       GET-HEARTRATE.
           EXEC SQL
               SELECT HEARTRATE
               INTO  :CA-HEART-RATE
               FROM HEARTRATE
               WHERE PATIENTID = :DB2-PATIENT-ID AND
                     HRDATETIME = :DB2-TIMESTAMP
               END-EXEC.
           Evaluate SQLCODE
             When 0
               MOVE '00' TO CA-RETURN-CODE
             When 100
               MOVE '01' TO CA-RETURN-CODE
             When -913
               MOVE '01' TO CA-RETURN-CODE
             When Other
               MOVE '90' TO CA-RETURN-CODE
               PERFORM WRITE-ERROR-MESSAGE
               EXEC CICS RETURN END-EXEC
           END-Evaluate.
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
