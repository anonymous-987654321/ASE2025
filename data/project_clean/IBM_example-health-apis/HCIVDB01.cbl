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
       COPY HCERRSWS.
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
       COPY HCERRSPD.