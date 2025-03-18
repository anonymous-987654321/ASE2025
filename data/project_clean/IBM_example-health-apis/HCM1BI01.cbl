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
       COPY HCERRSWS.
       LINKAGE SECTION.
       01  DFHCOMMAREA.
             COPY HCCMARE2.
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
       COPY HCERRSPD.