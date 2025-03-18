       IDENTIFICATION DIVISION.
       PROGRAM-ID. HCMADB01.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
        01  WS-HEADER.
           03 WS-EYECATCHER            PIC X(16)
                                        VALUE 'HCMADB01------WS'.
           03 WS-TRANSID               PIC X(4).
           03 WS-TERMID                PIC X(4).
           03 WS-TASKNUM               PIC 9(7).
           03 WS-FILLER                PIC X.
           03 WS-ADDR-DFHCOMMAREA      USAGE is POINTER.
           03 WS-CALEN                 PIC S9(4) COMP.
       01  WS-RESP                   PIC S9(8) COMP.
       COPY HCERRSWS.
       01  WS-COMMAREA-LENGTHS.
           03 WS-CA-HEADER-LEN         PIC S9(4) COMP VALUE +18.
           03 WS-REQUIRED-CA-LEN       PIC S9(4)      VALUE +0.
       01  DB2-OUT.
           03 DB2-MEDICATION-ID-INT    PIC S9(9) COMP.
           03 DB2-PATIENT-ID           PIC S9(9) COMP.
           03 DB2-AMOUNT            PIC S9(4) COMP.
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
           INITIALIZE DB2-OUT.
           IF EIBCALEN IS EQUAL TO ZERO
               MOVE ' NO COMMAREA RECEIVED' TO EM-VARIABLE
               PERFORM WRITE-ERROR-MESSAGE
               EXEC CICS ABEND ABCODE('HCCA') NODUMP END-EXEC
           END-IF
           MOVE '00' TO CA-RETURN-CODE
           MOVE EIBCALEN TO WS-CALEN.
           SET WS-ADDR-DFHCOMMAREA TO ADDRESS OF DFHCOMMAREA.
           ADD WS-CA-HEADER-LEN TO WS-REQUIRED-CA-LEN
           IF EIBCALEN IS LESS THAN WS-REQUIRED-CA-LEN
             MOVE '98' TO CA-RETURN-CODE
             EXEC CICS RETURN END-EXEC
           END-IF
           PERFORM Obtain-Patient-Id.
           PERFORM INSERT-MEDICATION.
           EXEC CICS RETURN END-EXEC.
       MAINLINE-EXIT.
           EXIT.
       Obtain-Patient-Id.
           MOVE CA-PATIENT-ID TO DB2-PATIENT-ID.
           MOVE CA-AMOUNT TO DB2-AMOUNT.
       INSERT-MEDICATION.
           MOVE ' INSERT MEDICATION' TO EM-SQLREQ
             EXEC SQL
               INSERT INTO MEDICATION
                         ( MEDICATIONID,
                           PATIENTID,
                           DRUGNAME,
                           STRENGTH,
                           AMOUNT,
                           ROUTE,
                           FREQUENCY,
                           IDENTIFIER,
                           TYPE )
                  VALUES ( DEFAULT,
                           :DB2-PATIENT-ID,
                           :CA-DRUG-NAME,
                           :CA-STRENGTH,
                           :DB2-AMOUNT,
                           :CA-ROUTE,
                           :CA-FREQUENCY,
                           :CA-IDENTIFIER,
                           :CA-BIOMED-TYPE  )
             END-EXEC
             IF SQLCODE NOT EQUAL 0
               MOVE '90' TO CA-RETURN-CODE
               PERFORM WRITE-ERROR-MESSAGE
               EXEC CICS RETURN END-EXEC
             ELSE
               EXEC SQL
                    SET :DB2-MEDICATION-ID-INT = IDENTITY_VAL_LOCAL()
               END-EXEC
             END-IF
           MOVE DB2-PATIENT-ID TO CA-PATIENT-ID.
           MOVE DB2-MEDICATION-ID-INT TO CA-PRESCRIPTION-ID.
           EXIT.
       COPY HCERRSPD.