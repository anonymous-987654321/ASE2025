       IDENTIFICATION DIVISION.
       PROGRAM-ID. HCMAPL01.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 MSGEND                       PIC X(24) VALUE
                                        'Transaction ended      '.
       COPY HCMAPS.
       01 COMM-AREA.
       COPY HCCMAREA.
       PROCEDURE DIVISION.
       MAINLINE SECTION.
           IF EIBCALEN > 0
              GO TO A-GAIN.
           Initialize HCMAMAPI.
           Initialize HCMAMAPO.
           Initialize COMM-AREA.
           Move LOW-VALUES To HCMACNOI.
           Move SPACES to CA-REQUEST-ID.
           Move 0 to CA-RETURN-CODE.
           Move 0 to CA-PATIENT-ID.
           Move -1 To HCMACNOL.
           EXEC CICS SEND MAP ('HCMAMAP')
                     MAPSET ('HCMAPS')
                     ERASE
                     CURSOR
                     END-EXEC.
       A-GAIN.
           EXEC CICS HANDLE AID
                     CLEAR(CLEARIT)
                     PF3(ENDIT)
                     PF12(CANCELIT)
                     END-EXEC.
           EXEC CICS HANDLE CONDITION
                     MAPFAIL(ENDIT)
                     END-EXEC.
           EXEC CICS RECEIVE MAP('HCMAMAP')
                     INTO(HCMAMAPI) ASIS TERMINAL
                     MAPSET('HCMAPS') END-EXEC.
           IF HCMADNAMO EQUAL ZEROS OR SPACES OR LOW-VALUES
              PERFORM GET-PATIENT
              Move 'Enter medication information'
                  To  HCMAMSGO
           ELSE
              Move '01AMED'          To CA-REQUEST-ID
              Move HCMACNOI          To CA-PATIENT-ID
              MOVE HCMADNAMI TO CA-DRUG-NAME
              MOVE HCMADSTRI TO CA-STRENGTH
              MOVE HCMADAMOI TO CA-AMOUNT
              MOVE HCMADROUI TO CA-ROUTE
              MOVE HCMADFREI TO CA-FREQUENCY OF CA-MEDICATION-REQUEST
              MOVE HCMAIDENI TO CA-IDENTIFIER
              MOVE HCMATYPEI TO CA-BIOMED-TYPE
              MOVE HCMASDTAI TO CA-START-DATE
              MOVE HCMAEDTAI TO CA-END-DATE
              EXEC CICS LINK PROGRAM('HCMABA01')
                        COMMAREA(COMM-AREA)
                        LENGTH(32500)
              END-EXEC
              IF CA-RETURN-CODE > 0
                 Exec CICS Syncpoint Rollback End-Exec
                 GO TO NO-ADD
              END-IF
              Move 'New medication added'
                  To  HCMAMSGO
           END-IF
           Move -1 To HCMADNAML
           EXEC CICS SEND MAP ('HCMAMAP')
                     FROM(HCMAMAPO)
                     MAPSET ('HCMAPS')
                     CURSOR
           END-EXEC
           GO TO ENDIT-STARTIT
           EXEC CICS RETURN
           END-EXEC.
       ENDIT-STARTIT.
           EXEC CICS RETURN
                TRANSID('HCMA')
                COMMAREA(COMM-AREA)
                END-EXEC.
       ENDIT.
           EXEC CICS SEND TEXT
                     FROM(MSGEND)
                     LENGTH(LENGTH OF MSGEND)
                     ERASE
                     FREEKB
           END-EXEC
           EXEC CICS RETURN
           END-EXEC.
       CANCELIT.
           EXEC CICS RETURN
                TRANSID('HCAZ')
                IMMEDIATE
                END-EXEC.
       GET-PATIENT.
           Move '01IPAT'   To CA-REQUEST-ID
           Move HCMACNOO   To CA-PATIENT-ID
           EXEC CICS LINK PROGRAM('HCP1BI01')
                     COMMAREA(COMM-AREA)
                     LENGTH(32500)
           END-EXEC
           IF CA-RETURN-CODE > 0
              GO TO NO-PATIENT-DATA
           END-IF
           Move CA-FIRST-NAME to HCMAFNAI
           Move CA-LAST-NAME  to HCMALNAI.
       CLEARIT.
           Initialize HCMAMAPI.
           EXEC CICS SEND MAP ('HCMAMAP')
                     MAPSET ('HCMAPS')
                     MAPONLY
           END-EXEC
           EXEC CICS RETURN
                TRANSID('HCMA')
                COMMAREA(COMM-AREA)
                END-EXEC.
       NO-ADD.
           Evaluate CA-RETURN-CODE
             When 70
               Move 'Patient does not exist'          To  HCMAMSGO
               Go To ERROR-OUT
             When Other
               Move 'Error Adding Medication'        To  HCMAMSGO
               Go To ERROR-OUT
           End-Evaluate.
       NO-UPD.
           Move 'Error Updating Medication'    To  HCMAMSGO
           Go To ERROR-OUT.
       NO-DELETE.
           Move 'Error Deleting Medication'    To  HCMAMSGO
           Go To ERROR-OUT.
       NO-PATIENT-DATA.
           Move 'No patient data was returned.'  To  HCMAMSGO
           Go To ERROR-OUT.
       NO-MED-DATA.
           Move 'No medication data was returned.' To  HCMAMSGO
           Go To ERROR-OUT.
       ERROR-OUT.
           EXEC CICS SEND MAP ('HCMAMAP')
                     FROM(HCMAMAPO)
                     MAPSET ('HCMAPS')
                     CURSOR
           END-EXEC.
           Initialize HCMAMAPI.
           Initialize HCMAMAPO.
           GO TO ENDIT-STARTIT.