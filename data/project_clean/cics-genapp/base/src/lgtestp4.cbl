       IDENTIFICATION DIVISION.
       PROGRAM-ID. LGTESTP4.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 MSGEND                       PIC X(24) VALUE
                                        'Transaction ended      '.
       01 COMM-AREA.
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
           IF EIBCALEN > 0
              GO TO A-GAIN.
           Initialize SSMAPP4I.
           Initialize SSMAPP4O.
           Initialize COMM-AREA.
           MOVE '0000000000'   To ENP4CNOO.
           MOVE '0000000000'   To ENP4PNOO.
           MOVE LOW-VALUES     To ENP4FPEO.
           MOVE LOW-VALUES     To ENP4FPRO.
           MOVE LOW-VALUES     To ENP4CPEO.
           MOVE LOW-VALUES     To ENP4CPRO.
           MOVE LOW-VALUES     To ENP4XPEO.
           MOVE LOW-VALUES     To ENP4XPRO.
           MOVE LOW-VALUES     To ENP4WPEO.
           MOVE LOW-VALUES     To ENP4WPRO.
           MOVE LOW-VALUES     To ENP4STAO.
           EXEC CICS SEND MAP ('SSMAPP4')
                     MAPSET ('SSMAP')
                     ERASE
                     END-EXEC.
       A-GAIN.
           EXEC CICS HANDLE AID
                     CLEAR(CLEARIT)
                     PF3(ENDIT) END-EXEC.
           EXEC CICS HANDLE CONDITION
                     MAPFAIL(ENDIT)
                     END-EXEC.
           EXEC CICS RECEIVE MAP('SSMAPP4')
                     INTO(SSMAPP4I)
                     MAPSET('SSMAP') END-EXEC.
           EVALUATE ENP4OPTO
             WHEN '1'
                 If (
                     ENP4CNOO Not = Spaces      AND
                     ENP4CNOO Not = Low-Values  AND
                     ENP4CNOO Not = 0           AND
                     ENP4CNOO Not = 0000000000
                                                   )
                                                    AND
                    (
                     ENP4PNOO Not = Spaces      AND
                     ENP4PNOO Not = Low-Values  AND
                     ENP4PNOO Not = 0           AND
                     ENP4PNOO Not = 0000000000
                                                   )
                        Move '01ICOM'   To CA-REQUEST-ID
                        Move ENP4CNOO   To CA-CUSTOMER-NUM
                        Move ENP4PNOO   To CA-POLICY-NUM
                 Else
                 If (
                     ENP4PNOO Not = Spaces      AND
                     ENP4PNOO Not = Low-Values  AND
                     ENP4PNOO Not = 0           AND
                     ENP4PNOO Not = 0000000000
                                                   )
                        Move '02ICOM'   To CA-REQUEST-ID
                        Move ENP4PNOO   To CA-POLICY-NUM
                 Else
                 If (
                     ENP4CNOO Not = Spaces      AND
                     ENP4CNOO Not = Low-Values  AND
                     ENP4CNOO Not = 0           AND
                     ENP4CNOO Not = 0000000000
                                                   )
                        Move '03ICOM'   To CA-REQUEST-ID
                        Move ENP4CNOO   To CA-CUSTOMER-NUM
                 Else
                 If (
                     ENP4HPCO NOT = Spaces      AND
                     ENP4HPCO NOT = Low-Values  AND
                     ENP4HPCO Not = 0           AND
                     ENP4HPCO NOT = 00000000
                                                   )
                        Move '05ICOM'   To CA-REQUEST-ID
                        Move ENP4HPCO   To CA-B-PostCode
                 End-If
                 End-If
                 End-If
                 End-If
                 EXEC CICS LINK PROGRAM('LGIPOL01')
                           COMMAREA(COMM-AREA)
                           LENGTH(32500)
                 END-EXEC
                 IF CA-RETURN-CODE > 0
                   GO TO NO-DATA
                 END-IF
                 Move CA-POLICY-NUM        To  ENP4PNOI
                 Move CA-CUSTOMER-NUM      To  ENP4CNOI
                 Move CA-ISSUE-DATE        To  ENP4IDAI
                 Move CA-EXPIRY-DATE       To  ENP4EDAI
                 Move CA-B-Address         To  ENP4ADDI
                 Move CA-B-Postcode        To  ENP4HPCI
                 Move CA-B-Latitude        To  ENP4LATI
                 Move CA-B-Longitude       To  ENP4LONI
                 Move CA-B-Customer        To  ENP4CUSI
                 Move CA-B-PropType        To  ENP4PTYI
                 Move CA-B-FirePeril       To  ENP4FPEI
                 Move CA-B-FirePremium     To  ENP4FPRI
                 Move CA-B-CrimePeril      To  ENP4CPEI
                 Move CA-B-CrimePremium    To  ENP4CPRI
                 Move CA-B-FloodPeril      To  ENP4XPEI
                 Move CA-B-FloodPremium    To  ENP4XPRI
                 Move CA-B-WeatherPeril    To  ENP4WPEI
                 Move CA-B-WeatherPremium  To  ENP4WPRI
                 Move CA-B-Status          To  ENP4STAI
                 Move CA-B-RejectReason    To  ENP4REJI
                 EXEC CICS SEND MAP ('SSMAPP4')
                           FROM(SSMAPP4O)
                           MAPSET ('SSMAP')
                 END-EXEC
                 GO TO ENDIT-STARTIT
             WHEN '2'
                 Move '01ACOM'             To  CA-REQUEST-ID
                 Move ENP4CNOO             To  CA-CUSTOMER-NUM
                 Move ENP4IDAO             To  CA-ISSUE-DATE
                 Move ENP4EDAO             To  CA-EXPIRY-DATE
                 Move ENP4ADDO             To  CA-B-Address
                 Move ENP4HPCO             To  CA-B-Postcode
                 Move ENP4LATO             To  CA-B-Latitude
                 Move ENP4LONO             To  CA-B-Longitude
                 Move ENP4CUSO             To  CA-B-Customer
                 Move ENP4PTYO             To  CA-B-PropType
                 Move ENP4FPEO             To  CA-B-FirePeril
                 Move ENP4FPRO             To  CA-B-FirePremium
                 Move ENP4CPEO             To  CA-B-CrimePeril
                 Move ENP4CPRO             To  CA-B-CrimePremium
                 Move ENP4XPEO             To  CA-B-FloodPeril
                 Move ENP4XPRO             To  CA-B-FloodPremium
                 Move ENP4WPEO             To  CA-B-WeatherPeril
                 Move ENP4WPRO             To  CA-B-WeatherPremium
                 Move ENP4STAO             To  CA-B-Status
                 Move ENP4REJO             To  CA-B-RejectReason
                 EXEC CICS LINK PROGRAM('LGAPOL01')
                           COMMAREA(COMM-AREA)
                           LENGTH(32500)
                 END-EXEC
                 IF CA-RETURN-CODE > 0
                   Exec CICS Syncpoint Rollback End-Exec
                   GO TO NO-ADD
                 END-IF
                 Move CA-CUSTOMER-NUM To ENP4CNOI
                 Move CA-POLICY-NUM   To ENP4PNOI
                 Move ' '             To ENP4OPTI
                 Move 'New Commercial Policy Inserted'
                   To  ERP4FLDO
                 EXEC CICS SEND MAP ('SSMAPP4')
                           FROM(SSMAPP4O)
                           MAPSET ('SSMAP')
                 END-EXEC
                 GO TO ENDIT-STARTIT
             WHEN '3'
                 Move '01DCOM'   To CA-REQUEST-ID
                 Move ENP4CNOO   To CA-CUSTOMER-NUM
                 Move ENP4PNOO   To CA-POLICY-NUM
                 EXEC CICS LINK PROGRAM('LGDPOL01')
                           COMMAREA(COMM-AREA)
                           LENGTH(32500)
                 END-EXEC
                 IF CA-RETURN-CODE > 0
                   Exec CICS Syncpoint Rollback End-Exec
                   GO TO NO-DELETE
                 END-IF
                 Move Spaces             To ENP4EDAI
                 Move Spaces             To ENP4ADDI
                 Move Spaces             To ENP4HPCI
                 Move Spaces             To ENP4LATI
                 Move Spaces             To ENP4LONI
                 Move Spaces             To ENP4CUSI
                 Move Spaces             To ENP4PTYI
                 Move Spaces             To ENP4FPEI
                 Move Spaces             To ENP4FPRI
                 Move Spaces             To ENP4CPEI
                 Move Spaces             To ENP4CPRI
                 Move Spaces             To ENP4XPEI
                 Move Spaces             To ENP4XPRI
                 Move Spaces             To ENP4WPEI
                 Move Spaces             To ENP4WPRI
                 Move Spaces             To ENP4STAI
                 Move Spaces             To ENP4REJI
                 Move ' '             To ENP4OPTI
                 Move 'Commercial Policy Deleted'
                   To  ERP4FLDO
                 EXEC CICS SEND MAP ('SSMAPP4')
                           FROM(SSMAPP4O)
                           MAPSET ('SSMAP')
                 END-EXEC
                 GO TO ENDIT-STARTIT
             WHEN OTHER
                 Move 'Please enter a valid option'
                   To  ERP4FLDO
                 Move -1 To ENP4OPTL
                 EXEC CICS SEND MAP ('SSMAPP4')
                           FROM(SSMAPP4O)
                           MAPSET ('SSMAP')
                           CURSOR
                 END-EXEC
                 GO TO ENDIT-STARTIT
           END-EVALUATE.
           EXEC CICS RETURN
           END-EXEC.
       ENDIT-STARTIT.
           EXEC CICS RETURN
                TRANSID('SSP4')
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
       CLEARIT.
           Initialize SSMAPP4I.
           EXEC CICS SEND MAP ('SSMAPP4')
                     MAPSET ('SSMAP')
                     MAPONLY
           END-EXEC
           EXEC CICS RETURN
                TRANSID('SSP4')
                COMMAREA(COMM-AREA)
                END-EXEC.
       NO-ADD.
           Evaluate CA-RETURN-CODE
             When 70
               Move 'Customer does not exist'        To  ERP4FLDO
               Go To ERROR-OUT
             When Other
               Move 'Error Adding Commercial Policy' To  ERP4FLDO
               Go To ERROR-OUT
           End-Evaluate.
       NO-UPD.
           Move 'Error Updating Commercial Policy'   To  ERP4FLDO
           Go To ERROR-OUT.
       NO-DELETE.
           Move 'Error Deleting Commercial Policy'   To  ERP4FLDO
           Go To ERROR-OUT.
       NO-DATA.
           Move 'No data was returned.'              To  ERP4FLDO
           Go To ERROR-OUT.
       ERROR-OUT.
           EXEC CICS SEND MAP ('SSMAPP4')
                     FROM(SSMAPP4O)
                     MAPSET ('SSMAP')
           END-EXEC.
           Initialize SSMAPP4I.
           Initialize SSMAPP4O.
           Initialize COMM-AREA.
           GO TO ENDIT-STARTIT.
