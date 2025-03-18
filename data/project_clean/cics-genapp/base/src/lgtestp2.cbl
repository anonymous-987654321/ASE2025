       IDENTIFICATION DIVISION.
       PROGRAM-ID. LGTESTP2.
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
           Initialize SSMAPP2I.
           Initialize SSMAPP2O.
           Initialize COMM-AREA.
           MOVE '0000000000'   To ENP2CNOO.
           MOVE '0000000000'   To ENP2PNOO.
           EXEC CICS SEND MAP ('SSMAPP2')
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
           EXEC CICS RECEIVE MAP('SSMAPP2')
                     INTO(SSMAPP2I)
                     MAPSET('SSMAP') END-EXEC.
           EVALUATE ENP2OPTO
             WHEN '1'
                 Move '01IEND'   To CA-REQUEST-ID
                 Move ENP2CNOO   To CA-CUSTOMER-NUM
                 Move ENP2PNOO   To CA-POLICY-NUM
                 EXEC CICS LINK PROGRAM('LGIPOL01')
                           COMMAREA(COMM-AREA)
                           LENGTH(32500)
                 END-EXEC
                 IF CA-RETURN-CODE > 0
                   GO TO NO-DATA
                 END-IF
                 Move CA-ISSUE-DATE     To  ENP2IDAI
                 Move CA-EXPIRY-DATE    To  ENP2EDAI
                 Move CA-E-FUND-NAME    To  ENP2FNMI
                 Move CA-E-TERM         To  ENP2TERI
                 Move CA-E-SUM-ASSURED  To  ENP2SUMI
                 Move CA-E-LIFE-ASSURED To  ENP2LIFI
                 Move CA-E-WITH-PROFITS To  ENP2WPRI
                 Move CA-E-MANAGED-FUND To  ENP2MANI
                 Move CA-E-EQUITIES     To  ENP2EQUI
                 EXEC CICS SEND MAP ('SSMAPP2')
                           FROM(SSMAPP2O)
                           MAPSET ('SSMAP')
                 END-EXEC
                 GO TO ENDIT-STARTIT
             WHEN '2'
                 Move '01AEND'          To CA-REQUEST-ID
                 Move ENP2CNOI          To CA-CUSTOMER-NUM
                 Move 0                 To CA-PAYMENT
                 Move 0                 To CA-BROKERID
                 Move '        '        To CA-BROKERSREF
                 Move ENP2IDAI          To CA-ISSUE-DATE
                 Move ENP2EDAI          To CA-EXPIRY-DATE
                 Move ENP2FNMI          To CA-E-FUND-NAME
                 Move ENP2TERI          To CA-E-TERM
                 Move ENP2SUMI          To CA-E-SUM-ASSURED
                 Move ENP2LIFI          To CA-E-LIFE-ASSURED
                 Move ENP2WPRI          To CA-E-WITH-PROFITS
                 Move ENP2MANI          To CA-E-MANAGED-FUND
                 Move ENP2EQUI          To CA-E-EQUITIES
                 EXEC CICS LINK PROGRAM('LGAPOL01')
                           COMMAREA(COMM-AREA)
                           LENGTH(32500)
                 END-EXEC
                 IF CA-RETURN-CODE > 0
                   Exec CICS Syncpoint Rollback End-Exec
                   GO TO NO-ADD
                 END-IF
                 Move CA-CUSTOMER-NUM To ENP2CNOI
                 Move CA-POLICY-NUM   To ENP2PNOI
                 Move CA-E-FUND-NAME  To ENP2FNMI
                 Move ' '             To ENP2OPTI
                 Move 'New Life Policy Inserted'
                   To  ERP2FLDO
                 EXEC CICS SEND MAP ('SSMAPP2')
                           FROM(SSMAPP2O)
                           MAPSET ('SSMAP')
                 END-EXEC
                 GO TO ENDIT-STARTIT
             WHEN '3'
                 Move '01DEND'   To CA-REQUEST-ID
                 Move ENP2CNOO   To CA-CUSTOMER-NUM
                 Move ENP2PNOO   To CA-POLICY-NUM
                 EXEC CICS LINK PROGRAM('LGDPOL01')
                           COMMAREA(COMM-AREA)
                           LENGTH(32500)
                 END-EXEC
                 IF CA-RETURN-CODE > 0
                   Exec CICS Syncpoint Rollback End-Exec
                   GO TO NO-DELETE
                 END-IF
                 Move Spaces            To  ENP2IDAI
                 Move Spaces            To  ENP2EDAI
                 Move Spaces            To  ENP2FNMI
                 Move Spaces            To  ENP2TERI
                 Move Spaces            To  ENP2SUMI
                 Move Spaces            To  ENP2LIFI
                 Move Spaces            To  ENP2WPRI
                 Move Spaces            To  ENP2MANI
                 Move Spaces            To  ENP2EQUI
                 Move 'Life Policy Deleted'
                   To  ERP2FLDO
                 EXEC CICS SEND MAP ('SSMAPP2')
                           FROM(SSMAPP2O)
                           MAPSET ('SSMAP')
                 END-EXEC
                 GO TO ENDIT-STARTIT
             WHEN '4'
                 Move '01IEND'   To CA-REQUEST-ID
                 Move ENP2CNOO   To CA-CUSTOMER-NUM
                 Move ENP2PNOO   To CA-POLICY-NUM
                 EXEC CICS LINK PROGRAM('LGIPOL01')
                           COMMAREA(COMM-AREA)
                           LENGTH(32500)
                 END-EXEC
                 IF CA-RETURN-CODE > 0
                   GO TO NO-DATA
                 END-IF
                 Move CA-ISSUE-DATE     To  ENP2IDAI
                 Move CA-EXPIRY-DATE    To  ENP2EDAI
                 Move CA-E-FUND-NAME    To  ENP2FNMI
                 Move CA-E-TERM         To  ENP2TERI
                 Move CA-E-SUM-ASSURED  To  ENP2SUMI
                 Move CA-E-LIFE-ASSURED To  ENP2LIFI
                 Move CA-E-WITH-PROFITS To  ENP2WPRI
                 Move CA-E-MANAGED-FUND To  ENP2MANI
                 Move CA-E-EQUITIES     To  ENP2EQUI
                 EXEC CICS SEND MAP ('SSMAPP2')
                           FROM(SSMAPP2O)
                           MAPSET ('SSMAP')
                 END-EXEC
                 EXEC CICS RECEIVE MAP('SSMAPP2')
                           INTO(SSMAPP2I)
                           MAPSET('SSMAP') END-EXEC
                 Move '01UEND'          To CA-REQUEST-ID
                 Move ENP2CNOI          To CA-CUSTOMER-NUM
                 Move 0                 To CA-PAYMENT
                 Move 0                 To CA-BROKERID
                 Move '        '        To CA-BROKERSREF
                 Move ENP2IDAI          To CA-ISSUE-DATE
                 Move ENP2EDAI          To CA-EXPIRY-DATE
                 Move ENP2FNMI          To CA-E-FUND-NAME
                 Move ENP2TERI          To CA-E-TERM
                 Move ENP2SUMI          To CA-E-SUM-ASSURED
                 Move ENP2LIFI          To CA-E-LIFE-ASSURED
                 Move ENP2WPRI          To CA-E-WITH-PROFITS
                 Move ENP2MANI          To CA-E-MANAGED-FUND
                 Move ENP2EQUI          To CA-E-EQUITIES
                 EXEC CICS LINK PROGRAM('LGUPOL01')
                           COMMAREA(COMM-AREA)
                           LENGTH(32500)
                 END-EXEC
                 IF CA-RETURN-CODE > 0
                   GO TO NO-UPD
                 END-IF
                 Move CA-CUSTOMER-NUM To ENP2CNOI
                 Move CA-POLICY-NUM   To ENP2PNOI
                 Move ' '             To ENP2OPTI
                 Move 'Life Policy Updated'
                   To  ERP2FLDO
                 EXEC CICS SEND MAP ('SSMAPP2')
                           FROM(SSMAPP2O)
                           MAPSET ('SSMAP')
                 END-EXEC
                 GO TO ENDIT-STARTIT
             WHEN OTHER
                 Move 'Please enter a valid option'
                   To  ERP2FLDO
                 Move -1 To ENP2OPTL
                 EXEC CICS SEND MAP ('SSMAPP2')
                           FROM(SSMAPP2O)
                           MAPSET ('SSMAP')
                           CURSOR
                 END-EXEC
                 GO TO ENDIT-STARTIT
           END-EVALUATE.
           EXEC CICS RETURN
           END-EXEC.
       ENDIT-STARTIT.
           EXEC CICS RETURN
                TRANSID('SSP2')
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
           Initialize SSMAPP2I.
           EXEC CICS SEND MAP ('SSMAPP2')
                     MAPSET ('SSMAP')
                     MAPONLY
           END-EXEC
           EXEC CICS RETURN
                TRANSID('SSP2')
                COMMAREA(COMM-AREA)
                END-EXEC.
       NO-ADD.
           Evaluate CA-RETURN-CODE
             When 70
               Move 'Customer does not exist'          To  ERP1FLDO
               Go To ERROR-OUT
             When Other
               Move 'Error Adding Life Policy'        To  ERP1FLDO
               Go To ERROR-OUT
           End-Evaluate.
       NO-UPD.
           Move 'Error Updating Life Policy'       To  ERP2FLDO
           Go To ERROR-OUT.
       NO-DELETE.
           Move 'Error Deleting Life Policy'       To  ERP2FLDO
           Go To ERROR-OUT.
       NO-DATA.
           Move 'No data was returned.'            To  ERP2FLDO
           Go To ERROR-OUT.
       ERROR-OUT.
           EXEC CICS SEND MAP ('SSMAPP2')
                     FROM(SSMAPP2O)
                     MAPSET ('SSMAP')
           END-EXEC.
           Initialize SSMAPP2I.
           Initialize SSMAPP2O.
           Initialize COMM-AREA.
           GO TO ENDIT-STARTIT.
