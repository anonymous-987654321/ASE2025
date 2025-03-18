       CBL CICS('SP,EDF')
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRDTAGY4.
       AUTHOR. Jon Collett.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-370.
       OBJECT-COMPUTER.  IBM-370.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 SORTCODE           PIC 9(6) VALUE 987654.
       01 WS-CONT-IN.
          03 WS-CONT-IN-EYECATCHER      PIC X(4).
          03 WS-CONT-IN-KEY.
             05 WS-CONT-IN-SORTCODE     PIC 9(6) DISPLAY.
             05 WS-CONT-IN-NUMBER       PIC 9(10) DISPLAY.
          03 WS-CONT-IN-NAME            PIC X(60).
          03 WS-CONT-IN-ADDRESS         PIC X(160).
          03 WS-CONT-IN-DATE-OF-BIRTH   PIC 9(8).
          03 WS-CONT-IN-DOB-GROUP REDEFINES WS-CONT-IN-DATE-OF-BIRTH.
             05 WS-CONT-IN-BIRTH-DAY    PIC 99.
             05 WS-CONT-IN-BIRTH-MONTH  PIC 99.
             05 WS-CONT-IN-BIRTH-YEAR   PIC 9999.
          03 WS-CONT-IN-CREDIT-SCORE    PIC 999.
          03 WS-CONT-IN-CS-REVIEW-DATE  PIC 9(8).
          03 WS-CONT-IN-SUCCESS         PIC X.
          03 WS-CONT-IN-FAIL-CODE       PIC X.
       01 WS-CICS-WORK-AREA.
          05 WS-CICS-RESP               PIC S9(8) COMP.
          05 WS-CICS-RESP2              PIC S9(8) COMP.
       01 WS-DELAY-AMT                  PIC S9(8) COMP
                                                   VALUE 0.
       01 WS-CONTAINER-NAME             PIC X(16)  VALUE SPACES.
       01 WS-CHANNEL-NAME               PIC X(16)  VALUE SPACES.
       01 WS-CONTAINER-LEN              PIC S9(8) COMP
                                                   VALUE 0.
       01 WS-NEW-CREDSCORE              PIC 999    VALUE 0.
       01 WS-SEED                       PIC S9(15) COMP.
       01 WS-U-TIME                     PIC S9(15) COMP-3.
       01 WS-ORIG-DATE                  PIC X(10).
       01 WS-ORIG-DATE-GRP REDEFINES WS-ORIG-DATE.
          03 WS-ORIG-DATE-DD            PIC 99.
          03 FILLER                     PIC X.
          03 WS-ORIG-DATE-MM            PIC 99.
          03 FILLER                     PIC X.
          03 WS-ORIG-DATE-YYYY          PIC 9999.
       01 WS-ORIG-DATE-GRP-X.
          03 WS-ORIG-DATE-DD-X          PIC XX.
          03 FILLER                     PIC X      VALUE '.'.
          03 WS-ORIG-DATE-MM-X          PIC XX.
          03 FILLER                     PIC X      VALUE '.'.
          03 WS-ORIG-DATE-YYYY-X        PIC X(4).
       01 WS-TIME-DATA.
          03 WS-TIME-NOW                PIC 9(6).
          03 WS-TIME-NOW-GRP REDEFINES WS-TIME-NOW.
             05 WS-TIME-NOW-GRP-HH      PIC 99.
             05 WS-TIME-NOW-GRP-MM      PIC 99.
             05 WS-TIME-NOW-GRP-SS      PIC 99.
       01 WS-ABEND-PGM                  PIC X(8)   VALUE 'ABNDPROC'.
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
       PROCEDURE DIVISION.
       PREMIERE SECTION.
       A010.
           MOVE 'CIPD            ' TO WS-CONTAINER-NAME.
           MOVE 'CIPCREDCHANN    ' TO WS-CHANNEL-NAME.
           MOVE EIBTASKN           TO WS-SEED.
           COMPUTE WS-DELAY-AMT = ((3 - 1)
           EXEC CICS DELAY
                FOR SECONDS(WS-DELAY-AMT)
                RESP(WS-CICS-RESP)
                RESP2(WS-CICS-RESP2)
           END-EXEC.
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
              MOVE 'PLOP'      TO ABND-CODE
              EXEC CICS ASSIGN PROGRAM(ABND-PROGRAM)
              END-EXEC
              MOVE ZEROS      TO ABND-SQLCODE
              STRING 'A010  - *** The delay messed up! ***'
                      DELIMITED BY SIZE,
                      ' EIBRESP=' DELIMITED BY SIZE,
                      ABND-RESPCODE DELIMITED BY SIZE,
                      ' RESP2=' DELIMITED BY SIZE,
                      ABND-RESP2CODE DELIMITED BY SIZE
                      INTO ABND-FREEFORM
              END-STRING
              EXEC CICS LINK PROGRAM(WS-ABEND-PGM)
                          COMMAREA(ABNDINFO-REC)
              END-EXEC
              DISPLAY '*** The delay messed up ! ***'
              EXEC CICS ABEND
                 ABCODE('PLOP')
              END-EXEC
           END-IF.
           COMPUTE WS-CONTAINER-LEN = LENGTH OF WS-CONT-IN.
           EXEC CICS GET CONTAINER(WS-CONTAINER-NAME)
                     CHANNEL(WS-CHANNEL-NAME)
                     INTO(WS-CONT-IN)
                     FLENGTH(WS-CONTAINER-LEN)
                     RESP(WS-CICS-RESP)
                     RESP2(WS-CICS-RESP2)
           END-EXEC.
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
              DISPLAY 'CRDTAGY4 - UNABLE TO GET CONTAINER. RESP='
                 WS-CICS-RESP ', RESP2=' WS-CICS-RESP2
              DISPLAY 'CONTAINER=' WS-CONTAINER-NAME ' CHANNEL='
                       WS-CHANNEL-NAME ' FLENGTH='
                       WS-CONTAINER-LEN
              PERFORM GET-ME-OUT-OF-HERE
           END-IF.
           COMPUTE WS-NEW-CREDSCORE = ((999 - 1)
           MOVE WS-NEW-CREDSCORE TO WS-CONT-IN-CREDIT-SCORE.
           COMPUTE WS-CONTAINER-LEN = LENGTH OF WS-CONT-IN.
           EXEC CICS PUT CONTAINER(WS-CONTAINER-NAME)
                         FROM(WS-CONT-IN)
                         FLENGTH(WS-CONTAINER-LEN)
                         CHANNEL(WS-CHANNEL-NAME)
                         RESP(WS-CICS-RESP)
                         RESP2(WS-CICS-RESP2)
           END-EXEC.
           IF WS-CICS-RESP NOT = DFHRESP(NORMAL)
              DISPLAY 'CRDTAGY4- UNABLE TO PUT CONTAINER. RESP='
                 WS-CICS-RESP ', RESP2=' WS-CICS-RESP2
              DISPLAY  'CONTAINER='  WS-CONTAINER-NAME
              ' CHANNEL=' WS-CHANNEL-NAME ' FLENGTH='
                    WS-CONTAINER-LEN
              PERFORM GET-ME-OUT-OF-HERE
           END-IF.
           PERFORM GET-ME-OUT-OF-HERE.
       A999.
           EXIT.
       GET-ME-OUT-OF-HERE SECTION.
       GMOFH010.
           EXEC CICS RETURN
           END-EXEC.
       GMOFH999.
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
