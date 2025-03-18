       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TPROG01.
       AUTHOR.        COMPUWARE ISPW TRAINING.
       DATE-WRITTEN.  JANUARY 24TH, 1996.
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE  ASSIGN UT-S-INPUT
             ORGANIZATION IS SEQUENTIAL
             ACCESS IS SEQUENTIAL.
           SELECT OUTFILE ASSIGN UT-S-OUTPUT
             ORGANIZATION IS SEQUENTIAL
             ACCESS IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  INFILE
           LABEL RECORDS OMITTED
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F
           DATA RECORD IS INPUT-REC.
       01  INPUT-REC         PIC X(80).
       FD  OUTFILE
           LABEL RECORDS OMITTED
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F
           DATA RECORD IS OUTPUT-REC.
       01  OUTPUT-REC.
           05  CC                PIC X(1).
           05  OUTPUT-LINE       PIC X(132).
       WORKING-STORAGE SECTION.
       01  STAR-LINE-1.
           05 FILLER                  PIC X(20).
           05 FILLER                  PIC X(90) VALUE ALL '*'.
       01  FLASH-LINE.
           05 FILLER                  PIC X(20).
           05 FILLER                  PIC X(5) VALUE ALL '*'.
           05 FILLER                  PIC X(37).
           05 FILLER                  PIC X(17)
              VALUE 'FLASH FLASH FLASH'.
           05 FILLER                  PIC X(26).
           05 FILLER                  PIC X(5) VALUE ALL '*'.
       01  BODY-LINE.
           05 FILLER                  PIC X(20).
           05 FILLER                  PIC X(5) VALUE ALL '*'.
           05 BODY-TEXT               PIC X(80).
           05 FILLER                  PIC X(5) VALUE ALL '*'.
       01  MESSAGE-LINE.
           05 FILLER                  PIC X(44) VALUE
           ' A VERY IMPORTANT MESSAGE BROUGHT TO YOU BY '.
           05 MESSANGER               PIC X(36).
       PROCEDURE DIVISION.
       00000-MAIN-PROCEDURE.
           OPEN OUTPUT OUTFILE.
           MOVE 'R' TO ACTION-FLAG.
           CALL 'TSUBR01' USING PASS-ME-AROUND.
           IF NOT INFILE-EOF THEN
              MOVE TEXT-PORTION TO MESSANGER
           MOVE SPACES TO CC.
           MOVE STAR-LINE-1 TO OUTPUT-REC.
           WRITE OUTPUT-REC.
           WRITE OUTPUT-REC.
           WRITE OUTPUT-REC.
           MOVE SPACES TO BODY-TEXT.
           MOVE BODY-LINE TO OUTPUT-REC.
           WRITE OUTPUT-REC.
           MOVE FLASH-LINE TO OUTPUT-REC.
           WRITE OUTPUT-REC.
           WRITE OUTPUT-REC.
           WRITE OUTPUT-REC.
           MOVE SPACES TO BODY-TEXT.
           MOVE BODY-LINE TO OUTPUT-REC.
           WRITE OUTPUT-REC.
           WRITE OUTPUT-REC.
           MOVE MESSAGE-LINE TO BODY-TEXT.
           MOVE BODY-LINE TO OUTPUT-REC.
           WRITE OUTPUT-REC.
           MOVE SPACES TO BODY-TEXT.
           MOVE BODY-LINE TO OUTPUT-REC.
           WRITE OUTPUT-REC.
           PERFORM GET-MESSAGE THRU GET-MESSAGE-X
              UNTIL INFILE-EOF.
           MOVE SPACES TO BODY-TEXT.
           MOVE BODY-LINE TO OUTPUT-REC.
           WRITE OUTPUT-REC.
           WRITE OUTPUT-REC.
           MOVE STAR-LINE-1 TO OUTPUT-REC.
           WRITE OUTPUT-REC.
           WRITE OUTPUT-REC.
           WRITE OUTPUT-REC.
           MOVE 'C' TO ACTION-FLAG.
           CALL 'TSUBR01' USING PASS-ME-AROUND.
           CLOSE OUTFILE.
           GOBACK.
       GET-MESSAGE.
              MOVE 'R' TO ACTION-FLAG.
              CALL 'TSUBR01' USING PASS-ME-AROUND.
                 IF NOT INFILE-EOF THEN
                   MOVE TEXT-PORTION TO BODY-TEXT
                   MOVE BODY-LINE TO OUTPUT-REC
                   WRITE OUTPUT-REC.
       GET-MESSAGE-X.
           EXIT.