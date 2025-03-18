       IDENTIFICATION DIVISION.
       PROGRAM-ID. 'EPSCSMRX'.
       AUTHOR. WD4Z.
       INSTALLATION. 9.0.0.V200809191411.
       DATE-WRITTEN. 1/19/09 2:11 PM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       1 XML2LS-LANG-BUFFER-LENGTH PIC 9(9) COMP.
       1 LS2XML-LANG-BUFFER-LENGTH PIC 9(9) COMP.
       1 LS2XML-XML-BUFFER-LENGTH PIC 9(9) COMP.
       1 XML2LS-XML-CCSID PIC 9(9) COMP.
       1 HOST-LANG-CCSID PIC 9(9) COMP.
       1 LS2XML-XML-CCSID PIC 9(9) COMP.
       1 XML2LS-PROPERTIES PIC X.
       1 LS2XML-PROPERTIES PIC X.
       PROCEDURE DIVISION USING
           XML2LS-LANG-BUFFER-LENGTH
           LS2XML-LANG-BUFFER-LENGTH
           LS2XML-XML-BUFFER-LENGTH
           XML2LS-XML-CCSID
           HOST-LANG-CCSID
           LS2XML-XML-CCSID
           XML2LS-PROPERTIES
           LS2XML-PROPERTIES
           .
       MAINLINE SECTION.
           IF ADDRESS OF XML2LS-LANG-BUFFER-LENGTH
                         NOT EQUAL NULL
            MOVE 106
              TO XML2LS-LANG-BUFFER-LENGTH
           END-IF
           IF ADDRESS OF LS2XML-LANG-BUFFER-LENGTH
                         NOT EQUAL NULL
            MOVE 106
              TO LS2XML-LANG-BUFFER-LENGTH
           END-IF
           IF ADDRESS OF LS2XML-XML-BUFFER-LENGTH
                         NOT EQUAL NULL
            MOVE 758
              TO LS2XML-XML-BUFFER-LENGTH
           END-IF
           IF ADDRESS OF XML2LS-XML-CCSID
                         NOT EQUAL NULL
            MOVE 1140
              TO XML2LS-XML-CCSID
           END-IF
           IF ADDRESS OF HOST-LANG-CCSID
                         NOT EQUAL NULL
            MOVE 1140
              TO HOST-LANG-CCSID
           END-IF
           IF ADDRESS OF LS2XML-XML-CCSID
                         NOT EQUAL NULL
            MOVE 1140
              TO LS2XML-XML-CCSID
           END-IF
           IF ADDRESS OF XML2LS-PROPERTIES
                         NOT EQUAL NULL
            MOVE X'00'
              TO XML2LS-PROPERTIES
           END-IF
           IF ADDRESS OF LS2XML-PROPERTIES
                         NOT EQUAL NULL
            MOVE X'00'
              TO LS2XML-PROPERTIES
           END-IF
           GOBACK
           .
       END PROGRAM 'EPSCSMRX'.
