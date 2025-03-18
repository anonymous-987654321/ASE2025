       CBL CICS(SP)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ZECSPLT.
       AUTHOR.     Randy Frerking and Rich Jackson.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ST-CODE                PIC  X(02) VALUE SPACES.
       01  EOF                    PIC  X(01) VALUE SPACES.
       01  ZC                     PIC  X(02) VALUE 'ZC'.
       01  URI-MAP.
           02  URI-PREFIX         PIC  X(04) VALUE SPACES.
           02  URI-SUFFIX         PIC  X(04) VALUE SPACES.
       01  URI-TRAN               PIC  X(04) VALUE SPACES.
       01  ZC-LENGTH              PIC S9(04) COMP VALUE 4.
       01  ZX-TRANID.
           02  FILLER             PIC  X(02) VALUE 'ZX'.
           02  ZX-SUFFIX          PIC  X(02) VALUE SPACES.
       01  CSSL                   PIC  X(04) VALUE '@tdq@'.
       01  TD-LENGTH              PIC S9(04) COMP VALUE ZEROES.
       01  TD-RECORD.
           02  FILLER             PIC  X(13) VALUE 'zECS start ZX'.
           02  TD-SUFFIX          PIC  X(02) VALUE SPACES.
           02  FILLER             PIC  X(01) VALUE SPACES.
           02  FILLER             PIC  X(04) VALUE 'for '.
           02  TD-TRAN            PIC  X(04) VALUE SPACES.
           02  FILLER             PIC  X(01) VALUE SPACES.
           02  TD-PATH            PIC  X(80) VALUE SPACES.
       01  URI-PATH               PIC X(256) VALUE SPACES.
       LINKAGE SECTION.
       01  DFHCOMMAREA            PIC  X(01).
       PROCEDURE DIVISION.
           PERFORM 1000-INQUIRE-START      THRU 1000-EXIT.
           PERFORM 2000-INQUIRE-NEXT       THRU 2000-EXIT
                   WITH TEST AFTER
                   UNTIL EOF EQUAL 'Y'.
           PERFORM 3000-INQUIRE-END        THRU 3000-EXIT.
           PERFORM 9000-RETURN             THRU 9000-EXIT.
       1000-INQUIRE-START.
           EXEC CICS INQUIRE URIMAP START
                NOHANDLE
           END-EXEC.
       1000-EXIT.
           EXIT.
       2000-INQUIRE-NEXT.
           EXEC CICS INQUIRE URIMAP(URI-MAP)
                PATH(URI-PATH)
                TRANSACTION(URI-TRAN)
                NEXT
                NOHANDLE
           END-EXEC.
           IF  EIBRESP NOT EQUAL DFHRESP(NORMAL)
               MOVE 'Y'    TO EOF.
           IF  EIBRESP     EQUAL DFHRESP(NORMAL)
               PERFORM 2100-CHECK-URIMAP   THRU 2100-EXIT.
       2000-EXIT.
           EXIT.
       2100-CHECK-URIMAP.
           IF  URI-PREFIX(1:2) EQUAL ZC
           AND URI-SUFFIX      EQUAL SPACES
               PERFORM 2200-START          THRU 2200-EXIT.
       2100-EXIT.
           EXIT.
       2200-START.
           MOVE URI-PREFIX(3:2)      TO ZX-SUFFIX
                                        TD-SUFFIX.
           MOVE LENGTH OF TD-RECORD  TO TD-LENGTH.
           EXEC CICS START TRANSID(ZX-TRANID)
                FROM(URI-TRAN)
                LENGTH(4)
                NOHANDLE
           END-EXEC.
           MOVE URI-TRAN             TO TD-TRAN.
           MOVE URI-PATH             TO TD-PATH.
           EXEC CICS WRITEQ TD QUEUE(CSSL)
                FROM  (TD-RECORD)
                LENGTH(TD-LENGTH)
                NOHANDLE
           END-EXEC.
           EXEC CICS WRITE OPERATOR
                TEXT(TD-RECORD)
                NOHANDLE
           END-EXEC.
       2200-EXIT.
           EXIT.
       3000-INQUIRE-END.
           EXEC CICS INQUIRE URIMAP END
                NOHANDLE
           END-EXEC.
       3000-EXIT.
           EXIT.
       9000-RETURN.
           EXEC CICS RETURN
           END-EXEC.
       9000-EXIT.
           EXIT.
