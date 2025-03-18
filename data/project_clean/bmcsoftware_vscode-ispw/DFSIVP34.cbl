 CBL  APOST                                                                     
       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID.  DFSIVP34.                                                   
       ENVIRONMENT DIVISION.                                                    
       CONFIGURATION SECTION.                                                   
       SOURCE-COMPUTER. IBM-370.                                                
       OBJECT-COMPUTER. IBM-370.                                                
       DATA DIVISION.                                                           
       WORKING-STORAGE SECTION.                                                 
       77  GET-UNIQUE       PICTURE X(4)  VALUE 'GU  '.                         
       77  GET-HOLD-UNIQUE  PICTURE X(4)  VALUE 'GHU '.                         
       77  GET-NEXT         PICTURE X(4)  VALUE 'GN  '.                         
       77  GET-HOLD-NEXT    PICTURE X(4)  VALUE 'GHN '.                         
       77  DLET             PICTURE X(4)  VALUE 'DLET'.                         
       77  ISRT             PICTURE X(4)  VALUE 'ISRT'.                         
       77  REPL             PICTURE X(4)  VALUE 'REPL'.                         
       77  MESSAGE-EXIST    PIC X(2) VALUE 'CF'.                                
       77  NO-MORE-SEGMENT  PIC X(2) VALUE 'QD'.                                
       77  NO-MORE-MESSAGE  PIC X(2) VALUE 'QC'.                                
       77  MDEL    PICTURE X(40) VALUE                                          
                     'ENTRY WAS DELETED                       '.                
       77  MADD    PICTURE X(40) VALUE                                          
                     'ENTRY WAS ADDED                         '.                
       77  MDIS    PICTURE X(40) VALUE                                          
                     'ENTRY WAS DISPLAYED                     '.                
       77  MUPD    PICTURE X(40) VALUE                                          
                     'ENTRY WAS UPDATED                       '.                
       77  MEND    PICTURE X(40) VALUE                                          
                     'CONVERSATION HAS ENDED                  '.                
       77  MMORE   PICTURE X(40) VALUE                                          
                     'DATA IS NOT ENOUGH.  PLEASE KEY IN MORE '.                
       77  MINV    PICTURE X(40) VALUE                                          
                     'PROCESS CODE IS NOT VALID               '.                
       77  MNODATA PICTURE X(40) VALUE                                          
                     'NO DATA WAS INPUT.  PLEASE KEY IN MORE  '.                
       77  MNONAME PICTURE X(40) VALUE                                          
                     'LAST NAME WAS NOT SPECIFIED             '.                
       77  MNOENT  PICTURE X(40) VALUE                                          
                     'SPECIFIED PERSON WAS NOT FOUND          '.                
       77  MISRTE  PICTURE X(40) VALUE                                          
                     'ADDITION OF ENTRY HAS FAILED            '.                
       77  MDLETE  PICTURE X(40) VALUE                                          
                     'DELETION OF ENTRY HAS FAILED            '.                
       77  MREPLE  PICTURE X(40) VALUE                                          
                     'UPDATE OF ENTRY HAS FAILED              '.                
       77  SSA1    PICTURE X(9) VALUE 'A1111111 '.                              
       77  MODNAME PICTURE X(8) VALUE SPACES.                                   
       77  TRAN-CODE  PICTURE X(8) VALUE 'IVTCB'.                               
       77  REPLY      PICTURE X(16).                                            
       77  TEMP-ONE   PICTURE X(8) VALUE SPACES.                                
       77  TEMP-TWO   PICTURE X(8) VALUE SPACES.                                
       01  INPUT-MSG.                                                           
           02  IN-LL          PICTURE S9(3) COMP.                               
           02  IN-ZZ          PICTURE S9(3) COMP.                               
           02  IN-FILL        PICTURE X(4).                                     
           02  IN-COMMAND     PICTURE X(8).                                     
           02  TEMP-COMMAND REDEFINES IN-COMMAND.                               
               04  TEMP-IOCMD    PIC X(3).                                      
               04  TEMP-FILLER   PIC X(5).                                      
           02  IN-LAST-NAME   PICTURE X(10).                                    
           02  IN-FIRST-NAME  PICTURE X(10).                                    
           02  IN-EXTENSION   PICTURE X(10).                                    
           02  IN-ZIP-CODE    PICTURE X(7).                                     
       01  OUTPUT-AREA.                                                         
           02  OUT-LL       PICTURE S9(3) COMP VALUE +95.                       
           02  OUT-ZZ       PICTURE S9(3) COMP VALUE +0.                        
           02  OUTPUT-LINE  PICTURE X(85) VALUE SPACES.                         
           02  OUTPUT-DATA REDEFINES OUTPUT-LINE.                               
               04  OUT-MESSAGE   PIC X(40).                                     
               04  OUT-COMMAND   PIC X(8).                                      
               04  OUT-DATA-TYPE.                                               
                   06  OUT-LAST-NAME   PIC X(10).                               
                   06  OUT-FIRST-NAME  PIC X(10).                               
                   06  OUT-EXTENSION   PIC X(10).                               
                   06  OUT-ZIP-CODE    PIC X(7).                                
           02  OUT-SEGMENT-NO  PICTURE X(4) VALUE '0001'.                       
       01  IOAREA.                                                              
           02  IO-LINE PICTURE X(37) VALUE SPACES.                              
           02  IO-DATA REDEFINES IO-LINE.                                       
               04  IO-LAST-NAME    PIC X(10).                                   
               04  IO-FIRST-NAME   PIC X(10).                                   
               04  IO-EXTENSION    PIC X(10).                                   
               04  IO-ZIP-CODE     PIC X(7).                                    
           02  IO-FILLER       PIC X(3) VALUE SPACES.                           
           02  IO-COMMAND      PIC X(8) VALUE SPACES.                           
       01  SPA.                                                                 
           02  SPA-LL        PICTURE X(2).                                      
           02  SPA-ZZ        PICTURE X(4).                                      
           02  SPA-TRANCODE  PICTURE X(8).                                      
           02  SPA-CALL      PICTURE X(2).                                      
           02  SPA-COMMAND   PICTURE X(8).                                      
           02  SPA-DATA.                                                        
               04  SPA-LAST-NAME    PIC X(10).                                  
               04  SPA-FIRST-NAME   PIC X(10).                                  
               04  SPA-EXTENSION    PIC X(10).                                  
               04  SPA-ZIP-CODE     PIC X(7).                                   
           02  FILLER        PICTURE X(19).                                     
       01 DC-TEXT.                                                              
          02  TEXT1         PIC  X(7) VALUE 'STATUS '.                          
          02  ERROR-STATUS  PIC  X(2).                                          
          02  TEXT2         PIC  X(12) VALUE 'DLI  CALL = '.                    
          02  ERROR-CALL    PIC  X(4).                                          
       01 SSA.                                                                  
          02  SEGMENT-NAME  PIC X(8)  VALUE 'A1111111'.                         
          02  SEG-KEY-NAME  PIC X(11) VALUE '(A1111111 ='.                      
          02  SSA-KEY       PIC X(10).                                          
          02  FILLER        PIC X VALUE ')'.                                    
       01 FLAGS.                                                                
          02  SET-DATA-FLAG  PIC X VALUE '0'.                                   
             88  NO-SET-DATA       VALUE '1'.                                   
          02  TADD-FLAG      PIC X VALUE '0'.                                   
             88  PROCESS-TADD      VALUE '1'.                                   
       01 COUNTERS.                                                             
          02  SPA-CALL-NO    PIC   9(2) COMP VALUE 0.                           
          02  L-SPACE-CTR    PIC   9(2) COMP VALUE 0.                           
       LINKAGE SECTION.                                                         
       01  IOPCB.                                                               
           02  LTERM-NAME   PICTURE X(8).                                       
           02  FILLER       PICTURE X(2).                                       
           02  TPSTATUS     PICTURE XX.                                         
           02  FILLER       PICTURE X(20).                                      
       01  DBPCB.                                                               
           02  DBNAME       PICTURE X(8).                                       
           02  SEG-LEVEL-NO PICTURE X(2).                                       
           02  DBSTATUS     PICTURE XX.                                         
           02  FILLER       PICTURE X(20).                                      
       PROCEDURE DIVISION USING IOPCB, DBPCB.                                   
       MAIN-RTN.                                                                
           MOVE GET-UNIQUE TO ERROR-CALL.                                       
           CALL 'CBLTDLI' USING GET-UNIQUE, IOPCB, SPA.                         
           IF TPSTATUS  = '  ' OR MESSAGE-EXIST                                 
           THEN                                                                 
             CALL 'CBLTDLI' USING GET-NEXT, IOPCB, INPUT-MSG                    
             IF TPSTATUS = SPACES                                               
               THEN PERFORM PROCESS-INPUT THRU PROCESS-INPUT-END                
             ELSE IF TPSTATUS = NO-MORE-SEGMENT                                 
                  THEN GOBACK                                                   
                  ELSE                                                          
                    MOVE GET-NEXT TO ERROR-CALL                                 
                    PERFORM WRITE-DC-TEXT THRU WRITE-DC-TEXT-END                
           ELSE IF TPSTATUS = NO-MORE-MESSAGE                                   
                THEN GOBACK                                                     
                ELSE PERFORM WRITE-DC-TEXT THRU WRITE-DC-TEXT-END.              
           GOBACK.                                                              
       PROCESS-INPUT.                                                           
           IF IN-LL < 5                                                         
             MOVE MNODATA TO OUT-MESSAGE                                        
             PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END.                        
           INSPECT IN-COMMAND TALLYING L-SPACE-CTR FOR LEADING SPACE            
             REPLACING LEADING SPACE BY '*'.                                    
           IF L-SPACE-CTR > 0                                                   
             UNSTRING IN-COMMAND DELIMITED BY ALL '*' INTO TEMP-ONE             
               TEMP-TWO                                                         
             MOVE TEMP-TWO TO IN-COMMAND                                        
             MOVE 0 TO L-SPACE-CTR                                              
             MOVE SPACES TO TEMP-TWO.                                           
           INSPECT IN-LAST-NAME TALLYING L-SPACE-CTR FOR LEADING                
             SPACE REPLACING LEADING SPACE BY '*'.                              
           IF L-SPACE-CTR > 0                                                   
             UNSTRING IN-LAST-NAME DELIMITED BY ALL '*' INTO TEMP-ONE           
               TEMP-TWO                                                         
             MOVE TEMP-TWO TO IN-LAST-NAME                                      
             MOVE 0 TO L-SPACE-CTR                                              
             MOVE SPACES TO TEMP-TWO.                                           
           INSPECT IN-FIRST-NAME TALLYING L-SPACE-CTR FOR LEADING               
             SPACE REPLACING LEADING SPACE BY '*'.                              
           IF L-SPACE-CTR > 0                                                   
             UNSTRING IN-FIRST-NAME DELIMITED BY ALL '*' INTO TEMP-ONE          
               TEMP-TWO                                                         
             MOVE TEMP-TWO TO IN-FIRST-NAME                                     
             MOVE 0 TO L-SPACE-CTR                                              
             MOVE SPACES TO TEMP-TWO.                                           
           INSPECT IN-EXTENSION TALLYING L-SPACE-CTR FOR LEADING                
             SPACE REPLACING LEADING SPACE BY '*'.                              
           IF L-SPACE-CTR > 0                                                   
             UNSTRING IN-EXTENSION DELIMITED BY ALL '*' INTO TEMP-ONE           
               TEMP-TWO                                                         
             MOVE TEMP-TWO TO IN-EXTENSION                                      
             MOVE 0 TO L-SPACE-CTR                                              
             MOVE SPACES TO TEMP-TWO.                                           
           INSPECT IN-ZIP-CODE TALLYING L-SPACE-CTR FOR LEADING SPACE           
             REPLACING LEADING SPACE BY '*'.                                    
           IF L-SPACE-CTR > 0                                                   
             UNSTRING IN-ZIP-CODE DELIMITED BY ALL '*' INTO TEMP-ONE            
               TEMP-TWO                                                         
             MOVE TEMP-TWO TO IN-ZIP-CODE                                       
             MOVE 0 TO L-SPACE-CTR                                              
             MOVE SPACES TO TEMP-TWO.                                           
           MOVE IN-LAST-NAME TO IO-LAST-NAME.                                   
           MOVE IN-COMMAND TO IO-COMMAND.                                       
           IF SPA-CALL-NO = 0                                                   
             MOVE IN-LAST-NAME TO IO-LAST-NAME                                  
             MOVE IN-COMMAND TO IO-COMMAND                                      
           ELSE IF IN-LAST-NAME EQUAL SPACES                                    
                THEN MOVE SPA-LAST-NAME TO IO-LAST-NAME                         
                ELSE MOVE IN-LAST-NAME TO IO-LAST-NAME.                         
           IF IN-COMMAND EQUAL SPACES                                           
             MOVE SPA-COMMAND TO IO-COMMAND                                     
           ELSE                                                                 
             MOVE IN-COMMAND TO IO-COMMAND.                                     
           IF IO-COMMAND EQUAL SPACES                                           
           THEN MOVE MINV TO OUT-MESSAGE                                        
                PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END                      
           ELSE IF IO-LAST-NAME EQUAL SPACES AND TEMP-IOCMD NOT = 'END'         
                THEN MOVE MNONAME TO OUT-MESSAGE                                
                    PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END                  
           ELSE IF TEMP-IOCMD EQUAL 'ADD'                                       
                THEN PERFORM TO-ADD THRU TO-ADD-END                             
           ELSE IF TEMP-IOCMD EQUAL 'TAD'                                       
                THEN MOVE 1 TO TADD-FLAG                                        
                    PERFORM TO-ADD THRU TO-ADD-END                              
           ELSE IF TEMP-IOCMD EQUAL 'UPD'                                       
                THEN PERFORM TO-UPD THRU TO-UPD-END                             
           ELSE IF TEMP-IOCMD EQUAL 'DEL'                                       
                THEN PERFORM TO-DEL THRU TO-DEL-END                             
           ELSE IF TEMP-IOCMD EQUAL 'DIS'                                       
                THEN PERFORM TO-DIS THRU TO-DIS-END                             
           ELSE IF TEMP-IOCMD EQUAL 'END'                                       
                THEN PERFORM TO-END THRU TO-END-END                             
           ELSE                                                                 
               MOVE MINV TO OUT-MESSAGE                                         
               PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END.                      
       PROCESS-INPUT-END.                                                       
           EXIT.                                                                
       TO-ADD.                                                                  
           IF IO-LAST-NAME EQUAL SPA-LAST-NAME                                  
           THEN MOVE SPA-DATA TO IO-DATA.                                       
           IF IN-FIRST-NAME EQUAL SPACES OR                                     
              IN-EXTENSION EQUAL SPACES OR                                      
              IN-ZIP-CODE EQUAL SPACES                                          
           THEN                                                                 
              MOVE MMORE TO OUT-MESSAGE                                         
              PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END                        
           ELSE                                                                 
              MOVE IN-FIRST-NAME TO IO-FIRST-NAME                               
              MOVE IN-EXTENSION  TO IO-EXTENSION                                
              MOVE IN-ZIP-CODE   TO IO-ZIP-CODE                                 
              MOVE IO-DATA       TO SPA-DATA                                    
              MOVE IO-DATA       TO OUT-DATA-TYPE                               
              MOVE IO-COMMAND    TO OUT-COMMAND                                 
              PERFORM ISRT-DB THRU ISRT-DB-END.                                 
       TO-ADD-END.                                                              
           EXIT.                                                                
       TO-UPD.                                                                  
           MOVE 0 TO SET-DATA-FLAG.                                             
           MOVE IO-LAST-NAME TO SSA-KEY.                                        
           PERFORM GET-HOLD-UNIQUE-DB THRU GET-HOLD-UNIQUE-DB-END.              
           IF DBSTATUS = SPACES                                                 
           THEN                                                                 
             IF IN-FIRST-NAME NOT = SPACES                                      
               MOVE 1 TO SET-DATA-FLAG                                          
               MOVE IN-FIRST-NAME TO IO-FIRST-NAME                              
             END-IF                                                             
             IF IN-EXTENSION  NOT = SPACES                                      
               MOVE 1 TO SET-DATA-FLAG                                          
               MOVE IN-EXTENSION  TO IO-EXTENSION                               
             END-IF                                                             
             IF IN-ZIP-CODE   NOT = SPACES                                      
               MOVE 1 TO SET-DATA-FLAG                                          
               MOVE IN-ZIP-CODE   TO IO-ZIP-CODE                                
             END-IF                                                             
             MOVE IO-DATA TO OUT-DATA-TYPE.                                     
             MOVE IO-COMMAND TO OUT-COMMAND.                                    
             IF NO-SET-DATA                                                     
             THEN                                                               
               PERFORM REPL-DB THRU REPL-DB-END                                 
             ELSE                                                               
               MOVE MNODATA TO OUT-MESSAGE                                      
               PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END.                      
       TO-UPD-END.                                                              
           EXIT.                                                                
       TO-DEL.                                                                  
           MOVE IO-LAST-NAME TO SSA-KEY.                                        
           PERFORM GET-HOLD-UNIQUE-DB THRU GET-HOLD-UNIQUE-DB-END.              
           IF DBSTATUS = SPACES                                                 
           THEN                                                                 
              MOVE IO-DATA TO OUT-DATA-TYPE                                     
              MOVE IO-COMMAND TO OUT-COMMAND                                    
              PERFORM DLET-DB THRU DLET-DB-END.                                 
       TO-DEL-END.                                                              
           EXIT.                                                                
       TO-DIS.                                                                  
           MOVE IO-LAST-NAME TO SSA-KEY.                                        
           PERFORM GET-UNIQUE-DB THRU GET-UNIQUE-DB-END.                        
           IF DBSTATUS = SPACES                                                 
           THEN                                                                 
              MOVE IO-DATA TO OUT-DATA-TYPE                                     
              MOVE IO-COMMAND TO OUT-COMMAND                                    
              MOVE MDIS TO OUT-MESSAGE                                          
              PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END.                       
       TO-DIS-END.                                                              
           EXIT.                                                                
       TO-END.                                                                  
           MOVE SPACES TO SPA-TRANCODE.                                         
           MOVE MEND TO OUT-MESSAGE.                                            
           PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END.                          
       TO-END-END.                                                              
           EXIT.                                                                
       ISRT-DB.                                                                 
           MOVE ISRT TO ERROR-CALL.                                             
           CALL 'CBLTDLI' USING ISRT, DBPCB, IOAREA, SSA1.                      
           IF DBSTATUS  = SPACES                                                
           THEN                                                                 
              IF PROCESS-TADD                                                   
                 DISPLAY 'INSERT IS DONE, REPLY' UPON CONSOLE                   
                 ACCEPT REPLY FROM CONSOLE                                      
                 MOVE 0 TO TADD-FLAG                                            
              END-IF                                                            
              MOVE MADD TO OUT-MESSAGE                                          
              PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END                        
           ELSE                                                                 
              MOVE MISRTE TO OUT-MESSAGE                                        
              MOVE DBSTATUS TO ERROR-STATUS                                     
              PERFORM WRITE-DC-TEXT THRU WRITE-DC-TEXT-END                      
              PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END.                       
       ISRT-DB-END.                                                             
           EXIT.                                                                
       GET-UNIQUE-DB.                                                           
           MOVE GET-UNIQUE TO ERROR-CALL.                                       
           CALL 'CBLTDLI' USING GET-UNIQUE, DBPCB, IOAREA, SSA.                 
           IF DBSTATUS NOT = SPACES                                             
           THEN                                                                 
              MOVE MNOENT TO OUT-MESSAGE                                        
              MOVE DBSTATUS TO ERROR-STATUS                                     
              PERFORM WRITE-DC-TEXT THRU WRITE-DC-TEXT-END                      
              PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END.                       
       GET-UNIQUE-DB-END.                                                       
           EXIT.                                                                
       GET-HOLD-UNIQUE-DB.                                                      
           MOVE GET-HOLD-UNIQUE TO ERROR-CALL.                                  
           CALL 'CBLTDLI' USING GET-HOLD-UNIQUE, DBPCB, IOAREA, SSA.            
           IF DBSTATUS NOT = SPACES                                             
           THEN                                                                 
              MOVE MNOENT TO OUT-MESSAGE                                        
              MOVE DBSTATUS TO ERROR-STATUS                                     
              PERFORM WRITE-DC-TEXT THRU WRITE-DC-TEXT-END                      
              PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END.                       
       GET-HOLD-UNIQUE-DB-END.                                                  
           EXIT.                                                                
       REPL-DB.                                                                 
           MOVE REPL TO ERROR-CALL.                                             
           CALL 'CBLTDLI' USING REPL, DBPCB, IOAREA.                            
           IF DBSTATUS = SPACES                                                 
           THEN                                                                 
              MOVE MUPD TO OUT-MESSAGE                                          
              PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END                        
           ELSE                                                                 
              MOVE MREPLE TO OUT-MESSAGE                                        
              MOVE DBSTATUS TO ERROR-STATUS                                     
              PERFORM WRITE-DC-TEXT THRU WRITE-DC-TEXT-END                      
              PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END.                       
       REPL-DB-END.                                                             
           EXIT.                                                                
       DLET-DB.                                                                 
           MOVE DLET TO ERROR-CALL.                                             
           CALL 'CBLTDLI' USING DLET, DBPCB, IOAREA.                            
           IF DBSTATUS = SPACES                                                 
           THEN                                                                 
              MOVE MDEL TO OUT-MESSAGE                                          
              PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END                        
           ELSE                                                                 
              MOVE MDLETE TO OUT-MESSAGE                                        
              MOVE DBSTATUS TO ERROR-STATUS                                     
              PERFORM WRITE-DC-TEXT THRU WRITE-DC-TEXT-END                      
              PERFORM TERM-ROUTINE THRU TERM-ROUTINE-END.                       
       DLET-DB-END.                                                             
           EXIT.                                                                
       TERM-ROUTINE.                                                            
           MOVE SPACES TO MODNAME.                                              
           PERFORM INSERT-SPA THRU INSERT-SPA-END.                              
           IF IN-COMMAND = 'END'                                                
             MOVE TRAN-CODE TO MODNAME.                                         
           PERFORM INSERT-IO THRU INSERT-IO-END.                                
       TERM-ROUTINE-END.                                                        
           EXIT.                                                                
       INSERT-SPA.                                                              
           MOVE ISRT TO ERROR-CALL.                                             
           MOVE IO-DATA TO SPA-DATA.                                            
           MOVE IO-COMMAND TO SPA-COMMAND.                                      
           ADD 1 TO SPA-CALL-NO.                                                
           MOVE SPA-CALL-NO TO SPA-CALL.                                        
           CALL 'CBLTDLI' USING ISRT, IOPCB, SPA.                               
           IF TPSTATUS NOT = SPACES                                             
             THEN PERFORM WRITE-DC-TEXT THRU WRITE-DC-TEXT-END.                 
       INSERT-SPA-END.                                                          
           EXIT.                                                                
       INSERT-IO.                                                               
           MOVE ISRT TO ERROR-CALL.                                             
           IF MODNAME EQUAL SPACES                                              
           THEN                                                                 
              CALL 'CBLTDLI' USING ISRT, IOPCB, OUTPUT-AREA                     
           ELSE                                                                 
              CALL 'CBLTDLI' USING ISRT, IOPCB, OUTPUT-AREA, MODNAME            
              MOVE SPACES TO MODNAME.                                           
           IF TPSTATUS NOT = SPACES                                             
             THEN PERFORM WRITE-DC-TEXT THRU WRITE-DC-TEXT-END.                 
       INSERT-IO-END.                                                           
           EXIT.                                                                
       WRITE-DC-TEXT.                                                           
           MOVE TPSTATUS TO ERROR-STATUS.                                       
           DISPLAY DC-TEXT UPON CONSOLE.                                        
       WRITE-DC-TEXT-END.                                                       
           EXIT.                                                                
