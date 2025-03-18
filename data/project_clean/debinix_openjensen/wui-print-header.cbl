       IDENTIFICATION DIVISION.
       program-id. wui-print-header.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       working-storage section.
       01  wc-newline     PIC X VALUE x'0a'.        
       linkage section.
       01  ln-rtn-code    PIC S99.    
       PROCEDURE DIVISION USING ln-rtn-code.
       000-print-html-header.
           DISPLAY
               "Content-Type: text/html; charset=utf-8"
               wc-newline
               wc-newline
           END-DISPLAY        
           EXIT PROGRAM
           .
