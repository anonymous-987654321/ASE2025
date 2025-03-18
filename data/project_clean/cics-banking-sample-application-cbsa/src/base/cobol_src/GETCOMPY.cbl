       CBL CICS('SP,EDF')
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GETCOMPY.
       AUTHOR. James O'Grady.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-370.
       OBJECT-COMPUTER.  IBM-370.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       LINKAGE SECTION.
       01 DFHCOMMAREA.
           03 GETCompanyOperation.
             06 company-name pic x(40).
       PROCEDURE DIVISION USING DFHCOMMAREA.
       PREMIERE SECTION.
       A010.
           move 'CICS Banking Sample Application' to COMPANY-NAME.
           EXEC CICS RETURN
           END-EXEC.
           GOBACK.
