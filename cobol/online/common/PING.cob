      *>================================================================
      *> PROGRAM-ID : PING
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : COMMON SERVICES
      *> PURPOSE    : ENGINE HEALTH CHECK.  ECHOES NUMERIC FIELDS (USED
      *>              BY THE CLIENT TO VERIFY COMMAREA MAPPING) AND
      *>              READS SERVER TIME / VERSION THROUGH SQLIO.
      *> 2011-08-08 ANW  INITIAL VERSION FOR .NET CLIENT
      *>================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PING.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       01  WS-ROW.
           05  WS-R-DBNAME             PIC X(30).
           05  WS-R-TIME               PIC X(30).
           05  WS-R-VERSION            PIC X(80).
       01  WS-I                        PIC 9(2).
       LINKAGE SECTION.
       COPY PINGCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
       0000-MAIN.
           MOVE "PING" TO CA-PROGRAM
           MOVE "STERP ENGINE 4.2.17" TO PG-ENGINE-VERSION
           COMPUTE PG-ECHO-DOUBLE = PG-ECHO-AMOUNT * 2
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 3
               COMPUTE PG-ECHO-QTY(WS-I) = PG-ECHO-QTY(WS-I) * -1
           END-PERFORM
           MOVE "EXEC" TO SQLIO-FUNC
           MOVE 1 TO SQLIO-CURSOR
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT DB_NAME(), "
                  "CONVERT(varchar(30), SYSDATETIME(), 121), "
                  "LEFT(@@VERSION, 80)"
                  DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE "SNGL" TO SQLIO-FUNC
           MOVE 3 TO SQLIO-COL-CNT
           MOVE 30 TO SQLIO-COL-WIDTH(1)
           MOVE 30 TO SQLIO-COL-WIDTH(2)
           MOVE 80 TO SQLIO-COL-WIDTH(3)
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-OK
               MOVE WS-R-DBNAME  TO PG-DB-NAME
               MOVE WS-R-TIME    TO PG-DB-SERVER-TIME
               MOVE WS-R-VERSION TO PG-DB-VERSION
               MOVE 0 TO CA-RETURN-CODE
               MOVE "ENGINE AND DATABASE AVAILABLE" TO CA-MESSAGE
           ELSE
               MOVE 4 TO CA-RETURN-CODE
               MOVE "SQL0001" TO CA-REASON-CODE
               MOVE SQLIO-ERRMSG TO CA-MESSAGE
           END-IF
           GOBACK.
