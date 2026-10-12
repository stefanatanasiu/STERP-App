      *>================================================================
      *> PROGRAM-ID : SQLIO
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : COMMON SERVICES
      *> PURPOSE    : SINGLE POINT OF ACCESS FROM COBOL TO THE STERP
      *>              SQL SERVER DATABASE ("HOME GROWN EXEC SQL").
      *>              WRAPS THE C ODBC SHIM (c\sterpsql.c, STSQLxxx).
      *> CALLED BY  : ALL ONLINE AND BATCH PROGRAMS THAT NEED DATA
      *> INTERFACE  : CALL "SQLIO" USING SQLIO-CA ROW-AREA
      *>              (SEE COPYBOOK SQLIOCA FOR FUNCTIONS)
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1999-03-11 RJH  INITIAL VERSION - REPLACES ISAM HANDLER FHSTOCK
      *> 2003-07-02 KMP  CURSOR NUMBER, NESTED LOOPS
      *> 2009-11-19 KMP  PARAMETER BINDING (AUDIT FINDING 2009/14)
      *> 2012-04-23 ANW  REWRITE FOR GNUCOBOL, SNGL FUNCTION
      *> 2026-10-04 STERP TRACE TO STERP_SQLTRACE
      *>================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQLIO.
       AUTHOR. R J HARGREAVES.
       DATE-WRITTEN. 11/03/1999.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-RC                       PIC S9(9) COMP-5 VALUE 0.
       01  WS-LEN                      PIC S9(9) COMP-5 VALUE 0.
       01  WS-IDX                      PIC S9(9) COMP-5 VALUE 0.
       01  WS-ISNULL                   PIC S9(9) COMP-5 VALUE 0.
       01  WS-TXN-MODE                 PIC S9(9) COMP-5 VALUE 0.
       01  WS-MSG-LEN                  PIC S9(9) COMP-5 VALUE 512.
       01  WS-FIRST-TIME               PIC X VALUE "Y".
       01  WS-TRACE-ENV                PIC X(8) VALUE SPACES.
       01  WS-TRACE-ON                 PIC X VALUE "N".
       01  WS-CONNECTED                PIC X VALUE "N".
       LINKAGE SECTION.
       COPY SQLIOCA.
       01  LK-ROW                      PIC X(32000).
       PROCEDURE DIVISION USING SQLIO-CA LK-ROW.
       0000-MAIN.
           IF WS-FIRST-TIME = "Y"
               PERFORM 0100-FIRST-TIME
           END-IF
           MOVE 0 TO SQLIO-SQLCODE
           MOVE "00000" TO SQLIO-SQLSTATE
           MOVE SPACES TO SQLIO-ERRMSG
           IF WS-TRACE-ON = "Y" OR SQLIO-TRACE = "Y"
               PERFORM 9000-TRACE
           END-IF
           EVALUATE TRUE
               WHEN SQLIO-CONNECT    PERFORM 1000-CONNECT
               WHEN SQLIO-DISCONNECT PERFORM 1100-DISCONNECT
               WHEN SQLIO-EXECUTE    PERFORM 2000-EXECUTE
               WHEN SQLIO-FETCH      PERFORM 3000-FETCH
               WHEN SQLIO-SINGLE     PERFORM 4000-SINGLE
               WHEN SQLIO-MORE       PERFORM 5000-MORE
               WHEN SQLIO-CLOSE      PERFORM 5100-CLOSE
               WHEN SQLIO-BEGIN      MOVE 1 TO WS-TXN-MODE
                                     PERFORM 6000-TXN
               WHEN SQLIO-COMMIT     MOVE 2 TO WS-TXN-MODE
                                     PERFORM 6000-TXN
               WHEN SQLIO-ROLLBACK   MOVE 3 TO WS-TXN-MODE
                                     PERFORM 6000-TXN
               WHEN SQLIO-TXN-STATE  MOVE 4 TO WS-TXN-MODE
                                     PERFORM 6000-TXN
                                     MOVE WS-TXN-MODE
                                         TO SQLIO-ROWCOUNT
               WHEN OTHER
                   MOVE -1 TO SQLIO-SQLCODE
                   MOVE "HY000" TO SQLIO-SQLSTATE
                   STRING "SQLIO: INVALID FUNCTION " SQLIO-FUNC
                       DELIMITED BY SIZE INTO SQLIO-ERRMSG
           END-EVALUATE
           IF SQLIO-SQLCODE = -1 AND SQLIO-ERRMSG = SPACES
               CALL "STSQLERR" USING SQLIO-SQLSTATE SQLIO-ERRMSG
                                      WS-MSG-LEN
           END-IF
           IF (WS-TRACE-ON = "Y" OR SQLIO-TRACE = "Y")
              AND SQLIO-SQLCODE = -1
               DISPLAY "SQLIO ERR " SQLIO-SQLSTATE " "
                       FUNCTION TRIM(SQLIO-ERRMSG) UPON SYSERR
           END-IF
           GOBACK.

       0100-FIRST-TIME.
           MOVE "N" TO WS-FIRST-TIME
           ACCEPT WS-TRACE-ENV FROM ENVIRONMENT "STERP_SQLTRACE"
           IF WS-TRACE-ENV(1:1) = "Y" OR WS-TRACE-ENV(1:1) = "y"
               MOVE "Y" TO WS-TRACE-ON
           END-IF.

       1000-CONNECT.
           MOVE LENGTH OF SQLIO-STMT TO WS-LEN
           CALL "STSQLCON" USING SQLIO-STMT WS-LEN RETURNING WS-RC
           MOVE WS-RC TO SQLIO-SQLCODE
           IF WS-RC = 0
               MOVE "Y" TO WS-CONNECTED
           END-IF.

       1100-DISCONNECT.
           CALL "STSQLDIS" RETURNING WS-RC
           MOVE "N" TO WS-CONNECTED
           MOVE WS-RC TO SQLIO-SQLCODE.

       2000-EXECUTE.
           MOVE LENGTH OF SQLIO-STMT TO WS-LEN
           CALL "STSQLPRP" USING SQLIO-CURSOR SQLIO-STMT WS-LEN
                RETURNING WS-RC
           IF WS-RC NOT = 0
               MOVE WS-RC TO SQLIO-SQLCODE
           ELSE
               PERFORM 2100-BIND-ONE
                   VARYING WS-IDX FROM 1 BY 1
                   UNTIL WS-IDX > SQLIO-PARM-CNT OR WS-RC NOT = 0
               IF WS-RC NOT = 0
                   MOVE WS-RC TO SQLIO-SQLCODE
               ELSE
                   CALL "STSQLEXE" USING SQLIO-CURSOR RETURNING WS-RC
                   MOVE WS-RC TO SQLIO-SQLCODE
                   MOVE 0 TO SQLIO-ROWCOUNT
                   IF WS-RC NOT = -1
                       CALL "STSQLROW" USING SQLIO-CURSOR
                                             SQLIO-ROWCOUNT
                   END-IF
               END-IF
           END-IF.

       2100-BIND-ONE.
           MOVE 256 TO WS-LEN
           IF SQLIO-PARM-NULL(WS-IDX) = "Y"
               MOVE 1 TO WS-ISNULL
           ELSE
               MOVE 0 TO WS-ISNULL
           END-IF
           CALL "STSQLPRM" USING SQLIO-CURSOR WS-IDX
                                 SQLIO-PARM-VAL(WS-IDX) WS-LEN WS-ISNULL
                RETURNING WS-RC.

       3000-FETCH.
           CALL "STSQLFET" USING SQLIO-CURSOR LK-ROW SQLIO-COL-TABLE
                                 SQLIO-COL-CNT
                RETURNING WS-RC
           MOVE WS-RC TO SQLIO-SQLCODE.

       4000-SINGLE.
           PERFORM 2000-EXECUTE
           IF SQLIO-SQLCODE NOT = -1
               PERFORM 3000-FETCH
               IF SQLIO-SQLCODE NOT = -1
                   CALL "STSQLCLS" USING SQLIO-CURSOR RETURNING WS-RC
               END-IF
           END-IF.

       5000-MORE.
           CALL "STSQLMOR" USING SQLIO-CURSOR RETURNING WS-RC
           MOVE WS-RC TO SQLIO-SQLCODE.

       5100-CLOSE.
           CALL "STSQLCLS" USING SQLIO-CURSOR RETURNING WS-RC
           MOVE WS-RC TO SQLIO-SQLCODE.

       6000-TXN.
           CALL "STSQLTXN" USING WS-TXN-MODE RETURNING WS-RC
           MOVE WS-RC TO SQLIO-SQLCODE.

       9000-TRACE.
           IF SQLIO-EXECUTE OR SQLIO-SINGLE
               DISPLAY "SQLIO " SQLIO-FUNC " C" SQLIO-CURSOR " "
                       FUNCTION TRIM(SQLIO-STMT) UPON SYSERR
           ELSE
               DISPLAY "SQLIO " SQLIO-FUNC " C" SQLIO-CURSOR
                       UPON SYSERR
           END-IF.
