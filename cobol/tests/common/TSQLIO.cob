      *>================================================================
      *> TSQLIO - unit test driver for SQLIO / PING (common services)
      *> Output protocol (TAP): "ok n - text" / "not ok n - text",
      *> RETURN-CODE = number of failures.
      *>================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSQLIO.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY TESTKITW.
       01  WS-ROW.
           05  WS-C1                   PIC X(10).
           05  WS-C2                   PIC X(20).
       COPY PINGCA REPLACING ==DFHCOMMAREA== BY ==WS-PING==.
       PROCEDURE DIVISION.
           MOVE "TSQLIO" TO TK-SUITE
           PERFORM TK-BEGIN
           MOVE "TSTA" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           MOVE "SQLIO reports driver autocommit on" TO TK-NAME
           IF SQLIO-OK AND SQLIO-ROWCOUNT = 1
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF
           MOVE "BEGN" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           MOVE "TSTA" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           MOVE "SQLIO reports a caller-owned driver UOW" TO TK-NAME
           IF SQLIO-OK AND SQLIO-ROWCOUNT = 0
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF
           MOVE "ROLB" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           MOVE "TSTA" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           MOVE "SQLIO rollback restores driver autocommit" TO TK-NAME
           IF SQLIO-OK AND SQLIO-ROWCOUNT = 1
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           MOVE "SNGL" TO SQLIO-FUNC
           MOVE "SELECT ?, CAST(? AS decimal(18,2)) * 2" TO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE "ABC" TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE -12.5 TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 10 TO SQLIO-COL-WIDTH(1)
           MOVE 20 TO SQLIO-COL-WIDTH(2)
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           MOVE "SQLIO single row with parameters" TO TK-NAME
           IF SQLIO-OK AND WS-C1 = "ABC"
              AND FUNCTION NUMVAL(WS-C2) = -25
               PERFORM TK-PASS
           ELSE
               MOVE SQLIO-ERRMSG TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF

           MOVE "SNGL" TO SQLIO-FUNC
           MOVE "SELECT name FROM sys.objects WHERE 1 = 0" TO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           MOVE "SQLIO not found returns 100" TO TK-NAME
           IF SQLIO-NOTFOUND PERFORM TK-PASS ELSE PERFORM TK-FAIL END-IF

           MOVE "EXEC" TO SQLIO-FUNC
           MOVE "SELECT * FROM dbo.NoSuchTable" TO SQLIO-STMT
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           MOVE "SQLIO error returns -1 with message" TO TK-NAME
           IF SQLIO-ERROR AND SQLIO-ERRMSG NOT = SPACES
               PERFORM TK-PASS
           ELSE
               PERFORM TK-FAIL
           END-IF

           INITIALIZE WS-PING
           MOVE 100.25 TO PG-ECHO-AMOUNT
           MOVE 1.5 TO PG-ECHO-QTY(2)
           CALL "PING" USING WS-PING
           MOVE "PING doubles COMP-3 amount" TO TK-NAME
           IF PG-ECHO-DOUBLE = 200.50 PERFORM TK-PASS
           ELSE PERFORM TK-FAIL END-IF
           MOVE "PING negates quantities" TO TK-NAME
           IF PG-ECHO-QTY(2) = -1.5 PERFORM TK-PASS
           ELSE PERFORM TK-FAIL END-IF
           MOVE "PING reads database" TO TK-NAME
           IF CA-RETURN-CODE OF WS-PING = 0 AND PG-DB-NAME NOT = SPACES
               PERFORM TK-PASS
           ELSE
               MOVE CA-MESSAGE OF WS-PING TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF

           PERFORM TK-END
           STOP RUN.

       COPY TESTKITP.
