      *> TSQLIO2 - SQLIO skips row-count results before a SELECT
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSQLIO2.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY TESTKITW.
       01  WS-ROW                      PIC X(20).
       PROCEDURE DIVISION.
           MOVE "TSQLIO2" TO TK-SUITE
           PERFORM TK-BEGIN
           MOVE "BEGN" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           MOVE SPACES TO SQLIO-STMT
           STRING "DECLARE @t TABLE (i int IDENTITY, v int); "
                  "INSERT @t (v) VALUES (7); "
                  "SELECT CAST(SCOPE_IDENTITY() AS int);"
                  DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 0 TO SQLIO-PARM-CNT
           MOVE 1 TO SQLIO-COL-CNT
           MOVE 20 TO SQLIO-COL-WIDTH(1)
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
      *> REQ: COM-014
           MOVE "SELECT after INSERT without NOCOUNT returns the row"
             TO TK-NAME
           IF SQLIO-OK AND FUNCTION NUMVAL(WS-ROW) = 1
               PERFORM TK-PASS
           ELSE
               MOVE SQLIO-ERRMSG TO TK-DETAIL
               PERFORM TK-FAIL
           END-IF
           MOVE "ROLB" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           PERFORM TK-END
           STOP RUN.
       COPY TESTKITP.
