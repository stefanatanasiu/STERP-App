      *>================================================================
      *> SQLIOCA  - SQLIO control area ("home grown EXEC SQL")
      *>   CALL "SQLIO" USING SQLIO-CA <row-area>
      *> Functions (SQLIO-FUNC):
      *>   CONN  connect (SQLIO-STMT = conn string or SPACES = default)
      *>   DISC  disconnect
      *>   EXEC  prepare SQLIO-STMT, bind SQLIO-PARM(1..PARM-CNT),
      *>         execute on SQLIO-CURSOR (1-8)
      *>   FTCH  fetch next row into <row-area> using SQLIO-COL-CNT and
      *>         SQLIO-COL-WIDTH(n); every column arrives as left
      *>         justified text (use FUNCTION NUMVAL for numbers)
      *>   SNGL  EXEC + FTCH first row + CLOS (single row lookup)
      *>   MORE  next result set    CLOS  close cursor
      *>   BEGN / COMT / ROLB  transaction control
      *> SQLIO-SQLCODE: 0 OK, 100 not found / end of data, -1 error
      *>   (SQLIO-SQLSTATE and SQLIO-ERRMSG describe the error)
      *> Parameters: use ? markers in SQLIO-STMT. Numbers -> MOVE to
      *>   SQLIO-NUM-EDIT then MOVE FUNCTION TRIM(SQLIO-NUM-EDIT).
      *>   Dates -> YYYYMMDD text.  SQLIO-PARM-NULL = "Y" for NULL.
      *> 1999-03-11 RJH   2009-11-19 KMP parameter binding
      *>================================================================
       01  SQLIO-CA.
           05  SQLIO-FUNC              PIC X(4).
               88  SQLIO-CONNECT                 VALUE "CONN".
               88  SQLIO-DISCONNECT              VALUE "DISC".
               88  SQLIO-EXECUTE                 VALUE "EXEC".
               88  SQLIO-FETCH                   VALUE "FTCH".
               88  SQLIO-SINGLE                  VALUE "SNGL".
               88  SQLIO-MORE                    VALUE "MORE".
               88  SQLIO-CLOSE                   VALUE "CLOS".
               88  SQLIO-BEGIN                   VALUE "BEGN".
               88  SQLIO-COMMIT                  VALUE "COMT".
               88  SQLIO-ROLLBACK                VALUE "ROLB".
      *>   TSTA returns driver autocommit in ROWCOUNT: 1=on, 0=off.
               88  SQLIO-TXN-STATE               VALUE "TSTA".
           05  SQLIO-CURSOR            PIC S9(9) COMP-5 VALUE 1.
           05  SQLIO-SQLCODE           PIC S9(9) COMP-5 VALUE 0.
               88  SQLIO-OK                      VALUE 0.
               88  SQLIO-NOTFOUND                VALUE 100.
               88  SQLIO-ERROR                   VALUE -1.
           05  SQLIO-SQLSTATE          PIC X(5).
           05  SQLIO-ERRMSG            PIC X(512).
           05  SQLIO-ROWCOUNT          PIC S9(9) COMP-5 VALUE 0.
           05  SQLIO-STMT              PIC X(4000).
           05  SQLIO-PARM-CNT          PIC S9(9) COMP-5 VALUE 0.
           05  SQLIO-PARM-TABLE.
               10  SQLIO-PARM          OCCURS 40 TIMES.
                   15  SQLIO-PARM-VAL  PIC X(256).
                   15  SQLIO-PARM-NULL PIC X.
           05  SQLIO-COL-CNT           PIC S9(9) COMP-5 VALUE 0.
           05  SQLIO-COL-TABLE.
               10  SQLIO-COL-WIDTH     PIC S9(9) COMP-5
                                       OCCURS 80 TIMES.
           05  SQLIO-NUM-EDIT          PIC -(12)9.9(6).
           05  SQLIO-INT-EDIT          PIC -(17)9.
           05  SQLIO-TRACE             PIC X VALUE "N".
