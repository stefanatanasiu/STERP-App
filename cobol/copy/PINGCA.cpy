      *>================================================================
      *> PINGCA - COMMAREA for PING (engine / database health check)
      *>================================================================
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  PG-BODY.
               10  PG-ENGINE-VERSION   PIC X(20).
               10  PG-DB-NAME          PIC X(30).
               10  PG-DB-SERVER-TIME   PIC X(30).
               10  PG-DB-VERSION       PIC X(80).
               10  PG-ECHO-AMOUNT      PIC S9(11)V99 COMP-3.
               10  PG-ECHO-DOUBLE      PIC S9(11)V99
                                       SIGN LEADING SEPARATE.
               10  PG-ECHO-TABLE       OCCURS 3 TIMES.
                   15  PG-ECHO-CODE    PIC X(4).
                   15  PG-ECHO-QTY     PIC S9(9)V9(6).
