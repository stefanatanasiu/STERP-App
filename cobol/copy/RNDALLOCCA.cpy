      *>================================================================
      *> RNDALLOCCA - COMMAREA for RNDALLOC
      *>               (financial rounding and allocation service)
      *> CA-FUNCTION values
      *>   ROUND     round RA-VALUE to RA-PLACES decimals, exact halves
      *>             away from zero          -> RA-RESULT
      *>   TRUNC     truncate RA-VALUE to RA-PLACES decimals (toward
      *>             zero)                   -> RA-RESULT
      *>   UNITPRC   RA-VALUE / RA-VALUE2 kept to RA-PLACES decimals
      *>             (six decimal unit price, no premature rounding)
      *>                                     -> RA-RESULT
      *>   EXTEND    RA-VALUE * RA-VALUE2 computed at twelve decimals
      *>             then rounded to RA-PLACES
      *>                                     -> RA-RESULT, RA-UNROUNDED
      *>   ALLOC     spread RA-AMOUNT over RA-LINE-CNT lines in stable
      *>             line order.  RA-MODE "EQUAL" or "WEIGHT" (weights
      *>             in RA-WEIGHT, all >= 0, at least one > 0).
      *>             -> RA-PART(1..n), RA-ALLOCATED, RA-RESIDUAL,
      *>                RA-BUMPED (lines that received a spare unit)
      *> RETURN CODES 00 ok, 08 invalid input (reason RND0001..RND0009)
      *>================================================================
      *> REQ: COM-011   AC: AC-006
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  RA-BODY.
               10  RA-PLACES           PIC S9(4) SIGN LEADING SEPARATE.
               10  RA-VALUE            PIC S9(13)V9(12) COMP-3.
               10  RA-VALUE2           PIC S9(13)V9(12) COMP-3.
               10  RA-RESULT           PIC S9(13)V9(12) COMP-3.
               10  RA-UNROUNDED        PIC S9(13)V9(12) COMP-3.
               10  RA-AMOUNT           PIC S9(13)V9(6) COMP-3.
               10  RA-MODE             PIC X(8).
                   88  RA-EQUAL                  VALUE "EQUAL".
                   88  RA-WEIGHTED               VALUE "WEIGHT".
               10  RA-LINE-CNT         PIC S9(9) COMP-5.
               10  RA-ALLOCATED        PIC S9(13)V9(6) COMP-3.
               10  RA-RESIDUAL         PIC S9(13)V9(6) COMP-3.
               10  RA-BUMPED           PIC S9(9) COMP-5.
               10  RA-MAX-LINES        PIC S9(9) COMP-5.
               10  RA-LINE OCCURS 500 TIMES.
                   15  RA-LINE-REF     PIC X(8).
                   15  RA-WEIGHT       PIC S9(13)V9(6) COMP-3.
                   15  RA-PART         PIC S9(13)V9(6) COMP-3.
