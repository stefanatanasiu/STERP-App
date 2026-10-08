      *>================================================================
      *> STRUTILCA - COMMAREA for STRUTIL (text and formatting service)
      *> CA-FUNCTION values
      *>   UPPER     upper case SU-TEXT-IN into SU-TEXT-OUT
      *>   CODE      upper case and validate a business code (D-21):
      *>             A-Z 0-9 and - . / _ only, up to SU-MAX-LEN
      *>             characters (default 40).  SU-VALID Y/N, the first
      *>             offending character in SU-BAD-CHAR / SU-BAD-POS
      *>   DOCREF    normalise a document reference for duplicate
      *>             detection: upper case, spaces and punctuation
      *>             removed, and leading zeros of the trailing numeric
      *>             part removed when SU-ZERO-POLICY is STRIP
      *>   MONEY     format SU-AMOUNT as money with thousands
      *>             separators into SU-TEXT-OUT, prefixed with the
      *>             currency - see "Pound sign handling" below
      *>   QTY       format SU-AMOUNT as a quantity, SU-PLACES decimals,
      *>             trailing zeros trimmed when SU-ZERO-POLICY = STRIP
      *>   SQUEEZE   collapse runs of spaces, trim both ends
      *> POUND SIGN HANDLING
      *>   COBOL sources and COMMAREAs are single byte.  The pound sign
      *>   is held as X"A3" (ISO-8859-1 / Windows-1252), which is what
      *>   the .NET client decodes.  Because UTF-8 would need two bytes
      *>   and would corrupt fixed width fields, the DEFAULT for
      *>   messages is the three letter code: "GBP 1,234.56".  Set
      *>   SU-SYMBOL to "Y" to get X"A3" instead: "<A3>1,234.56".
      *>   CA-MESSAGE text produced by other services always uses GBP.
      *> RETURN CODES 00 ok, 04 valid but changed (code folded to upper
      *>   case, reference normalised), 08 invalid (STR0001..STR0009).
      *>================================================================
      *> REQ: COM-004, COM-024   AC: AC-003
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  SU-BODY.
               10  SU-TEXT-IN          PIC X(200).
               10  SU-TEXT-OUT         PIC X(200).
               10  SU-MAX-LEN          PIC S9(4) SIGN LEADING SEPARATE.
               10  SU-LENGTH-OUT       PIC S9(4) SIGN LEADING SEPARATE.
               10  SU-VALID            PIC X.
               10  SU-CHANGED          PIC X.
               10  SU-AMOUNT           PIC S9(13)V9(6) COMP-3.
               10  SU-PLACES           PIC S9(4) SIGN LEADING SEPARATE.
               10  SU-CURRENCY         PIC X(3).
               10  SU-SYMBOL           PIC X.
               10  SU-ZERO-POLICY      PIC X(8).
                   88  SU-ZERO-KEEP              VALUE "KEEP".
                   88  SU-ZERO-STRIP             VALUE "STRIP".
               10  SU-BAD-CHAR         PIC X.
               10  SU-BAD-POS          PIC S9(4) SIGN LEADING SEPARATE.
