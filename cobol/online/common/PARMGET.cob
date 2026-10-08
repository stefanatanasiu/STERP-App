      *>================================================================
      *> PROGRAM-ID : PARMGET
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : COMMON SERVICES
      *> PURPOSE    : EFFECTIVE DATED PARAMETER LOOKUP.  EVERY TUNING
      *>              VALUE IN STERP (TOLERANCES, LIMITS, POLICY
      *>              SWITCHES) IS HELD IN core.Parameter WITH A
      *>              COMPANY SCOPE AND AN EFFECTIVE DATE, SO THAT A
      *>              CHANGE OF POLICY NEVER REWRITES HISTORY.
      *>              THE COMPANY ROW WINS OVER THE ALL COMPANY ROW
      *>              ("*"); THE MOST RECENT EFFECTIVE DATE ON OR
      *>              BEFORE THE AS OF DATE WINS.
      *> CALLED BY  : EVERY MODULE AND THE .NET CLIENT
      *> INTERFACE  : CALL "PARMGET" USING WS-PARMGET-CA
      *>              COPYBOOK PARMGETCA
      *> CALLS      : SQLIO -> core.usp_GetParameter
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1997-02-28 KMP  CR0812 SYSTEM CONTROL RECORD REPLACED BY A
      *>                 PARAMETER FILE WITH EFFECTIVE DATES
      *> 2026-10-05 STERP core.Parameter AND usp_GetParameter
      *>================================================================
      *> REQ: COM-011, COM-025
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARMGET.
       AUTHOR. K M PATEL.
       DATE-WRITTEN. 28/02/1997.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       01  WS-FLAGS.
           05  WS-ERROR                PIC X VALUE "N".
               88  WS-INPUT-BAD                  VALUE "Y".
       01  WS-WORK.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-CH                   PIC X VALUE SPACE.
           05  WS-DIGITS               PIC S9(4) VALUE 0.
           05  WS-BAD-CHARS            PIC S9(4) VALUE 0.
           05  WS-NOW                  PIC X(21) VALUE SPACES.
       01  WS-ROW.
           05  WS-P-CODE               PIC X(40).
           05  WS-P-COMPANY            PIC X(10).
           05  WS-P-VALUE              PIC X(200).
           05  WS-P-TYPE               PIC X(12).
           05  WS-P-UNIT               PIC X(20).
           05  WS-P-EFFECTIVE          PIC X(10).
           05  WS-P-DECISION           PIC X(10).
           05  WS-P-DESC               PIC X(100).
           05  WS-P-OWNER              PIC X(20).
       01  WS-ISO.
           05  WS-ISO-Y                PIC X(4).
           05  FILLER                  PIC X.
           05  WS-ISO-M                PIC X(2).
           05  FILLER                  PIC X.
           05  WS-ISO-D                PIC X(2).
       01  WS-COMPACT.
           05  WS-C-Y                  PIC X(4).
           05  WS-C-M                  PIC X(2).
           05  WS-C-D                  PIC X(2).
       LINKAGE SECTION.
       COPY PARMGETCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           PERFORM 0200-VALIDATE
           IF WS-INPUT-BAD
               GO TO 0000-EXIT
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "GET"      PERFORM 1000-READ-PARAMETER
               WHEN "GETNUM"   PERFORM 1000-READ-PARAMETER
                               PERFORM 1500-INSIST-NUMERIC
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           GOBACK.

       0100-INITIALISE SECTION.
       0100-START.
           INITIALIZE WS-WORK
           MOVE "N" TO WS-ERROR
           MOVE "PARMGET" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE "COM-025" TO CA-REQ-REF
           MOVE 0 TO CA-MSG-COUNT
           MOVE SPACES TO PM-VALUE-TEXT PM-DATA-TYPE PM-UNIT
           MOVE SPACES TO PM-DECISION-REF PM-OWNER-ROLE PM-DESCRIPTION
           MOVE SPACES TO PM-SCOPE
           MOVE 0 TO PM-VALUE-NUM PM-EFFECTIVE-FROM
           MOVE "N" TO PM-FOUND
           MOVE "N" TO PM-IS-NUMERIC
           MOVE FUNCTION UPPER-CASE(PM-PARAM-CODE) TO PM-PARAM-CODE
           IF PM-AS-OF = 0
               MOVE CA-BUSINESS-DATE TO PM-AS-OF
           END-IF
           IF PM-AS-OF = 0
               MOVE FUNCTION CURRENT-DATE TO WS-NOW
               MOVE WS-NOW(1:8) TO PM-AS-OF
           END-IF.
       0100-EXIT.
           EXIT.

       0200-VALIDATE SECTION.
       0200-START.
           IF PM-PARAM-CODE = SPACES OR PM-PARAM-CODE = LOW-VALUES
               MOVE "PRM0001" TO CA-REASON-CODE
               MOVE "A parameter code is required" TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF FUNCTION TEST-DATE-YYYYMMDD(PM-AS-OF) NOT = 0
               MOVE "PRM0002" TO CA-REASON-CODE
               MOVE "The as of date is not a valid calendar date"
                   TO CA-MESSAGE
               PERFORM 9000-REJECT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       1000-READ-PARAMETER SECTION.
       1000-START.
      *>     The equivalent of core.usp_GetParameter, issued directly.
      *>     That procedure raises STERP-E-0003 when nothing is held,
      *>     and because it runs with SET XACT_ABORT ON the raise also
      *>     dooms the CALLER's transaction.  COM-025 says a missing
      *>     tuning parameter must not stop a business action, so the
      *>     absence of a row has to arrive here as "no rows" (SQLCODE
      *>     100), not as a severe error.  The selection rule is kept
      *>     identical to the procedure: the company specific row wins
      *>     over the '*' row, then the latest effective date that is
      *>     not in the future.  See docs\tech-debt\common.md.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) p.ParamCode, p.CompanyCode, "
                  "p.Value, p.DataType, p.UnitOfValue, "
                  "p.EffectiveFrom, p.DecisionRef, p.Description, "
                  "p.OwnerRole FROM core.Parameter p "
                  "WHERE p.ParamCode = ? "
                  "AND p.CompanyCode IN (?, '*') "
                  "AND p.EffectiveFrom <= ? "
                  "ORDER BY CASE WHEN p.CompanyCode = '*' THEN 1 "
                  "ELSE 0 END, p.EffectiveFrom DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE PM-PARAM-CODE TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           IF CA-COMPANY = SPACES
               MOVE "*" TO SQLIO-PARM-VAL(2)
           ELSE
               MOVE CA-COMPANY TO SQLIO-PARM-VAL(2)
           END-IF
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE PM-AS-OF TO SQLIO-PARM-VAL(3)
           MOVE "N" TO SQLIO-PARM-NULL(3)
           MOVE 9 TO SQLIO-COL-CNT
           MOVE 40 TO SQLIO-COL-WIDTH(1)
           MOVE 10 TO SQLIO-COL-WIDTH(2)
           MOVE 200 TO SQLIO-COL-WIDTH(3)
           MOVE 12 TO SQLIO-COL-WIDTH(4)
           MOVE 20 TO SQLIO-COL-WIDTH(5)
           MOVE 10 TO SQLIO-COL-WIDTH(6)
           MOVE 10 TO SQLIO-COL-WIDTH(7)
           MOVE 100 TO SQLIO-COL-WIDTH(8)
           MOVE 20 TO SQLIO-COL-WIDTH(9)
           MOVE 6 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 1000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               PERFORM 1200-NOT-HELD
               GO TO 1000-EXIT
           END-IF
           MOVE "Y" TO PM-FOUND
           MOVE WS-P-VALUE TO PM-VALUE-TEXT
           MOVE WS-P-TYPE TO PM-DATA-TYPE
           MOVE WS-P-UNIT TO PM-UNIT
           MOVE WS-P-DECISION TO PM-DECISION-REF
           MOVE WS-P-DESC TO PM-DESCRIPTION
           MOVE WS-P-OWNER TO PM-OWNER-ROLE
           IF WS-P-COMPANY = "*"
               MOVE "ALL" TO PM-SCOPE
           ELSE
               MOVE "COMPANY" TO PM-SCOPE
           END-IF
           PERFORM 1300-CONVERT-DATE
           PERFORM 1400-CONVERT-NUMBER
           MOVE SPACES TO CA-MESSAGE
           STRING FUNCTION TRIM(PM-PARAM-CODE) " = "
                  FUNCTION TRIM(PM-VALUE-TEXT)
                  " (" FUNCTION TRIM(PM-SCOPE) ")"
               DELIMITED BY SIZE INTO CA-MESSAGE.
       1000-EXIT.
           EXIT.

      *>   a missing tuning parameter must not stop a business action
      *>   when the caller has offered a documented default
       1200-NOT-HELD SECTION.
       1200-START.
           IF PM-USE-DEFAULT = "Y"
               MOVE PM-DEFAULT-TEXT TO PM-VALUE-TEXT
               MOVE "DEFAULT" TO PM-SCOPE
               PERFORM 1400-CONVERT-NUMBER
               MOVE 4 TO CA-RETURN-CODE
               MOVE "PRM0004" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Parameter " FUNCTION TRIM(PM-PARAM-CODE)
                      " is not held - the supplied default was used"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CA-MSG-COUNT
           ELSE
               MOVE 16 TO CA-RETURN-CODE
               MOVE "PRM0003" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Parameter " FUNCTION TRIM(PM-PARAM-CODE)
                      " is not held for this company and date"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       1200-EXIT.
           EXIT.

      *>   SQL dates arrive as yyyy-mm-dd
       1300-CONVERT-DATE SECTION.
       1300-START.
           MOVE 0 TO PM-EFFECTIVE-FROM
           IF WS-P-EFFECTIVE = SPACES
               GO TO 1300-EXIT
           END-IF
           MOVE WS-P-EFFECTIVE TO WS-ISO
           MOVE WS-ISO-Y TO WS-C-Y
           MOVE WS-ISO-M TO WS-C-M
           MOVE WS-ISO-D TO WS-C-D
           IF WS-COMPACT IS NUMERIC
               MOVE WS-COMPACT TO PM-EFFECTIVE-FROM
           END-IF.
       1300-EXIT.
           EXIT.

      *>   a parameter value is text; it is also offered as a number
      *>   when every character is part of one
       1400-CONVERT-NUMBER SECTION.
       1400-START.
           MOVE 0 TO PM-VALUE-NUM
           MOVE "N" TO PM-IS-NUMERIC
           MOVE 0 TO WS-DIGITS
           MOVE 0 TO WS-BAD-CHARS
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 200
               MOVE PM-VALUE-TEXT(WS-I:1) TO WS-CH
               EVALUATE TRUE
                   WHEN WS-CH >= "0" AND WS-CH <= "9"
                       ADD 1 TO WS-DIGITS
                   WHEN WS-CH = "." OR WS-CH = "-" OR WS-CH = "+"
                       CONTINUE
                   WHEN WS-CH = SPACE OR WS-CH = LOW-VALUE
                       CONTINUE
                   WHEN OTHER
                       ADD 1 TO WS-BAD-CHARS
               END-EVALUATE
           END-PERFORM
           IF WS-DIGITS > 0 AND WS-BAD-CHARS = 0
               MOVE "Y" TO PM-IS-NUMERIC
               COMPUTE PM-VALUE-NUM = FUNCTION NUMVAL(PM-VALUE-TEXT)
           END-IF.
       1400-EXIT.
           EXIT.

       1500-INSIST-NUMERIC SECTION.
       1500-START.
           IF CA-RETURN-CODE > 4
               GO TO 1500-EXIT
           END-IF
           IF PM-IS-NUMERIC = "N"
               MOVE "PRM0005" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Parameter " FUNCTION TRIM(PM-PARAM-CODE)
                      " is not numeric - value is ["
                      FUNCTION TRIM(PM-VALUE-TEXT) "]"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
           END-IF.
       1500-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       9000-REJECT SECTION.
       9000-START.
           MOVE "Y" TO WS-ERROR
           MOVE 8 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION SECTION.
       9100-START.
           MOVE "PRM0009" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "PARMGET does not support function "
                  FUNCTION TRIM(CA-FUNCTION)
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT.
       9100-EXIT.
           EXIT.

      *>   The core procedures raise business refusals with
      *>   THROW 5xxxx, 'STERP-E-nnnn: ...' - those are a business
      *>   answer (08), not a system failure (12).
       9300-SQL-FAILED SECTION.
       9300-START.
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
           IF SQLIO-ERRMSG(1:512) = SPACES
               MOVE "SQL failure in PARMGET" TO CA-MESSAGE
           END-IF
           IF CA-MESSAGE(1:8) = "STERP-E-"
              OR CA-MESSAGE(1:8) = "STERP-W-"
               MOVE "PRM0008" TO CA-REASON-CODE
               MOVE 8 TO CA-RETURN-CODE
           ELSE
               MOVE "SQL0001" TO CA-REASON-CODE
               MOVE 12 TO CA-RETURN-CODE
           END-IF
           ADD 1 TO CA-MSG-COUNT.
       9300-EXIT.
           EXIT.
