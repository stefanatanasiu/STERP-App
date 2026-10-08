      *>================================================================
      *> PROGRAM-ID : PAYTERMC
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : CHECK PURCHASE ORDER PAYMENT TERMS AGAINST THE
      *>              COMPANY PAYMENT POLICY AND THE STATUTORY MAXIMUM
      *>              FOR THE SUPPLIER'S SIZE, PRODUCE THE AGREED TERMS
      *>              SNAPSHOT THAT FLOWS UNCHANGED TO THE SUPPLIER
      *>              INVOICE DUE DATE, AND FLAG THE ORDER FOR THE
      *>              PAYMENT PRACTICES EXTRACT (PO-027, D-25).
      *> CALLED BY  : POORDER, POAPPRV, the .NET purchasing facade
      *> INTERFACE  : CALL "PAYTERMC" USING DFHCOMMAREA (PAYTERMCCA)
      *> CALLS      : TERMCALC, PARMGET, SQLIO -> po.SupplierDiligence
      *>----------------------------------------------------------------
      *> THE TWO LIMITS ARE NOT THE SAME THING.  THE COMPANY POLICY
      *> FIGURE IS WHAT WE SAY WE DO - LONGER THAN THAT IS A WARNING
      *> AND IS REPORTED.  THE STATUTORY MAXIMUM IS WHAT WE MAY DO -
      *> LONGER THAN THAT IS A BREACH AND NEEDS AN AUTHORISED
      *> EXCEPTION BEFORE THE ORDER CAN BE ISSUED.
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 2012-09-03 KMP  CR2001 LATE PAYMENT REGULATIONS
      *> 2017-04-06 KMP  CR2341 PAYMENT PRACTICES REPORTING
      *> 2026-10-06 STERP D-25 STATUTORY MAXIMUM AND EXCEPTIONS
      *>================================================================
      *> REQ: PO-027, D-25
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYTERMC.
       AUTHOR. K M PATEL.
       DATE-WRITTEN. 03/09/2012.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY TERMCALCCA REPLACING ==DFHCOMMAREA== BY ==WS-TERM-CA==
                      LEADING ==CA-== BY ==TM-==.
       COPY PARMGETCA REPLACING ==DFHCOMMAREA== BY ==WS-PARM-CA==
                      LEADING ==CA-== BY ==PG-==.
       01  WS-SWITCHES.
           05  WS-FATAL-SW             PIC X VALUE "N".
               88  WS-FATAL                      VALUE "Y".
       01  WS-WORK.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-SIZE-ROW             PIC X(20).
           05  WS-DAY-ED               PIC -(4)9.
           05  WS-PCT-ED               PIC -(3)9.99.
           05  WS-LIMIT-NAME           PIC X(20).
           05  WS-MSG                  PIC X(200).
       01  WS-DEFAULTS.
           05  WS-DEF-POLICY           PIC S9(5) VALUE 30.
           05  WS-DEF-STATUTORY        PIC S9(5) VALUE 60.
           05  WS-DEF-SME              PIC S9(5) VALUE 60.
       LINKAGE SECTION.
       COPY PAYTERMCCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAINLINE SECTION.
       0000-START.
           PERFORM 1000-INIT THRU 1000-EXIT
           IF WS-FATAL
               GO TO 0000-RETURN
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "CHECK"
                   PERFORM 2000-CHECK THRU 2000-EXIT
               WHEN "APPLY"
                   PERFORM 2000-CHECK THRU 2000-EXIT
                   IF NOT WS-FATAL
                       PERFORM 5000-DUE-DATE THRU 5000-EXIT
                   END-IF
               WHEN "DUEDATE"
                   PERFORM 3000-RESOLVE THRU 3000-EXIT
                   IF NOT WS-FATAL
                       PERFORM 5000-DUE-DATE THRU 5000-EXIT
                   END-IF
               WHEN OTHER
                   MOVE "PTC0006" TO CA-REASON-CODE
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Function " FUNCTION TRIM(CA-FUNCTION)
                          " is not supported by PAYTERMC"
                       DELIMITED BY SIZE INTO CA-MESSAGE
                   MOVE 8 TO CA-RETURN-CODE
           END-EVALUATE.
       0000-RETURN.
           GOBACK.
       0000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1000-INIT SECTION.
       1000-START.
           MOVE "N" TO WS-FATAL-SW
           MOVE "PAYTERMC" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE 0 TO PT-SETT-DISC-PCT PT-SETT-DISC-DAYS
      *>     PT-TERMS-DAYS is both an input (when the caller supplies
      *>     a bare day count) and an output (when a terms code is
      *>     resolved), so it is NOT cleared here.
           MOVE 0 TO PT-DUE-DATE PT-EXCESS-DAYS PT-APPROVAL-ID
           MOVE SPACES TO PT-TERMS-BASIS PT-TERMS-SNAPSHOT
           MOVE "WITHIN" TO PT-OUTCOME
           MOVE "N" TO PT-REPORTABLE PT-NEEDS-EXCEPTION
           IF PT-LIMIT-SOURCE = SPACES
               MOVE "LOOKUP" TO PT-LIMIT-SOURCE
           END-IF
           IF PT-SUPPLIER-SIZE = SPACES
               MOVE "LARGE" TO PT-SUPPLIER-SIZE
           END-IF
           IF PT-ORDER-DATE = 0
               MOVE CA-BUSINESS-DATE TO PT-ORDER-DATE
           END-IF
           IF CA-COMPANY = SPACES
               MOVE 8 TO CA-RETURN-CODE
               MOVE "PTC0001" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A company code is needed to check payment "
                      "terms against the company payment policy"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE "Y" TO WS-FATAL-SW
           END-IF.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> THE CHECK ITSELF.  RESOLVE THE TERMS, RESOLVE THE TWO LIMITS,
      *> THEN COMPARE.  THE TIGHTER OF THE STATUTORY MAXIMUM AND ANY
      *> SMALL SUPPLIER MAXIMUM IS THE FIGURE THAT BLOCKS.
      *>----------------------------------------------------------------
       2000-CHECK SECTION.
       2000-START.
           PERFORM 3000-RESOLVE THRU 3000-EXIT
           IF WS-FATAL
               GO TO 2000-EXIT
           END-IF
           PERFORM 3500-LIMITS THRU 3500-EXIT
           IF WS-FATAL
               GO TO 2000-EXIT
           END-IF
           PERFORM 4000-ASSESS THRU 4000-EXIT
           PERFORM 4500-SNAPSHOT THRU 4500-EXIT.
       2000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> RESOLVE THE AGREED TERMS.  A TERMS CODE IS READ THROUGH
      *> TERMCALC SO THAT PURCHASING AND THE PURCHASE LEDGER CANNOT
      *> DISAGREE ABOUT WHAT THE CODE MEANS.
      *>----------------------------------------------------------------
       3000-RESOLVE SECTION.
       3000-START.
           IF PT-TERMS-CODE = SPACES AND PT-TERMS-DAYS = 0
               MOVE 8 TO CA-RETURN-CODE
               MOVE "PTC0001" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Either a payment terms code or a day count "
                      "is needed before the terms can be checked"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE "Y" TO WS-FATAL-SW
               GO TO 3000-EXIT
           END-IF
           IF PT-TERMS-CODE = SPACES
               MOVE "INVOICE" TO PT-TERMS-BASIS
               GO TO 3000-EXIT
           END-IF
           INITIALIZE WS-TERM-CA
           MOVE "GETTERMS" TO TM-FUNCTION
           MOVE CA-COMPANY TO TM-COMPANY
           MOVE CA-SITE TO TM-SITE
           MOVE CA-USER TO TM-USER
           MOVE PT-ORDER-DATE TO TM-BUSINESS-DATE
           MOVE CA-CORRELATION-ID TO TM-CORRELATION-ID
           MOVE PT-TERMS-CODE TO TC-TERMS-CODE
           MOVE PT-ORDER-DATE TO TC-INVOICE-DATE
           CALL "TERMCALC" USING WS-TERM-CA
           IF TM-RETURN-CODE >= 8
               MOVE 16 TO CA-RETURN-CODE
               MOVE "PTC0002" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Payment terms " FUNCTION TRIM(PT-TERMS-CODE)
                      " are not on file: " TM-MESSAGE(1:120)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE "Y" TO WS-FATAL-SW
               GO TO 3000-EXIT
           END-IF
           MOVE TC-BASIS-TYPE TO PT-TERMS-BASIS
           MOVE TC-DISCOUNT-PCT TO PT-SETT-DISC-PCT
           MOVE TC-DISCOUNT-DAYS TO PT-SETT-DISC-DAYS
           IF TC-TERM-DAYS NOT = 0
               MOVE TC-TERM-DAYS TO PT-TERMS-DAYS
           ELSE
               MOVE TC-DAYS TO PT-TERMS-DAYS
           END-IF
           IF PT-TERMS-DAYS < 0
               MOVE 8 TO CA-RETURN-CODE
               MOVE "PTC0007" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Payment terms " FUNCTION TRIM(PT-TERMS-CODE)
                      " resolve to a negative number of days"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE "Y" TO WS-FATAL-SW
           END-IF.
       3000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> RESOLVE THE POLICY AND STATUTORY LIMITS.  THE BASELINE OF
      *> D-25 IS 60 DAYS FROM RECEIPT OF A VALID INVOICE FOR ANY
      *> SUPPLIER; THE COMPANY POLICY IS NORMALLY TIGHTER.
      *>----------------------------------------------------------------
       3500-LIMITS SECTION.
       3500-START.
           IF PT-LIMITS-PASSED
               MOVE PT-POLICY-DAYS-IN TO PT-POLICY-DAYS
               MOVE PT-STAT-MAX-IN TO PT-STAT-MAX-DAYS
           ELSE
               MOVE "PO.TERMS.POLICYDAYS" TO WS-LIMIT-NAME
               MOVE WS-DEF-POLICY TO WS-DAY-ED
               PERFORM 3600-PARM THRU 3600-EXIT
               MOVE PM-VALUE-NUM TO PT-POLICY-DAYS
               MOVE "PO.TERMS.STATMAX" TO WS-LIMIT-NAME
               MOVE WS-DEF-STATUTORY TO WS-DAY-ED
               PERFORM 3600-PARM THRU 3600-EXIT
               MOVE PM-VALUE-NUM TO PT-STAT-MAX-DAYS
               IF PT-SIZE-SME OR PT-SIZE-MICRO
                   MOVE "PO.TERMS.SMEMAX" TO WS-LIMIT-NAME
                   MOVE WS-DEF-SME TO WS-DAY-ED
                   PERFORM 3600-PARM THRU 3600-EXIT
                   IF PM-VALUE-NUM > 0 AND
                      PM-VALUE-NUM < PT-STAT-MAX-DAYS
                       MOVE PM-VALUE-NUM TO PT-STAT-MAX-DAYS
                   END-IF
               END-IF
           END-IF
           IF PT-POLICY-DAYS <= 0
               MOVE WS-DEF-POLICY TO PT-POLICY-DAYS
           END-IF
           IF PT-STAT-MAX-DAYS <= 0
               MOVE WS-DEF-STATUTORY TO PT-STAT-MAX-DAYS
           END-IF
           MOVE PT-STAT-MAX-DAYS TO PT-EFFECTIVE-LIMIT.
       3500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       3600-PARM SECTION.
       3600-START.
           INITIALIZE WS-PARM-CA
           MOVE "GETNUM" TO PG-FUNCTION
           MOVE CA-COMPANY TO PG-COMPANY
           MOVE CA-SITE TO PG-SITE
           MOVE CA-USER TO PG-USER
           MOVE PT-ORDER-DATE TO PG-BUSINESS-DATE
           MOVE WS-LIMIT-NAME TO PM-PARAM-CODE
           MOVE PT-ORDER-DATE TO PM-AS-OF
           MOVE "Y" TO PM-USE-DEFAULT
           MOVE FUNCTION TRIM(WS-DAY-ED) TO PM-DEFAULT-TEXT
           CALL "PARMGET" USING WS-PARM-CA
           IF PG-RETURN-CODE >= 8 OR PM-VALUE-NUM = 0
               COMPUTE PM-VALUE-NUM = FUNCTION NUMVAL(WS-DAY-ED)
           END-IF.
       3600-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> COMPARE AND DECIDE.  AN EXCEPTION ONLY COUNTS WHEN IT CARRIES
      *> BOTH AN APPROVER AND A REASON - "AUTHORISED" MEANS SOMEONE
      *> NAMED SAID YES AND SAID WHY (PO-027).
      *>----------------------------------------------------------------
       4000-ASSESS SECTION.
       4000-START.
           IF PT-TERMS-DAYS > PT-POLICY-DAYS
               MOVE "Y" TO PT-REPORTABLE
           END-IF
           IF PT-TERMS-DAYS > PT-EFFECTIVE-LIMIT
               COMPUTE PT-EXCESS-DAYS =
                       PT-TERMS-DAYS - PT-EFFECTIVE-LIMIT
               MOVE "Y" TO PT-NEEDS-EXCEPTION
               MOVE "BREACH" TO PT-OUTCOME
               MOVE 6 TO CA-RETURN-CODE
               MOVE "PTC0004" TO CA-REASON-CODE
               MOVE PT-TERMS-DAYS TO WS-DAY-ED
               MOVE SPACES TO WS-MSG
               STRING "Payment terms of "
                      FUNCTION TRIM(WS-DAY-ED)
                      " days exceed the maximum of "
                   DELIMITED BY SIZE INTO WS-MSG
               MOVE PT-EFFECTIVE-LIMIT TO WS-DAY-ED
               MOVE SPACES TO CA-MESSAGE
               STRING FUNCTION TRIM(WS-MSG) " "
                      FUNCTION TRIM(WS-DAY-ED)
                      " days allowed for a "
                      FUNCTION TRIM(PT-SUPPLIER-SIZE)
                      " supplier and need an authorised exception"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               IF PT-EXCEPTION-BY NOT = SPACES AND
                  PT-EXCEPTION-REASON NOT = SPACES
                   MOVE "EXCEPTED" TO PT-OUTCOME
                   MOVE "N" TO PT-NEEDS-EXCEPTION
                   MOVE 4 TO CA-RETURN-CODE
                   MOVE "PTC0004" TO CA-REASON-CODE
                   MOVE SPACES TO CA-MESSAGE
                   STRING "Terms above the statutory maximum "
                          "authorised by "
                          FUNCTION TRIM(PT-EXCEPTION-BY)
                          " - reported in payment practices data"
                       DELIMITED BY SIZE INTO CA-MESSAGE
               ELSE
                   IF PT-EXCEPTION-BY NOT = SPACES OR
                      PT-EXCEPTION-REASON NOT = SPACES
                       MOVE "PTC0005" TO CA-REASON-CODE
                       MOVE SPACES TO CA-MESSAGE
                       STRING "An exception to the payment terms "
                              "maximum needs both an approver and "
                              "a reason"
                           DELIMITED BY SIZE INTO CA-MESSAGE
                   END-IF
               END-IF
               GO TO 4000-EXIT
           END-IF
           IF PT-TERMS-DAYS > PT-POLICY-DAYS
               COMPUTE PT-EXCESS-DAYS =
                       PT-TERMS-DAYS - PT-POLICY-DAYS
               MOVE "WARN" TO PT-OUTCOME
               MOVE 4 TO CA-RETURN-CODE
               MOVE "PTC0003" TO CA-REASON-CODE
               MOVE PT-TERMS-DAYS TO WS-DAY-ED
               MOVE SPACES TO WS-MSG
               STRING "Payment terms of "
                      FUNCTION TRIM(WS-DAY-ED)
                      " days are longer than the company policy of "
                   DELIMITED BY SIZE INTO WS-MSG
               MOVE PT-POLICY-DAYS TO WS-DAY-ED
               MOVE SPACES TO CA-MESSAGE
               STRING FUNCTION TRIM(WS-MSG) " "
                      FUNCTION TRIM(WS-DAY-ED)
                      " days and will appear in the payment "
                      "practices report"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 4000-EXIT
           END-IF
           MOVE "WITHIN" TO PT-OUTCOME
           MOVE 0 TO PT-EXCESS-DAYS
           MOVE 0 TO CA-RETURN-CODE.
       4000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> THE SNAPSHOT IS THE TEXT THAT GOES ON THE ORDER AND ON THE
      *> SUPPLIER INVOICE.  IT IS DELIBERATELY SELF CONTAINED SO THAT
      *> A LATER CHANGE TO THE TERMS CODE CANNOT REWRITE HISTORY.
      *>----------------------------------------------------------------
       4500-SNAPSHOT SECTION.
       4500-START.
           MOVE SPACES TO PT-TERMS-SNAPSHOT
           MOVE PT-TERMS-DAYS TO WS-DAY-ED
           EVALUATE PT-TERMS-BASIS
               WHEN "EOM"
                   STRING FUNCTION TRIM(WS-DAY-ED)
                          " days from end of month"
                       DELIMITED BY SIZE INTO PT-TERMS-SNAPSHOT
               WHEN "DAYOFNEXTMONTH"
                   STRING "day " FUNCTION TRIM(WS-DAY-ED)
                          " of the following month"
                       DELIMITED BY SIZE INTO PT-TERMS-SNAPSHOT
               WHEN OTHER
                   STRING FUNCTION TRIM(WS-DAY-ED)
                          " days from receipt of a valid invoice"
                       DELIMITED BY SIZE INTO PT-TERMS-SNAPSHOT
           END-EVALUATE
           IF PT-SETT-DISC-PCT > 0
               MOVE PT-SETT-DISC-PCT TO WS-PCT-ED
               MOVE PT-SETT-DISC-DAYS TO WS-DAY-ED
               MOVE SPACES TO WS-MSG
               STRING FUNCTION TRIM(PT-TERMS-SNAPSHOT)
                      ", " FUNCTION TRIM(WS-PCT-ED)
                      "% settlement discount within "
                      FUNCTION TRIM(WS-DAY-ED) " days"
                   DELIMITED BY SIZE INTO WS-MSG
               MOVE WS-MSG TO PT-TERMS-SNAPSHOT
           END-IF.
       4500-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> THE AGREED TERMS FLOW UNCHANGED TO THE INVOICE DUE DATE.
      *>----------------------------------------------------------------
       5000-DUE-DATE SECTION.
       5000-START.
           IF PT-INVOICE-DATE = 0
               MOVE PT-ORDER-DATE TO PT-INVOICE-DATE
           END-IF
           INITIALIZE WS-TERM-CA
           MOVE "DUEDATE" TO TM-FUNCTION
           MOVE CA-COMPANY TO TM-COMPANY
           MOVE CA-SITE TO TM-SITE
           MOVE CA-USER TO TM-USER
           MOVE PT-INVOICE-DATE TO TM-BUSINESS-DATE
           MOVE CA-CORRELATION-ID TO TM-CORRELATION-ID
           MOVE PT-TERMS-CODE TO TC-TERMS-CODE
           IF PT-TERMS-CODE = SPACES
               MOVE PT-TERMS-BASIS TO TC-BASIS-TYPE
               MOVE PT-TERMS-DAYS TO TC-DAYS
           END-IF
           MOVE PT-INVOICE-DATE TO TC-INVOICE-DATE
           MOVE PT-INVOICE-DATE TO TC-RECEIVED-DATE
           MOVE PT-VALUE-GBP TO TC-NET-AMOUNT
           CALL "TERMCALC" USING WS-TERM-CA
           IF TM-RETURN-CODE >= 8
               IF CA-RETURN-CODE < 8
                   MOVE 8 TO CA-RETURN-CODE
                   MOVE "PTC0008" TO CA-REASON-CODE
                   MOVE TM-MESSAGE TO CA-MESSAGE
               END-IF
               GO TO 5000-EXIT
           END-IF
           MOVE TC-DUE-DATE TO PT-DUE-DATE.
       5000-EXIT.
           EXIT.
       END PROGRAM PAYTERMC.
