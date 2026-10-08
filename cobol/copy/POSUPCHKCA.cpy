      *>================================================================
      *> POSUPCHKCA - COMMAREA for POSUPCHK, supplier due diligence.
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *>
      *> PO-026 and D-29.  Evidence is held in po.SupplierDiligence,
      *> one row per check per occasion, so the history of what was
      *> known WHEN is preserved: a sanctions pass recorded last year
      *> does not rewrite itself when this year's check fails.
      *>
      *> The outcome of a CHECK is the worst of:
      *>   FAIL     a blocking result (sanctions hit, Companies House
      *>            dissolved) - the order cannot be issued at all
      *>   EXPIRED  mandatory evidence past ValidTo - authorised
      *>            exception required
      *>   MISSING  mandatory evidence never held - exception required
      *>   REVIEW   a result that needs a human look - warning only
      *>   PASS     everything mandatory is held and in date
      *>
      *> A blocking FAIL is never overridable by the EXCEPTION
      *> function; only EXPIRED and MISSING are, and only with an
      *> approver and a reason (PO-026 "authorised exception").
      *>
      *> CA-FUNCTION values (CA-FUNCTION is 8 bytes - a longer name
      *> would arrive truncated and never match)
      *>   CHECK     evaluate the supplier for order issue
      *>   RECORD    record one piece of evidence
      *>   EXCEPT    record an authorised exception for missing or
      *>             expired evidence
      *>   ENQUIRE   list the current evidence
      *>
      *> CA-RETURN-CODE 00 clear, 04 review, 06 exception required,
      *>   08 blocked, 12 severe, 16 supplier has no evidence at all
      *>
      *> REASON CODES
      *>   DIL0001 company or supplier missing
      *>   DIL0002 no due diligence evidence held at all
      *>   DIL0003 blocking screening result - cannot trade
      *>   DIL0004 mandatory evidence has expired
      *>   DIL0005 mandatory evidence has never been held
      *>   DIL0006 evidence needs review
      *>   DIL0007 an exception needs an approver and a reason
      *>   DIL0008 unsupported function
      *>   DIL0009 unknown evidence type
      *>   DIL0010 a blocking result cannot be excepted
      *>----------------------------------------------------------------
      *> 2004-02-09 DJM  CR1152 SUPPLIER APPROVAL EVIDENCE
      *> 2016-04-18 KMP  CR2290 MODERN SLAVERY STATEMENT
      *> 2026-10-06 STERP SANCTIONS BLOCK, D-29 MANDATORY SET
      *> 2026-10-07 STERP CR9016 EXCEPT FITS CA-FUNCTION, EXCEPTION
      *>                 DID NOT
      *>================================================================
      *> REQ: PO-026, D-29
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  DD-BODY.
               10  DD-SUPPLIER         PIC X(20).
               10  DD-CHECK-DATE       PIC 9(8).
               10  DD-ORDER-VALUE-GBP  PIC S9(13)V99.
               10  DD-ORDER-NUMBER     PIC X(30).
      *>     ---- evidence fields ----
               10  DD-EVIDENCE-TYPE    PIC X(20).
                   88  DD-SANCTIONS              VALUE "SANCTIONS".
                   88  DD-COMPANIESHSE           VALUE "COMPANIESHSE".
                   88  DD-VATNUMBER              VALUE "VATNUMBER".
                   88  DD-BUSINESSSIZE           VALUE "BUSINESSSIZE".
                   88  DD-MODERNSLAVERY          VALUE "MODERNSLAVERY".
                   88  DD-ANTIBRIBERY            VALUE "ANTIBRIBERY".
                   88  DD-INSURANCE              VALUE "INSURANCE".
                   88  DD-QUALITY                VALUE "QUALITY".
                   88  DD-TYPE-KNOWN             VALUE "SANCTIONS",
                       "COMPANIESHSE", "VATNUMBER", "BUSINESSSIZE",
                       "MODERNSLAVERY", "ANTIBRIBERY", "INSURANCE",
                       "QUALITY".
               10  DD-EVIDENCE-REF     PIC X(100).
               10  DD-RESULT           PIC X(12).
                   88  DD-RES-PASS               VALUE "PASS".
                   88  DD-RES-REVIEW             VALUE "REVIEW".
                   88  DD-RES-FAIL               VALUE "FAIL".
                   88  DD-RES-EXPIRED            VALUE "EXPIRED".
                   88  DD-RES-NOTHELD            VALUE "NOTHELD".
               10  DD-IS-BLOCKING      PIC X.
                   88  DD-BLOCKING               VALUE "Y".
               10  DD-IS-MANDATORY     PIC X.
                   88  DD-MANDATORY              VALUE "Y".
               10  DD-VALID-FROM       PIC 9(8).
               10  DD-VALID-TO         PIC 9(8).
               10  DD-NEXT-REVIEW      PIC 9(8).
               10  DD-NOTE             PIC X(300).
               10  DD-CHECKED-BY       PIC X(20).
      *>     ---- exception fields ----
               10  DD-EXCEPTION-BY     PIC X(20).
               10  DD-EXCEPTION-REASON PIC X(300).
      *>     ---- answers ----
               10  DD-DILIGENCE-ID     PIC S9(9) COMP-5.
               10  DD-OVERALL          PIC X(12).
                   88  DD-CLEAR                  VALUE "PASS".
                   88  DD-NEEDS-REVIEW           VALUE "REVIEW".
                   88  DD-NEEDS-EXCEPT           VALUE "EXCEPTION".
                   88  DD-BLOCKED                VALUE "BLOCKED".
               10  DD-CAN-ORDER        PIC X.
                   88  DD-MAY-ORDER              VALUE "Y".
               10  DD-FAIL-CNT         PIC 9(4).
               10  DD-REVIEW-CNT       PIC 9(4).
               10  DD-EXPIRED-CNT      PIC 9(4).
               10  DD-MISSING-CNT      PIC 9(4).
               10  DD-EXCEPTED-CNT     PIC 9(4).
               10  DD-HELD-CNT         PIC 9(4).
               10  DD-MISSING-LIST     PIC X(200).
               10  DD-WORST-TYPE       PIC X(20).
               10  DD-NEXT-REVIEW-DUE  PIC 9(8).
               10  DD-APPROVAL-ID      PIC S9(17).
               10  DD-DETAIL-CNT       PIC 9(2).
               10  DD-DETAIL-TABLE.
                   15  DD-DETAIL OCCURS 8 TIMES.
                       20  DD-D-TYPE       PIC X(20).
                       20  DD-D-RESULT     PIC X(12).
                       20  DD-D-VALID-TO   PIC 9(8).
                       20  DD-D-MANDATORY  PIC X.
                       20  DD-D-EXCEPTED   PIC X.
