      *>================================================================
      *> EXPEVIDCA - COMMAREA FOR EXPEVID, EXPORT AND CUSTOMS EVIDENCE
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : SALES ORDER MANAGEMENT
      *>
      *> ZERO RATING AN EXPORT IS CONDITIONAL, NOT AUTOMATIC.  THE
      *> COMMERCIAL AND OFFICIAL EVIDENCE MUST BE OBTAINED WITHIN THE
      *> PERMITTED PERIOD, WHICH IS THREE MONTHS FROM THE SUPPLY BY
      *> BASELINE (SO-026).  AN EXPORT THAT REACHES ITS DEADLINE WITH
      *> NO PROOF BECOMES OVERDUE, RAISES AN EXCEPTION AND REPORTS THE
      *> VAT THAT MUST NOW BE ACCOUNTED FOR.
      *>
      *> A DESPATCH TO AN EU OR REST OF WORLD ADDRESS MAY NOT CARRY A
      *> DOMESTIC VAT CODE WITHOUT AUTHORISED EVIDENCE (EXP0003).
      *> GREAT BRITAIN TO NORTHERN IRELAND MOVEMENTS OF GOODS ARE UK
      *> DOMESTIC SUPPLIES AND ARE NOT EXPORTS (D-23).
      *>
      *> CA-FUNCTION VALUES
      *>   CAPTURE    RECORD INCOTERMS RULE AND NAMED PLACE, COMMODITY
      *>              CODE, COUNTRY OF ORIGIN, CUSTOMS VALUE, CUSTOMER
      *>              EORI AND THE EXPORT DECLARATION REFERENCE.  A
      *>              DESPATCH LINE WITH NO COMMODITY CODE BLOCKS THE
      *>              CUSTOMS HAND OFF (EXP0007).
      *>   RECORDPRF  RECORD THE PROOF OF EXPORT TYPE AND DATE AND SET
      *>              XE-PROOF-DEADLINE THROUGH DATECALC.
      *>   CHECKDUE   LIST EXPORTS WHOSE EVIDENCE IS DUE OR OVERDUE,
      *>              MARK THE OVERDUE ONES, RAISE AN EXCEPTION THROUGH
      *>              EXCPRAIS AND REPORT THE VAT THAT FALLS DUE.
      *>   STATUS     READ BACK ONE DESPATCH AND ITS EVIDENCE STATE.
      *>
      *> REASON CODES
      *>   EXP0001 DESPATCH REFERENCE MISSING OR NOT KNOWN
      *>   EXP0002 DESTINATION COUNTRY MISSING OR NOT TWO CHARACTERS
      *>   EXP0003 DOMESTIC VAT CODE ON AN EU OR REST OF WORLD SUPPLY
      *>   EXP0004 INCOTERMS RULE OR NAMED PLACE MISSING
      *>   EXP0005 CUSTOMER EORI MISSING FOR A CUSTOMS DECLARATION
      *>   EXP0006 PROOF TYPE OR PROOF DATE MISSING
      *>   EXP0007 COMMODITY CODE MISSING - CUSTOMS HAND OFF BLOCKED
      *>   EXP0008 PROOF OBTAINED AFTER THE PERMITTED PERIOD
      *>   EXP0009 UNSUPPORTED FUNCTION
      *>   EXP0010 COUNTRY OF ORIGIN OR CUSTOMS VALUE MISSING
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1994-06-07 RJH  CR0288 EC SALES AND EXPORT DOCUMENTATION
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT YEARS ON EVIDENCE DATES
      *> 2021-01-04 DLS  CR3004 BREXIT - GB EXPORTS AND NI PROTOCOL
      *> 2026-10-06 STERP SO-026 THREE MONTH PROOF OF EXPORT DEADLINE
      *>================================================================
      *> REQ: SO-025, SO-026
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  XE-BODY.
               10  XE-DESPATCH-REF     PIC X(30).
               10  XE-DESPATCH-ID      PIC S9(17).
               10  XE-ORDER-NUMBER     PIC X(30).
               10  XE-CUSTOMER         PIC X(20).
               10  XE-DESPATCH-DATE    PIC 9(8).
               10  XE-SUPPLY-DATE      PIC 9(8).
               10  XE-AS-AT-DATE       PIC 9(8).
               10  XE-IS-EXPORT        PIC X.
                   88  XE-EXPORT                 VALUE "Y".
               10  XE-DEST-COUNTRY     PIC X(2).
               10  XE-DEST-REGION      PIC X(3).
                   88  XE-REGION-GB              VALUE "GB".
                   88  XE-REGION-NI              VALUE "NI".
                   88  XE-REGION-EU              VALUE "EU".
                   88  XE-REGION-ROW             VALUE "ROW".
               10  XE-IS-DOMESTIC      PIC X.
               10  XE-INCOTERMS-RULE   PIC X(3).
               10  XE-INCOTERMS-PLACE  PIC X(60).
               10  XE-CUSTOMER-EORI    PIC X(17).
               10  XE-DECLARATION-REF  PIC X(40).
               10  XE-PROOF-TYPE       PIC X(30).
               10  XE-PROOF-DATE       PIC 9(8).
               10  XE-PROOF-DEADLINE   PIC 9(8).
               10  XE-PROOF-MONTHS     PIC S9(4) SIGN LEADING SEPARATE.
               10  XE-EVIDENCE-STATUS  PIC X(12).
                   88  XE-EV-NA                  VALUE "NA".
                   88  XE-EV-PENDING             VALUE "PENDING".
                   88  XE-EV-OBTAINED            VALUE "OBTAINED".
                   88  XE-EV-OVERDUE             VALUE "OVERDUE".
                   88  XE-EV-FAILED              VALUE "FAILED".
               10  XE-VAT-CODE         PIC X(4).
               10  XE-VAT-RATE-PCT     PIC S9(3)V9(4) COMP-3.
               10  XE-NET-GBP          PIC S9(11)V99 COMP-3.
               10  XE-VAT-DUE-GBP      PIC S9(11)V99 COMP-3.
               10  XE-CUSTOMS-VAL-GBP  PIC S9(11)V99 COMP-3.
               10  XE-DAYS-TO-DEADLINE PIC S9(9) COMP-5.
               10  XE-IS-OVERDUE       PIC X.
               10  XE-CUSTOMS-BLOCKED  PIC X.
               10  XE-EXCEPTION-ID     PIC S9(9) COMP-5.
               10  XE-EXCEPTION-COUNT  PIC S9(4) SIGN LEADING SEPARATE.
               10  XE-EVIDENCE-REF     PIC X(60).
               10  XE-OVERRIDE-APPROVL PIC S9(9) COMP-5.
               10  XE-TOTAL-VAT-DUE    PIC S9(11)V99 COMP-3.
               10  XE-LINE-CNT         PIC 9(3).
               10  XE-LINE-TAB.
                   15  XE-LINES        OCCURS 20 TIMES.
                       20  XE-L-LINE-NUM   PIC S9(9) COMP-5.
                       20  XE-L-ITEM       PIC X(40).
                       20  XE-L-DESC       PIC X(60).
                       20  XE-L-QTY        PIC S9(9)V9(6) COMP-3.
                       20  XE-L-COMMODITY  PIC X(10).
                       20  XE-L-ORIGIN     PIC X(2).
                       20  XE-L-CUST-VALUE PIC S9(11)V99 COMP-3.
                       20  XE-L-VAT-CODE   PIC X(4).
                       20  XE-L-BLOCKED    PIC X.
                       20  XE-L-MESSAGE    PIC X(80).
               10  XE-DUE-CNT          PIC 9(3).
               10  XE-DUE-TRUNCATED    PIC X.
               10  XE-DUE-TAB.
                   15  XE-DUES         OCCURS 40 TIMES.
                       20  XE-D-DESP-REF   PIC X(30).
                       20  XE-D-CUSTOMER   PIC X(20).
                       20  XE-D-DESP-DATE  PIC 9(8).
                       20  XE-D-DEADLINE   PIC 9(8).
                       20  XE-D-COUNTRY    PIC X(2).
                       20  XE-D-STATUS     PIC X(12).
                       20  XE-D-NET-GBP    PIC S9(11)V99 COMP-3.
                       20  XE-D-VAT-DUE    PIC S9(11)V99 COMP-3.
                       20  XE-D-MESSAGE    PIC X(80).
