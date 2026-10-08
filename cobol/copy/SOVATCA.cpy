      *>================================================================
      *> SOVATCA  - COMMAREA for SOVAT, the sales line VAT liability
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : SALES ORDER MANAGEMENT
      *> CALLED BY : SALES ORDER ENTRY, QUOTATION ENTRY, DESPATCH
      *>             CONFIRMATION, INVOICING, THE .NET CLIENT
      *>
      *> EACH LINE TAKES ITS UK VAT LIABILITY FROM THE ITEM'S VAT
      *> CLASSIFICATION, THE CUSTOMER'S STATUS AND LOCATION, THE PLACE
      *> OF SUPPLY AND THE DELIVERY EVIDENCE (SO-025, SO-026, SO-003).
      *>   S    standard rated 20%          R    reduced rated 5%
      *>   Z    zero rated 0%               E    exempt
      *>   O    outside the scope of UK VAT
      *>   RC   reverse charge services to an overseas business
      *>   DRC  domestic reverse charge for specified goods
      *>   EXP  zero rated export of goods out of Great Britain
      *> NORTHERN IRELAND (XI) IS A UK SUPPLY WITH UK VAT.  MOVEMENTS
      *> OF GOODS BETWEEN NORTHERN IRELAND AND THE EU ARE OUTSIDE THE
      *> BASELINE (D-23) AND ARE REJECTED WITH VAT0009.
      *> ZERO RATING AN EXPORT IS CONDITIONAL: THE EVIDENCE STATUS IS
      *> PENDING UNTIL PROOF OF EXPORT IS HELD, AND A MISSING
      *> CONDITION BLOCKS INVOICING RATHER THAN DEFAULTING TO ZERO.
      *>
      *> CA-FUNCTION VALUES  (CA-FUNCTION IS EIGHT CHARACTERS)
      *>   DECIDE    decide the liability for one line
      *>             ("DETERMIN" is accepted as the same request)
      *>   EXPLAIN   DECIDE plus the ordered rule trace
      *>   EVIDENCE  re-test the export evidence and deadline only
      *>   OVERRIDE  record an authorised override of the decision
      *>
      *> REASON CODES
      *>   VAT0001 company, customer or item missing
      *>   VAT0002 no effective VAT classification for the item
      *>   VAT0003 reverse charge sale without a valid customer VAT
      *>           number - invoicing blocked
      *>   VAT0004 export zero rating pending proof of export
      *>   VAT0005 proof of export is overdue - account for VAT
      *>   VAT0006 a domestic VAT code cannot be used for an overseas
      *>           delivery without authorised evidence
      *>   VAT0007 override requires an approval reference and a reason
      *>   VAT0008 unsupported function
      *>   VAT0009 NI to EU goods movement is outside the baseline
      *>   VAT0010 VAT rate not held for that code on the supply date
      *>   VAT0011 customer VAT location not one of GB, XI, EU, ROW
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1994-02-28 RJH  CR0271 VAT LIABILITY BY PRODUCT GROUP
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT TAX POINT DATES
      *> 2007-06-01 DJM  CR1521 MOBILE TELEPHONE DOMESTIC REVERSE CHARGE
      *> 2021-01-04 DLS  CR3004 BREXIT - GB EXPORTS AND NI PROTOCOL
      *> 2026-10-06 STERP SO-025, SO-026 EVIDENCE DRIVEN ZERO RATING
      *>================================================================
      *> REQ: SO-025, SO-026, SO-003
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  SV-BODY.
      *>       ---- the supply ----
               10  SV-DOC-TYPE         PIC X(20).
               10  SV-DOC-REF          PIC X(40).
               10  SV-LINE-NUM         PIC S9(9) COMP-5.
               10  SV-ITEM-CODE        PIC X(40).
               10  SV-LINE-TYPE        PIC X(10).
                   88  SV-IS-GOODS               VALUE "GOODS".
                   88  SV-IS-SERVICE             VALUE "SERVICE".
                   88  SV-IS-CHARGE              VALUE "CHARGE".
               10  SV-SUPPLY-DATE      PIC 9(8).
               10  SV-DESPATCH-DATE    PIC 9(8).
               10  SV-NET-AMOUNT       PIC S9(13)V9(2).
               10  SV-CURRENCY         PIC X(3).
               10  SV-NET-AMOUNT-GBP   PIC S9(13)V9(2).
      *>       ---- the customer ----
               10  SV-CUSTOMER         PIC X(20).
               10  SV-CUST-LOCATION    PIC X(3).
                   88  SV-CUST-GB                VALUE "GB".
                   88  SV-CUST-XI                VALUE "XI".
                   88  SV-CUST-EU                VALUE "EU".
                   88  SV-CUST-ROW               VALUE "ROW".
               10  SV-CUST-COUNTRY     PIC X(2).
               10  SV-CUST-VAT-REG-NO  PIC X(15).
               10  SV-CUST-VAT-CHECKED PIC X(8).
               10  SV-CUST-IS-BUSINESS PIC X.
                   88  SV-IS-BUSINESS            VALUE "Y".
               10  SV-CUST-DRC-ELIG    PIC X.
                   88  SV-DRC-CUSTOMER           VALUE "Y".
               10  SV-CUST-EORI        PIC X(17).
      *>       ---- the destination and the item ----
               10  SV-DELIVER-LOCATION PIC X(3).
               10  SV-DELIVER-COUNTRY  PIC X(2).
               10  SV-PLACE-OF-SUPPLY  PIC X(3).
               10  SV-ITEM-VAT-CODE    PIC X(4).
               10  SV-ITEM-DRC-GOODS   PIC X.
                   88  SV-IS-DRC-GOODS           VALUE "Y".
               10  SV-ITEM-VAT-SOURCE  PIC X(12).
               10  SV-READ-MASTER      PIC X.
                   88  SV-READ-DB                VALUE "Y".
      *>       ---- export evidence (SO-026) ----
               10  SV-PROOF-TYPE       PIC X(20).
               10  SV-PROOF-REF        PIC X(60).
               10  SV-PROOF-OBTAINED   PIC X.
                   88  SV-HAS-PROOF              VALUE "Y".
               10  SV-PROOF-DATE       PIC 9(8).
               10  SV-PROOF-DEADLINE   PIC 9(8).
               10  SV-EVIDENCE-MONTHS  PIC S9(4) SIGN LEADING SEPARATE.
               10  SV-EXPORT-DECL-REF  PIC X(40).
               10  SV-INCOTERMS-RULE   PIC X(3).
      *>       ---- an authorised override ----
               10  SV-OVERRIDE-CODE    PIC X(4).
               10  SV-OVERRIDE-REASON  PIC X(200).
               10  SV-OVERRIDE-APPROVL PIC X(20).
               10  SV-OVERRIDE-USER    PIC X(20).
               10  SV-OVERRIDE-APPLIED PIC X.
                   88  SV-WAS-OVERRIDDEN         VALUE "Y".
      *>       ---- the decision ----
               10  SV-VAT-CODE         PIC X(4).
               10  SV-VAT-RATE-PCT     PIC S9(5)V9(4).
               10  SV-VAT-AMOUNT       PIC S9(13)V9(2).
               10  SV-LIABILITY        PIC X(12).
               10  SV-MECHANISM        PIC X(12).
               10  SV-SELF-ACCOUNTS    PIC X.
                   88  SV-CUSTOMER-ACCOUNTS      VALUE "Y".
               10  SV-TREATMENT-BASIS  PIC X(20).
               10  SV-INVOICE-WORDING  PIC X(200).
               10  SV-EVIDENCE-REQD    PIC X(100).
               10  SV-EVIDENCE-STATUS  PIC X(10).
                   88  SV-EV-NA                  VALUE "NA".
                   88  SV-EV-PENDING             VALUE "PENDING".
                   88  SV-EV-OBTAINED            VALUE "OBTAINED".
                   88  SV-EV-OVERDUE             VALUE "OVERDUE".
                   88  SV-EV-FAILED              VALUE "FAILED".
               10  SV-BLOCK-INVOICE    PIC X.
                   88  SV-IS-BLOCKED             VALUE "Y".
               10  SV-BLOCK-REASON     PIC X(8).
               10  SV-BLOCK-TEXT       PIC X(200).
               10  SV-DRC-DEMINIMIS    PIC S9(11)V9(2).
               10  SV-DRC-APPLIES      PIC X.
               10  SV-RULE-CNT         PIC 9(2).
               10  SV-RULE OCCURS 10 TIMES.
                   15  SV-R-SEQ        PIC 9(2).
                   15  SV-R-TEXT       PIC X(90).
