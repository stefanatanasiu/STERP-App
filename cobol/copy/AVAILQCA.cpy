      *>================================================================
      *> AVAILQCA - COMMAREA for AVAILQ, the stock enquiry and
      *>            availability service.
      *> SYSTEM   : STERP - Stefan's ERP
      *> MODULE   : ITEM AND PART MAINTENANCE (STOCK CONTROL)
      *>
      *> Sales, kitting and planning all ask AVAILQ "how much may I
      *> use?".  The quantities follow section 6 of the specification
      *> and are never double counted:
      *>
      *>   physical   owned on hand in scope, every quality condition
      *>   eligible   owned on hand satisfying quality, expiry, site,
      *>              revision and location availability
      *>   reserved   active reservations against the eligible stock
      *>   free       eligible - reserved
      *>   expected   confirmed supply not yet received (purchase and
      *>              transfer receipts) - reported separately
      *>   planned    planned assembly output - reported separately
      *>
      *> Stock reserved to AV-DEMAND-REF itself is reported in
      *> AV-QTY-OWN-DEMAND and added back into AV-QTY-FOR-DEMAND, but
      *> never into the free quantity offered to another demand.
      *> Safety stock is a planning target, so AV-QTY-FREE ignores it
      *> and AV-QTY-POLICY shows the protected view as well.
      *>
      *> CA-FUNCTION values
      *>   AVAIL     the quantity summary for the scope
      *>   ENQUIRE   AVAIL plus one line per balance bucket
      *>   BYLOC     one line per location, owned stock only
      *>   LOTS      one line per lot with expiry and condition
      *>
      *> Blank scope fields mean "all".  AV-CONDITION filters a single
      *> quality status; AV-OWNER-TYPE defaults to OWNED, so customer
      *> and supplier owned material in our custody is never counted as
      *> ordinary available stock (ITM-012).
      *>
      *> REASON CODES
      *>   AVL0001 item not known in this company
      *>   AVL0002 unsupported function
      *>   AVL0003 more buckets exist than the table can carry
      *>   AVL0004 expiry controlled item has stock without an expiry
      *>   AVL0005 as-at date is not a valid date
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1996-06-03 RJH  CR0431 FREE STOCK ENQUIRY
      *> 2001-03-19 KMP  CR1022 THIRD PARTY OWNED MATERIAL SEPARATED
      *> 2026-10-05 STERP ITM-012, ITM-013 DEFINITIONS
      *>================================================================
      *> REQ: ITM-012, ITM-013
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  AV-BODY.
               10  AV-ITEM-CODE        PIC X(40).
               10  AV-REVISION         PIC X(4).
               10  AV-SITE             PIC X(10).
               10  AV-WAREHOUSE        PIC X(20).
               10  AV-LOCATION         PIC X(20).
               10  AV-LOT-CODE         PIC X(30).
               10  AV-SERIAL-NO        PIC X(40).
               10  AV-OWNER-TYPE       PIC X(10).
               10  AV-OWNER-CODE       PIC X(20).
               10  AV-CONDITION        PIC X(12).
               10  AV-AS-AT            PIC 9(8).
               10  AV-DEMAND-TYPE      PIC X(12).
               10  AV-DEMAND-REF       PIC X(40).
               10  AV-DEMAND-LINE      PIC S9(9) COMP-5.
               10  AV-STOCK-UOM        PIC X(10).
               10  AV-QTY-PHYSICAL     PIC S9(13)V9(6).
               10  AV-QTY-ELIGIBLE     PIC S9(13)V9(6).
               10  AV-QTY-RESERVED     PIC S9(13)V9(6).
               10  AV-QTY-FREE         PIC S9(13)V9(6).
               10  AV-QTY-EXPECTED     PIC S9(13)V9(6).
               10  AV-QTY-PLANNED      PIC S9(13)V9(6).
               10  AV-QTY-QUARANTINE   PIC S9(13)V9(6).
               10  AV-QTY-INSPECTION   PIC S9(13)V9(6).
               10  AV-QTY-REJECTED     PIC S9(13)V9(6).
               10  AV-QTY-EXPIRED      PIC S9(13)V9(6).
               10  AV-QTY-INTRANSIT    PIC S9(13)V9(6).
               10  AV-QTY-THIRDPARTY   PIC S9(13)V9(6).
               10  AV-QTY-UNAVAIL-LOC  PIC S9(13)V9(6).
               10  AV-QTY-OWN-DEMAND   PIC S9(13)V9(6).
               10  AV-QTY-FOR-DEMAND   PIC S9(13)V9(6).
               10  AV-QTY-SAFETY       PIC S9(13)V9(6).
               10  AV-QTY-POLICY       PIC S9(13)V9(6).
               10  AV-EXPECTED-DATE    PIC 9(8).
               10  AV-DATA-COMPLETE    PIC X.
                   88  AV-IS-COMPLETE            VALUE "Y".
               10  AV-RESTRICTED       PIC X.
               10  AV-LINE-CNT         PIC 9(3).
               10  AV-LINES OCCURS 40 TIMES.
                   15  AV-L-SITE       PIC X(10).
                   15  AV-L-WAREHOUSE  PIC X(20).
                   15  AV-L-LOCATION   PIC X(20).
                   15  AV-L-LOC-TYPE   PIC X(12).
                   15  AV-L-REVISION   PIC X(4).
                   15  AV-L-LOT        PIC X(30).
                   15  AV-L-SERIAL     PIC X(40).
                   15  AV-L-OWNER-TYPE PIC X(10).
                   15  AV-L-OWNER      PIC X(20).
                   15  AV-L-CONDITION  PIC X(12).
                   15  AV-L-QTY        PIC S9(13)V9(6).
                   15  AV-L-RESERVED   PIC S9(13)V9(6).
                   15  AV-L-FREE       PIC S9(13)V9(6).
                   15  AV-L-EXPIRY     PIC 9(8).
                   15  AV-L-ELIGIBLE   PIC X.
                   15  AV-L-WHY        PIC X(30).
