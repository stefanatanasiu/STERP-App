      *>================================================================
      *> SOAVAILCA - COMMAREA for SOAVAIL, sales availability and the
      *>             promise enquiry.
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : SALES ORDER MANAGEMENT (DISTRIBUTION)
      *>
      *> SO-010.  The answer is never one number.  Four different
      *> things are reported, each on its own labelled line with the
      *> source document and the date it becomes usable:
      *>
      *>   FREESTOCK  physical free stock here and now.  Only this
      *>              kind carries SA-L-GUARANTEED = "Y".
      *>   RESERVED   eligible stock already promised to other demand
      *>   EXPECTED   confirmed purchase or transfer receipts not yet
      *>              received - a supplier promise, not our stock
      *>   PLANNED    planned assembly output
      *>   KITBUILD   a trial kit / assembly result, feasible only
      *>   SHORTFALL  the quantity nothing above can cover
      *>
      *> An expected receipt and a trial kit are forecasts.  They are
      *> offered so the salesperson can promise honestly, and they are
      *> flagged SA-L-GUARANTEED = "N" with the fact said in words in
      *> SA-L-TEXT.  Nothing in this COMMAREA reserves anything: a
      *> reservation is SORESV's job and it is the only thing that can
      *> stop another order taking the same units.
      *>
      *> CA-FUNCTION values
      *>   ENQUIRE   one item at one site - the full labelled picture
      *>   PROMISE   the earliest feasible date on which
      *>             SA-QTY-REQUIRED can be met, built from free stock
      *>             first and then labelled expected receipts
      *>   ORDER     one summary line per order line of
      *>             SA-ORDER-NUMBER
      *>
      *> REASON CODES
      *>   SAV0001 item and company are required
      *>   SAV0002 unsupported function
      *>   SAV0003 availability could not be obtained from AVAILQ
      *>   SAV0004 order not known in this company
      *>   SAV0005 the order has more lines than the table can carry
      *>   SAV0006 a required quantity is needed to promise
      *>   SAV0007 no feasible promise inside the horizon
      *>   SAV0008 the picture is incomplete - treat it as indicative
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-05-11 RJH  CR0931 ORDER PROMISING (ACUCOBOL DISTRIBUTION)
      *> 1999-11-30 KMP  Y2K - PROMISE DATES HOLD FOUR DIGIT YEARS
      *> 2003-02-18 DJM  CR1101 EXPECTED RECEIPTS SHOWN SEPARATELY
      *>                 AFTER THE RDG DOUBLE PROMISE INCIDENT
      *> 2026-10-06 STERP SO-010 LABELLED SOURCES AND GUARANTEE FLAG
      *>================================================================
      *> REQ: SO-010
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  SA-BODY.
               10  SA-ITEM-CODE        PIC X(40).
               10  SA-REVISION         PIC X(4).
               10  SA-SITE             PIC X(10).
               10  SA-WAREHOUSE        PIC X(20).
               10  SA-CUSTOMER         PIC X(20).
               10  SA-ORDER-NUMBER     PIC X(30).
               10  SA-LINE-NUM         PIC S9(9) COMP-5.
               10  SA-SCHEDULE-NO      PIC S9(9) COMP-5.
               10  SA-UOM-CODE         PIC X(10).
               10  SA-STOCK-UOM        PIC X(10).
               10  SA-QTY-REQUIRED     PIC S9(13)V9(6).
               10  SA-REQUIRED-DATE    PIC 9(8).
               10  SA-AS-AT            PIC 9(8).
               10  SA-HORIZON-DAYS     PIC S9(5).
               10  SA-WITH-RECEIPTS    PIC X.
                   88  SA-USE-RECEIPTS           VALUE "Y".
               10  SA-WITH-KIT         PIC X.
                   88  SA-USE-KIT                VALUE "Y".
      *>       the stock picture, straight from AVAILQ (ITM-012)
               10  SA-QTY-PHYSICAL     PIC S9(13)V9(6).
               10  SA-QTY-ELIGIBLE     PIC S9(13)V9(6).
               10  SA-QTY-RESERVED     PIC S9(13)V9(6).
               10  SA-QTY-FREE         PIC S9(13)V9(6).
               10  SA-QTY-OWN-DEMAND   PIC S9(13)V9(6).
               10  SA-QTY-EXPECTED     PIC S9(13)V9(6).
               10  SA-QTY-PLANNED      PIC S9(13)V9(6).
               10  SA-QTY-KITABLE      PIC S9(13)V9(6).
               10  SA-QTY-QUARANTINE   PIC S9(13)V9(6).
               10  SA-QTY-EXPIRED      PIC S9(13)V9(6).
               10  SA-QTY-REJECTED     PIC S9(13)V9(6).
               10  SA-QTY-THIRDPARTY   PIC S9(13)V9(6).
               10  SA-EXPECTED-DATE    PIC 9(8).
      *>       the promise.  SA-QTY-GUARANTEED is the part that is
      *>       physically here; the rest depends on a labelled source.
               10  SA-QTY-GUARANTEED   PIC S9(13)V9(6).
               10  SA-QTY-PROMISED     PIC S9(13)V9(6).
               10  SA-QTY-SHORTFALL    PIC S9(13)V9(6).
               10  SA-PROMISE-DATE     PIC 9(8).
               10  SA-PROMISE-BASIS    PIC X(12).
                   88  SA-FROM-STOCK             VALUE "FREESTOCK".
                   88  SA-FROM-RECEIPT           VALUE "EXPECTED".
                   88  SA-FROM-KIT               VALUE "KITBUILD".
                   88  SA-NO-PROMISE             VALUE "NONE".
               10  SA-PROMISE-FULL     PIC X.
                   88  SA-IS-FULL                VALUE "Y".
               10  SA-GUARANTEED       PIC X.
                   88  SA-IS-GUARANTEED          VALUE "Y".
               10  SA-DATA-COMPLETE    PIC X.
               10  SA-PROMISE-TEXT     PIC X(200).
      *>       ORDER - one row per sales order line
               10  SA-ORDER-LINE-CNT   PIC 9(3).
               10  SA-ORDER-LINES OCCURS 20 TIMES.
                   15  SA-O-LINE-NUM   PIC S9(9) COMP-5.
                   15  SA-O-ITEM       PIC X(40).
                   15  SA-O-REVISION   PIC X(4).
                   15  SA-O-SITE       PIC X(10).
                   15  SA-O-REQUIRED   PIC S9(13)V9(6).
                   15  SA-O-FREE       PIC S9(13)V9(6).
                   15  SA-O-RESERVED   PIC S9(13)V9(6).
                   15  SA-O-EXPECTED   PIC S9(13)V9(6).
                   15  SA-O-KITABLE    PIC S9(13)V9(6).
                   15  SA-O-SHORT      PIC S9(13)V9(6).
                   15  SA-O-PROMISE    PIC 9(8).
                   15  SA-O-GUARANTEED PIC X.
                   15  SA-O-STATUS     PIC X(12).
      *>       the labelled supply lines - SA-L-KIND says what each is
               10  SA-LINE-CNT         PIC 9(3).
               10  SA-LINES OCCURS 40 TIMES.
                   15  SA-L-SEQ        PIC S9(9) COMP-5.
                   15  SA-L-KIND       PIC X(12).
                   15  SA-L-LABEL      PIC X(30).
                   15  SA-L-ITEM       PIC X(40).
                   15  SA-L-SITE       PIC X(10).
                   15  SA-L-WHSE       PIC X(20).
                   15  SA-L-QTY        PIC S9(13)V9(6).
                   15  SA-L-DATE       PIC 9(8).
                   15  SA-L-DOC-TYPE   PIC X(20).
                   15  SA-L-DOC-REF    PIC X(40).
                   15  SA-L-DOC-LINE   PIC S9(9) COMP-5.
                   15  SA-L-GUARANTEED PIC X.
                       88  SA-L-IS-GUARANTEED    VALUE "Y".
                   15  SA-L-TEXT       PIC X(100).
