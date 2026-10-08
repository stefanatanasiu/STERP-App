      *>================================================================
      *> SOROTCA  - COMMAREA FOR SOROT, TERMS OF SALE AND RETENTION OF
      *>            TITLE
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : SALES ORDER MANAGEMENT
      *>
      *> AN ORDER RECORDS THE VERSION OF THE TERMS AND CONDITIONS OF
      *> SALE THAT APPLIES TO IT, INCLUDING WHETHER RETENTION OF TITLE
      *> APPLIES.  WHEN A CUSTOMER STOPS PAYING, THE COMPANY MUST BE
      *> ABLE TO SAY EXACTLY WHICH GOODS WERE SUPPLIED UNDER THOSE
      *> TERMS AND REMAIN UNPAID (SO-029, AC-124).
      *>
      *> CA-FUNCTION VALUES
      *>   TERMS      RECORD OR READ BACK THE TERMS AND CONDITIONS
      *>              VERSION AND THE RETENTION OF TITLE FLAG FOR ONE
      *>              SALES ORDER.
      *>   UNPAIDLST  LIST, FOR ONE CUSTOMER, THE GOODS SUPPLIED UNDER
      *>              RETENTION OF TITLE THAT REMAIN UNPAID.  EACH
      *>              ENTRY REPRODUCES THE ITEM, THE LOT OR SERIAL
      *>              NUMBER, THE INVOICE NUMBER, THE INVOICE DATE AND
      *>              THE DESPATCH DATE, WITH THE UNPAID AMOUNT.
      *>              ANYTHING ALREADY SETTLED IS EXCLUDED.
      *>
      *> THE LIST JOINS so.Despatch AND so.DespatchLine TO THE SALES
      *> LEDGER TABLES ar.SalesInvoice AND ar.SalesInvoiceLine, AND
      *> TESTS SETTLEMENT THROUGH ar.OpenItem.SettlementStatus AND
      *> ResidualAmountGBP.  THE SALES LEDGER IS READ, NEVER WRITTEN.
      *>
      *> REASON CODES
      *>   ROT0001 CUSTOMER CODE MISSING
      *>   ROT0002 ORDER NUMBER MISSING OR NOT KNOWN
      *>   ROT0003 TERMS AND CONDITIONS VERSION MISSING
      *>   ROT0004 NO GOODS ARE HELD UNDER RETENTION OF TITLE
      *>   ROT0005 THE LIST WAS TRUNCATED - NARROW THE ENQUIRY
      *>   ROT0006 UNSUPPORTED FUNCTION
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1993-10-11 RJH  CR0197 ROMALPA CLAUSE - GOODS NOT PAID FOR
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT YEARS ON INVOICE DATES
      *> 2026-10-06 STERP SO-029 UNPAID GOODS UNDER RETENTION OF TITLE
      *>================================================================
      *> REQ: SO-029
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  RO-BODY.
               10  RO-ORDER-NUMBER     PIC X(30).
               10  RO-CUSTOMER         PIC X(20).
               10  RO-TERMS-CODE       PIC X(12).
               10  RO-TERMS-VERSION    PIC X(20).
               10  RO-TERMS-SNAPSHOT   PIC X(100).
               10  RO-ROT-APPLIES      PIC X.
                   88  RO-ROT-YES                VALUE "Y".
                   88  RO-ROT-NO                 VALUE "N".
               10  RO-ROT-CLAUSE       PIC X(200).
               10  RO-AS-AT-DATE       PIC 9(8).
               10  RO-ORDER-FOUND      PIC X.
               10  RO-UPDATED          PIC X.
               10  RO-AUDIT-ID         PIC S9(9) COMP-5.
               10  RO-TOTAL-UNPAID-GBP PIC S9(11)V99 COMP-3.
               10  RO-TOTAL-ROT-GBP    PIC S9(11)V99 COMP-3.
               10  RO-TOTAL-ROT-QTY    PIC S9(13)V9(6) COMP-3.
               10  RO-INVOICE-CNT      PIC S9(4) SIGN LEADING SEPARATE.
               10  RO-ITEM-CNT         PIC 9(4).
               10  RO-TRUNCATED        PIC X.
               10  RO-ITEM-TAB.
                   15  RO-ITEMS        OCCURS 80 TIMES.
                       20  RO-I-ITEM       PIC X(40).
                       20  RO-I-DESC       PIC X(60).
                       20  RO-I-LOT        PIC X(30).
                       20  RO-I-SERIAL     PIC X(40).
                       20  RO-I-QTY        PIC S9(9)V9(6) COMP-3.
                       20  RO-I-UOM        PIC X(10).
                       20  RO-I-INVOICE-NO PIC X(30).
                       20  RO-I-INV-DATE   PIC 9(8).
                       20  RO-I-DESP-REF   PIC X(30).
                       20  RO-I-DESP-DATE  PIC 9(8).
                       20  RO-I-ORDER-NO   PIC X(30).
                       20  RO-I-NET-GBP    PIC S9(11)V99 COMP-3.
                       20  RO-I-UNPAID-GBP PIC S9(11)V99 COMP-3.
                       20  RO-I-SETTLEMENT PIC X(12).
