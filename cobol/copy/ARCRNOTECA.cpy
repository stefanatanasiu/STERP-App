      *>================================================================
      *> ARCRNOTECA - COMMAREA FOR ARCRNOTE, SALES CREDIT NOTES
      *>   CA-FUNCTION: CHECK     remaining creditable on the original
      *>                POST      post the credit note
      *>   A financial credit neither edits the original invoice nor
      *>   returns stock (AR-006).  Lines quote the original line.
      *>   REQ: AR-006, AR-012, AR-020, AR-023
      *> 1991-05-02 ORIGINAL SALES LEDGER PORT (MVS)
      *> 2026-10-05 STERP REMAINING CREDITABLE CONTROL
      *>================================================================
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  AN-BODY.
               10  AN-CREDIT-NO        PIC X(30).
               10  AN-ORIG-INVOICE     PIC X(30).
               10  AN-CUSTOMER         PIC X(20).
               10  AN-CREDIT-TYPE      PIC X(10).
                   88  AN-LINE-CREDIT            VALUE "LINE".
                   88  AN-VALUE-CREDIT           VALUE "VALUE".
                   88  AN-GOODWILL               VALUE "GOODWILL".
               10  AN-REASON-CODE      PIC X(20).
               10  AN-REASON           PIC X(200).
               10  AN-DOC-DATE         PIC 9(8).
               10  AN-TAX-POINT        PIC 9(8).
               10  AN-POSTING-DATE     PIC 9(8).
               10  AN-CURRENCY         PIC X(3).
               10  AN-FX-RATE          PIC S9(9)V9(8) COMP-3.
               10  AN-RETURN-STOCK     PIC X.
               10  AN-APPROVAL-ID      PIC S9(9) COMP-5.
               10  AN-APPROVED-BY      PIC X(20).
      *>       control values read from the original document
               10  AN-ORIG-NET-GBP     PIC S9(11)V99 COMP-3.
               10  AN-ORIG-VAT-GBP     PIC S9(11)V99 COMP-3.
               10  AN-ORIG-GROSS-GBP   PIC S9(11)V99 COMP-3.
               10  AN-PRIOR-CREDIT     PIC S9(11)V99 COMP-3.
               10  AN-CREDITABLE-REM   PIC S9(11)V99 COMP-3.
               10  AN-ORIG-INVOICE-ID  PIC S9(9) COMP-5.
      *>       the credit itself
               10  AN-NET              PIC S9(11)V99 COMP-3.
               10  AN-VAT              PIC S9(11)V99 COMP-3.
               10  AN-GROSS            PIC S9(11)V99 COMP-3.
               10  AN-NET-GBP          PIC S9(11)V99 COMP-3.
               10  AN-VAT-GBP          PIC S9(11)V99 COMP-3.
               10  AN-GROSS-GBP        PIC S9(11)V99 COMP-3.
               10  AN-LINE-CNT         PIC 9(3).
               10  AN-LINE-TAB.
                   15  AN-LINES        OCCURS 20 TIMES.
                       20  AN-L-ORIG-LINE      PIC S9(9) COMP-5.
                       20  AN-L-ITEM           PIC X(40).
                       20  AN-L-DESC           PIC X(60).
                       20  AN-L-QTY            PIC S9(9)V9(6) COMP-3.
                       20  AN-L-NET            PIC S9(11)V99 COMP-3.
                       20  AN-L-VAT-CODE       PIC X(4).
                       20  AN-L-VAT-RATE       PIC S9(3)V9(4) COMP-3.
                       20  AN-L-VAT-AMT        PIC S9(11)V99 COMP-3.
                       20  AN-L-REV-ACCOUNT    PIC X(12).
                       20  AN-L-ORIG-NET       PIC S9(11)V99 COMP-3.
                       20  AN-L-ORIG-CREDIT    PIC S9(11)V99 COMP-3.
                       20  AN-L-REMAINING      PIC S9(11)V99 COMP-3.
                       20  AN-L-MESSAGE        PIC X(80).
               10  AN-CREDIT-ID        PIC S9(9) COMP-5.
               10  AN-OPEN-ITEM-ID     PIC S9(9) COMP-5.
               10  AN-JOURNAL-NO       PIC X(30).
               10  FILLER              PIC X(40).
