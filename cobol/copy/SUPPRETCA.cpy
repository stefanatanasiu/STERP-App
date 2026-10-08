      *>================================================================
      *> SUPPRETCA - COMMAREA for SUPPRET, return of goods to the
      *>             supplier.
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : PURCHASE ORDER MANAGEMENT (DISTRIBUTION)
      *>
      *> A return is a physical despatch with an expected credit.  The
      *> two are separate facts and SUPPRET keeps them apart (PO-013):
      *>   * the goods leave stock when they are despatched, valued at
      *>     the cost they were received at;
      *>   * the credit is EXPECTED, not taken.  Until the supplier's
      *>     credit note arrives the expected credit is a chased
      *>     balance, never an assumed reduction of the liability.
      *>
      *> Before the invoice the return clears goods received not
      *> invoiced, so the ledger stops carrying a liability for goods
      *> we no longer hold.  After the invoice the liability is real
      *> and the return creates a return clearing balance instead; the
      *> supplier's credit note settles it (SR-POST-INVOICE = "Y").
      *>
      *> A return does NOT automatically reopen the purchase order.
      *> Whether the goods are to be replaced is a commercial decision
      *> and is recorded as one (SR-REPLACEMENT = "Y" plus the
      *> replacement order), so a short delivery is never silently
      *> turned back into an outstanding commitment (AC-055).
      *>
      *> CA-FUNCTION values
      *>   CREATE    record the return and its expected credit
      *>   DESPATCH  move the goods out of stock and post
      *>   CREDIT    match the supplier's credit note to the return
      *>   ENQUIRE   outstanding expected credits for a supplier
      *>
      *> REASON CODES
      *>   SRT0001 no lines supplied
      *>   SRT0002 return quantity exceeds the quantity received
      *>   SRT0003 a return must state its reason
      *>   SRT0004 despatch evidence is required before posting
      *>   SRT0005 stock movement refused
      *>   SRT0006 nominal posting refused
      *>   SRT0007 unsupported function
      *>   SRT0008 return not found or already credited
      *>   SRT0009 credit note value differs from the expected credit
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-09-14 RJH  CR0833 RETURNS TO SUPPLIER
      *> 2002-04-08 DJM  CR1056 EXPECTED CREDIT CHASING
      *> 2026-10-06 STERP PO-013 THROUGH STKMOVE AND POPOST
      *>================================================================
      *> REQ: PO-013, PO-017, PO-020
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  SR-BODY.
               10  SR-RETURN-REF       PIC X(30).
               10  SR-SUPPLIER         PIC X(20).
               10  SR-RMA-REF          PIC X(40).
               10  SR-RECEIPT-REF      PIC X(30).
               10  SR-ORDER-NUMBER     PIC X(30).
               10  SR-RETURN-DATE      PIC 9(8).
               10  SR-POSTING-DATE     PIC 9(8).
               10  SR-REASON-CODE      PIC X(20).
               10  SR-REASON           PIC X(300).
               10  SR-DESPATCH-EVID    PIC X(100).
               10  SR-WAREHOUSE        PIC X(20).
               10  SR-POST-INVOICE     PIC X.
                   88  SR-AFTER-INVOICE          VALUE "Y".
               10  SR-REPLACEMENT      PIC X.
               10  SR-REPLACE-ORDER    PIC X(30).
               10  SR-CREDIT-TERMS-DAY PIC S9(4).
               10  SR-CREDIT-NOTE-REF  PIC X(40).
               10  SR-CREDIT-VALUE-GBP PIC S9(13)V99.
               10  SR-LINE-CNT         PIC S9(4).
               10  SR-LINE-TABLE.
                   15  SR-LINE OCCURS 12 TIMES.
                       20  SR-L-NUM        PIC S9(9) COMP-5.
                       20  SR-L-RCPT-LINE  PIC S9(9) COMP-5.
                       20  SR-L-ITEM       PIC X(40).
                       20  SR-L-REVISION   PIC X(4).
                       20  SR-L-LOT        PIC X(30).
                       20  SR-L-SERIAL     PIC X(40).
                       20  SR-L-LOCATION   PIC X(20).
                       20  SR-L-CONDITION  PIC X(12).
                       20  SR-L-RECEIVED   PIC S9(13)V9(6).
                       20  SR-L-RETURNED   PIC S9(13)V9(6).
                       20  SR-L-UOM        PIC X(10).
                       20  SR-L-RCPT-COST  PIC S9(11)V9(6).
                       20  SR-L-CARRY-COST PIC S9(11)V9(6).
                       20  SR-L-WAS-ACCEPT PIC X.
      *>                 ---- answers, per line ----
                       20  SR-L-RETURN-GBP PIC S9(13)V99.
                       20  SR-L-CLEAR-GBP  PIC S9(13)V99.
                       20  SR-L-VAR-GBP    PIC S9(13)V99.
                       20  SR-L-MOVEMENT   PIC S9(17).
                       20  SR-L-OUTCOME    PIC X(10).
      *>         ---- answers, header ----
               10  SR-RETURN-ID        PIC S9(17).
               10  SR-TOTAL-GBP        PIC S9(13)V99.
               10  SR-CLEAR-TOTAL-GBP  PIC S9(13)V99.
               10  SR-VARIANCE-GBP     PIC S9(13)V99.
               10  SR-EXPECTED-CR-GBP  PIC S9(13)V99.
               10  SR-CHASE-DUE-DATE   PIC 9(8).
               10  SR-CLEARING-TARGET  PIC X(12).
               10  SR-JOURNAL-NUMBER   PIC X(30).
               10  SR-REOPENS-ORDER    PIC X.
               10  SR-STATUS           PIC X(12).
               10  SR-OUTCOME          PIC X(10).
