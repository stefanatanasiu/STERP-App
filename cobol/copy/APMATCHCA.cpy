      *>================================================================
      *> APMATCHCA - COMMAREA for APMATCH (three-way matching of a
      *>             purchase invoice line to the order and the eligible
      *>             receipt, with separate quantity, price, charge and
      *>             VAT tolerances).
      *> FUNCTIONS : MATCH     propose a match for one invoice line
      *>             MATCHALL  propose matches for every line held
      *>             TOLCHK    tolerance arithmetic only, no database
      *>             CONFIRM   write the accepted match rows
      *> 1991-06-20 original THREE WAY MATCH (GRN / ORDER / INVOICE)
      *> 2026-10-05 STERP separate charge and VAT tolerances (D-08)
      *>================================================================
      *> REQ: AP-006, AP-007, D-08
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  MT-BODY.
               10  MT-SUPPLIER         PIC X(20).
               10  MT-REGISTER-NUMBER  PIC X(30).
               10  MT-INVOICE-ID       PIC S9(9) COMP-5.
               10  MT-LINE-NUM         PIC 9(4).
               10  MT-ORDER-NUMBER     PIC X(30).
               10  MT-ORDER-LINE       PIC 9(5).
               10  MT-RECEIPT-REF      PIC X(30).
               10  MT-RECEIPT-LINE     PIC 9(5).
               10  MT-SERVICE-REF      PIC X(30).
               10  MT-ITEM             PIC X(40).
               10  MT-BASIS            PIC X(10).
                   88  MT-BY-QTY                 VALUE "QUANTITY".
                   88  MT-BY-VALUE               VALUE "VALUE".
                   88  MT-BY-MILESTONE           VALUE "MILESTONE".
               10  MT-INV-QTY          PIC S9(11)V9(6) COMP-3.
               10  MT-INV-UOM          PIC X(10).
               10  MT-INV-CONV         PIC S9(9)V9(6) COMP-3.
               10  MT-INV-STOCK-QTY    PIC S9(11)V9(6) COMP-3.
               10  MT-INV-PRICE        PIC S9(11)V9(6) COMP-3.
               10  MT-INV-PRICE-PER    PIC S9(9)V9(6) COMP-3.
               10  MT-INV-NET          PIC S9(11)V99 COMP-3.
               10  MT-INV-CHARGE       PIC S9(11)V99 COMP-3.
               10  MT-INV-VAT          PIC S9(11)V99 COMP-3.
               10  MT-INV-VATRATE      PIC S9(5)V9(4) COMP-3.
               10  MT-ORD-QTY          PIC S9(11)V9(6) COMP-3.
               10  MT-ORD-PRICE        PIC S9(11)V9(6) COMP-3.
               10  MT-ORD-PRICE-PER    PIC S9(9)V9(6) COMP-3.
               10  MT-ORD-CONV         PIC S9(9)V9(6) COMP-3.
               10  MT-ORD-CHARGE       PIC S9(11)V99 COMP-3.
               10  MT-RCV-ACCEPTED     PIC S9(11)V9(6) COMP-3.
               10  MT-RCV-QUARANTINE   PIC S9(11)V9(6) COMP-3.
               10  MT-RCV-RETURNED     PIC S9(11)V9(6) COMP-3.
               10  MT-PREV-MATCHED     PIC S9(11)V9(6) COMP-3.
               10  MT-AVAILABLE-QTY    PIC S9(11)V9(6) COMP-3.
               10  MT-MATCHED-QTY      PIC S9(11)V9(6) COMP-3.
               10  MT-UNMATCHED-QTY    PIC S9(11)V9(6) COMP-3.
               10  MT-MATCHED-VALUE    PIC S9(13)V99 COMP-3.
               10  MT-GRNI-CLEARED     PIC S9(13)V99 COMP-3.
               10  MT-QTY-VARIANCE     PIC S9(11)V9(6) COMP-3.
               10  MT-PRICE-VAR-UNIT   PIC S9(11)V9(6) COMP-3.
               10  MT-PRICE-VAR-PCT    PIC S9(7)V9(4) COMP-3.
               10  MT-PRICE-VAR-GBP    PIC S9(13)V99 COMP-3.
               10  MT-CHARGE-VAR-GBP   PIC S9(13)V99 COMP-3.
               10  MT-VAT-VAR-GBP      PIC S9(13)V99 COMP-3.
               10  MT-ROUNDING-GBP     PIC S9(13)V99 COMP-3.
               10  MT-TOL-QTY-PCT      PIC S9(5)V9(4) COMP-3.
               10  MT-TOL-PRICE-PCT    PIC S9(5)V9(4) COMP-3.
               10  MT-TOL-PRICE-UNIT   PIC S9(11)V9(6) COMP-3.
               10  MT-TOL-PRICE-GBP    PIC S9(11)V99 COMP-3.
               10  MT-TOL-CHARGE-GBP   PIC S9(11)V99 COMP-3.
               10  MT-TOL-VAT-GBP      PIC S9(11)V99 COMP-3.
               10  MT-TOL-ROUND-GBP    PIC S9(11)V99 COMP-3.
               10  MT-QTY-OK           PIC X.
               10  MT-PRICE-OK         PIC X.
               10  MT-CHARGE-OK        PIC X.
               10  MT-VAT-OK           PIC X.
               10  MT-ZERO-REFERENCE   PIC X.
               10  MT-WITHIN-TOLERANCE PIC X.
               10  MT-NEEDS-APPROVAL   PIC X.
               10  MT-MATCH-STATUS     PIC X(12).
                   88  MT-PROPOSED               VALUE "PROPOSED".
                   88  MT-MATCHED                VALUE "MATCHED".
                   88  MT-DISPUTED               VALUE "DISPUTED".
                   88  MT-REVERSED               VALUE "REVERSED".
               10  MT-MATCH-ID         PIC S9(9) COMP-5.
               10  MT-INVOICE-STATE    PIC X(14).
               10  MT-LINES-MATCHED    PIC 9(4).
               10  MT-LINES-VARIANCE   PIC 9(4).
               10  MT-LINES-UNMATCHED  PIC 9(4).
               10  MT-TOLERANCE-NOTE   PIC X(200).
               10  MT-NOTE             PIC X(200).
