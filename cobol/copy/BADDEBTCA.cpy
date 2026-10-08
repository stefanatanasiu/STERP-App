      *>================================================================
      *> BADDEBTCA - COMMAREA FOR BADDEBT, VAT BAD DEBT RELIEF
      *>   CA-FUNCTION: ASSESS    eligibility and the relief amount
      *>                CLAIM     claim the relief in a VAT return
      *>                REPAY     repay relief on a later receipt
      *>   Conditions: output VAT accounted and paid, the debt is at
      *>   least six months overdue from the later of supply and due
      *>   date, written off in the VAT account, not sold or factored.
      *>   Spec fixture F5: 1,200.00 invoice (1,000.00 + 200.00 VAT)
      *>   due 31/01/2027 - relief 200.00 first claimable in a return
      *>   covering 31/07/2027; a later 600.00 receipt repays 100.00.
      *>   REQ: AR-026, FIN-018
      *> 2004-01-05 KMP CR1702 BAD DEBT RELIEF SIX MONTH RULE
      *> 2026-10-05 STERP MTD BOX 4 / BOX 1 LINKAGE
      *>================================================================
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  BD-BODY.
               10  BD-CUSTOMER         PIC X(20).
               10  BD-INVOICE-NO       PIC X(30).
               10  BD-OPEN-ITEM-ID     PIC S9(9) COMP-5.
               10  BD-RELIEF-ID        PIC S9(9) COMP-5.
               10  BD-SUPPLY-DATE      PIC 9(8).
               10  BD-DUE-DATE         PIC 9(8).
               10  BD-ELIGIBLE-FROM    PIC 9(8).
               10  BD-RETURN-END-DATE  PIC 9(8).
               10  BD-VAT-PERIOD-KEY   PIC X(10).
               10  BD-WRITTEN-OFF      PIC X.
               10  BD-WRITEOFF-DATE    PIC 9(8).
               10  BD-WRITEOFF-GBP     PIC S9(11)V99 COMP-3.
               10  BD-OUTSTANDING-GBP  PIC S9(11)V99 COMP-3.
               10  BD-VAT-ACCOUNTED    PIC S9(11)V99 COMP-3.
               10  BD-VAT-PAID         PIC X.
               10  BD-SOLD-FACTORED    PIC X.
               10  BD-RELIEF-GBP       PIC S9(11)V99 COMP-3.
               10  BD-RELIEF-CLAIMED   PIC S9(11)V99 COMP-3.
               10  BD-MONTHS-REQUIRED  PIC S9(3).
               10  BD-ELIGIBLE         PIC X.
               10  BD-BLOCK-REASON     PIC X(200).
               10  BD-STATUS           PIC X(12).
               10  BD-VAT-BOX          PIC 9(2).
      *>       repayment on a later receipt
               10  BD-RECEIPT-GBP      PIC S9(11)V99 COMP-3.
               10  BD-REPAY-GBP        PIC S9(11)V99 COMP-3.
               10  BD-REPAID-TOTAL     PIC S9(11)V99 COMP-3.
               10  BD-REPAY-PERIOD     PIC X(10).
               10  BD-JOURNAL-NO       PIC X(30).
               10  FILLER              PIC X(40).
