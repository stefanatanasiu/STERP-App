      *>================================================================
      *> ARENQCA - COMMAREA FOR ARENQ, DEBTORS ENQUIRY METRICS
      *>   CA-FUNCTION: BALANCE   balance, ageing and credit position
      *>                METRICS   turnover, DSO, receipt effectiveness
      *>   Users can distinguish invoiced revenue from order value,
      *>   and DSO from days overdue for an individual invoice
      *>   (AR-022).  DSO on zero credit sales is undefined, not zero.
      *>   REQ: AR-017, AR-021, AR-022
      *> 1993-07-19 KMP CR0301 SALES LEDGER ENQUIRY SCREEN
      *> 2026-10-05 STERP DSO BASIS MADE EXPLICIT
      *>================================================================
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  EQ-BODY.
               10  EQ-CUSTOMER         PIC X(20).
               10  EQ-AS-AT            PIC 9(8).
               10  EQ-FROM-DATE        PIC 9(8).
               10  EQ-TO-DATE          PIC 9(8).
               10  EQ-CUSTOMER-NAME    PIC X(120).
               10  EQ-CURRENCY         PIC X(3).
               10  EQ-STATUS           PIC X(10).
      *>       balance and ageing, sterling, as at EQ-AS-AT
               10  EQ-BALANCE          PIC S9(11)V99 COMP-3.
               10  EQ-DEBITS           PIC S9(11)V99 COMP-3.
               10  EQ-CREDITS          PIC S9(11)V99 COMP-3.
               10  EQ-DEPOSITS         PIC S9(11)V99 COMP-3.
               10  EQ-NOT-DUE          PIC S9(11)V99 COMP-3.
               10  EQ-BUCKET-1         PIC S9(11)V99 COMP-3.
               10  EQ-BUCKET-2         PIC S9(11)V99 COMP-3.
               10  EQ-BUCKET-3         PIC S9(11)V99 COMP-3.
               10  EQ-BUCKET-4         PIC S9(11)V99 COMP-3.
               10  EQ-OVERDUE-TOTAL    PIC S9(11)V99 COMP-3.
               10  EQ-DISPUTED         PIC S9(11)V99 COMP-3.
               10  EQ-OPEN-ITEMS       PIC S9(9) COMP-5.
               10  EQ-OLDEST-DUE       PIC 9(8).
               10  EQ-WORST-DAYS       PIC S9(5).
      *>       credit position
               10  EQ-CREDIT-LIMIT     PIC S9(11)V99 COMP-3.
               10  EQ-EXPOSURE         PIC S9(11)V99 COMP-3.
               10  EQ-UTILISED-PCT     PIC S9(5)V99 COMP-3.
               10  EQ-HOLD-FLAG        PIC X.
      *>       trading metrics for the stated period
               10  EQ-TURNOVER-INV     PIC S9(11)V99 COMP-3.
               10  EQ-ORDER-VALUE      PIC S9(11)V99 COMP-3.
               10  EQ-CREDIT-SALES     PIC S9(11)V99 COMP-3.
               10  EQ-RECEIPTS-VALUE   PIC S9(11)V99 COMP-3.
               10  EQ-RECEIPT-COUNT    PIC S9(9) COMP-5.
               10  EQ-DISHONOUR-COUNT  PIC S9(9) COMP-5.
               10  EQ-DISPUTE-COUNT    PIC S9(9) COMP-5.
               10  EQ-AVG-DAYS-PAID    PIC S9(5).
               10  EQ-PERIOD-DAYS      PIC S9(5).
               10  EQ-DSO              PIC S9(7)V99 COMP-3.
               10  EQ-DSO-DEFINED      PIC X.
               10  EQ-DSO-BASIS        PIC X(60).
               10  FILLER              PIC X(40).
