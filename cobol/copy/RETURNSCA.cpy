      *>================================================================
      *> RETURNSCA - COMMAREA FOR RETURNS, CUSTOMER RETURN AUTHORISATION
      *> SYSTEM    : STERP - Stefan's ERP
      *> MODULE    : SALES ORDER MANAGEMENT
      *>
      *> A RETURN AUTHORISATION IS NEITHER A RECEIPT NOR A CREDIT.
      *> IT RESERVES A REFERENCE AND A DECISION; IT MOVES NO STOCK AND
      *> IT RAISES NO MONEY (SO-017).  THE GOODS BECOME A RECEIPT ONLY
      *> AT RECEIVE, AND THE MONEY ONLY WHEN SOCREDIT IS ASKED FOR A
      *> CREDIT NOTE AFTER INSPECTION (SO-018, D-12).
      *>
      *> CA-FUNCTION VALUES
      *>   REQUEST    REGISTER A REQUEST.  THE WINDOW TEST OF D-12 IS
      *>              APPLIED HERE: 30 DAYS FROM DELIVERY FOR NON
      *>              FAULTY GOODS, THE WARRANTY PERIOD (DEFAULT 12
      *>              MONTHS, PARAMETER SO.RETURN.WARRANTY.MONTHS) FOR
      *>              FAULTY GOODS.  OUTSIDE THE WINDOW THE REQUEST IS
      *>              STILL RECORDED BUT IT IS HELD (RETURN CODE 06,
      *>              REASON RET0004) UNTIL AN APPROVAL IS SUPPLIED.
      *>   AUTHORISE  AUTHORISE THE AUTHORISED QUANTITIES.  STILL NO
      *>              STOCK AND NO CREDIT.
      *>   RECEIVE    RECORD THE PHYSICAL RECEIPT.  THE GOODS GO TO A
      *>              QUARANTINE / HOLD CONDITION THROUGH STKMOVE AND
      *>              ARE UNAVAILABLE UNTIL A PERMITTED DISPOSITION.
      *>   INSPECT    RECORD THE INSPECTION AND THE DISPOSITION OF EACH
      *>              LINE, DUE WITHIN FIVE WORKING DAYS OF RECEIPT.
      *>   DISPOSE    CARRY OUT THE DISPOSITIONS.  ONLY RESTOCK RETURNS
      *>              GOODS TO SALEABLE STOCK, AT THE ORIGINAL UNIT
      *>              COST, WITH ANY DIFFERENCE RECORDED AS A VARIANCE.
      *>   CLOSE      CLOSE THE AUTHORISATION.  THE ORIGINAL ORDER IS
      *>              NEVER REOPENED (SO-017).
      *>
      *> REASON CODES
      *>   RET0001 CUSTOMER, ORIGINAL DOCUMENT OR LINES MISSING
      *>   RET0002 RETURN REFERENCE NOT KNOWN
      *>   RET0003 FUNCTION NOT VALID FOR THE CURRENT STATUS
      *>   RET0004 OUTSIDE THE PERMITTED RETURN WINDOW (D-12)
      *>   RET0005 QUANTITY IS ZERO, NEGATIVE OR ABOVE THE AUTHORITY
      *>   RET0006 DISPOSITION NOT RECOGNISED
      *>   RET0007 THE QUARANTINE MOVEMENT WAS REFUSED BY STKMOVE
      *>   RET0008 INSPECTION IS OVERDUE (FIVE WORKING DAYS)
      *>   RET0009 UNSUPPORTED FUNCTION
      *>   RET0010 A DISPOSITION IS NEEDED BEFORE THE GOODS MAY MOVE
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1997-02-18 RJH  CR0601 SALES RETURNS AND RMA CONTROL
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT YEARS ON RETURN DATES
      *> 2004-09-22 DJM  CR1221 QUARANTINE BEFORE DISPOSITION
      *> 2026-10-06 STERP SO-017 SO-018 D-12 RETURN WINDOW AND HOLD
      *>================================================================
      *> REQ: SO-017, SO-018
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  RT-BODY.
               10  RT-RETURN-REF       PIC X(30).
               10  RT-RETURN-ID        PIC S9(17).
               10  RT-CUSTOMER         PIC X(20).
               10  RT-SITE             PIC X(10).
               10  RT-ORIG-ORDER       PIC X(30).
               10  RT-ORIG-DESPATCH    PIC X(30).
               10  RT-ORIG-INVOICE     PIC X(30).
               10  RT-DELIVERY-DATE    PIC 9(8).
               10  RT-REQUEST-DATE     PIC 9(8).
               10  RT-AUTH-DATE        PIC 9(8).
               10  RT-RECEIVED-DATE    PIC 9(8).
               10  RT-INSPECT-DATE     PIC 9(8).
               10  RT-INSPECT-DUE      PIC 9(8).
               10  RT-CLOSED-DATE      PIC 9(8).
               10  RT-REASON-CODE      PIC X(20).
               10  RT-REASON           PIC X(200).
               10  RT-IS-FAULTY        PIC X.
                   88  RT-FAULTY                 VALUE "Y".
                   88  RT-NOT-FAULTY             VALUE "N".
               10  RT-WITHIN-WINDOW    PIC X.
                   88  RT-IN-WINDOW              VALUE "Y".
               10  RT-WARRANTY-CLAIM   PIC X.
               10  RT-WINDOW-DAYS      PIC S9(4) SIGN LEADING SEPARATE.
               10  RT-WARRANTY-MONTHS  PIC S9(4) SIGN LEADING SEPARATE.
               10  RT-WINDOW-END-DATE  PIC 9(8).
               10  RT-DAYS-SINCE-DEL   PIC S9(9) COMP-5.
               10  RT-INSPECT-DAYS     PIC S9(4) SIGN LEADING SEPARATE.
               10  RT-APPROVAL-ID      PIC S9(9) COMP-5.
               10  RT-APPROVAL-NEEDED  PIC X.
               10  RT-AUTHORISED-BY    PIC X(20).
               10  RT-REPLACE-DECISION PIC X(12).
               10  RT-REPLACE-ORDER    PIC X(30).
               10  RT-CREDIT-NOTE      PIC X(30).
               10  RT-STATUS           PIC X(12).
               10  RT-WAREHOUSE        PIC X(20).
               10  RT-QUARANTINE-LOC   PIC X(20).
               10  RT-HOLD-CONDITION   PIC X(12).
               10  RT-SALEABLE-COND    PIC X(12).
      *>       evidence that the promise of SO-017 has been kept
               10  RT-STOCK-MOVED      PIC X.
               10  RT-MOVEMENT-COUNT   PIC S9(4) SIGN LEADING SEPARATE.
               10  RT-CREDIT-RAISED    PIC X.
               10  RT-ORDER-REOPENED   PIC X.
               10  RT-QTY-TO-SALEABLE  PIC S9(13)V9(6) COMP-3.
               10  RT-QTY-ON-HOLD      PIC S9(13)V9(6) COMP-3.
               10  RT-TOTAL-VALUE-GBP  PIC S9(11)V99 COMP-3.
               10  RT-TOTAL-VAR-GBP    PIC S9(11)V99 COMP-3.
               10  RT-LINE-CNT         PIC 9(3).
               10  RT-LINE-TAB.
                   15  RT-LINES        OCCURS 20 TIMES.
                       20  RT-L-LINE-NUM   PIC S9(9) COMP-5.
                       20  RT-L-ITEM       PIC X(40).
                       20  RT-L-REVISION   PIC X(4).
                       20  RT-L-LOT        PIC X(30).
                       20  RT-L-SERIAL     PIC X(40).
                       20  RT-L-UOM        PIC X(10).
                       20  RT-L-REQ-QTY    PIC S9(9)V9(6) COMP-3.
                       20  RT-L-AUTH-QTY   PIC S9(9)V9(6) COMP-3.
                       20  RT-L-RECV-QTY   PIC S9(9)V9(6) COMP-3.
                       20  RT-L-ACC-QTY    PIC S9(9)V9(6) COMP-3.
                       20  RT-L-REJ-QTY    PIC S9(9)V9(6) COMP-3.
                       20  RT-L-CRED-QTY   PIC S9(9)V9(6) COMP-3.
                       20  RT-L-DISPOSITION PIC X(12).
                       20  RT-L-ORIG-COST  PIC S9(11)V9(6) COMP-3.
                       20  RT-L-CURR-COST  PIC S9(11)V9(6) COMP-3.
                       20  RT-L-VALUE-GBP  PIC S9(11)V99 COMP-3.
                       20  RT-L-VAR-GBP    PIC S9(11)V99 COMP-3.
                       20  RT-L-CRED-PRICE PIC S9(11)V9(6) COMP-3.
                       20  RT-L-VAT-CODE   PIC X(4).
                       20  RT-L-MOVE-ID    PIC S9(17).
                       20  RT-L-HOLD-ID    PIC S9(9) COMP-5.
                       20  RT-L-SALEABLE   PIC X.
                       20  RT-L-MESSAGE    PIC X(80).
