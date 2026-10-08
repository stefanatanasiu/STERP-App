      *>================================================================
      *> PROGRAM-ID : DESPATCH
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : DESPATCH CONFIRMATION, THE COST OF SALE AND THE
      *>              TRANSFER OF CONTROL (SO-013, SO-007, SO-022,
      *>              D-03).
      *>
      *>              CONFIRM IS ONE TRANSACTION.  INSIDE BEGN..COMT
      *>              THE CREDIT POSITION IS TESTED AGAIN, THE
      *>              QUANTITY IS CHECKED AGAINST THE PERMITTED
      *>              REMAINING DEMAND AND AGAINST PHYSICAL
      *>              AVAILABILITY, THE PERIOD IS TESTED, THE
      *>              REFERENCE IS DRAWN FROM NUMGEN, so.Despatch AND
      *>              so.DespatchLine ARE WRITTEN, OWNED STOCK IS
      *>              REDUCED THROUGH STKMOVE, THE RESERVATION IS
      *>              CONSUMED THROUGH STKRESV AND THE COST OF SALE IS
      *>              POSTED ONCE BY STAGING fin.JournalStage AND
      *>              CALLING fin.usp_PostJournal UNDER THE BUSINESS
      *>              EVENT SO.DESPATCH.COGS.  ANY FAILURE ROLLS THE
      *>              LOT BACK: A FAILED CONFIRMATION MUST NEVER LOOK
      *>              PARTLY SHIPPED.
      *>
      *>              REPEATING THE INSTRUCTION WITH THE SAME
      *>              CA-CORRELATION-ID RETURNS THE DESPATCH THAT
      *>              ALREADY EXISTS WITH RETURN CODE 04 AND CONSUMES
      *>              NO STOCK AND POSTS NO SECOND JOURNAL (COM-015).
      *>
      *>              D-03.  DOMESTIC SALES PASS CONTROL AT DESPATCH
      *>              CONFIRMATION.  EXPORTS FOLLOW THE INCOTERMS
      *>              RULE: EXW, FCA AND FOB AT HANDOVER TO THE
      *>              CARRIER; CPT, CIP, DAP, DPU AND DDP ON
      *>              DELIVERY.  GOODS LEAVING ON DELIVERED TERMS IN
      *>              THE LAST TWO WORKING DAYS OF A PERIOD ARE
      *>              FLAGGED FOR CUT OFF REVIEW.
      *> CALLED BY  : THE DESPATCH FORMS, THE WAREHOUSE WORKLIST,
      *>              SOINVGEN (ENQUIRY), THE .NET CLIENT
      *> INTERFACE  : CALL "DESPATCH" USING WS-DESPATCH-CA
      *>              COPYBOOK DESPATCHCA
      *> CALLS      : IDEMPOT, CREDCHK, PERIODCK, NUMGEN, DATECALC,
      *>              UOMCONV, AVAILQ, STKMOVE, STKRESV, AUDITLOG,
      *>              SQLIO -> fin.JournalStage / fin.usp_PostJournal
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1998-07-20 RJH  CR0951 DESPATCH CONFIRMATION (ACUCOBOL)
      *> 1999-11-30 KMP  Y2K - DESPATCH AND POSTING DATES
      *> 2004-10-11 DJM  CR1204 COGS POSTED FROM THE DESPATCH, ONCE
      *> 2021-01-04 DLS  CR3004 BREXIT - INCOTERMS CONTROL POINT
      *> 2026-10-06 STERP SO-013, SO-007, SO-022, D-03
      *> 2026-10-07 STERP CR2607 A DESPATCH FOLLOWS THE PICK, SO THE
      *>                  BIN IS DEFAULTED (2425) WHEN THE CALLER
      *>                  DOES NOT RESTATE IT
      *>================================================================
      *> REQ: SO-013, SO-007, SO-022
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DESPATCH.
       AUTHOR. R J HOLLAND.
       DATE-WRITTEN. 20/07/1998.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY IDEMPOTCA REPLACING ==DFHCOMMAREA== BY ==WS-IDM-CA==.
       COPY CREDCHKCA REPLACING ==DFHCOMMAREA== BY ==WS-CRD-CA==.
       COPY PERIODCKCA REPLACING ==DFHCOMMAREA== BY ==WS-PER-CA==.
       COPY NUMGENCA REPLACING ==DFHCOMMAREA== BY ==WS-NUM-CA==.
       COPY DATECALCCA REPLACING ==DFHCOMMAREA== BY ==WS-DTC-CA==.
       COPY UOMCONVCA REPLACING ==DFHCOMMAREA== BY ==WS-UOM-CA==.
       COPY AVAILQCA REPLACING ==DFHCOMMAREA== BY ==WS-AVQ-CA==.
       COPY STKMOVECA REPLACING ==DFHCOMMAREA== BY ==WS-SKM-CA==.
       COPY STKRESVCA REPLACING ==DFHCOMMAREA== BY ==WS-SKR-CA==.
       COPY AUDITLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-AUD-CA==.
      *>   The business event under which the attributable cost of
      *>   sale is posted.  Its legs live in fin.EventAccountMap; a
      *>   missing mapping blocks the posting rather than inventing a
      *>   suspense account (COM-014).
       01  WS-CONST.
           05  WS-COGS-EVENT           PIC X(30)
                                       VALUE "SO.DESPATCH.COGS".
           05  WS-SERIES-DESP          PIC X(20) VALUE "SDES"  .
           05  WS-SERIES-ALT           PIC X(20) VALUE "SDES".
           05  WS-OPERATION            PIC X(60)
                                       VALUE "SO.DESPATCH.CONFIRM".
       01  WS-ROW.
           05  WS-COL OCCURS 14 TIMES  PIC X(100).
       01  WS-LEG-TABLE.
           05  WS-LEG-CNT              PIC S9(4) VALUE 0.
           05  WS-LEGS OCCURS 12 TIMES.
               10  WS-LEG-CODE         PIC X(12).
               10  WS-LEG-ACCOUNT      PIC X(12).
               10  WS-LEG-DRCR         PIC X(2).
               10  WS-LEG-CC           PIC X(20).
               10  WS-LEG-PROJ         PIC X(20).
       01  WS-WORK.
           05  WS-BAD                  PIC X VALUE "N".
               88  WS-BAD-INPUT                  VALUE "Y".
           05  WS-IN-TRAN              PIC X VALUE "N".
               88  WS-TRAN-OPEN                  VALUE "Y".
           05  WS-IDEM-OPEN            PIC X VALUE "N".
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-J                    PIC S9(4) VALUE 0.
           05  WS-K                    PIC S9(4) VALUE 0.
           05  WS-DONE                 PIC X VALUE "N".
               88  WS-AT-END                     VALUE "Y".
           05  WS-QTY                  PIC S9(13)V9(6) VALUE 0.
           05  WS-QTY2                 PIC S9(13)V9(6) VALUE 0.
           05  WS-AMT                  PIC S9(11)V99 VALUE 0.
           05  WS-COST                 PIC S9(11)V9(6) VALUE 0.
           05  WS-DATE-N               PIC 9(8) VALUE 0.
           05  WS-PERIOD-END           PIC 9(8) VALUE 0.
           05  WS-CUTOFF-FROM          PIC 9(8) VALUE 0.
           05  WS-LINE-NO              PIC S9(4) VALUE 0.
           05  WS-TEXT                 PIC X(200) VALUE SPACES.
           05  WS-NARR                 PIC X(200) VALUE SPACES.
      *>   COM-005/COM-015.  A correlation id names ONE business
      *>   instruction.  The despatch instruction, each stock
      *>   movement and the cost of sale journal are three different
      *>   instructions, so each is given its own derived id.
      *>   Sharing the caller's id let inv.usp_PostStockMovement
      *>   complete the key with a movement reference, after which
      *>   fin.usp_PostJournal believed the journal had already been
      *>   posted and handed back an empty result set (DSP0007), and
      *>   the second and later lines of a despatch were not moved.
           05  WS-SUB-CORR             PIC X(36) VALUE SPACES.
           05  WS-SUB-SUFFIX           PIC X(8) VALUE SPACES.
           05  WS-SUB-SEQ              PIC 9(3) VALUE 0.
           05  WS-BASE-LEN             PIC S9(4) VALUE 0.
           05  WS-SUF-LEN              PIC S9(4) VALUE 0.
      *>   CR2607.  The bin a line leaves from, defaulted by 2425
      *>   when the caller states none.
           05  WS-BIN-SITE             PIC X(10) VALUE SPACES.
           05  WS-BIN-WHSE             PIC X(20) VALUE SPACES.
           05  WS-BIN-LOCN             PIC X(20) VALUE SPACES.
           05  WS-BIN-LOT              PIC X(30) VALUE SPACES.
           05  WS-BIN-SERIAL           PIC X(40) VALUE SPACES.
           05  WS-QTY-ED               PIC Z(9)9.9(6).
           05  WS-MONEY-ED             PIC -(11)9.99.
           05  WS-INT-ED               PIC -(9)9.
           05  WS-SIGN                 PIC S9 VALUE 1.
       LINKAGE SECTION.
       COPY DESPATCHCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       1000-MAIN.
           PERFORM 1100-INITIALISE THRU 1100-EXIT
           PERFORM 1200-VALIDATE THRU 1200-EXIT
           IF WS-BAD-INPUT
               GO TO 1000-RETURN
           END-IF
           EVALUATE CA-FUNCTION OF DFHCOMMAREA
               WHEN "CONFIRM"
                   PERFORM 2000-CONFIRM THRU 2000-EXIT
               WHEN "REVERSE"
                   PERFORM 3000-REVERSE THRU 3000-EXIT
               WHEN "ENQUIRE"
                   PERFORM 4000-ENQUIRE THRU 4000-EXIT
               WHEN OTHER
                   PERFORM 9100-BAD-FUNCTION THRU 9100-EXIT
           END-EVALUATE.
       1000-RETURN.
           IF WS-TRAN-OPEN
               PERFORM 9500-ROLLBACK THRU 9500-EXIT
           END-IF
           IF WS-IDEM-OPEN = "Y"
               PERFORM 9600-IDEM-END THRU 9600-EXIT
           END-IF
           GOBACK.

      *>----------------------------------------------------------------
       1100-INITIALISE.
           MOVE "N" TO WS-BAD WS-IN-TRAN WS-DONE
           MOVE "N" TO WS-IDEM-OPEN
           MOVE 0 TO WS-I WS-J WS-K WS-LINE-NO WS-LEG-CNT
           MOVE 0 TO WS-QTY WS-QTY2 WS-AMT WS-COST
           MOVE 0 TO WS-DATE-N WS-PERIOD-END WS-CUTOFF-FROM
           MOVE 1 TO WS-SIGN
           MOVE SPACES TO WS-TEXT WS-NARR
           MOVE "DESPATCH" TO CA-PROGRAM OF DFHCOMMAREA
           MOVE 0 TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE 0 TO CA-MSG-COUNT OF DFHCOMMAREA
           MOVE SPACES TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
           MOVE "SO-013" TO CA-REQ-REF OF DFHCOMMAREA
           MOVE WS-COGS-EVENT TO DS-COGS-EVENT
           MOVE "N" TO DS-WAS-DUPLICATE
           MOVE SPACES TO DS-IDEM-OUTCOME
           IF CA-FUNCTION OF DFHCOMMAREA = "CONFIRM"
               MOVE 0 TO DS-DESPATCH-ID DS-JOURNAL-ID
               MOVE 0 TO DS-COGS-GBP
               MOVE SPACES TO DS-JOURNAL-NO
               MOVE "N" TO DS-IS-CUTOFF
               MOVE 0 TO DS-WORKDAYS-LEFT
               MOVE SPACES TO DS-CONTROL-BASIS
               MOVE "DESPATCH" TO DS-CONTROL-POINT
           END-IF
           IF DS-DESPATCH-DATE = 0
               MOVE CA-BUSINESS-DATE OF DFHCOMMAREA
                   TO DS-DESPATCH-DATE
           END-IF
           IF DS-POSTING-DATE = 0
               MOVE DS-DESPATCH-DATE TO DS-POSTING-DATE
           END-IF
           IF DS-SITE = SPACES
               MOVE CA-SITE OF DFHCOMMAREA TO DS-SITE
           END-IF
           IF DS-IS-EXPORT NOT = "Y"
               MOVE "N" TO DS-IS-EXPORT
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 14
               MOVE 100 TO SQLIO-COL-WIDTH(WS-I)
           END-PERFORM.
       1100-EXIT.
           EXIT.

       1200-VALIDATE.
           IF CA-COMPANY OF DFHCOMMAREA = SPACES
               MOVE "DSP0001" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "A company code is required." TO WS-TEXT
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 1200-EXIT
           END-IF
           IF CA-FUNCTION OF DFHCOMMAREA = "ENQUIRE"
               IF DS-DESPATCH-REF = SPACES
                   MOVE "DSP0001" TO CA-REASON-CODE OF DFHCOMMAREA
                   MOVE "A despatch reference is required."
                       TO WS-TEXT
                   PERFORM 9000-REJECT THRU 9000-EXIT
               END-IF
               GO TO 1200-EXIT
           END-IF
           IF CA-FUNCTION OF DFHCOMMAREA = "REVERSE"
               IF DS-REVERSAL-OF = SPACES
                  OR DS-REVERSE-APPR = SPACES
                  OR DS-REVERSE-REASON = SPACES
                   MOVE "DSP0010" TO CA-REASON-CODE OF DFHCOMMAREA
                   MOVE SPACES TO WS-TEXT
                   STRING "A reversal needs the original despatch "
                          "reference, an approval reference and a "
                          "reason."
                       DELIMITED BY SIZE INTO WS-TEXT
                   PERFORM 9000-REJECT THRU 9000-EXIT
               END-IF
               GO TO 1200-EXIT
           END-IF
           IF DS-ORDER-NUMBER = SPACES
               MOVE "DSP0001" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE "An order number is required to despatch."
                   TO WS-TEXT
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 1200-EXIT
           END-IF
           IF CA-CORRELATION-ID OF DFHCOMMAREA = SPACES
               MOVE "DSP0001" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO WS-TEXT
               STRING "A correlation id is required so that a "
                      "repeated confirmation has no second effect."
                   DELIMITED BY SIZE INTO WS-TEXT
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 1200-EXIT
           END-IF
           IF DS-LINE-CNT < 1 OR DS-LINE-CNT > 30
               MOVE "DSP0013" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO WS-TEXT
               STRING "A despatch needs between one and thirty "
                      "lines."
                   DELIMITED BY SIZE INTO WS-TEXT
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       1200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000  CONFIRM.  The order of work matters: everything that can
      *>       refuse the despatch is asked before a single row is
      *>       written, and everything that writes is inside one
      *>       transaction that either commits whole or disappears.
      *>----------------------------------------------------------------
       2000-CONFIRM.
           PERFORM 2050-IDEMPOTENT-BEGIN THRU 2050-EXIT
           IF CA-FAILED OF DFHCOMMAREA OR DS-IS-DUPLICATE
               GO TO 2000-EXIT
           END-IF
           PERFORM 2100-CREDIT-CHECK THRU 2100-EXIT
           IF CA-RETURN-CODE OF DFHCOMMAREA >= 6
               GO TO 2000-EXIT
           END-IF
           PERFORM 2150-PERIOD-CHECK THRU 2150-EXIT
           IF CA-RETURN-CODE OF DFHCOMMAREA >= 6
               GO TO 2000-EXIT
           END-IF
           PERFORM 2200-CONTROL-POINT THRU 2200-EXIT
           PERFORM 2250-CUTOFF-REVIEW THRU 2250-EXIT
           PERFORM 2300-NEXT-REFERENCE THRU 2300-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 2000-EXIT
           END-IF
           PERFORM 9400-BEGIN THRU 9400-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 2000-EXIT
           END-IF
           PERFORM 2400-CHECK-LINES THRU 2400-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 2000-EXIT
           END-IF
           PERFORM 2500-WRITE-HEADER THRU 2500-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 2000-EXIT
           END-IF
           PERFORM 2550-WRITE-LINES THRU 2550-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 2000-EXIT
           END-IF
           PERFORM 2600-POST-COGS THRU 2600-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 2000-EXIT
           END-IF
           PERFORM 9450-COMMIT THRU 9450-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 2000-EXIT
           END-IF
           PERFORM 2700-AUDIT-CONTROL THRU 2700-EXIT
           MOVE "CONFIRMED" TO DS-STATUS
           MOVE "COMPLETED" TO ID-STATUS
           PERFORM 2900-CONFIRM-WORDS THRU 2900-EXIT.
       2000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2050  COM-015.  STARTED means this caller owns the
      *>       instruction.  COMPLETED means the goods already went -
      *>       answer 04 with the original despatch and touch nothing.
      *>       INPROGRESS is an unresolved outcome, which is NOT the
      *>       same as "already done" and must not be retried blindly.
      *>----------------------------------------------------------------
       2050-IDEMPOTENT-BEGIN.
           INITIALIZE WS-IDM-CA
           MOVE "BEGIN" TO CA-FUNCTION OF WS-IDM-CA
           MOVE CA-COMPANY OF DFHCOMMAREA TO CA-COMPANY OF WS-IDM-CA
           MOVE CA-SITE OF DFHCOMMAREA TO CA-SITE OF WS-IDM-CA
           MOVE CA-USER OF DFHCOMMAREA TO CA-USER OF WS-IDM-CA
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA
               TO CA-BUSINESS-DATE OF WS-IDM-CA
           MOVE CA-CORRELATION-ID OF DFHCOMMAREA
               TO CA-CORRELATION-ID OF WS-IDM-CA
           MOVE WS-OPERATION TO ID-OPERATION
           CALL "IDEMPOT" USING WS-IDM-CA
           MOVE ID-OUTCOME TO DS-IDEM-OUTCOME
           EVALUATE TRUE
               WHEN ID-STARTED
                   MOVE "Y" TO WS-IDEM-OPEN
                   MOVE "FAILED" TO ID-STATUS
               WHEN ID-COMPLETED
                   MOVE "Y" TO DS-WAS-DUPLICATE
                   MOVE ID-RESULT-REF TO DS-DESPATCH-REF
                   PERFORM 4000-ENQUIRE THRU 4000-EXIT
                   MOVE 4 TO CA-RETURN-CODE OF DFHCOMMAREA
                   MOVE "DSP0012" TO CA-REASON-CODE OF DFHCOMMAREA
                   MOVE SPACES TO WS-TEXT
                   STRING "This despatch was already confirmed as "
                          FUNCTION TRIM(DS-DESPATCH-REF)
                          "; no stock has been consumed and no "
                          "further cost of sale has been posted."
                       DELIMITED BY SIZE INTO WS-TEXT
                   MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
                   ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
               WHEN ID-INPROGRESS
                   MOVE 6 TO CA-RETURN-CODE OF DFHCOMMAREA
                   MOVE "DSP0012" TO CA-REASON-CODE OF DFHCOMMAREA
                   MOVE SPACES TO WS-TEXT
                   STRING "Another confirmation of this instruction "
                          "is running; its outcome is not yet known."
                       DELIMITED BY SIZE INTO WS-TEXT
                   MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
                   ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
               WHEN OTHER
                   MOVE 8 TO CA-RETURN-CODE OF DFHCOMMAREA
                   MOVE "DSP0012" TO CA-REASON-CODE OF DFHCOMMAREA
                   MOVE CA-MESSAGE OF WS-IDM-CA
                       TO CA-MESSAGE OF DFHCOMMAREA
           END-EVALUATE.
       2050-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2100  SO-007 and D-06.  A previously accepted order can still
      *>       be held at despatch if the customer's position has
      *>       materially worsened, so the test is made here and not
      *>       assumed from the acceptance decision.
      *>----------------------------------------------------------------
       2100-CREDIT-CHECK.
           IF DS-CUSTOMER = SPACES
               PERFORM 2110-READ-CUSTOMER THRU 2110-EXIT
               IF CA-FAILED OF DFHCOMMAREA
                   GO TO 2100-EXIT
               END-IF
           END-IF
           INITIALIZE WS-CRD-CA
           MOVE "CHECK" TO CA-FUNCTION OF WS-CRD-CA
           MOVE CA-COMPANY OF DFHCOMMAREA TO CA-COMPANY OF WS-CRD-CA
           MOVE CA-SITE OF DFHCOMMAREA TO CA-SITE OF WS-CRD-CA
           MOVE CA-USER OF DFHCOMMAREA TO CA-USER OF WS-CRD-CA
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA
               TO CA-BUSINESS-DATE OF WS-CRD-CA
           MOVE CA-CORRELATION-ID OF DFHCOMMAREA
               TO CA-CORRELATION-ID OF WS-CRD-CA
           MOVE DS-CUSTOMER TO CK-CUSTOMER
           MOVE "DESPATCH" TO CK-CHECK-POINT
           MOVE DS-ORDER-NUMBER TO CK-DOCUMENT-REF
           MOVE DS-DESPATCH-DATE TO CK-EVAL-DATE
           MOVE "GBP" TO CK-CURRENCY
           MOVE 1 TO CK-FX-RATE
           MOVE 0 TO CK-ORDER-VALUE
           MOVE 0 TO CK-ORDER-VALUE-GBP
           CALL "CREDCHK" USING WS-CRD-CA
           MOVE CK-DECISION TO DS-CREDIT-DECISION
           MOVE CK-EXPOSURE TO DS-CREDIT-EXPOSURE
           MOVE CK-EXCESS TO DS-CREDIT-EXCESS
           MOVE CK-DECISION-TEXT TO DS-CREDIT-TEXT
           MOVE CK-OVERRIDE-ID TO DS-CREDIT-OVERRIDE
           IF CA-RETURN-CODE OF WS-CRD-CA >= 12
      *>       the credit service itself is unavailable - refuse
      *>       rather than ship on an unknown credit position
               MOVE 12 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "DSP0006" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE CA-MESSAGE OF WS-CRD-CA
                   TO CA-MESSAGE OF DFHCOMMAREA
               GO TO 2100-EXIT
           END-IF
           IF CK-BLOCKED
               MOVE 8 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "DSP0006" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE CK-DECISION-TEXT TO CA-MESSAGE OF DFHCOMMAREA
               ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
               GO TO 2100-EXIT
           END-IF
           IF CK-HELD
               MOVE 6 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "DSP0006" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO WS-TEXT
               STRING "Despatch is held on credit: "
                      FUNCTION TRIM(CK-DECISION-TEXT)
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
               ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           END-IF.
       2100-EXIT.
           EXIT.

       2110-READ-CUSTOMER.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT o.CustomerCode, ISNULL(o.SiteCode,''), "
                  "ISNULL(o.IncotermsRule,''), "
                  "ISNULL(o.DeliveryAddressSnapshot,''), "
                  "o.ReleaseStatus "
                  "FROM so.SalesOrder o WHERE o.CompanyCode = ? "
                  "AND o.OrderNumber = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE DS-ORDER-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 5 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2110-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "DSP0002" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO WS-TEXT
               STRING "Order " FUNCTION TRIM(DS-ORDER-NUMBER)
                      " is not known in this company."
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
               GO TO 2110-EXIT
           END-IF
           MOVE WS-COL(1) TO DS-CUSTOMER
           IF DS-SITE = SPACES
               MOVE WS-COL(2) TO DS-SITE
           END-IF
           IF DS-INCOTERMS = SPACES
               MOVE WS-COL(3) TO DS-INCOTERMS
           END-IF
           IF DS-ADDRESS-SNAP = SPACES
               MOVE WS-COL(4) TO DS-ADDRESS-SNAP
           END-IF.
       2110-EXIT.
           EXIT.

      *>   COM-009.  A SOFT period is blocked but has an authorised
      *>   route, so it answers 06 and not 08.
       2150-PERIOD-CHECK.
           INITIALIZE WS-PER-CA
           MOVE "CHECK" TO CA-FUNCTION OF WS-PER-CA
           MOVE CA-COMPANY OF DFHCOMMAREA TO CA-COMPANY OF WS-PER-CA
           MOVE CA-USER OF DFHCOMMAREA TO CA-USER OF WS-PER-CA
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA
               TO CA-BUSINESS-DATE OF WS-PER-CA
           MOVE DS-POSTING-DATE TO PC-POSTING-DATE
           MOVE DS-DESPATCH-DATE TO PC-DOCUMENT-DATE
           MOVE DS-DESPATCH-DATE TO PC-TAX-POINT
           MOVE "POST" TO PC-ACTION
           CALL "PERIODCK" USING WS-PER-CA
           MOVE PC-FISCAL-YEAR TO DS-FISCAL-YEAR
           MOVE PC-PERIOD-NO TO DS-PERIOD-NO
           MOVE PC-PERIOD-STATUS TO DS-PERIOD-STATUS
           IF CA-RETURN-CODE OF WS-PER-CA >= 6
               MOVE CA-RETURN-CODE OF WS-PER-CA
                   TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "DSP0005" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE CA-MESSAGE OF WS-PER-CA
                   TO CA-MESSAGE OF DFHCOMMAREA
               ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           END-IF.
       2150-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2200  D-03 TRANSFER OF CONTROL.  Domestic sales pass control
      *>       when the despatch is confirmed.  An export passes it
      *>       where the Incoterms rule says, and the point is
      *>       RECORDED with its basis so revenue recognition can be
      *>       explained rather than assumed.
      *>----------------------------------------------------------------
       2200-CONTROL-POINT.
           MOVE FUNCTION UPPER-CASE(DS-INCOTERMS) TO DS-INCOTERMS
           IF DS-IS-EXPORT NOT = "Y"
               MOVE "DESPATCH" TO DS-CONTROL-POINT
               MOVE DS-DESPATCH-DATE TO DS-CONTROL-DATE
               MOVE SPACES TO WS-TEXT
               STRING "Domestic supply - control passes to the "
                      "customer at despatch confirmation (D-03)."
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO DS-CONTROL-BASIS
               GO TO 2200-EXIT
           END-IF
           EVALUATE DS-INCOTERMS
               WHEN "EXW"
               WHEN "FCA"
               WHEN "FAS"
               WHEN "FOB"
               WHEN "CFR"
               WHEN "CIF"
                   MOVE "HANDOVER" TO DS-CONTROL-POINT
                   MOVE DS-DESPATCH-DATE TO DS-CONTROL-DATE
                   MOVE SPACES TO WS-TEXT
                   STRING "Export on " FUNCTION TRIM(DS-INCOTERMS)
                          " terms - control passes at handover to "
                          "the carrier (D-03)."
                       DELIMITED BY SIZE INTO WS-TEXT
               WHEN "CPT"
               WHEN "CIP"
               WHEN "DAP"
               WHEN "DPU"
               WHEN "DDP"
                   MOVE "DELIVERY" TO DS-CONTROL-POINT
                   MOVE 0 TO DS-CONTROL-DATE
                   MOVE SPACES TO WS-TEXT
                   STRING "Export on " FUNCTION TRIM(DS-INCOTERMS)
                          " delivered terms - control passes on "
                          "delivery, not at despatch (D-03)."
                       DELIMITED BY SIZE INTO WS-TEXT
               WHEN OTHER
                   MOVE "DESPATCH" TO DS-CONTROL-POINT
                   MOVE DS-DESPATCH-DATE TO DS-CONTROL-DATE
                   MOVE SPACES TO WS-TEXT
                   STRING "No Incoterms rule is recorded for this "
                          "export; control is taken to pass at "
                          "despatch and needs review."
                       DELIMITED BY SIZE INTO WS-TEXT
                   MOVE 4 TO CA-RETURN-CODE OF DFHCOMMAREA
                   MOVE "DSP0005" TO CA-REASON-CODE OF DFHCOMMAREA
                   ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           END-EVALUATE
           MOVE WS-TEXT TO DS-CONTROL-BASIS.
       2200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2250  CUT OFF.  Goods that leave on delivered terms in the
      *>       last two WORKING days of a period have probably not
      *>       arrived by period end, so the revenue and the cost may
      *>       belong to the next period.  DATECALC owns the working
      *>       day calendar; this program does not count weekends.
      *>----------------------------------------------------------------
       2250-CUTOFF-REVIEW.
           MOVE "N" TO DS-IS-CUTOFF
           IF DS-CONTROL-POINT NOT = "DELIVERY"
               GO TO 2250-EXIT
           END-IF
           PERFORM 2260-READ-PERIOD-END THRU 2260-EXIT
           IF WS-PERIOD-END = 0
               GO TO 2250-EXIT
           END-IF
           MOVE WS-PERIOD-END TO DS-PERIOD-END-DATE
           INITIALIZE WS-DTC-CA
           MOVE "ADDBUS" TO CA-FUNCTION OF WS-DTC-CA
           MOVE CA-COMPANY OF DFHCOMMAREA TO CA-COMPANY OF WS-DTC-CA
           MOVE CA-USER OF DFHCOMMAREA TO CA-USER OF WS-DTC-CA
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA
               TO CA-BUSINESS-DATE OF WS-DTC-CA
           MOVE WS-PERIOD-END TO DC-DATE-1
           MOVE -1 TO DC-NUMBER
           MOVE "ENG" TO DC-CALENDAR
           CALL "DATECALC" USING WS-DTC-CA
           IF CA-RETURN-CODE OF WS-DTC-CA >= 8
               GO TO 2250-EXIT
           END-IF
           MOVE DC-RESULT-DATE TO WS-CUTOFF-FROM
           INITIALIZE WS-DTC-CA
           MOVE "DAYSBTWN" TO CA-FUNCTION OF WS-DTC-CA
           MOVE CA-COMPANY OF DFHCOMMAREA TO CA-COMPANY OF WS-DTC-CA
           MOVE CA-USER OF DFHCOMMAREA TO CA-USER OF WS-DTC-CA
           MOVE DS-DESPATCH-DATE TO DC-DATE-1
           MOVE WS-PERIOD-END TO DC-DATE-2
           CALL "DATECALC" USING WS-DTC-CA
           IF CA-RETURN-CODE OF WS-DTC-CA < 8
               MOVE DC-RESULT-NUM TO DS-WORKDAYS-LEFT
           END-IF
      *>   WS-CUTOFF-FROM is the penultimate working day; anything
      *>   despatched on or after it is inside the two day window
           IF DS-DESPATCH-DATE >= WS-CUTOFF-FROM
              AND DS-DESPATCH-DATE <= WS-PERIOD-END
               MOVE "Y" TO DS-IS-CUTOFF
               MOVE SPACES TO WS-TEXT
               STRING FUNCTION TRIM(DS-CONTROL-BASIS)
                      "  Despatched inside the last two working "
                      "days of the period - flagged for cut off "
                      "review."
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO DS-CONTROL-BASIS
           END-IF.
       2250-EXIT.
           EXIT.

       2260-READ-PERIOD-END.
           MOVE 0 TO WS-PERIOD-END
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(char(8), p.EndDate, 112), p.Status "
                  "FROM fin.Period p WHERE p.CompanyCode = ? AND "
                  "? BETWEEN CONVERT(char(8), p.StartDate, 112) AND "
                  "CONVERT(char(8), p.EndDate, 112)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE DS-DESPATCH-DATE TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR OR SQLIO-NOTFOUND
               MOVE 0 TO SQLIO-SQLCODE
               GO TO 2260-EXIT
           END-IF
           MOVE WS-COL(1)(1:8) TO WS-PERIOD-END.
       2260-EXIT.
           EXIT.

       2300-NEXT-REFERENCE.
           INITIALIZE WS-NUM-CA
           MOVE "NEXT" TO CA-FUNCTION OF WS-NUM-CA
           MOVE CA-COMPANY OF DFHCOMMAREA TO CA-COMPANY OF WS-NUM-CA
           MOVE CA-SITE OF DFHCOMMAREA TO CA-SITE OF WS-NUM-CA
           MOVE CA-USER OF DFHCOMMAREA TO CA-USER OF WS-NUM-CA
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA
               TO CA-BUSINESS-DATE OF WS-NUM-CA
           MOVE CA-CORRELATION-ID OF DFHCOMMAREA
               TO CA-CORRELATION-ID OF WS-NUM-CA
           MOVE WS-SERIES-DESP TO NG-SERIES-CODE
           CALL "NUMGEN" USING WS-NUM-CA
           IF CA-RETURN-CODE OF WS-NUM-CA >= 8
      *>       the SODESP series is not deployed everywhere; the 1998
      *>       distribution series SDES is the documented fall back
               MOVE WS-SERIES-ALT TO NG-SERIES-CODE
               MOVE 0 TO CA-RETURN-CODE OF WS-NUM-CA
               MOVE SPACES TO CA-MESSAGE OF WS-NUM-CA
               CALL "NUMGEN" USING WS-NUM-CA
           END-IF
           IF CA-RETURN-CODE OF WS-NUM-CA >= 8
               MOVE CA-RETURN-CODE OF WS-NUM-CA
                   TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "DSP0001" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE CA-MESSAGE OF WS-NUM-CA
                   TO CA-MESSAGE OF DFHCOMMAREA
               GO TO 2300-EXIT
           END-IF
           MOVE NG-DOCUMENT-NUMBER TO DS-DESPATCH-REF.
       2300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2400  THE TWO CEILINGS.  An actual quantity may not exceed
      *>       the permitted remaining demand
      *>         AcceptedQty - CancelledQty - DespatchedQty
      *>       nor the physical availability.  Both refusals use
      *>       DSP0004 and both happen before anything is written.
      *>----------------------------------------------------------------
       2400-CHECK-LINES.
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > DS-LINE-CNT
               IF NOT CA-FAILED OF DFHCOMMAREA
                   PERFORM 2420-ONE-LINE-CHECK THRU 2420-EXIT
               END-IF
           END-PERFORM.
       2400-EXIT.
           EXIT.

       2420-ONE-LINE-CHECK.
           MOVE SPACES TO DS-L-MESSAGE(WS-I)
           MOVE "CHECKED" TO DS-L-STATUS(WS-I)
           IF DS-L-QTY(WS-I) <= 0
               MOVE 8 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "DSP0003" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO WS-TEXT
               STRING "A despatch quantity must be greater than "
                      "zero."
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
               MOVE WS-TEXT TO DS-L-MESSAGE(WS-I)
               GO TO 2420-EXIT
           END-IF
           PERFORM 2430-READ-ORDER-LINE THRU 2430-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 2420-EXIT
           END-IF
           PERFORM 2425-DEFAULT-BIN THRU 2425-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 2420-EXIT
           END-IF
           IF DS-L-QTY(WS-I) > DS-L-ALLOWED(WS-I)
               MOVE 8 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "DSP0004" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE DS-L-ALLOWED(WS-I) TO WS-QTY-ED
               MOVE SPACES TO WS-TEXT
               STRING "Line " FUNCTION TRIM(DS-L-ITEM(WS-I))
                      " may despatch at most "
                      FUNCTION TRIM(WS-QTY-ED)
                      " - accepted less cancelled less already "
                      "despatched."
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
               MOVE WS-TEXT TO DS-L-MESSAGE(WS-I)
               MOVE "REFUSED" TO DS-L-STATUS(WS-I)
               ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
               GO TO 2420-EXIT
           END-IF
           PERFORM 2440-CONVERT-QTY THRU 2440-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 2420-EXIT
           END-IF
           PERFORM 2450-CHECK-AVAILABLE THRU 2450-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 2420-EXIT
           END-IF
           PERFORM 2460-READ-UNIT-COST THRU 2460-EXIT.
       2420-EXIT.
           EXIT.

      *>   CR2607.  SO-012/SO-013.  A despatch follows a pick; it does
      *>   not restate the bin.  When the caller leaves the warehouse
      *>   or the location of a line blank the despatch names the bin
      *>   itself: the confirmed pick line when a pick list is quoted,
      *>   otherwise the owned, accepted balance that can supply the
      *>   quantity.  The lot and the serial come from the same place,
      *>   because a traceable item may not move without them.
       2425-DEFAULT-BIN.
           IF DS-L-WHSE(WS-I) NOT = SPACES
               AND DS-L-LOCATION(WS-I) NOT = SPACES
               GO TO 2425-EXIT
           END-IF
           MOVE SPACES TO WS-BIN-SITE WS-BIN-WHSE WS-BIN-LOCN
                          WS-BIN-LOT WS-BIN-SERIAL
           IF DS-PICKLIST-REF NOT = SPACES
               PERFORM 2426-BIN-FROM-PICK THRU 2426-EXIT
               IF CA-FAILED OF DFHCOMMAREA
                   GO TO 2425-EXIT
               END-IF
           END-IF
           IF WS-BIN-WHSE = SPACES OR WS-BIN-LOCN = SPACES
               MOVE SPACES TO WS-BIN-SITE WS-BIN-WHSE WS-BIN-LOCN
                              WS-BIN-LOT WS-BIN-SERIAL
               PERFORM 2427-BIN-FROM-STOCK THRU 2427-EXIT
               IF CA-FAILED OF DFHCOMMAREA
                   GO TO 2425-EXIT
               END-IF
           END-IF
           IF DS-SITE = SPACES
               MOVE WS-BIN-SITE TO DS-SITE
           END-IF
           IF DS-L-WHSE(WS-I) = SPACES
               MOVE WS-BIN-WHSE TO DS-L-WHSE(WS-I)
           END-IF
           IF DS-L-LOCATION(WS-I) = SPACES
               MOVE WS-BIN-LOCN TO DS-L-LOCATION(WS-I)
           END-IF
           IF DS-L-LOT(WS-I) = SPACES
               MOVE WS-BIN-LOT TO DS-L-LOT(WS-I)
           END-IF
           IF DS-L-SERIAL(WS-I) = SPACES
               MOVE WS-BIN-SERIAL TO DS-L-SERIAL(WS-I)
           END-IF.
       2425-EXIT.
           EXIT.

      *>   The pick line that was actually picked for this order line.
       2426-BIN-FROM-PICK.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) p.SiteCode, "
                  "ISNULL(NULLIF(e.WarehouseCode,''), "
                  "ISNULL(p.WarehouseCode,'')), "
                  "ISNULL(e.LocationCode,''), ISNULL(e.LotCode,''), "
                  "ISNULL(e.SerialNo,'') "
                  "FROM so.PickListLine e JOIN so.PickList p ON "
                  "p.PickListId = e.PickListId "
                  "WHERE p.CompanyCode = ? AND p.PickListRef = ? "
                  "AND e.ItemCode = ? AND e.PickedQty > 0 "
                  "AND e.Status IN ('PICKED','SHORT','CLOSED') "
                  "ORDER BY CASE WHEN e.SalesOrderLineId = ? THEN 0 "
                  "ELSE 1 END, e.LineNum"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE DS-PICKLIST-REF TO SQLIO-PARM-VAL(2)
           MOVE DS-L-ITEM(WS-I) TO SQLIO-PARM-VAL(3)
           MOVE DS-L-SOL-ID(WS-I) TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           MOVE 5 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2426-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               GO TO 2426-EXIT
           END-IF
           MOVE WS-COL(1) TO WS-BIN-SITE
           MOVE WS-COL(2) TO WS-BIN-WHSE
           MOVE WS-COL(3) TO WS-BIN-LOCN
           MOVE WS-COL(4) TO WS-BIN-LOT
           MOVE WS-COL(5) TO WS-BIN-SERIAL.
       2426-EXIT.
           EXIT.

      *>   No pick list, or a pick line that never named a bin: the
      *>   balance that can supply the quantity, largest first - the
      *>   same choice the warehouse would make.
       2427-BIN-FROM-STOCK.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) b.SiteCode, b.WarehouseCode, "
                  "b.LocationCode, ISNULL(b.LotCode,''), "
                  "ISNULL(b.SerialNo,'') FROM inv.StockBalance b "
                  "JOIN inv.Location v ON v.CompanyCode = "
                  "b.CompanyCode AND v.SiteCode = b.SiteCode AND "
                  "v.WarehouseCode = b.WarehouseCode AND "
                  "v.LocationCode = b.LocationCode "
                  "WHERE b.CompanyCode = ? AND b.ItemCode = ? "
                  "AND (? = '' OR b.SiteCode = ?) "
                  "AND b.QualityStatus = 'ACCEPTED' "
                  "AND b.OwnerType = 'OWNED' AND v.IsAvailable = 1 "
                  "AND v.IsOwnedStock = 1 AND b.QtyOnHand > 0 "
                  "ORDER BY CASE WHEN b.QtyOnHand >= "
                  "CONVERT(decimal(19,6), ?) THEN 0 ELSE 1 END, "
                  "b.QtyOnHand DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE DS-L-ITEM(WS-I) TO SQLIO-PARM-VAL(2)
           MOVE DS-SITE TO SQLIO-PARM-VAL(3)
           MOVE DS-SITE TO SQLIO-PARM-VAL(4)
           MOVE DS-L-QTY(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(5)
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           MOVE 5 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2427-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               GO TO 2427-EXIT
           END-IF
           MOVE WS-COL(1) TO WS-BIN-SITE
           MOVE WS-COL(2) TO WS-BIN-WHSE
           MOVE WS-COL(3) TO WS-BIN-LOCN
           MOVE WS-COL(4) TO WS-BIN-LOT
           MOVE WS-COL(5) TO WS-BIN-SERIAL.
       2427-EXIT.
           EXIT.

       2430-READ-ORDER-LINE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), l.SalesOrderLineId), "
                  "ISNULL(l.ItemCode,''), ISNULL(l.Revision,''), "
                  "l.Description, CONVERT(varchar(30), "
                  "l.AcceptedQty - l.CancelledQty - l.DespatchedQty)"
                  ", ISNULL(l.UomCode,''), "
                  "CONVERT(varchar(30), l.ConversionFactor), "
                  "l.Status, l.LineType, "
                  "ISNULL(l.LinkedKitRef,'') "
                  "FROM so.SalesOrderLine l WHERE l.CompanyCode = ? "
                  "AND l.OrderNumber = ? AND l.LineNum = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE DS-ORDER-NUMBER TO SQLIO-PARM-VAL(2)
           MOVE DS-L-ORDER-LINE(WS-I) TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 3
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           MOVE 10 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2430-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "DSP0002" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE DS-L-ORDER-LINE(WS-I) TO WS-INT-ED
               MOVE SPACES TO WS-TEXT
               STRING "Order line " FUNCTION TRIM(WS-INT-ED)
                      " of " FUNCTION TRIM(DS-ORDER-NUMBER)
                      " is not known."
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
               GO TO 2430-EXIT
           END-IF
           COMPUTE DS-L-SOL-ID(WS-I) = FUNCTION NUMVAL(WS-COL(1))
           IF DS-L-ITEM(WS-I) = SPACES
               MOVE WS-COL(2) TO DS-L-ITEM(WS-I)
           END-IF
           IF DS-L-REVISION(WS-I) = SPACES
               MOVE WS-COL(3) TO DS-L-REVISION(WS-I)
           END-IF
           MOVE WS-COL(4) TO DS-L-DESC(WS-I)
           COMPUTE DS-L-ALLOWED(WS-I) = FUNCTION NUMVAL(WS-COL(5))
           IF DS-L-ALLOWED(WS-I) < 0
               MOVE 0 TO DS-L-ALLOWED(WS-I)
           END-IF
           IF DS-L-UOM(WS-I) = SPACES
               MOVE WS-COL(6) TO DS-L-UOM(WS-I)
           END-IF
           MOVE WS-COL(10) TO DS-L-KIT-REF(WS-I)
           IF WS-COL(8) NOT = "OPEN"
               MOVE 8 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "DSP0003" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO WS-TEXT
               STRING "Order line is " FUNCTION TRIM(WS-COL(8))
                      " and has nothing outstanding to despatch."
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
               MOVE WS-TEXT TO DS-L-MESSAGE(WS-I)
           END-IF.
       2430-EXIT.
           EXIT.

      *>   ITM-005.  A missing conversion is an error, never an
      *>   implied one for one, so the factor is asked for explicitly.
       2440-CONVERT-QTY.
           INITIALIZE WS-UOM-CA
           MOVE "TOSTOCK" TO CA-FUNCTION OF WS-UOM-CA
           MOVE CA-COMPANY OF DFHCOMMAREA TO CA-COMPANY OF WS-UOM-CA
           MOVE CA-SITE OF DFHCOMMAREA TO CA-SITE OF WS-UOM-CA
           MOVE CA-USER OF DFHCOMMAREA TO CA-USER OF WS-UOM-CA
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA
               TO CA-BUSINESS-DATE OF WS-UOM-CA
           MOVE DS-L-ITEM(WS-I) TO UC-ITEM-CODE
           MOVE DS-L-UOM(WS-I) TO UC-FROM-UOM
           MOVE "SALES" TO UC-USAGE-TYPE
           MOVE DS-DESPATCH-DATE TO UC-EFF-DATE
           MOVE DS-L-QTY(WS-I) TO UC-ENTERED-QTY
           CALL "UOMCONV" USING WS-UOM-CA
           IF CA-RETURN-CODE OF WS-UOM-CA >= 8
               MOVE 8 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "DSP0003" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE CA-MESSAGE OF WS-UOM-CA
                   TO CA-MESSAGE OF DFHCOMMAREA
               MOVE CA-MESSAGE OF WS-UOM-CA TO DS-L-MESSAGE(WS-I)
               GO TO 2440-EXIT
           END-IF
           MOVE UC-STOCK-QTY TO DS-L-STOCK-QTY(WS-I)
           MOVE UC-FACTOR TO DS-L-FACTOR(WS-I).
       2440-EXIT.
           EXIT.

      *>   Physical availability.  Stock already reserved to THIS
      *>   order is available to it, which is why AV-QTY-OWN-DEMAND is
      *>   added back; stock reserved to anybody else is not.
       2450-CHECK-AVAILABLE.
           INITIALIZE WS-AVQ-CA
           MOVE "AVAIL" TO CA-FUNCTION OF WS-AVQ-CA
           MOVE CA-COMPANY OF DFHCOMMAREA TO CA-COMPANY OF WS-AVQ-CA
           MOVE CA-SITE OF DFHCOMMAREA TO CA-SITE OF WS-AVQ-CA
           MOVE CA-USER OF DFHCOMMAREA TO CA-USER OF WS-AVQ-CA
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA
               TO CA-BUSINESS-DATE OF WS-AVQ-CA
           MOVE DS-L-ITEM(WS-I) TO AV-ITEM-CODE
           MOVE DS-L-REVISION(WS-I) TO AV-REVISION
           MOVE DS-SITE TO AV-SITE
           MOVE DS-L-WHSE(WS-I) TO AV-WAREHOUSE
           MOVE DS-L-LOT(WS-I) TO AV-LOT-CODE
           MOVE DS-L-SERIAL(WS-I) TO AV-SERIAL-NO
           MOVE "OWNED" TO AV-OWNER-TYPE
           MOVE DS-DESPATCH-DATE TO AV-AS-AT
           MOVE "SALESORDER" TO AV-DEMAND-TYPE
           MOVE DS-ORDER-NUMBER TO AV-DEMAND-REF
           MOVE DS-L-ORDER-LINE(WS-I) TO AV-DEMAND-LINE
           CALL "AVAILQ" USING WS-AVQ-CA
           IF CA-RETURN-CODE OF WS-AVQ-CA >= 8
               MOVE 8 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "DSP0004" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE CA-MESSAGE OF WS-AVQ-CA
                   TO CA-MESSAGE OF DFHCOMMAREA
               GO TO 2450-EXIT
           END-IF
           COMPUTE WS-QTY = AV-QTY-FREE + AV-QTY-OWN-DEMAND
           MOVE WS-QTY TO DS-L-AVAILABLE(WS-I)
           IF DS-L-STOCK-QTY(WS-I) > WS-QTY
               MOVE 8 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "DSP0004" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE WS-QTY TO WS-QTY-ED
               MOVE SPACES TO WS-TEXT
               STRING "Only " FUNCTION TRIM(WS-QTY-ED) " of "
                      FUNCTION TRIM(DS-L-ITEM(WS-I))
                      " is physically available to this order."
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
               MOVE WS-TEXT TO DS-L-MESSAGE(WS-I)
               MOVE "REFUSED" TO DS-L-STATUS(WS-I)
               ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           END-IF.
       2450-EXIT.
           EXIT.

      *>   SO-006.  A missing cost is NOT treated as zero.  Without a
      *>   comparable cost there is no attributable cost of sale, so
      *>   the confirmation is refused rather than posted at nil.
       2460-READ-UNIT-COST.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP (1) CONVERT(varchar(30), CASE WHEN "
                  "c.CostMethod = 'STD' THEN ISNULL("
                  "c.StandardUnitCost, c.AverageUnitCost) ELSE "
                  "c.AverageUnitCost END), c.CostMethod "
                  "FROM cst.ItemCost c WHERE c.CompanyCode = ? AND "
                  "c.ItemCode = ? AND (c.SiteCode = ? OR "
                  "c.SiteCode = '*') ORDER BY CASE WHEN "
                  "c.SiteCode = ? THEN 0 ELSE 1 END"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE DS-L-ITEM(WS-I) TO SQLIO-PARM-VAL(2)
           MOVE DS-SITE TO SQLIO-PARM-VAL(3)
           MOVE DS-SITE TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2460-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 8 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "DSP0007" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO WS-TEXT
               STRING "No comparable cost is held for "
                      FUNCTION TRIM(DS-L-ITEM(WS-I))
                      "; a missing cost is not treated as zero."
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
               MOVE WS-TEXT TO DS-L-MESSAGE(WS-I)
               ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
               GO TO 2460-EXIT
           END-IF
           COMPUTE WS-COST = FUNCTION NUMVAL(WS-COL(1))
           MOVE WS-COST TO DS-L-UNIT-COST(WS-I)
           MOVE WS-COL(2) TO DS-L-COST-METH(WS-I)
           COMPUTE DS-L-COGS-GBP(WS-I) ROUNDED =
               WS-COST * DS-L-STOCK-QTY(WS-I).
       2460-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       2500-WRITE-HEADER.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT so.Despatch (CompanyCode, SiteCode, "
                  "DespatchRef, CustomerCode, OrderNumber, "
                  "DespatchDate, PostingDate, FiscalYear, PeriodNo, "
                  "DeliveryAddressSnapshot, IncotermsRule, "
                  "IncotermsNamedPlace, CarrierCode, "
                  "CarrierReference, IsExport, DestinationCountry, "
                  "CustomerEori, ExportDeclarationRef, Status, "
                  "CogsPostedGBP, IsCutoffReview, CorrelationId, "
                  "ConfirmedBy, ConfirmedAt, CreatedBy) VALUES "
                  "(?, ?, ?, ?, ?, ?, ?, NULLIF(?,0), NULLIF(?,0), "
                  "NULLIF(?,''), NULLIF(?,''), NULLIF(?,''), "
                  "NULLIF(?,''), NULLIF(?,''), ?, NULLIF(?,''), "
                  "NULLIF(?,''), NULLIF(?,''), 'CONFIRMED', 0, ?, "
                  "?, ?, SYSDATETIME(), ?); SELECT CONVERT("
                  "varchar(24), SCOPE_IDENTITY()), ''"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 22 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 22
               MOVE SPACES TO SQLIO-PARM-VAL(WS-J)
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE DS-SITE TO SQLIO-PARM-VAL(2)
           MOVE DS-DESPATCH-REF TO SQLIO-PARM-VAL(3)
           MOVE DS-CUSTOMER TO SQLIO-PARM-VAL(4)
           MOVE DS-ORDER-NUMBER TO SQLIO-PARM-VAL(5)
           MOVE DS-DESPATCH-DATE TO SQLIO-PARM-VAL(6)
           MOVE DS-POSTING-DATE TO SQLIO-PARM-VAL(7)
           MOVE DS-FISCAL-YEAR TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(8)
           MOVE DS-PERIOD-NO TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(9)
           MOVE DS-ADDRESS-SNAP(1:200) TO SQLIO-PARM-VAL(10)
           MOVE DS-INCOTERMS TO SQLIO-PARM-VAL(11)
           MOVE DS-INCOTERMS-PLACE TO SQLIO-PARM-VAL(12)
           MOVE DS-CARRIER-CODE TO SQLIO-PARM-VAL(13)
           MOVE DS-CARRIER-REF TO SQLIO-PARM-VAL(14)
           IF DS-EXPORT
               MOVE "1" TO SQLIO-PARM-VAL(15)
           ELSE
               MOVE "0" TO SQLIO-PARM-VAL(15)
           END-IF
           MOVE DS-DEST-COUNTRY TO SQLIO-PARM-VAL(16)
           MOVE DS-CUSTOMER-EORI TO SQLIO-PARM-VAL(17)
           MOVE DS-EXPORT-DECL-REF TO SQLIO-PARM-VAL(18)
           IF DS-NEEDS-CUTOFF
               MOVE "1" TO SQLIO-PARM-VAL(19)
           ELSE
               MOVE "0" TO SQLIO-PARM-VAL(19)
           END-IF
           MOVE CA-CORRELATION-ID OF DFHCOMMAREA
               TO SQLIO-PARM-VAL(20)
           MOVE CA-USER OF DFHCOMMAREA TO SQLIO-PARM-VAL(21)
           MOVE CA-USER OF DFHCOMMAREA TO SQLIO-PARM-VAL(22)
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2500-EXIT
           END-IF
           COMPUTE DS-DESPATCH-ID = FUNCTION NUMVAL(WS-COL(1)).
       2500-EXIT.
           EXIT.

       2550-WRITE-LINES.
           MOVE 0 TO DS-COGS-GBP
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > DS-LINE-CNT
               IF NOT CA-FAILED OF DFHCOMMAREA
                   MOVE WS-I TO DS-L-LINE-NUM(WS-I)
                   PERFORM 2560-INSERT-LINE THRU 2560-EXIT
                   IF NOT CA-FAILED OF DFHCOMMAREA
                       PERFORM 2570-MOVE-STOCK THRU 2570-EXIT
                   END-IF
                   IF NOT CA-FAILED OF DFHCOMMAREA
                       PERFORM 2580-CONSUME-RESV THRU 2580-EXIT
                   END-IF
                   IF NOT CA-FAILED OF DFHCOMMAREA
                       PERFORM 2590-UPDATE-DEMAND THRU 2590-EXIT
                   END-IF
                   IF NOT CA-FAILED OF DFHCOMMAREA
                       ADD DS-L-COGS-GBP(WS-I) TO DS-COGS-GBP
                       MOVE "CONFIRMED" TO DS-L-STATUS(WS-I)
                   END-IF
               END-IF
           END-PERFORM.
       2550-EXIT.
           EXIT.

       2560-INSERT-LINE.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT so.DespatchLine (DespatchId, LineNum, "
                  "SalesOrderLineId, OrderNumber, OrderLineNum, "
                  "ScheduleNo, ItemCode, Revision, Description, "
                  "PickedQty, DespatchedQty, UomCode, "
                  "ConversionFactor, StockQty, WarehouseCode, "
                  "LocationCode, LotCode, SerialNo, UnitCostGBP, "
                  "CogsGBP, CostMethod, KitRef) VALUES "
                  "(?, ?, NULLIF(?,0), ?, ?, NULLIF(?,0), ?, "
                  "NULLIF(?,''), ?, ?, ?, NULLIF(?,''), ?, ?, "
                  "NULLIF(?,''), NULLIF(?,''), NULLIF(?,''), "
                  "NULLIF(?,''), ?, ?, NULLIF(?,''), NULLIF(?,''))"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 22 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 22
               MOVE SPACES TO SQLIO-PARM-VAL(WS-J)
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           MOVE DS-DESPATCH-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE DS-L-LINE-NUM(WS-I) TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE DS-L-SOL-ID(WS-I) TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE DS-ORDER-NUMBER TO SQLIO-PARM-VAL(4)
           MOVE DS-L-ORDER-LINE(WS-I) TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE DS-L-SCHEDULE(WS-I) TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(6)
           MOVE DS-L-ITEM(WS-I) TO SQLIO-PARM-VAL(7)
           MOVE DS-L-REVISION(WS-I) TO SQLIO-PARM-VAL(8)
           MOVE DS-L-DESC(WS-I) TO SQLIO-PARM-VAL(9)
           MOVE DS-L-PICKED(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(10)
           MOVE DS-L-QTY(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(11)
           MOVE DS-L-UOM(WS-I) TO SQLIO-PARM-VAL(12)
           MOVE DS-L-FACTOR(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(13)
           MOVE DS-L-STOCK-QTY(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(14)
           MOVE DS-L-WHSE(WS-I) TO SQLIO-PARM-VAL(15)
           MOVE DS-L-LOCATION(WS-I) TO SQLIO-PARM-VAL(16)
           MOVE DS-L-LOT(WS-I) TO SQLIO-PARM-VAL(17)
           MOVE DS-L-SERIAL(WS-I) TO SQLIO-PARM-VAL(18)
           MOVE DS-L-UNIT-COST(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(19)
           MOVE DS-L-COGS-GBP(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(20)
           MOVE DS-L-COST-METH(WS-I) TO SQLIO-PARM-VAL(21)
           MOVE DS-L-KIT-REF(WS-I) TO SQLIO-PARM-VAL(22)
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2560-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.
       2560-EXIT.
           EXIT.

      *>   ITM-015.  A balance is never edited.  Owned stock leaves
      *>   through an authorised movement that states its source,
      *>   ownership, traceability and quality condition.
       2570-MOVE-STOCK.
           INITIALIZE WS-SKM-CA
           MOVE "ISSUE" TO CA-FUNCTION OF WS-SKM-CA
           MOVE CA-COMPANY OF DFHCOMMAREA TO CA-COMPANY OF WS-SKM-CA
           MOVE CA-SITE OF DFHCOMMAREA TO CA-SITE OF WS-SKM-CA
           MOVE CA-USER OF DFHCOMMAREA TO CA-USER OF WS-SKM-CA
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA
               TO CA-BUSINESS-DATE OF WS-SKM-CA
           MOVE SPACES TO WS-SUB-SUFFIX
           MOVE WS-I TO WS-SUB-SEQ
           STRING "-M" WS-SUB-SEQ DELIMITED BY SIZE
               INTO WS-SUB-SUFFIX
           PERFORM 9700-SUB-CORR THRU 9700-EXIT
           MOVE WS-SUB-CORR TO CA-CORRELATION-ID OF WS-SKM-CA
           MOVE DS-L-ITEM(WS-I) TO SM-ITEM-CODE
           MOVE DS-L-REVISION(WS-I) TO SM-REVISION
           MOVE DS-SITE TO SM-SITE
           MOVE DS-L-WHSE(WS-I) TO SM-WAREHOUSE
           MOVE DS-L-LOCATION(WS-I) TO SM-LOCATION
           MOVE DS-L-LOT(WS-I) TO SM-LOT-CODE
           MOVE DS-L-SERIAL(WS-I) TO SM-SERIAL-NO
           MOVE "OWNED" TO SM-OWNER-TYPE
           MOVE "ACCEPTED" TO SM-CONDITION
           MOVE DS-L-QTY(WS-I) TO SM-QUANTITY
           MOVE DS-L-UOM(WS-I) TO SM-UOM-CODE
           MOVE DS-L-UNIT-COST(WS-I) TO SM-UNIT-COST
           MOVE DS-L-COGS-GBP(WS-I) TO SM-VALUE-GBP
           MOVE DS-DESPATCH-DATE TO SM-MOVEMENT-DATE
           MOVE DS-POSTING-DATE TO SM-POSTING-DATE
           MOVE "SALEDESP" TO SM-REASON-CODE
           MOVE "SO" TO SM-SOURCE-MODULE
           MOVE "DESPATCH" TO SM-SOURCE-DOC-TYPE
           MOVE DS-DESPATCH-REF TO SM-SOURCE-DOC-REF
           MOVE DS-L-LINE-NUM(WS-I) TO SM-SOURCE-DOC-LINE
           MOVE SPACES TO WS-TEXT
           STRING "Despatch " FUNCTION TRIM(DS-DESPATCH-REF)
                  " against sales order "
                  FUNCTION TRIM(DS-ORDER-NUMBER)
               DELIMITED BY SIZE INTO WS-TEXT
           MOVE WS-TEXT TO SM-REASON
           CALL "STKMOVE" USING WS-SKM-CA
           IF CA-RETURN-CODE OF WS-SKM-CA >= 8
               MOVE CA-RETURN-CODE OF WS-SKM-CA
                   TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "DSP0004" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE CA-MESSAGE OF WS-SKM-CA
                   TO CA-MESSAGE OF DFHCOMMAREA
               MOVE CA-MESSAGE OF WS-SKM-CA TO DS-L-MESSAGE(WS-I)
               ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
               GO TO 2570-EXIT
           END-IF
           MOVE SM-MOVEMENT-ID TO DS-L-MOVE-ID(WS-I)
           IF SM-STOCK-QTY NOT = 0
               MOVE SM-STOCK-QTY TO DS-L-STOCK-QTY(WS-I)
           END-IF
           PERFORM 2575-RECORD-MOVEMENT THRU 2575-EXIT.
       2570-EXIT.
           EXIT.

       2575-RECORD-MOVEMENT.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.DespatchLine SET MovementId = "
                  "NULLIF(?,0) WHERE DespatchId = ? AND LineNum = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE DS-L-MOVE-ID(WS-I) TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE DS-DESPATCH-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE DS-L-LINE-NUM(WS-I) TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 3
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2575-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.
       2575-EXIT.
           EXIT.

      *>   The reservation is consumed, not released: the units that
      *>   left are accounted for against the demand that held them.
       2580-CONSUME-RESV.
           INITIALIZE WS-SKR-CA
           MOVE "CONSUME" TO CA-FUNCTION OF WS-SKR-CA
           MOVE CA-COMPANY OF DFHCOMMAREA TO CA-COMPANY OF WS-SKR-CA
           MOVE CA-SITE OF DFHCOMMAREA TO CA-SITE OF WS-SKR-CA
           MOVE CA-USER OF DFHCOMMAREA TO CA-USER OF WS-SKR-CA
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA
               TO CA-BUSINESS-DATE OF WS-SKR-CA
           MOVE DS-L-ITEM(WS-I) TO SR-ITEM-CODE
           MOVE DS-L-REVISION(WS-I) TO SR-REVISION
           MOVE DS-SITE TO SR-SITE
           MOVE DS-L-LOT(WS-I) TO SR-LOT-CODE
           MOVE DS-L-SERIAL(WS-I) TO SR-SERIAL-NO
           MOVE "SALESORDER" TO SR-DEMAND-TYPE
           MOVE DS-ORDER-NUMBER TO SR-DEMAND-REF
           MOVE DS-L-ORDER-LINE(WS-I) TO SR-DEMAND-LINE
           MOVE DS-L-STOCK-QTY(WS-I) TO SR-QTY-REQUESTED
           CALL "STKRESV" USING WS-SKR-CA
           MOVE SR-RESERVATION-ID TO DS-L-RES-ID(WS-I)
      *>   a demand with no reservation is legitimate (an unallocated
      *>   over the counter sale), so only a real failure stops us
           IF CA-RETURN-CODE OF WS-SKR-CA >= 8
              AND CA-REASON-CODE OF WS-SKR-CA NOT = "RSV0005"
               MOVE CA-RETURN-CODE OF WS-SKR-CA
                   TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "DSP0004" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE CA-MESSAGE OF WS-SKR-CA
                   TO CA-MESSAGE OF DFHCOMMAREA
               MOVE CA-MESSAGE OF WS-SKR-CA TO DS-L-MESSAGE(WS-I)
               ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           END-IF.
       2580-EXIT.
           EXIT.

      *>   Both quantity ledgers move together: the order line and the
      *>   schedule.  A confirmed despatch quantity can never be
      *>   erased by a later cancellation (specification section 4).
       2590-UPDATE-DEMAND.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.SalesOrderLine SET DespatchedQty = "
                  "DespatchedQty + ? WHERE SalesOrderLineId = ?; "
                  "UPDATE so.DeliverySchedule SET DespatchedQty = "
                  "DespatchedQty + ?, Status = CASE WHEN "
                  "DespatchedQty + ? >= Quantity THEN 'COMPLETE' "
                  "ELSE 'PARTIAL' END WHERE SalesOrderLineId = ? "
                  "AND ScheduleNo = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 6 TO SQLIO-PARM-CNT
           MOVE DS-L-QTY(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE DS-L-SOL-ID(WS-I) TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE DS-L-QTY(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE DS-L-SOL-ID(WS-I) TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE DS-L-SCHEDULE(WS-I) TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(6)
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 6
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2590-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           COMPUTE DS-L-REMAINING(WS-I) =
               DS-L-ALLOWED(WS-I) - DS-L-QTY(WS-I).
       2590-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2600  THE ATTRIBUTABLE COST OF SALE, POSTED EXACTLY ONCE.
      *>       The legs come from the approved mapping for business
      *>       event SO.DESPATCH.COGS; without a mapping the posting
      *>       is blocked rather than sent to a suspense account
      *>       (COM-014).  fin.usp_PostJournal returns the original
      *>       journal if this correlation id already posted one.
      *>----------------------------------------------------------------
       2600-POST-COGS.
           IF DS-COGS-GBP = 0
               MOVE 4 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "DSP0007" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO WS-TEXT
               STRING "The despatch carries no cost value, so no "
                      "cost of sale has been posted."
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
               GO TO 2600-EXIT
           END-IF
           PERFORM 2610-READ-EVENT-MAP THRU 2610-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 2600-EXIT
           END-IF
           MOVE "-COGS" TO WS-SUB-SUFFIX
           PERFORM 9700-SUB-CORR THRU 9700-EXIT
           PERFORM 2620-CLEAR-STAGE THRU 2620-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 2600-EXIT
           END-IF
           PERFORM 2630-STAGE-LEGS THRU 2630-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 2600-EXIT
           END-IF
           PERFORM 2650-CALL-POST THRU 2650-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 2600-EXIT
           END-IF
           PERFORM 2680-STAMP-HEADER THRU 2680-EXIT.
       2600-EXIT.
           EXIT.

      *>   COM-005.  A derived correlation id for one sub-instruction
      *>   of this despatch.  The caller's id is truncated only if
      *>   the suffix would not otherwise fit the 36 character field.
       9700-SUB-CORR.
           MOVE 0 TO WS-BASE-LEN
           INSPECT FUNCTION REVERSE(CA-CORRELATION-ID OF DFHCOMMAREA)
               TALLYING WS-BASE-LEN FOR LEADING SPACES
           COMPUTE WS-BASE-LEN = 36 - WS-BASE-LEN
           MOVE 0 TO WS-SUF-LEN
           INSPECT FUNCTION REVERSE(WS-SUB-SUFFIX)
               TALLYING WS-SUF-LEN FOR LEADING SPACES
           COMPUTE WS-SUF-LEN = 8 - WS-SUF-LEN
           IF WS-BASE-LEN + WS-SUF-LEN > 36
               COMPUTE WS-BASE-LEN = 36 - WS-SUF-LEN
           END-IF
           MOVE SPACES TO WS-SUB-CORR
           STRING CA-CORRELATION-ID OF DFHCOMMAREA(1:WS-BASE-LEN)
                  WS-SUB-SUFFIX(1:WS-SUF-LEN)
               DELIMITED BY SIZE INTO WS-SUB-CORR.
       9700-EXIT.
           EXIT.

       2610-READ-EVENT-MAP.
           MOVE 0 TO WS-LEG-CNT
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT m.LegCode, m.AccountCode, m.DrCr, "
                  "ISNULL(m.DimCostCentre,''), "
                  "ISNULL(m.DimProject,'') "
                  "FROM fin.EventAccountMap m WHERE "
                  "m.CompanyCode = ? AND m.EventCode = ? AND "
                  "m.IsActive = 1 ORDER BY m.SortOrder, m.LegCode"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE WS-COGS-EVENT TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 5 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2610-EXIT
           END-IF
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-AT-END
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-ROW
               IF SQLIO-ERROR OR SQLIO-NOTFOUND
                   MOVE "Y" TO WS-DONE
               ELSE
                   IF WS-LEG-CNT < 12
                       ADD 1 TO WS-LEG-CNT
                       MOVE WS-COL(1) TO WS-LEG-CODE(WS-LEG-CNT)
                       MOVE WS-COL(2) TO WS-LEG-ACCOUNT(WS-LEG-CNT)
                       MOVE WS-COL(3) TO WS-LEG-DRCR(WS-LEG-CNT)
                       MOVE WS-COL(4) TO WS-LEG-CC(WS-LEG-CNT)
                       MOVE WS-COL(5) TO WS-LEG-PROJ(WS-LEG-CNT)
                   END-IF
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF WS-LEG-CNT < 2
               MOVE 8 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "DSP0007" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO WS-TEXT
               STRING "No approved account mapping exists for "
                      FUNCTION TRIM(WS-COGS-EVENT)
                      "; the cost of sale is blocked rather than "
                      "posted to a suspense account."
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
               ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           END-IF.
       2610-EXIT.
           EXIT.

      *>   a retried attempt may have left staged lines behind
       2620-CLEAR-STAGE.
           MOVE SPACES TO SQLIO-STMT
           STRING "DELETE fin.JournalStage WHERE CorrelationId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE WS-SUB-CORR TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2620-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.
       2620-EXIT.
           EXIT.

       2630-STAGE-LEGS.
           MOVE SPACES TO WS-NARR
           STRING "Cost of sale on despatch "
                  FUNCTION TRIM(DS-DESPATCH-REF) " - control passes "
                  FUNCTION TRIM(DS-CONTROL-POINT)
               DELIMITED BY SIZE INTO WS-NARR
           MOVE 0 TO WS-LINE-NO
           PERFORM VARYING WS-K FROM 1 BY 1 UNTIL WS-K > WS-LEG-CNT
               IF NOT CA-FAILED OF DFHCOMMAREA
                   PERFORM 2640-ONE-LEG THRU 2640-EXIT
               END-IF
           END-PERFORM.
       2630-EXIT.
           EXIT.

       2640-ONE-LEG.
           ADD 1 TO WS-LINE-NO
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT fin.JournalStage (CorrelationId, LineNum, "
                  "CompanyCode, AccountCode, DebitAmount, "
                  "CreditAmount, CurrencyCode, DimCostCentre, "
                  "DimProject, Narrative, SubledgerType, "
                  "SubledgerRef, SourceRef, CreatedBy) VALUES "
                  "(?, ?, ?, ?, ?, ?, 'GBP', NULLIF(?,''), "
                  "NULLIF(?,''), ?, 'SO', ?, ?, ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 11 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 11
               MOVE SPACES TO SQLIO-PARM-VAL(WS-J)
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           MOVE WS-SUB-CORR TO SQLIO-PARM-VAL(1)
           MOVE WS-LINE-NO TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(3)
           MOVE WS-LEG-ACCOUNT(WS-K) TO SQLIO-PARM-VAL(4)
           MOVE DS-COGS-GBP TO WS-MONEY-ED
           IF WS-LEG-DRCR(WS-K) = "DR"
               MOVE FUNCTION TRIM(WS-MONEY-ED) TO SQLIO-PARM-VAL(5)
               MOVE "0" TO SQLIO-PARM-VAL(6)
           ELSE
               MOVE "0" TO SQLIO-PARM-VAL(5)
               MOVE FUNCTION TRIM(WS-MONEY-ED) TO SQLIO-PARM-VAL(6)
           END-IF
           MOVE WS-LEG-CC(WS-K) TO SQLIO-PARM-VAL(7)
           MOVE WS-LEG-PROJ(WS-K) TO SQLIO-PARM-VAL(8)
           MOVE WS-NARR TO SQLIO-PARM-VAL(9)
           MOVE DS-CUSTOMER TO SQLIO-PARM-VAL(10)
           MOVE DS-DESPATCH-REF TO SQLIO-PARM-VAL(11)
           MOVE 12 TO SQLIO-PARM-CNT
           MOVE CA-USER OF DFHCOMMAREA TO SQLIO-PARM-VAL(12)
           MOVE "N" TO SQLIO-PARM-NULL(12)
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2640-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.
       2640-EXIT.
           EXIT.

      *>   fin.usp_PostJournal owns balance, period, account,
      *>   dimension and event rules.  Its refusals THROW, which under
      *>   SET XACT_ABORT ON dooms this connection's transaction - so
      *>   a failure here is always a rollback, never a part shipment.
       2650-CALL-POST.
           MOVE SPACES TO SQLIO-STMT
           STRING "EXEC fin.usp_PostJournal ?, ?, ?, ?, ?, ?, ?, "
                  "?, ?, ?, ?, ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 12 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 12
               MOVE SPACES TO SQLIO-PARM-VAL(WS-J)
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE WS-SUB-CORR TO SQLIO-PARM-VAL(2)
           MOVE CA-USER OF DFHCOMMAREA TO SQLIO-PARM-VAL(3)
           MOVE SPACES TO WS-TEXT
           STRING "Cost of sale, despatch "
                  FUNCTION TRIM(DS-DESPATCH-REF)
               DELIMITED BY SIZE INTO WS-TEXT
           MOVE WS-TEXT TO SQLIO-PARM-VAL(4)
           MOVE DS-POSTING-DATE TO SQLIO-PARM-VAL(5)
           MOVE "SO" TO SQLIO-PARM-VAL(6)
           MOVE DS-DESPATCH-DATE TO SQLIO-PARM-VAL(7)
           MOVE "GBP" TO SQLIO-PARM-VAL(8)
           MOVE "SO" TO SQLIO-PARM-VAL(9)
           MOVE DS-DESPATCH-REF TO SQLIO-PARM-VAL(10)
           MOVE WS-COGS-EVENT TO SQLIO-PARM-VAL(11)
           MOVE "JNL" TO SQLIO-PARM-VAL(12)
           MOVE 5 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               MOVE 12 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "DSP0007" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE OF DFHCOMMAREA
               ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
               GO TO 2650-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 12 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "DSP0007" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO WS-TEXT
               STRING "The cost of sale posting returned no result."
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
               GO TO 2650-EXIT
           END-IF
           COMPUTE DS-JOURNAL-ID = FUNCTION NUMVAL(WS-COL(1))
           MOVE WS-COL(2) TO DS-JOURNAL-NO.
       2650-EXIT.
           EXIT.

       2680-STAMP-HEADER.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.Despatch SET CogsPostedGBP = ?, "
                  "JournalId = NULLIF(?,0) WHERE DespatchId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE DS-COGS-GBP TO WS-MONEY-ED
           MOVE FUNCTION TRIM(WS-MONEY-ED) TO SQLIO-PARM-VAL(1)
           MOVE DS-JOURNAL-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE DS-DESPATCH-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 3
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 2680-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.
       2680-EXIT.
           EXIT.

      *>   COM-013.  so.Despatch has no column for the recognition
      *>   point, so the decision and its basis are written to the
      *>   attributable history instead of being lost.
       2700-AUDIT-CONTROL.
           INITIALIZE WS-AUD-CA
           MOVE "WRITE" TO CA-FUNCTION OF WS-AUD-CA
           MOVE CA-COMPANY OF DFHCOMMAREA TO CA-COMPANY OF WS-AUD-CA
           MOVE CA-SITE OF DFHCOMMAREA TO CA-SITE OF WS-AUD-CA
           MOVE CA-USER OF DFHCOMMAREA TO CA-USER OF WS-AUD-CA
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA
               TO CA-BUSINESS-DATE OF WS-AUD-CA
           MOVE CA-CORRELATION-ID OF DFHCOMMAREA
               TO CA-CORRELATION-ID OF WS-AUD-CA
           MOVE "so.Despatch" TO AU-ENTITY-TYPE
           MOVE DS-DESPATCH-REF TO AU-ENTITY-KEY
           MOVE "CONFIRM" TO AU-ACTION
           MOVE "ControlPassesAt" TO AU-FIELD-NAME
           MOVE DS-CONTROL-POINT TO AU-NEW-VALUE
           MOVE DS-CONTROL-BASIS TO AU-REASON
           MOVE "DESPATCH" TO AU-SOURCE
           MOVE "N" TO AU-SENSITIVE
           CALL "AUDITLOG" USING WS-AUD-CA.
       2700-EXIT.
           EXIT.

       2900-CONFIRM-WORDS.
           MOVE DS-COGS-GBP TO WS-MONEY-ED
           MOVE SPACES TO WS-TEXT
           STRING "Despatch " FUNCTION TRIM(DS-DESPATCH-REF)
                  " confirmed; cost of sale GBP "
                  FUNCTION TRIM(WS-MONEY-ED) " posted once as "
                  FUNCTION TRIM(DS-JOURNAL-NO)
               DELIMITED BY SIZE INTO WS-TEXT
           MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
           IF DS-NEEDS-CUTOFF
               MOVE 4 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "DSP0005" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO WS-TEXT
               STRING FUNCTION TRIM(CA-MESSAGE OF DFHCOMMAREA)
                      "; flagged for period cut off review."
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
               ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           END-IF.
       2900-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000  REVERSE.  An authorised reversal is a NEW business
      *>       event, not an undo.  The original despatch and its
      *>       journal stay exactly where they are; a compensating
      *>       stock movement and a compensating journal are written
      *>       and the original is marked REVERSED so the history
      *>       reads as what actually happened.
      *>----------------------------------------------------------------
       3000-REVERSE.
           MOVE DS-REVERSAL-OF TO DS-DESPATCH-REF
           PERFORM 4000-ENQUIRE THRU 4000-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 3000-EXIT
           END-IF
           IF DS-STATUS = "REVERSED"
               MOVE 8 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "DSP0011" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO WS-TEXT
               STRING "Despatch " FUNCTION TRIM(DS-REVERSAL-OF)
                      " has already been reversed."
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
               GO TO 3000-EXIT
           END-IF
           PERFORM 2150-PERIOD-CHECK THRU 2150-EXIT
           IF CA-RETURN-CODE OF DFHCOMMAREA >= 8
               GO TO 3000-EXIT
           END-IF
           PERFORM 9400-BEGIN THRU 9400-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 3000-EXIT
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > DS-LINE-CNT
               IF NOT CA-FAILED OF DFHCOMMAREA
                   PERFORM 3100-REVERSE-LINE THRU 3100-EXIT
               END-IF
           END-PERFORM
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 3000-EXIT
           END-IF
           PERFORM 3200-REVERSE-JOURNAL THRU 3200-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 3000-EXIT
           END-IF
           PERFORM 3300-MARK-REVERSED THRU 3300-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 3000-EXIT
           END-IF
           PERFORM 9450-COMMIT THRU 9450-EXIT
           MOVE "REVERSED" TO DS-STATUS
           MOVE DS-REVERSAL-OF TO DS-REVERSED-BY
           MOVE SPACES TO WS-TEXT
           STRING "Despatch " FUNCTION TRIM(DS-REVERSAL-OF)
                  " reversed by compensating movements and journal "
                  FUNCTION TRIM(DS-JOURNAL-NO)
                  "; nothing has been deleted."
               DELIMITED BY SIZE INTO WS-TEXT
           MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA.
       3000-EXIT.
           EXIT.

       3100-REVERSE-LINE.
           INITIALIZE WS-SKM-CA
           MOVE "RETURN" TO CA-FUNCTION OF WS-SKM-CA
           MOVE CA-COMPANY OF DFHCOMMAREA TO CA-COMPANY OF WS-SKM-CA
           MOVE CA-SITE OF DFHCOMMAREA TO CA-SITE OF WS-SKM-CA
           MOVE CA-USER OF DFHCOMMAREA TO CA-USER OF WS-SKM-CA
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA
               TO CA-BUSINESS-DATE OF WS-SKM-CA
           MOVE SPACES TO WS-SUB-SUFFIX
           MOVE WS-I TO WS-SUB-SEQ
           STRING "-R" WS-SUB-SEQ DELIMITED BY SIZE
               INTO WS-SUB-SUFFIX
           PERFORM 9700-SUB-CORR THRU 9700-EXIT
           MOVE WS-SUB-CORR TO CA-CORRELATION-ID OF WS-SKM-CA
           MOVE DS-L-ITEM(WS-I) TO SM-ITEM-CODE
           MOVE DS-L-REVISION(WS-I) TO SM-REVISION
           MOVE DS-SITE TO SM-SITE
           MOVE DS-L-WHSE(WS-I) TO SM-WAREHOUSE
           MOVE DS-L-LOCATION(WS-I) TO SM-LOCATION
           MOVE DS-L-LOT(WS-I) TO SM-LOT-CODE
           MOVE DS-L-SERIAL(WS-I) TO SM-SERIAL-NO
           MOVE "OWNED" TO SM-OWNER-TYPE
           MOVE "ACCEPTED" TO SM-CONDITION
           MOVE DS-L-QTY(WS-I) TO SM-QUANTITY
           MOVE DS-L-UOM(WS-I) TO SM-UOM-CODE
           MOVE DS-L-UNIT-COST(WS-I) TO SM-UNIT-COST
           MOVE DS-DESPATCH-DATE TO SM-MOVEMENT-DATE
           MOVE DS-POSTING-DATE TO SM-POSTING-DATE
           MOVE "SALEREV" TO SM-REASON-CODE
           MOVE DS-REVERSE-REASON TO SM-REASON
           MOVE DS-REVERSE-APPR TO SM-APPROVAL-REF
           MOVE "SO" TO SM-SOURCE-MODULE
           MOVE "DESPATCHREV" TO SM-SOURCE-DOC-TYPE
           MOVE DS-REVERSAL-OF TO SM-SOURCE-DOC-REF
           MOVE DS-L-LINE-NUM(WS-I) TO SM-SOURCE-DOC-LINE
           CALL "STKMOVE" USING WS-SKM-CA
           IF CA-RETURN-CODE OF WS-SKM-CA >= 8
               MOVE CA-RETURN-CODE OF WS-SKM-CA
                   TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "DSP0004" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE CA-MESSAGE OF WS-SKM-CA
                   TO CA-MESSAGE OF DFHCOMMAREA
               GO TO 3100-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.SalesOrderLine SET DespatchedQty = "
                  "CASE WHEN DespatchedQty >= ? THEN DespatchedQty "
                  "- ? ELSE 0 END WHERE SalesOrderLineId = ?; "
                  "UPDATE so.DeliverySchedule SET DespatchedQty = "
                  "CASE WHEN DespatchedQty >= ? THEN DespatchedQty "
                  "- ? ELSE 0 END, Status = 'PARTIAL' WHERE "
                  "SalesOrderLineId = ? AND ScheduleNo = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 7 TO SQLIO-PARM-CNT
           MOVE DS-L-QTY(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(2)
           MOVE DS-L-SOL-ID(WS-I) TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(3)
           MOVE DS-L-QTY(WS-I) TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(4)
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE DS-L-SOL-ID(WS-I) TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(6)
           MOVE DS-L-SCHEDULE(WS-I) TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(7)
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 7
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 3100-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.
       3100-EXIT.
           EXIT.

      *>   the compensating journal: the same legs with debit and
      *>   credit exchanged, posted under its own correlation id
       3200-REVERSE-JOURNAL.
           IF DS-COGS-GBP = 0
               GO TO 3200-EXIT
           END-IF
           PERFORM 2610-READ-EVENT-MAP THRU 2610-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 3200-EXIT
           END-IF
           PERFORM VARYING WS-K FROM 1 BY 1 UNTIL WS-K > WS-LEG-CNT
               IF WS-LEG-DRCR(WS-K) = "DR"
                   MOVE "CR" TO WS-LEG-DRCR(WS-K)
               ELSE
                   MOVE "DR" TO WS-LEG-DRCR(WS-K)
               END-IF
           END-PERFORM
           PERFORM 2620-CLEAR-STAGE THRU 2620-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 3200-EXIT
           END-IF
           PERFORM 2630-STAGE-LEGS THRU 2630-EXIT
           IF CA-FAILED OF DFHCOMMAREA
               GO TO 3200-EXIT
           END-IF
           PERFORM 2650-CALL-POST THRU 2650-EXIT.
       3200-EXIT.
           EXIT.

       3300-MARK-REVERSED.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.Despatch SET Status = 'REVERSED', "
                  "ModifiedBy = ?, ModifiedAt = SYSDATETIME() "
                  "WHERE CompanyCode = ? AND DespatchRef = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-USER OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(2)
           MOVE DS-REVERSAL-OF TO SQLIO-PARM-VAL(3)
           PERFORM VARYING WS-J FROM 1 BY 1 UNTIL WS-J > 3
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 3300-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.
       3300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 4000  ENQUIRE - the despatch as it stands, with its lines.
      *>       Used on its own and by the idempotent repeat path.
      *>----------------------------------------------------------------
       4000-ENQUIRE.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(24), d.DespatchId), "
                  "d.CustomerCode, ISNULL(d.OrderNumber,''), "
                  "CONVERT(char(8), d.DespatchDate, 112), "
                  "CONVERT(char(8), d.PostingDate, 112), d.Status, "
                  "CONVERT(varchar(24), d.CogsPostedGBP), "
                  "CONVERT(varchar(24), ISNULL(d.JournalId,0)), "
                  "CASE WHEN d.IsCutoffReview = 1 THEN 'Y' ELSE 'N' "
                  "END, ISNULL(d.IncotermsRule,''), "
                  "CASE WHEN d.IsExport = 1 THEN 'Y' ELSE 'N' END, "
                  "ISNULL(d.SiteCode,'') FROM so.Despatch d "
                  "WHERE d.CompanyCode = ? AND d.DespatchRef = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY OF DFHCOMMAREA TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE DS-DESPATCH-REF TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 12 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 4000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO CA-RETURN-CODE OF DFHCOMMAREA
               MOVE "DSP0009" TO CA-REASON-CODE OF DFHCOMMAREA
               MOVE SPACES TO WS-TEXT
               STRING "Despatch " FUNCTION TRIM(DS-DESPATCH-REF)
                      " is not known in this company."
                   DELIMITED BY SIZE INTO WS-TEXT
               MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
               GO TO 4000-EXIT
           END-IF
           COMPUTE DS-DESPATCH-ID = FUNCTION NUMVAL(WS-COL(1))
           MOVE WS-COL(2) TO DS-CUSTOMER
           MOVE WS-COL(3) TO DS-ORDER-NUMBER
           MOVE WS-COL(4)(1:8) TO DS-DESPATCH-DATE
           MOVE WS-COL(5)(1:8) TO DS-POSTING-DATE
           MOVE WS-COL(6) TO DS-STATUS
           COMPUTE DS-COGS-GBP = FUNCTION NUMVAL(WS-COL(7))
           COMPUTE DS-JOURNAL-ID = FUNCTION NUMVAL(WS-COL(8))
           MOVE WS-COL(9)(1:1) TO DS-IS-CUTOFF
           MOVE WS-COL(10) TO DS-INCOTERMS
           MOVE WS-COL(11)(1:1) TO DS-IS-EXPORT
           MOVE WS-COL(12) TO DS-SITE
           PERFORM 4100-READ-LINES THRU 4100-EXIT.
       4000-EXIT.
           EXIT.

       4100-READ-LINES.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CONVERT(varchar(12), e.LineNum), "
                  "CONVERT(varchar(12), ISNULL(e.OrderLineNum,0)), "
                  "CONVERT(varchar(12), ISNULL(e.ScheduleNo,0)), "
                  "CONVERT(varchar(24), ISNULL(e.SalesOrderLineId,"
                  "0)), e.ItemCode, ISNULL(e.Revision,''), "
                  "CONVERT(varchar(30), e.DespatchedQty), "
                  "ISNULL(e.UomCode,''), CONVERT(varchar(30), "
                  "e.StockQty), ISNULL(e.WarehouseCode,''), "
                  "ISNULL(e.LocationCode,''), ISNULL(e.LotCode,''), "
                  "ISNULL(e.SerialNo,''), CONVERT(varchar(24), "
                  "e.CogsGBP) FROM so.DespatchLine e WHERE "
                  "e.DespatchId = ? ORDER BY e.LineNum"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 1 TO SQLIO-PARM-CNT
           MOVE DS-DESPATCH-ID TO SQLIO-INT-EDIT
           MOVE FUNCTION TRIM(SQLIO-INT-EDIT) TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE 14 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 4100-EXIT
           END-IF
           MOVE 0 TO DS-LINE-CNT
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-AT-END
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-ROW
               IF SQLIO-ERROR OR SQLIO-NOTFOUND
                   MOVE "Y" TO WS-DONE
               ELSE
                   IF DS-LINE-CNT < 30
                       ADD 1 TO DS-LINE-CNT
                       MOVE DS-LINE-CNT TO WS-J
                       PERFORM 4150-ONE-LINE THRU 4150-EXIT
                   END-IF
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.
       4100-EXIT.
           EXIT.

       4150-ONE-LINE.
           COMPUTE DS-L-LINE-NUM(WS-J) = FUNCTION NUMVAL(WS-COL(1))
           COMPUTE DS-L-ORDER-LINE(WS-J) = FUNCTION NUMVAL(WS-COL(2))
           COMPUTE DS-L-SCHEDULE(WS-J) = FUNCTION NUMVAL(WS-COL(3))
           COMPUTE DS-L-SOL-ID(WS-J) = FUNCTION NUMVAL(WS-COL(4))
           MOVE WS-COL(5) TO DS-L-ITEM(WS-J)
           MOVE WS-COL(6) TO DS-L-REVISION(WS-J)
           COMPUTE DS-L-QTY(WS-J) = FUNCTION NUMVAL(WS-COL(7))
           MOVE WS-COL(8) TO DS-L-UOM(WS-J)
           COMPUTE DS-L-STOCK-QTY(WS-J) = FUNCTION NUMVAL(WS-COL(9))
           MOVE WS-COL(10) TO DS-L-WHSE(WS-J)
           MOVE WS-COL(11) TO DS-L-LOCATION(WS-J)
           MOVE WS-COL(12) TO DS-L-LOT(WS-J)
           MOVE WS-COL(13) TO DS-L-SERIAL(WS-J)
           COMPUTE DS-L-COGS-GBP(WS-J) = FUNCTION NUMVAL(WS-COL(14))
           MOVE "CONFIRMED" TO DS-L-STATUS(WS-J).
       4150-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       9000-REJECT.
           MOVE 8 TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA
           ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           MOVE "Y" TO WS-BAD.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION.
           MOVE 8 TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE "DSP0008" TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SPACES TO WS-TEXT
           STRING "DESPATCH does not support the function "
                  FUNCTION TRIM(CA-FUNCTION OF DFHCOMMAREA)
               DELIMITED BY SIZE INTO WS-TEXT
           MOVE WS-TEXT TO CA-MESSAGE OF DFHCOMMAREA.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED.
           MOVE 12 TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE "SQL0001" TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE OF DFHCOMMAREA
           ADD 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ROW.
       9300-EXIT.
           EXIT.

       9400-BEGIN.
           MOVE "BEGN" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
               GO TO 9400-EXIT
           END-IF
           MOVE "Y" TO WS-IN-TRAN.
       9400-EXIT.
           EXIT.

       9450-COMMIT.
           MOVE "COMT" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           MOVE "N" TO WS-IN-TRAN
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED THRU 9300-EXIT
           END-IF.
       9450-EXIT.
           EXIT.

      *>   A failed confirmation must leave nothing partly shipped:
      *>   no despatch row, no movement, no consumed reservation and
      *>   no journal.  ROLB is the only acceptable ending.
       9500-ROLLBACK.
           MOVE "ROLB" TO SQLIO-FUNC
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           CALL "SQLIO" USING SQLIO-CA WS-ROW
           MOVE "N" TO WS-IN-TRAN
           MOVE 0 TO DS-DESPATCH-ID DS-JOURNAL-ID
           MOVE 0 TO DS-COGS-GBP
           MOVE SPACES TO DS-JOURNAL-NO
           MOVE "ROLLEDBACK" TO DS-STATUS.
       9500-EXIT.
           EXIT.

      *>   COM-015.  The instruction is closed either way, so a later
      *>   attempt can tell a success from a failure from an
      *>   unresolved outcome.
       9600-IDEM-END.
           MOVE "N" TO WS-IDEM-OPEN
           MOVE "END" TO CA-FUNCTION OF WS-IDM-CA
           MOVE CA-CORRELATION-ID OF DFHCOMMAREA
               TO CA-CORRELATION-ID OF WS-IDM-CA
           MOVE WS-OPERATION TO ID-OPERATION
           IF CA-RETURN-CODE OF DFHCOMMAREA < 8
              AND DS-DESPATCH-REF NOT = SPACES
               MOVE "COMPLETED" TO ID-STATUS
               MOVE DS-DESPATCH-REF TO ID-RESULT-REF
           ELSE
               MOVE "FAILED" TO ID-STATUS
               MOVE SPACES TO ID-RESULT-REF
           END-IF
           MOVE CA-MESSAGE OF DFHCOMMAREA TO ID-RESULT-MESSAGE
           CALL "IDEMPOT" USING WS-IDM-CA
           MOVE "CONFIRM" TO CA-FUNCTION OF DFHCOMMAREA.
       9600-EXIT.
           EXIT.



