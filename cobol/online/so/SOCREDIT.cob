      *>================================================================
      *> PROGRAM-ID : SOCREDIT
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : SALES ORDER MANAGEMENT (DISTRIBUTION)
      *> PURPOSE    : CREDITS, CONCESSIONS AND REPLACEMENTS THAT
      *>              FOLLOW A CUSTOMER RETURN.
      *>              CREDITNOTE FOLLOWS INSPECTION AND ALWAYS TAKES
      *>              THE ALREADY CREDITED VALUE AND QUANTITY INTO
      *>              ACCOUNT, SO NOTHING IS CREDITED TWICE (CRD0012).
      *>              THE POSTING ITSELF BELONGS TO THE SALES LEDGER,
      *>              SO IT IS HANDED TO THE AR TEAM'S ARCRNOTE.
      *>              CONCESSION IS AN AUTHORISED VALUE ONLY PRICE
      *>              CONCESSION WITH NO STOCK EFFECT AT ALL.
      *>              REPLACE CREATES A NEW LINE WITH ITS OWN
      *>              TRACEABLE FULFILMENT OBLIGATION, ITS OWN COST
      *>              AND ITS OWN DESPATCH HISTORY, EVEN WHEN IT IS
      *>              SUPPLIED AT NO ADDITIONAL SELLING PRICE.  IT IS
      *>              NEVER A SILENT RE-DESPATCH OF THE ORIGINAL LINE.
      *> CALLED BY  : STERP DESKTOP CLIENT (SALES RETURNS AND CREDITS)
      *> INTERFACE  : CALL "SOCREDIT" USING DFHCOMMAREA.  THE COMMAREA
      *>              IS DECLARED IN THE LINKAGE SECTION BELOW; THERE
      *>              IS NO SEPARATE COPYBOOK BECAUSE THE LAYOUT IS
      *>              USED BY THIS PROGRAM AND THE CLIENT ONLY.
      *> CALLS      : ARCRNOTE (AR), NUMGEN, AUDITLOG, SQLIO
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1997-04-30 RJH  CR0618 SALES CREDITS AGAINST RETURNS
      *> 1999-11-30 KMP  Y2K - FOUR DIGIT YEARS ON CREDIT DATES
      *> 2006-11-13 DJM  CR1502 VALUE ONLY CONCESSIONS, NO STOCK
      *> 2026-10-06 STERP SO-018 SO-019 SO-027 NO DOUBLE CREDIT
      *>================================================================
      *> REQ: SO-018, SO-019, SO-027
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SOCREDIT.
       AUTHOR. R J HOLDEN.
       DATE-WRITTEN. 30/04/1997.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY ARCRNOTECA REPLACING ==DFHCOMMAREA== BY ==WS-CRN-CA==.
       COPY NUMGENCA REPLACING ==DFHCOMMAREA== BY ==WS-NUM-CA==.
       COPY AUDITLOGCA REPLACING ==DFHCOMMAREA== BY ==WS-AUDIT-CA==.
       01  WS-CONTROL.
           05  WS-FUNC                 PIC X(8).
           05  WS-CO                   PIC X(10).
           05  WS-SITE                 PIC X(10).
           05  WS-USER                 PIC X(20).
           05  WS-BDATE                PIC 9(8).
           05  WS-CORR                 PIC X(36).
           05  WS-ERROR                PIC X VALUE "N".
           05  WS-IN-TXN               PIC X VALUE "N".
           05  WS-RC                   PIC 9(2) VALUE 0.
           05  WS-REASON               PIC X(8).
           05  WS-MSG                  PIC X(200).
       01  WS-WORK.
           05  WS-I                    PIC S9(4) VALUE 0.
           05  WS-J                    PIC S9(4) VALUE 0.
           05  WS-N                    PIC S9(15)V9(6) VALUE 0.
           05  WS-QTY1                 PIC S9(13)V9(6) VALUE 0.
           05  WS-AMT1                 PIC S9(13)V99 VALUE 0.
           05  WS-AMT2                 PIC S9(13)V99 VALUE 0.
           05  WS-TOT-QTY              PIC S9(13)V9(6) VALUE 0.
           05  WS-TOT-NET              PIC S9(13)V99 VALUE 0.
           05  WS-TOT-VAT              PIC S9(13)V99 VALUE 0.
           05  WS-LINES-READ           PIC S9(4) VALUE 0.
           05  WS-ORDER-ID             PIC S9(17) VALUE 0.
           05  WS-LINE-ID              PIC S9(17) VALUE 0.
           05  WS-NEXT-LINE            PIC S9(4) VALUE 0.
           05  WS-QTY-ED               PIC -(9)9.9(6).
           05  WS-AMT-ED               PIC -(11)9.99.
           05  WS-INT-ED               PIC -(8)9.
       01  WS-HEAD-ROW.
           05  WH-RETURN-ID            PIC X(24).
           05  WH-CUSTOMER             PIC X(20).
           05  WH-STATUS               PIC X(12).
           05  WH-INVOICE              PIC X(30).
           05  WH-ORDER                PIC X(30).
           05  WH-SITE                 PIC X(10).
           05  WH-CREDIT-NOTE          PIC X(30).
       01  WS-LINE-ROW.
           05  WL-LINE-NUM             PIC X(12).
           05  WL-ITEM                 PIC X(40).
           05  WL-REVISION             PIC X(4).
           05  WL-UOM                  PIC X(10).
           05  WL-ACC-QTY              PIC X(24).
           05  WL-CRED-QTY             PIC X(24).
           05  WL-PRICE                PIC X(24).
           05  WL-VAT-CODE             PIC X(4).
           05  WL-DISPOSITION          PIC X(12).
       01  WS-ONE-COL.
           05  WS-C1                   PIC X(40).
           05  WS-C2                   PIC X(40).
       LINKAGE SECTION.
      *>----------------------------------------------------------------
      *> SOCREDIT COMMAREA.  CA-FUNCTION values
      *>   CREDNOTE    credit the inspected return, net of anything
      *>               already credited (CRD0012)
      *>   CONCESS     authorised value only price concession, no
      *>               stock effect of any kind
      *>   REPLACE     new demand with its own fulfilment obligation
      *> REASON CODES
      *>   CRD0001 customer or original document missing
      *>   CRD0002 return reference not known
      *>   CRD0003 the credit note follows inspection
      *>   CRD0004 nothing creditable on this return
      *>   CRD0005 a concession needs an authorised approval
      *>   CRD0006 a concession value or percentage is required
      *>   CRD0007 replacement item or quantity missing
      *>   CRD0008 the sales ledger refused the credit note
      *>   CRD0009 unsupported function
      *>   CRD0010 replacement order could not be numbered
      *>   CRD0012 already credited - that value or quantity cannot
      *>           be credited a second time
      *>----------------------------------------------------------------
       01  DFHCOMMAREA.
           COPY STCAHDR.
           05  SC-BODY.
               10  SC-RETURN-REF       PIC X(30).
               10  SC-RETURN-ID        PIC S9(17).
               10  SC-CUSTOMER         PIC X(20).
               10  SC-SITE             PIC X(10).
               10  SC-ORIG-INVOICE     PIC X(30).
               10  SC-ORIG-ORDER       PIC X(30).
               10  SC-RETURN-STATUS    PIC X(12).
               10  SC-CREDIT-NOTE      PIC X(30).
               10  SC-CREDIT-TYPE      PIC X(10).
               10  SC-REASON-CODE      PIC X(20).
               10  SC-REASON           PIC X(200).
               10  SC-DOC-DATE         PIC 9(8).
               10  SC-TAX-POINT        PIC 9(8).
               10  SC-POSTING-DATE     PIC 9(8).
               10  SC-CURRENCY         PIC X(3).
               10  SC-FX-RATE          PIC S9(9)V9(8) COMP-3.
               10  SC-APPROVAL-ID      PIC S9(9) COMP-5.
               10  SC-APPROVED-BY      PIC X(20).
      *>       a credit never returns stock and never reopens an order
               10  SC-RETURN-STOCK     PIC X.
               10  SC-STOCK-EFFECT     PIC X.
               10  SC-ORDER-REOPENED   PIC X.
               10  SC-OWN-OBLIGATION   PIC X.
               10  SC-ORIG-NET-GBP     PIC S9(11)V99 COMP-3.
               10  SC-PRIOR-CREDIT-GBP PIC S9(11)V99 COMP-3.
               10  SC-CREDITABLE-REM   PIC S9(11)V99 COMP-3.
               10  SC-PRIOR-CRED-QTY   PIC S9(13)V9(6) COMP-3.
               10  SC-CREDIT-QTY       PIC S9(13)V9(6) COMP-3.
               10  SC-NET              PIC S9(11)V99 COMP-3.
               10  SC-VAT              PIC S9(11)V99 COMP-3.
               10  SC-GROSS            PIC S9(11)V99 COMP-3.
               10  SC-NET-GBP          PIC S9(11)V99 COMP-3.
               10  SC-VAT-GBP          PIC S9(11)V99 COMP-3.
               10  SC-GROSS-GBP        PIC S9(11)V99 COMP-3.
               10  SC-CREDIT-ID        PIC S9(9) COMP-5.
               10  SC-OPEN-ITEM-ID     PIC S9(9) COMP-5.
               10  SC-JOURNAL-NO       PIC X(30).
               10  SC-AUDIT-ID         PIC S9(9) COMP-5.
      *>       concession - value only, never a quantity
               10  SC-CONCESSION-PCT   PIC S9(3)V9(4) COMP-3.
               10  SC-CONCESSION-AMT   PIC S9(11)V99 COMP-3.
               10  SC-CONCESSION-VAT   PIC X(4).
               10  SC-CONCESSION-RATE  PIC S9(3)V9(4) COMP-3.
      *>       replacement - its own demand, cost and despatch history
               10  SC-REPLACE-ORDER    PIC X(30).
               10  SC-REPLACE-LINE     PIC S9(9) COMP-5.
               10  SC-REPLACE-ITEM     PIC X(40).
               10  SC-REPLACE-REVISION PIC X(4).
               10  SC-REPLACE-DESC     PIC X(60).
               10  SC-REPLACE-QTY      PIC S9(9)V9(6) COMP-3.
               10  SC-REPLACE-UOM      PIC X(10).
               10  SC-REPLACE-PRICE    PIC S9(11)V9(6) COMP-3.
               10  SC-REPLACE-COST     PIC S9(11)V9(6) COMP-3.
               10  SC-REPLACE-SITE     PIC X(10).
               10  SC-PROMISE-DATE     PIC 9(8).
               10  SC-NIL-CHARGE       PIC X.
               10  SC-LINE-CNT         PIC 9(3).
               10  SC-LINE-TAB.
                   15  SC-LINES        OCCURS 20 TIMES.
                       20  SC-L-RET-LINE   PIC S9(9) COMP-5.
                       20  SC-L-ORIG-LINE  PIC S9(9) COMP-5.
                       20  SC-L-ITEM       PIC X(40).
                       20  SC-L-DESC       PIC X(60).
                       20  SC-L-QTY        PIC S9(9)V9(6) COMP-3.
                       20  SC-L-ACC-QTY    PIC S9(9)V9(6) COMP-3.
                       20  SC-L-PRIOR-QTY  PIC S9(9)V9(6) COMP-3.
                       20  SC-L-REM-QTY    PIC S9(9)V9(6) COMP-3.
                       20  SC-L-PRICE      PIC S9(11)V9(6) COMP-3.
                       20  SC-L-NET        PIC S9(11)V99 COMP-3.
                       20  SC-L-VAT-CODE   PIC X(4).
                       20  SC-L-VAT-RATE   PIC S9(3)V9(4) COMP-3.
                       20  SC-L-VAT-AMT    PIC S9(11)V99 COMP-3.
                       20  SC-L-MESSAGE    PIC X(80).
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 1000-INITIALISE THRU 1000-EXIT
           IF WS-ERROR = "N"
               EVALUATE WS-FUNC
                   WHEN "CREDNOTE"  
                       PERFORM 2000-CREDIT-NOTE THRU 2000-EXIT
                   WHEN "CONCESS"   
                       PERFORM 3000-CONCESSION THRU 3000-EXIT
                   WHEN "REPLACE"
                       PERFORM 4000-REPLACE THRU 4000-EXIT
                   WHEN OTHER
                       PERFORM 9100-BAD-FUNCTION THRU 9100-EXIT
               END-EVALUATE
           END-IF
           PERFORM 9400-REPORT THRU 9400-EXIT.
       0000-RETURN.
           GOBACK.
       0000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       1000-INITIALISE SECTION.
       1000-START.
           MOVE "N" TO WS-ERROR
           MOVE "N" TO WS-IN-TXN
           MOVE 0 TO WS-RC WS-I WS-J WS-LINES-READ
           MOVE 0 TO WS-TOT-QTY WS-TOT-NET WS-TOT-VAT
           MOVE SPACES TO WS-REASON WS-MSG
           MOVE "SOCREDIT" TO CA-PROGRAM OF DFHCOMMAREA
           MOVE 0 TO CA-RETURN-CODE OF DFHCOMMAREA
           MOVE 0 TO CA-MSG-COUNT OF DFHCOMMAREA
           MOVE SPACES TO CA-REASON-CODE OF DFHCOMMAREA
           MOVE SPACES TO CA-MESSAGE OF DFHCOMMAREA
           MOVE "SO-018" TO CA-REQ-REF OF DFHCOMMAREA
           MOVE FUNCTION UPPER-CASE(CA-FUNCTION OF DFHCOMMAREA)
               TO WS-FUNC
           MOVE CA-COMPANY OF DFHCOMMAREA TO WS-CO
           MOVE CA-SITE OF DFHCOMMAREA TO WS-SITE
           MOVE CA-USER OF DFHCOMMAREA TO WS-USER
           MOVE CA-BUSINESS-DATE OF DFHCOMMAREA TO WS-BDATE
           MOVE CA-CORRELATION-ID OF DFHCOMMAREA TO WS-CORR
      *>   nothing this program does touches physical stock
           MOVE "N" TO SC-RETURN-STOCK
           MOVE "N" TO SC-STOCK-EFFECT
           MOVE "N" TO SC-ORDER-REOPENED
           MOVE "N" TO SC-OWN-OBLIGATION
           IF WS-BDATE = 0
               MOVE 20261006 TO WS-BDATE
               MOVE WS-BDATE TO CA-BUSINESS-DATE OF DFHCOMMAREA
           END-IF
           IF SC-DOC-DATE = 0
               MOVE WS-BDATE TO SC-DOC-DATE
           END-IF
           IF SC-TAX-POINT = 0
               MOVE SC-DOC-DATE TO SC-TAX-POINT
           END-IF
           IF SC-POSTING-DATE = 0
               MOVE SC-DOC-DATE TO SC-POSTING-DATE
           END-IF
           IF SC-CURRENCY = SPACES
               MOVE "GBP" TO SC-CURRENCY
           END-IF
           IF SC-FX-RATE = 0
               MOVE 1 TO SC-FX-RATE
           END-IF
           IF SC-SITE = SPACES
               MOVE WS-SITE TO SC-SITE
           END-IF
           IF SC-LINE-CNT > 20
               MOVE 20 TO SC-LINE-CNT
           END-IF
           IF WS-CO = SPACES
               MOVE "CRD0001" TO WS-REASON
               MOVE "A company code is required" TO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       1000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 2000  CREDITNOTE.  THE CREDIT FOLLOWS INSPECTION (D-12), AND
      *>       IT IS LIMITED BY WHAT IS LEFT: THE RETURN LINE'S
      *>       ACCEPTED QUANTITY LESS WHAT HAS ALREADY BEEN CREDITED,
      *>       AND THE ORIGINAL INVOICE'S REMAINING CREDITABLE VALUE
      *>       AS THE SALES LEDGER SEES IT.
      *>----------------------------------------------------------------
       2000-CREDIT-NOTE SECTION.
       2000-START.
           IF SC-RETURN-REF = SPACES
               MOVE "CRD0001" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "A credit note must quote the return "
                      "authorisation it follows"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           PERFORM 8000-LOAD-RETURN THRU 8000-EXIT
           IF WS-ERROR = "Y"
               GO TO 2000-EXIT
           END-IF
           IF SC-RETURN-STATUS NOT = "INSPECTED" AND
              SC-RETURN-STATUS NOT = "CLOSED"
               MOVE "CRD0003" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "The credit note follows inspection; return "
                      FUNCTION TRIM(SC-RETURN-REF) " is "
                      FUNCTION TRIM(SC-RETURN-STATUS)
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2000-EXIT
           END-IF
           PERFORM 8100-LOAD-RETURN-LINES THRU 8100-EXIT
           IF WS-ERROR = "Y"
               GO TO 2000-EXIT
           END-IF
           PERFORM 2100-NET-OF-PRIOR THRU 2100-EXIT
           IF WS-ERROR = "Y"
               GO TO 2000-EXIT
           END-IF
           MOVE "LINE" TO SC-CREDIT-TYPE
           PERFORM 2200-ASK-LEDGER THRU 2200-EXIT
           IF WS-ERROR = "Y"
               GO TO 2000-EXIT
           END-IF
           PERFORM 2300-POST-CREDIT THRU 2300-EXIT
           IF WS-ERROR = "Y"
               GO TO 2000-EXIT
           END-IF
           PERFORM 2400-RECORD-CREDITED THRU 2400-EXIT
           PERFORM 8700-AUDIT THRU 8700-EXIT
           MOVE SC-NET-GBP TO WS-AMT-ED
           MOVE SPACES TO WS-MSG
           STRING "Credit note " FUNCTION TRIM(SC-CREDIT-NOTE)
                  " raised for £" FUNCTION TRIM(WS-AMT-ED)
                  " net; no stock has moved and the original order "
                  "has not been reopened"
               DELIMITED BY SIZE INTO WS-MSG
           MOVE WS-MSG TO CA-MESSAGE OF DFHCOMMAREA.
       2000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 2100  WHAT IS LEFT TO CREDIT, LINE BY LINE.  A SECOND CREDIT
      *>       FOR THE SAME UNITS IS REFUSED WITH CRD0012 - THE SPEC
      *>       SAYS ALREADY CREDITED VALUE AND QUANTITY ARE CONSIDERED.
      *>----------------------------------------------------------------
       2100-NET-OF-PRIOR SECTION.
       2100-START.
           MOVE 0 TO WS-TOT-QTY WS-TOT-NET WS-TOT-VAT
           MOVE 0 TO SC-PRIOR-CRED-QTY
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > SC-LINE-CNT
               COMPUTE WS-QTY1 =
                   SC-L-ACC-QTY(WS-I) - SC-L-PRIOR-QTY(WS-I)
               IF WS-QTY1 < 0
                   MOVE 0 TO WS-QTY1
               END-IF
               IF SC-L-QTY(WS-I) = 0
                   MOVE WS-QTY1 TO SC-L-QTY(WS-I)
               END-IF
               MOVE WS-QTY1 TO SC-L-REM-QTY(WS-I)
               ADD SC-L-PRIOR-QTY(WS-I) TO SC-PRIOR-CRED-QTY
               IF SC-L-QTY(WS-I) > WS-QTY1
                   MOVE "CRD0012" TO WS-REASON
                   MOVE SPACES TO WS-MSG
                   MOVE WS-QTY1 TO WS-QTY-ED
                   STRING "Line " FUNCTION TRIM(SC-L-ITEM(WS-I))
                          " has already been credited; only "
                          FUNCTION TRIM(WS-QTY-ED)
                          " remains creditable"
                       DELIMITED BY SIZE INTO WS-MSG
                   MOVE WS-MSG(1:80) TO SC-L-MESSAGE(WS-I)
                   PERFORM 9000-REJECT THRU 9000-EXIT
                   GO TO 2100-EXIT
               END-IF
               COMPUTE WS-AMT1 ROUNDED =
                   SC-L-QTY(WS-I) * SC-L-PRICE(WS-I)
               MOVE WS-AMT1 TO SC-L-NET(WS-I)
               COMPUTE WS-AMT2 ROUNDED =
                   WS-AMT1 * SC-L-VAT-RATE(WS-I) / 100
               MOVE WS-AMT2 TO SC-L-VAT-AMT(WS-I)
               ADD SC-L-QTY(WS-I) TO WS-TOT-QTY
               ADD WS-AMT1 TO WS-TOT-NET
               ADD WS-AMT2 TO WS-TOT-VAT
           END-PERFORM
           IF WS-TOT-QTY = 0 AND WS-TOT-NET = 0
               MOVE "CRD0012" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "Everything accepted on this return has "
                      "already been credited; nothing is creditable "
                      "a second time"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 2100-EXIT
           END-IF
           MOVE WS-TOT-QTY TO SC-CREDIT-QTY
           MOVE WS-TOT-NET TO SC-NET
           MOVE WS-TOT-VAT TO SC-VAT
           COMPUTE SC-GROSS = WS-TOT-NET + WS-TOT-VAT.
       2100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 2200  THE SALES LEDGER KNOWS WHAT IS LEFT ON THE INVOICE.
      *>       ASKING IT FIRST MEANS THE REFUSAL IS EXPLAINED BEFORE
      *>       ANYTHING IS POSTED.
      *>----------------------------------------------------------------
       2200-ASK-LEDGER SECTION.
       2200-START.
           IF SC-ORIG-INVOICE = SPACES
               GO TO 2200-EXIT
           END-IF
           PERFORM 8200-BUILD-CRN THRU 8200-EXIT
           MOVE "CHECK" TO CA-FUNCTION OF WS-CRN-CA
           CALL "ARCRNOTE" USING WS-CRN-CA
           IF CA-RETURN-CODE OF WS-CRN-CA > 4
               MOVE "CRD0008" TO WS-REASON
               MOVE CA-MESSAGE OF WS-CRN-CA TO WS-MSG
               MOVE CA-RETURN-CODE OF WS-CRN-CA TO WS-RC
               MOVE "Y" TO WS-ERROR
               GO TO 2200-EXIT
           END-IF
           MOVE AN-PRIOR-CREDIT TO SC-PRIOR-CREDIT-GBP
           MOVE AN-CREDITABLE-REM TO SC-CREDITABLE-REM
           MOVE AN-ORIG-NET-GBP TO SC-ORIG-NET-GBP
           IF SC-CREDITABLE-REM > 0 AND SC-NET > SC-CREDITABLE-REM
               MOVE "CRD0012" TO WS-REASON
               MOVE SC-CREDITABLE-REM TO WS-AMT-ED
               MOVE SPACES TO WS-MSG
               STRING "Only £" FUNCTION TRIM(WS-AMT-ED)
                      " remains creditable on invoice "
                      FUNCTION TRIM(SC-ORIG-INVOICE)
                      "; the rest has already been credited"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       2200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       2300-POST-CREDIT SECTION.
       2300-START.
           PERFORM 8200-BUILD-CRN THRU 8200-EXIT
           MOVE "POST" TO CA-FUNCTION OF WS-CRN-CA
           CALL "ARCRNOTE" USING WS-CRN-CA
           IF CA-RETURN-CODE OF WS-CRN-CA > 4
               MOVE "CRD0008" TO WS-REASON
               MOVE CA-MESSAGE OF WS-CRN-CA TO WS-MSG
               MOVE CA-RETURN-CODE OF WS-CRN-CA TO WS-RC
               MOVE "Y" TO WS-ERROR
               GO TO 2300-EXIT
           END-IF
           MOVE AN-CREDIT-NO TO SC-CREDIT-NOTE
           MOVE AN-CREDIT-ID TO SC-CREDIT-ID
           MOVE AN-OPEN-ITEM-ID TO SC-OPEN-ITEM-ID
           MOVE AN-JOURNAL-NO TO SC-JOURNAL-NO
           MOVE AN-NET TO SC-NET
           MOVE AN-VAT TO SC-VAT
           MOVE AN-GROSS TO SC-GROSS
           MOVE AN-NET-GBP TO SC-NET-GBP
           MOVE AN-VAT-GBP TO SC-VAT-GBP
           MOVE AN-GROSS-GBP TO SC-GROSS-GBP.
       2300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 2400  THE CREDITED QUANTITY IS WRITTEN BACK TO THE RETURN
      *>       LINES SO THE NEXT ATTEMPT SEES IT.  WITHOUT THIS THE
      *>       CRD0012 TEST WOULD HAVE NOTHING TO READ.
      *>----------------------------------------------------------------
       2400-RECORD-CREDITED SECTION.
       2400-START.
           PERFORM 8600-BEGIN THRU 8600-EXIT
           IF WS-ERROR = "Y"
               GO TO 2400-EXIT
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1
                   UNTIL WS-I > SC-LINE-CNT OR WS-ERROR = "Y"
               IF SC-L-QTY(WS-I) > 0
                   MOVE SPACES TO SQLIO-STMT
                   STRING "UPDATE so.ReturnLine SET CreditedQty = "
                          "CreditedQty + ? WHERE ReturnId = ? AND "
                          "LineNum = ?"
                       DELIMITED BY SIZE INTO SQLIO-STMT
                   MOVE SC-L-QTY(WS-I) TO SQLIO-NUM-EDIT
                   MOVE FUNCTION TRIM(SQLIO-NUM-EDIT)
                       TO SQLIO-PARM-VAL(1)
                   MOVE SC-RETURN-ID TO WS-INT-ED
                   MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(2)
                   MOVE SC-L-RET-LINE(WS-I) TO WS-INT-ED
                   MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(3)
                   MOVE 3 TO SQLIO-PARM-CNT
                   PERFORM 8650-NO-NULLS THRU 8650-EXIT
                   PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
               END-IF
           END-PERFORM
           IF WS-ERROR = "Y"
               PERFORM 8620-ROLLBACK THRU 8620-EXIT
               GO TO 2400-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.ReturnAuthorisation SET "
                  "CreditNoteNumber = ?, ReplacementDecision = "
                  "CASE WHEN ReplacementDecision IN ('NONE','') "
                  "THEN 'CREDIT' ELSE ReplacementDecision END, "
                  "ModifiedBy = ?, ModifiedAt = SYSDATETIME() "
                  "WHERE ReturnId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE SC-CREDIT-NOTE TO SQLIO-PARM-VAL(1)
           MOVE WS-USER TO SQLIO-PARM-VAL(2)
           MOVE SC-RETURN-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(3)
           MOVE 3 TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT
           IF WS-ERROR = "Y"
               PERFORM 8620-ROLLBACK THRU 8620-EXIT
           ELSE
               PERFORM 8610-COMMIT THRU 8610-EXIT
           END-IF.
       2400-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 3000  CONCESSION.  A PRICE CONCESSION IS AN AUTHORISED
      *>       REDUCTION IN WHAT THE CUSTOMER PAYS.  THE GOODS STAY
      *>       WITH THE CUSTOMER, SO THERE IS NO RETURN, NO MOVEMENT,
      *>       NO DISPOSITION AND NO EFFECT ON STOCK WHATSOEVER.
      *>       IT IS VALUE ONLY, AND IT NEEDS AN APPROVAL.
      *>----------------------------------------------------------------
       3000-CONCESSION SECTION.
       3000-START.
           IF SC-ORIG-INVOICE = SPACES
               MOVE "CRD0001" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "A concession must name the invoice whose "
                      "price is being conceded"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 3000-EXIT
           END-IF
           IF SC-APPROVAL-ID = 0 AND SC-APPROVED-BY = SPACES
               MOVE "CRD0005" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "A price concession needs an authorised "
                      "approval before it can be given"
                   DELIMITED BY SIZE INTO WS-MSG
               MOVE 6 TO WS-RC
               MOVE "Y" TO WS-ERROR
               GO TO 3000-EXIT
           END-IF
           MOVE "VALUE" TO SC-CREDIT-TYPE
           MOVE "N" TO SC-RETURN-STOCK
           MOVE "N" TO SC-STOCK-EFFECT
           MOVE 0 TO SC-CREDIT-QTY
           PERFORM 3100-CONCESSION-VALUE THRU 3100-EXIT
           IF WS-ERROR = "Y"
               GO TO 3000-EXIT
           END-IF
           PERFORM 2200-ASK-LEDGER THRU 2200-EXIT
           IF WS-ERROR = "Y"
               GO TO 3000-EXIT
           END-IF
           PERFORM 2300-POST-CREDIT THRU 2300-EXIT
           IF WS-ERROR = "Y"
               GO TO 3000-EXIT
           END-IF
           PERFORM 8700-AUDIT THRU 8700-EXIT
           MOVE SC-NET-GBP TO WS-AMT-ED
           MOVE SPACES TO WS-MSG
           STRING "Value-only concession of £"
                  FUNCTION TRIM(WS-AMT-ED)
                  " allowed on invoice "
                  FUNCTION TRIM(SC-ORIG-INVOICE)
                  "; the goods stay with the customer and no stock "
                  "has moved"
               DELIMITED BY SIZE INTO WS-MSG
           MOVE WS-MSG TO CA-MESSAGE OF DFHCOMMAREA.
       3000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       3100-CONCESSION-VALUE SECTION.
       3100-START.
           IF SC-CONCESSION-AMT = 0 AND SC-CONCESSION-PCT = 0
               MOVE "CRD0006" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "State the concession as a value or as a "
                      "percentage of the original net amount"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 3100-EXIT
           END-IF
           IF SC-CONCESSION-AMT = 0
               IF SC-ORIG-NET-GBP = 0
                   PERFORM 8300-READ-INVOICE-NET THRU 8300-EXIT
               END-IF
               COMPUTE WS-AMT1 ROUNDED =
                   SC-ORIG-NET-GBP * SC-CONCESSION-PCT / 100
               MOVE WS-AMT1 TO SC-CONCESSION-AMT
           END-IF
           IF SC-CONCESSION-AMT <= 0
               MOVE "CRD0006" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "The concession works out at nil; there is "
                      "nothing to allow"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 3100-EXIT
           END-IF
           IF SC-CONCESSION-VAT = SPACES
               MOVE "S" TO SC-CONCESSION-VAT
           END-IF
           IF SC-CONCESSION-RATE = 0 AND SC-CONCESSION-VAT = "S"
               MOVE 20 TO SC-CONCESSION-RATE
           END-IF
           MOVE SC-CONCESSION-AMT TO SC-NET
           COMPUTE WS-AMT2 ROUNDED =
               SC-CONCESSION-AMT * SC-CONCESSION-RATE / 100
           MOVE WS-AMT2 TO SC-VAT
           COMPUTE SC-GROSS = SC-NET + SC-VAT
      *>   one value line, no quantity: a concession is not a return
           MOVE 1 TO SC-LINE-CNT
           MOVE 0 TO SC-L-RET-LINE(1)
           MOVE 0 TO SC-L-QTY(1)
           MOVE 0 TO SC-L-ACC-QTY(1)
           MOVE 0 TO SC-L-PRIOR-QTY(1)
           MOVE 0 TO SC-L-REM-QTY(1)
           MOVE 0 TO SC-L-PRICE(1)
           MOVE SC-CONCESSION-AMT TO SC-L-NET(1)
           MOVE SC-CONCESSION-VAT TO SC-L-VAT-CODE(1)
           MOVE SC-CONCESSION-RATE TO SC-L-VAT-RATE(1)
           MOVE WS-AMT2 TO SC-L-VAT-AMT(1)
           MOVE SPACES TO SC-L-DESC(1)
           STRING "Agreed price concession, value only"
               DELIMITED BY SIZE INTO SC-L-DESC(1)
           MOVE SPACES TO SC-L-MESSAGE(1)
           STRING "No stock effect" DELIMITED BY SIZE
               INTO SC-L-MESSAGE(1).
       3100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
      *> 4000  REPLACE.  A REPLACEMENT IS NEW DEMAND.  IT GETS ITS OWN
      *>       ORDER LINE, ITS OWN SCHEDULE, ITS OWN COST AND ITS OWN
      *>       DESPATCH HISTORY, EVEN WHEN THE SELLING PRICE IS NIL.
      *>       THE ORIGINAL LINE IS NOT RE-DESPATCHED AND THE ORIGINAL
      *>       ORDER IS NOT REOPENED (SO-017, SO-018).
      *>----------------------------------------------------------------
       4000-REPLACE SECTION.
       4000-START.
           IF SC-REPLACE-ITEM = SPACES OR SC-REPLACE-QTY <= 0
               MOVE "CRD0007" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "A replacement needs an item and a quantity "
                      "of its own"
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 4000-EXIT
           END-IF
           IF SC-RETURN-REF NOT = SPACES
               PERFORM 8000-LOAD-RETURN THRU 8000-EXIT
               IF WS-ERROR = "Y"
                   GO TO 4000-EXIT
               END-IF
           END-IF
           IF SC-CUSTOMER = SPACES
               MOVE "CRD0001" TO WS-REASON
               MOVE "A replacement needs a customer" TO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
               GO TO 4000-EXIT
           END-IF
           IF SC-REPLACE-SITE = SPACES
               MOVE SC-SITE TO SC-REPLACE-SITE
           END-IF
           IF SC-PROMISE-DATE = 0
               MOVE WS-BDATE TO SC-PROMISE-DATE
           END-IF
           IF SC-REPLACE-PRICE = 0
               MOVE "Y" TO SC-NIL-CHARGE
           ELSE
               MOVE "N" TO SC-NIL-CHARGE
           END-IF
           PERFORM 8600-BEGIN THRU 8600-EXIT
           IF WS-ERROR = "Y"
               GO TO 4000-EXIT
           END-IF
           PERFORM 4100-FIND-OR-MAKE-ORDER THRU 4100-EXIT
           IF WS-ERROR = "Y"
               PERFORM 8620-ROLLBACK THRU 8620-EXIT
               GO TO 4000-EXIT
           END-IF
           PERFORM 4200-WRITE-LINE THRU 4200-EXIT
           IF WS-ERROR = "Y"
               PERFORM 8620-ROLLBACK THRU 8620-EXIT
               GO TO 4000-EXIT
           END-IF
           PERFORM 4300-WRITE-SCHEDULE THRU 4300-EXIT
           IF WS-ERROR = "Y"
               PERFORM 8620-ROLLBACK THRU 8620-EXIT
               GO TO 4000-EXIT
           END-IF
           IF SC-RETURN-ID > 0
               PERFORM 4400-LINK-RETURN THRU 4400-EXIT
           END-IF
           IF WS-ERROR = "Y"
               PERFORM 8620-ROLLBACK THRU 8620-EXIT
               GO TO 4000-EXIT
           END-IF
           PERFORM 8610-COMMIT THRU 8610-EXIT
           MOVE "Y" TO SC-OWN-OBLIGATION
           PERFORM 8700-AUDIT THRU 8700-EXIT
           MOVE SC-REPLACE-QTY TO WS-QTY-ED
           MOVE SC-REPLACE-LINE TO WS-INT-ED
           MOVE SPACES TO WS-MSG
           IF SC-NIL-CHARGE = "Y"
               STRING "Replacement line " FUNCTION TRIM(WS-INT-ED)
                      " on order " FUNCTION TRIM(SC-REPLACE-ORDER)
                      " for " FUNCTION TRIM(WS-QTY-ED)
                      " at no additional charge; it carries its own "
                      "cost and despatch obligation"
                   DELIMITED BY SIZE INTO WS-MSG
           ELSE
               STRING "Replacement line " FUNCTION TRIM(WS-INT-ED)
                      " on order " FUNCTION TRIM(SC-REPLACE-ORDER)
                      " for " FUNCTION TRIM(WS-QTY-ED)
                      " created with its own fulfilment obligation"
                   DELIMITED BY SIZE INTO WS-MSG
           END-IF
           MOVE WS-MSG TO CA-MESSAGE OF DFHCOMMAREA.
       4000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       4100-FIND-OR-MAKE-ORDER SECTION.
       4100-START.
           IF SC-REPLACE-ORDER NOT = SPACES
               MOVE SPACES TO SQLIO-STMT
               STRING "SELECT CAST(o.SalesOrderId AS varchar(24)), "
                      "CAST(ISNULL((SELECT MAX(l.LineNum) FROM "
                      "so.SalesOrderLine l WHERE l.SalesOrderId = "
                      "o.SalesOrderId),0) AS varchar(12)) FROM "
                      "so.SalesOrder o WHERE o.CompanyCode = ? AND "
                      "o.OrderNumber = ?"
                   DELIMITED BY SIZE INTO SQLIO-STMT
               MOVE WS-CO TO SQLIO-PARM-VAL(1)
               MOVE SC-REPLACE-ORDER TO SQLIO-PARM-VAL(2)
               MOVE 2 TO SQLIO-PARM-CNT
               PERFORM 8650-NO-NULLS THRU 8650-EXIT
               MOVE 2 TO SQLIO-COL-CNT
               MOVE 40 TO SQLIO-COL-WIDTH(1)
               MOVE 40 TO SQLIO-COL-WIDTH(2)
               MOVE 1 TO SQLIO-CURSOR
               MOVE "SNGL" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
               IF SQLIO-ERROR
                   PERFORM 8900-SQL-FAILED THRU 8900-EXIT
                   GO TO 4100-EXIT
               END-IF
               IF NOT SQLIO-NOTFOUND
                   COMPUTE WS-N = FUNCTION NUMVAL(WS-C1)
                   MOVE WS-N TO WS-ORDER-ID
                   COMPUTE WS-N = FUNCTION NUMVAL(WS-C2)
                   COMPUTE WS-NEXT-LINE = WS-N + 1
                   GO TO 4100-EXIT
               END-IF
           END-IF
           PERFORM 4150-DRAW-ORDER-NUMBER THRU 4150-EXIT
           IF WS-ERROR = "Y"
               GO TO 4100-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT so.SalesOrder (CompanyCode, SiteCode, "
                  "OrderNumber, OrderType, CustomerCode, "
                  "CustomerOrderRef, OrderDate, CurrencyCode, "
                  "CommercialStatus, ReleaseStatus, BillingStatus, "
                  "CorrelationId, CreatedBy) VALUES (?, ?, ?, "
                  "'STANDARD', ?, NULLIF(?,''), CONVERT(date, ?, "
                  "112), ?, 'ACCEPTED', 'RELEASED', 'NOTELIGIBLE', "
                  "NULLIF(?,''), ?); SELECT "
                  "CAST(SCOPE_IDENTITY() AS varchar(24)), '0'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE SC-REPLACE-SITE TO SQLIO-PARM-VAL(2)
           MOVE SC-REPLACE-ORDER TO SQLIO-PARM-VAL(3)
           MOVE SC-CUSTOMER TO SQLIO-PARM-VAL(4)
           MOVE SC-RETURN-REF TO SQLIO-PARM-VAL(5)
           MOVE SC-DOC-DATE TO SQLIO-PARM-VAL(6)
           MOVE SC-CURRENCY TO SQLIO-PARM-VAL(7)
           MOVE WS-CORR TO SQLIO-PARM-VAL(8)
           MOVE WS-USER TO SQLIO-PARM-VAL(9)
           MOVE 9 TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 40 TO SQLIO-COL-WIDTH(1)
           MOVE 40 TO SQLIO-COL-WIDTH(2)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 8900-SQL-FAILED THRU 8900-EXIT
               GO TO 4100-EXIT
           END-IF
           COMPUTE WS-N = FUNCTION NUMVAL(WS-C1)
           MOVE WS-N TO WS-ORDER-ID
           MOVE 1 TO WS-NEXT-LINE.
       4100-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       4150-DRAW-ORDER-NUMBER SECTION.
       4150-START.
           INITIALIZE WS-NUM-CA
           MOVE "NEXT" TO CA-FUNCTION OF WS-NUM-CA
           MOVE WS-CO TO CA-COMPANY OF WS-NUM-CA
           MOVE WS-USER TO CA-USER OF WS-NUM-CA
           MOVE WS-CORR TO CA-CORRELATION-ID OF WS-NUM-CA
           MOVE SC-DOC-DATE TO CA-BUSINESS-DATE OF WS-NUM-CA
           MOVE "SORD" TO NG-SERIES-CODE
           CALL "NUMGEN" USING WS-NUM-CA
           IF CA-RETURN-CODE OF WS-NUM-CA > 4
               MOVE "CRD0010" TO WS-REASON
               MOVE CA-MESSAGE OF WS-NUM-CA TO WS-MSG
               MOVE CA-RETURN-CODE OF WS-NUM-CA TO WS-RC
               MOVE "Y" TO WS-ERROR
               GO TO 4150-EXIT
           END-IF
           MOVE NG-DOCUMENT-NUMBER TO SC-REPLACE-ORDER.
       4150-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       4200-WRITE-LINE SECTION.
       4200-START.
           IF SC-REPLACE-LINE > 0
               MOVE SC-REPLACE-LINE TO WS-NEXT-LINE
           ELSE
               MOVE WS-NEXT-LINE TO SC-REPLACE-LINE
           END-IF
           IF SC-REPLACE-DESC = SPACES
               MOVE SPACES TO SC-REPLACE-DESC
               STRING "Replacement for return "
                      FUNCTION TRIM(SC-RETURN-REF)
                   DELIMITED BY SIZE INTO SC-REPLACE-DESC
           END-IF
           COMPUTE WS-AMT1 ROUNDED =
               SC-REPLACE-QTY * SC-REPLACE-PRICE
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT so.SalesOrderLine (SalesOrderId, "
                  "CompanyCode, OrderNumber, LineNum, LineRef, "
                  "ItemCode, Revision, Description, LineType, "
                  "AcceptedQty, UomCode, UnitPrice, NetAmount, "
                  "CostSnapshotGBP, CostBasis, PriceSource, "
                  "SupplyType, Status) VALUES (?, ?, ?, ?, ?, ?, "
                  "NULLIF(?,''), ?, 'STOCK', ?, NULLIF(?,''), ?, ?, "
                  "NULLIF(?,'0'), 'REPLACEMENT', 'REPLACEMENT', "
                  "'STOCK', 'OPEN'); SELECT CAST(SCOPE_IDENTITY() "
                  "AS varchar(24)), '0'"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-ORDER-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(1)
           MOVE WS-CO TO SQLIO-PARM-VAL(2)
           MOVE SC-REPLACE-ORDER TO SQLIO-PARM-VAL(3)
           MOVE WS-NEXT-LINE TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(4)
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(5)
           MOVE SC-REPLACE-ITEM TO SQLIO-PARM-VAL(6)
           MOVE SC-REPLACE-REVISION TO SQLIO-PARM-VAL(7)
           MOVE SC-REPLACE-DESC TO SQLIO-PARM-VAL(8)
           MOVE SC-REPLACE-QTY TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(9)
           MOVE SC-REPLACE-UOM TO SQLIO-PARM-VAL(10)
           MOVE SC-REPLACE-PRICE TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(11)
           MOVE WS-AMT1 TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(12)
           MOVE SC-REPLACE-COST TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(13)
           MOVE 13 TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 40 TO SQLIO-COL-WIDTH(1)
           MOVE 40 TO SQLIO-COL-WIDTH(2)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 8900-SQL-FAILED THRU 8900-EXIT
               GO TO 4200-EXIT
           END-IF
           COMPUTE WS-N = FUNCTION NUMVAL(WS-C1)
           MOVE WS-N TO WS-LINE-ID.
       4200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       4300-WRITE-SCHEDULE SECTION.
       4300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "INSERT so.DeliverySchedule (SalesOrderLineId, "
                  "CompanyCode, OrderNumber, LineNum, ScheduleNo, "
                  "Quantity, SiteCode, RequestedDate, "
                  "OriginalPromiseDate, CurrentPromiseDate, "
                  "ChangeReason, FulfilmentMethod, Status) VALUES "
                  "(?, ?, ?, ?, 1, ?, ?, CONVERT(date, ?, 112), "
                  "CONVERT(date, ?, 112), CONVERT(date, ?, 112), ?, "
                  "'WAREHOUSE', 'OPEN')"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-LINE-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(1)
           MOVE WS-CO TO SQLIO-PARM-VAL(2)
           MOVE SC-REPLACE-ORDER TO SQLIO-PARM-VAL(3)
           MOVE WS-NEXT-LINE TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(4)
           MOVE SC-REPLACE-QTY TO SQLIO-NUM-EDIT
           MOVE FUNCTION TRIM(SQLIO-NUM-EDIT) TO SQLIO-PARM-VAL(5)
           MOVE SC-REPLACE-SITE TO SQLIO-PARM-VAL(6)
           MOVE SC-PROMISE-DATE TO SQLIO-PARM-VAL(7)
           MOVE SC-PROMISE-DATE TO SQLIO-PARM-VAL(8)
           MOVE SC-PROMISE-DATE TO SQLIO-PARM-VAL(9)
           MOVE SPACES TO SQLIO-PARM-VAL(10)
           STRING "Replacement raised from return "
                  FUNCTION TRIM(SC-RETURN-REF)
               DELIMITED BY SIZE INTO SQLIO-PARM-VAL(10)
           MOVE 10 TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT.
       4300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       4400-LINK-RETURN SECTION.
       4400-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "UPDATE so.ReturnAuthorisation SET "
                  "ReplacementDecision = 'REPLACE', "
                  "ReplacementOrderNumber = ?, ModifiedBy = ?, "
                  "ModifiedAt = SYSDATETIME() WHERE ReturnId = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE SC-REPLACE-ORDER TO SQLIO-PARM-VAL(1)
           MOVE WS-USER TO SQLIO-PARM-VAL(2)
           MOVE SC-RETURN-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(3)
           MOVE 3 TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           PERFORM 8500-EXEC-UPDATE THRU 8500-EXIT.
       4400-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8000-LOAD-RETURN SECTION.
       8000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(ReturnId AS varchar(24)), "
                  "CustomerCode, Status, "
                  "ISNULL(OriginalInvoiceNumber,''), "
                  "ISNULL(OriginalOrderNumber,''), SiteCode, "
                  "ISNULL(CreditNoteNumber,'') FROM "
                  "so.ReturnAuthorisation WHERE CompanyCode = ? AND "
                  "ReturnRef = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE SC-RETURN-REF TO SQLIO-PARM-VAL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           MOVE 7 TO SQLIO-COL-CNT
           MOVE 24 TO SQLIO-COL-WIDTH(1)
           MOVE 20 TO SQLIO-COL-WIDTH(2)
           MOVE 12 TO SQLIO-COL-WIDTH(3)
           MOVE 30 TO SQLIO-COL-WIDTH(4)
           MOVE 30 TO SQLIO-COL-WIDTH(5)
           MOVE 10 TO SQLIO-COL-WIDTH(6)
           MOVE 30 TO SQLIO-COL-WIDTH(7)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-HEAD-ROW
           IF SQLIO-ERROR
               PERFORM 8900-SQL-FAILED THRU 8900-EXIT
               GO TO 8000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE 16 TO WS-RC
               MOVE "Y" TO WS-ERROR
               MOVE "CRD0002" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "Return " FUNCTION TRIM(SC-RETURN-REF)
                      " is not held for this company"
                   DELIMITED BY SIZE INTO WS-MSG
               GO TO 8000-EXIT
           END-IF
           COMPUTE WS-N = FUNCTION NUMVAL(WH-RETURN-ID)
           MOVE WS-N TO SC-RETURN-ID
           MOVE WH-CUSTOMER TO SC-CUSTOMER
           MOVE WH-STATUS TO SC-RETURN-STATUS
           MOVE WH-SITE TO SC-SITE
           IF SC-ORIG-INVOICE = SPACES
               MOVE WH-INVOICE TO SC-ORIG-INVOICE
           END-IF
           IF SC-ORIG-ORDER = SPACES
               MOVE WH-ORDER TO SC-ORIG-ORDER
           END-IF.
       8000-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8100-LOAD-RETURN-LINES SECTION.
       8100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(LineNum AS varchar(12)), ItemCode, "
                  "ISNULL(Revision,''), ISNULL(UomCode,''), "
                  "CAST(AcceptedQty AS varchar(24)), "
                  "CAST(CreditedQty AS varchar(24)), "
                  "ISNULL(CAST(CreditUnitPrice AS varchar(24)),"
                  "'0'), ISNULL(VatCode,'S'), "
                  "ISNULL(Disposition,'') FROM so.ReturnLine "
                  "WHERE ReturnId = ? ORDER BY LineNum"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE SC-RETURN-ID TO WS-INT-ED
           MOVE FUNCTION TRIM(WS-INT-ED) TO SQLIO-PARM-VAL(1)
           MOVE 1 TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           MOVE 9 TO SQLIO-COL-CNT
           MOVE 12 TO SQLIO-COL-WIDTH(1)
           MOVE 40 TO SQLIO-COL-WIDTH(2)
           MOVE 4 TO SQLIO-COL-WIDTH(3)
           MOVE 10 TO SQLIO-COL-WIDTH(4)
           MOVE 24 TO SQLIO-COL-WIDTH(5)
           MOVE 24 TO SQLIO-COL-WIDTH(6)
           MOVE 24 TO SQLIO-COL-WIDTH(7)
           MOVE 4 TO SQLIO-COL-WIDTH(8)
           MOVE 12 TO SQLIO-COL-WIDTH(9)
           MOVE 2 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-LINE-ROW
           IF SQLIO-ERROR
               PERFORM 8900-SQL-FAILED THRU 8900-EXIT
               GO TO 8100-EXIT
           END-IF
           MOVE 0 TO WS-LINES-READ
           PERFORM 8150-FETCH-LINE THRU 8150-EXIT
               UNTIL SQLIO-NOTFOUND OR SQLIO-ERROR
                     OR WS-LINES-READ >= 20
           MOVE 2 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-LINE-ROW
           MOVE WS-LINES-READ TO SC-LINE-CNT
           IF SC-LINE-CNT = 0
               MOVE "CRD0004" TO WS-REASON
               MOVE SPACES TO WS-MSG
               STRING "No inspected lines are held on return "
                      FUNCTION TRIM(SC-RETURN-REF)
                   DELIMITED BY SIZE INTO WS-MSG
               PERFORM 9000-REJECT THRU 9000-EXIT
           END-IF.
       8100-EXIT.
           EXIT.

       8150-FETCH-LINE SECTION.
       8150-START.
           MOVE 2 TO SQLIO-CURSOR
           MOVE "FTCH" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-LINE-ROW
           IF SQLIO-NOTFOUND OR SQLIO-ERROR
               GO TO 8150-EXIT
           END-IF
           ADD 1 TO WS-LINES-READ
           MOVE WS-LINES-READ TO WS-J
           COMPUTE WS-N = FUNCTION NUMVAL(WL-LINE-NUM)
           MOVE WS-N TO SC-L-RET-LINE(WS-J)
           MOVE WL-ITEM TO SC-L-ITEM(WS-J)
           IF SC-L-DESC(WS-J) = SPACES
               MOVE WL-ITEM TO SC-L-DESC(WS-J)
           END-IF
           COMPUTE WS-N = FUNCTION NUMVAL(WL-ACC-QTY)
           MOVE WS-N TO SC-L-ACC-QTY(WS-J)
           COMPUTE WS-N = FUNCTION NUMVAL(WL-CRED-QTY)
           MOVE WS-N TO SC-L-PRIOR-QTY(WS-J)
           COMPUTE WS-N = FUNCTION NUMVAL(WL-PRICE)
           IF SC-L-PRICE(WS-J) = 0
               MOVE WS-N TO SC-L-PRICE(WS-J)
           END-IF
           IF SC-L-VAT-CODE(WS-J) = SPACES
               MOVE WL-VAT-CODE TO SC-L-VAT-CODE(WS-J)
           END-IF
           IF SC-L-VAT-RATE(WS-J) = 0
               IF SC-L-VAT-CODE(WS-J) = "S"
                   MOVE 20 TO SC-L-VAT-RATE(WS-J)
               END-IF
               IF SC-L-VAT-CODE(WS-J) = "R"
                   MOVE 5 TO SC-L-VAT-RATE(WS-J)
               END-IF
           END-IF.
       8150-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8200-BUILD-CRN SECTION.
       8200-START.
           INITIALIZE WS-CRN-CA
           MOVE WS-CO TO CA-COMPANY OF WS-CRN-CA
           MOVE SC-SITE TO CA-SITE OF WS-CRN-CA
           MOVE WS-USER TO CA-USER OF WS-CRN-CA
           MOVE SC-DOC-DATE TO CA-BUSINESS-DATE OF WS-CRN-CA
           MOVE WS-CORR TO CA-CORRELATION-ID OF WS-CRN-CA
           MOVE SC-ORIG-INVOICE TO AN-ORIG-INVOICE
           MOVE SC-CUSTOMER TO AN-CUSTOMER
           MOVE SC-CREDIT-TYPE TO AN-CREDIT-TYPE
           MOVE SC-REASON-CODE TO AN-REASON-CODE
           MOVE SC-REASON TO AN-REASON
           MOVE SC-DOC-DATE TO AN-DOC-DATE
           MOVE SC-TAX-POINT TO AN-TAX-POINT
           MOVE SC-POSTING-DATE TO AN-POSTING-DATE
           MOVE SC-CURRENCY TO AN-CURRENCY
           MOVE SC-FX-RATE TO AN-FX-RATE
      *>   SOCREDIT never asks the sales ledger to move stock; the
      *>   goods were dealt with by RETURNS and its dispositions
           MOVE "N" TO AN-RETURN-STOCK
           MOVE SC-APPROVAL-ID TO AN-APPROVAL-ID
           MOVE SC-APPROVED-BY TO AN-APPROVED-BY
           MOVE SC-LINE-CNT TO AN-LINE-CNT
           PERFORM VARYING WS-J FROM 1 BY 1
                   UNTIL WS-J > SC-LINE-CNT OR WS-J > 20
               MOVE SC-L-ORIG-LINE(WS-J) TO AN-L-ORIG-LINE(WS-J)
               MOVE SC-L-ITEM(WS-J) TO AN-L-ITEM(WS-J)
               MOVE SC-L-DESC(WS-J) TO AN-L-DESC(WS-J)
               MOVE SC-L-QTY(WS-J) TO AN-L-QTY(WS-J)
               MOVE SC-L-NET(WS-J) TO AN-L-NET(WS-J)
               MOVE SC-L-VAT-CODE(WS-J) TO AN-L-VAT-CODE(WS-J)
               MOVE SC-L-VAT-RATE(WS-J) TO AN-L-VAT-RATE(WS-J)
               MOVE SC-L-VAT-AMT(WS-J) TO AN-L-VAT-AMT(WS-J)
           END-PERFORM.
       8200-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8300-READ-INVOICE-NET SECTION.
       8300-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT CAST(NetAmountGBP AS varchar(24)), "
                  "ISNULL(CAST(CreditableRemainingGBP AS "
                  "varchar(24)),'0') FROM ar.SalesInvoice WHERE "
                  "CompanyCode = ? AND DocumentType = 'INVOICE' AND "
                  "InvoiceNumber = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE WS-CO TO SQLIO-PARM-VAL(1)
           MOVE SC-ORIG-INVOICE TO SQLIO-PARM-VAL(2)
           MOVE 2 TO SQLIO-PARM-CNT
           PERFORM 8650-NO-NULLS THRU 8650-EXIT
           MOVE 2 TO SQLIO-COL-CNT
           MOVE 40 TO SQLIO-COL-WIDTH(1)
           MOVE 40 TO SQLIO-COL-WIDTH(2)
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR OR SQLIO-NOTFOUND
               GO TO 8300-EXIT
           END-IF
           COMPUTE WS-N = FUNCTION NUMVAL(WS-C1)
           MOVE WS-N TO SC-ORIG-NET-GBP
           COMPUTE WS-N = FUNCTION NUMVAL(WS-C2)
           MOVE WS-N TO SC-CREDITABLE-REM.
       8300-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8500-EXEC-UPDATE SECTION.
       8500-START.
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 8900-SQL-FAILED THRU 8900-EXIT
           END-IF
           MOVE 1 TO SQLIO-CURSOR
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL.
       8500-EXIT.
           EXIT.

       8600-BEGIN SECTION.
       8600-START.
           MOVE "BEGN" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
           IF SQLIO-ERROR
               PERFORM 8900-SQL-FAILED THRU 8900-EXIT
           ELSE
               MOVE "Y" TO WS-IN-TXN
           END-IF.
       8600-EXIT.
           EXIT.

       8610-COMMIT SECTION.
       8610-START.
           IF WS-IN-TXN = "Y"
               MOVE "COMT" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
               MOVE "N" TO WS-IN-TXN
           END-IF.
       8610-EXIT.
           EXIT.

       8620-ROLLBACK SECTION.
       8620-START.
           IF WS-IN-TXN = "Y"
               MOVE "ROLB" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA WS-ONE-COL
               MOVE "N" TO WS-IN-TXN
           END-IF.
       8620-EXIT.
           EXIT.

       8650-NO-NULLS SECTION.
       8650-START.
           PERFORM VARYING WS-J FROM 1 BY 1
                   UNTIL WS-J > SQLIO-PARM-CNT
               MOVE "N" TO SQLIO-PARM-NULL(WS-J)
           END-PERFORM.
       8650-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8700-AUDIT SECTION.
       8700-START.
           INITIALIZE WS-AUDIT-CA
           MOVE "WRITE" TO CA-FUNCTION OF WS-AUDIT-CA
           MOVE WS-CO TO CA-COMPANY OF WS-AUDIT-CA
           MOVE SC-SITE TO CA-SITE OF WS-AUDIT-CA
           MOVE WS-USER TO CA-USER OF WS-AUDIT-CA
           MOVE WS-BDATE TO CA-BUSINESS-DATE OF WS-AUDIT-CA
           MOVE WS-CORR TO CA-CORRELATION-ID OF WS-AUDIT-CA
           MOVE "so.ReturnAuthorisation" TO AU-ENTITY-TYPE
           IF SC-RETURN-REF NOT = SPACES
               MOVE SC-RETURN-REF TO AU-ENTITY-KEY
           ELSE
               MOVE SC-ORIG-INVOICE TO AU-ENTITY-KEY
           END-IF
           MOVE WS-FUNC TO AU-ACTION
           MOVE "CreditNoteNumber" TO AU-FIELD-NAME
           IF WS-FUNC = "REPLACE"
               MOVE "ReplacementOrderNumber" TO AU-FIELD-NAME
               MOVE SC-REPLACE-ORDER TO AU-NEW-VALUE
           ELSE
               MOVE SC-CREDIT-NOTE TO AU-NEW-VALUE
           END-IF
           MOVE SC-REASON TO AU-REASON
           MOVE "SOCREDIT" TO AU-SOURCE
           MOVE "N" TO AU-SENSITIVE
           IF SC-APPROVAL-ID > 0
               MOVE SC-APPROVAL-ID TO WS-INT-ED
               MOVE FUNCTION TRIM(WS-INT-ED) TO AU-AUTHORITY-REF
           END-IF
           CALL "AUDITLOG" USING WS-AUDIT-CA
           MOVE AU-AUDIT-ID TO SC-AUDIT-ID.
       8700-EXIT.
           EXIT.
      *>----------------------------------------------------------------
       8900-SQL-FAILED SECTION.
       8900-START.
           MOVE "Y" TO WS-ERROR
           MOVE 12 TO WS-RC
           MOVE "SQL0001" TO WS-REASON
           MOVE SQLIO-ERRMSG(1:200) TO WS-MSG
           IF WS-MSG = SPACES
               MOVE "The database refused the request" TO WS-MSG
           END-IF.
       8900-EXIT.
           EXIT.

       9000-REJECT SECTION.
       9000-START.
           MOVE "Y" TO WS-ERROR
           IF WS-RC < 8
               MOVE 8 TO WS-RC
           END-IF.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION SECTION.
       9100-START.
           MOVE "CRD0009" TO WS-REASON
           MOVE SPACES TO WS-MSG
           STRING "Function " FUNCTION TRIM(WS-FUNC)
                  " is not supported by SOCREDIT; use CREDITNOTE, "
                  "CONCESSION or REPLACE"
               DELIMITED BY SIZE INTO WS-MSG
           PERFORM 9000-REJECT THRU 9000-EXIT.
       9100-EXIT.
           EXIT.

       9400-REPORT SECTION.
       9400-START.
           PERFORM 8620-ROLLBACK THRU 8620-EXIT
           MOVE WS-RC TO CA-RETURN-CODE OF DFHCOMMAREA
           IF WS-REASON NOT = SPACES
               MOVE WS-REASON TO CA-REASON-CODE OF DFHCOMMAREA
           END-IF
           IF WS-MSG NOT = SPACES
               MOVE WS-MSG TO CA-MESSAGE OF DFHCOMMAREA
           END-IF
           IF CA-MESSAGE OF DFHCOMMAREA NOT = SPACES
               MOVE 1 TO CA-MSG-COUNT OF DFHCOMMAREA
           END-IF
           MOVE "N" TO SC-STOCK-EFFECT
           MOVE "N" TO SC-RETURN-STOCK
           MOVE "N" TO SC-ORDER-REOPENED.
       9400-EXIT.
           EXIT.
