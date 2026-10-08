      *>================================================================
      *> PROGRAM-ID : SUBSTCHK
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : ITEM AND PART MAINTENANCE
      *> PURPOSE    : SUBSTITUTION AND EQUIVALENT REVISION RULES.  A
      *>              SUBSTITUTE IS A PROPOSAL WITH A STATED CONTEXT,
      *>              AN APPROVED QUANTITY RATIO, VALIDITY DATES AND
      *>              AN APPROVER.  IT NEVER SILENTLY REPLACES AN
      *>              ENGINEERING OR CUSTOMER SPECIFIED ITEM, AND
      *>              ACCEPTING ONE ON A KIT DOES NOT REWRITE THE
      *>              STANDARD BILL OF MATERIAL.
      *> CALLED BY  : KIT ISSUE, SALES ORDER ENTRY, PURCHASING,
      *>              THE .NET CLIENT
      *> INTERFACE  : CALL "SUBSTCHK" USING WS-SUBSTCHK-CA
      *>              COPYBOOK SUBSTCHKCA
      *> CALLS      : SQLIO (inv.ItemSubstitute, inv.Item,
      *>              inv.ItemRevision, inv.vw_StockAvailability),
      *>              ITMAUDIT
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1997-07-21 DJM  CR0721 ORIGINAL - APPROVED ALTERNATIVES
      *> 2005-11-30 KMP  CR1320 CUSTOMER AGREEMENT FLAG
      *> 2026-10-05 STERP ITM-019 CONTEXT, RATIO AND EVIDENCE
      *>================================================================
      *> REQ: ITM-019
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUBSTCHK.
       AUTHOR. D J MARSH.
       DATE-WRITTEN. 21/07/1997.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY ITMWORK.
       COPY ITMAUDTW.
       01  WS-CONTROL.
           05  WS-ERROR                PIC X VALUE "N".
               88  WS-INPUT-BAD                  VALUE "Y".
           05  WS-CTX-COLUMN           PIC X(16) VALUE SPACES.
           05  WS-SUB-LIFECYCLE        PIC X(10) VALUE SPACES.
           05  WS-SUB-CLASS            PIC X(12) VALUE SPACES.
           05  WS-SUB-DESC             PIC X(100) VALUE SPACES.
       LINKAGE SECTION.
       COPY SUBSTCHKCA.
       PROCEDURE DIVISION USING DFHCOMMAREA.
      *>----------------------------------------------------------------
       0000-MAIN SECTION.
       0000-ENTRY.
           PERFORM 0100-INITIALISE
           PERFORM 0200-VALIDATE
           IF WS-INPUT-BAD
               GO TO 0000-EXIT
           END-IF
           EVALUATE CA-FUNCTION
               WHEN "LIST"     PERFORM 1000-LIST
               WHEN "CHECK"    PERFORM 2000-CHECK
               WHEN "REVCHECK" PERFORM 3000-REVISION-CHECK
               WHEN "APPLY"    PERFORM 4000-APPLY
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           GOBACK.

      *>----------------------------------------------------------------
       0100-INITIALISE SECTION.
       0100-START.
           MOVE "N" TO WS-ERROR WS-SQL-FAILED
           MOVE "SUBSTCHK" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE "ITM-019" TO CA-REQ-REF
           MOVE 0 TO SB-LINE-CNT SB-QTY-SUBSTITUTE
           MOVE 0 TO SB-VALID-FROM SB-VALID-TO
           MOVE "N" TO SB-PERMITTED SB-INTERCHANGEABLE
           MOVE SPACES TO SB-APPROVED-BY SB-RESTRICTIONS
           MOVE "N" TO SB-NEEDS-AGREEMENT
           MOVE 0 TO SB-RATIO
           MOVE FUNCTION UPPER-CASE(SB-ITEM-CODE) TO SB-ITEM-CODE
           MOVE FUNCTION UPPER-CASE(SB-SUBSTITUTE) TO SB-SUBSTITUTE
           MOVE FUNCTION UPPER-CASE(SB-CONTEXT) TO SB-CONTEXT
           IF SB-CONTEXT = SPACES
               MOVE "KIT" TO SB-CONTEXT
           END-IF
           IF SB-AS-AT = 0
               MOVE CA-BUSINESS-DATE TO SB-AS-AT
           END-IF
           IF SB-AS-AT = 0
               MOVE 20261005 TO SB-AS-AT
           END-IF
           EVALUATE SB-CONTEXT
               WHEN "PURCHASE" MOVE "ContextPurchase" TO WS-CTX-COLUMN
               WHEN "SALES"    MOVE "ContextSales" TO WS-CTX-COLUMN
               WHEN "KIT"      MOVE "ContextKit" TO WS-CTX-COLUMN
               WHEN "BOM"      MOVE "ContextBom" TO WS-CTX-COLUMN
               WHEN OTHER      MOVE SPACES TO WS-CTX-COLUMN
           END-EVALUATE
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 24
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM.
       0100-EXIT.
           EXIT.

       0200-VALIDATE SECTION.
       0200-START.
           IF CA-COMPANY = SPACES OR SB-ITEM-CODE = SPACES
               MOVE "SUB0001" TO CA-REASON-CODE
               MOVE "A company and an item are required" TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 0200-EXIT
           END-IF
           IF WS-CTX-COLUMN = SPACES
               MOVE "SUB0002" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Context " FUNCTION TRIM(SB-CONTEXT)
                      " is not one of PURCHASE, SALES, KIT or BOM"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000  THE APPROVED SUBSTITUTES FOR THIS ITEM AND CONTEXT,
      *>       WITH THE FREE STOCK OF EACH SO THAT THE USER CAN SEE
      *>       WHETHER THE PROPOSAL IS WORTH MAKING.
      *>----------------------------------------------------------------
       1000-LIST SECTION.
       1000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT s.SubstituteItemCode, i.Description, "
                  "s.ConversionRatio, CONVERT(char(8), s.ValidFrom, "
                  "112), ISNULL(CONVERT(char(8), s.ValidTo, 112),''),"
                  " ISNULL(s.ApprovedBy,''), "
                  "s.RequiresCustomerAgreement, ISNULL((SELECT "
                  "SUM(v.QtyOnHand) FROM inv.vw_StockAvailability v "
                  "WHERE v.CompanyCode = s.CompanyCode AND v.ItemCode"
                  " = s.SubstituteItemCode AND v.OwnerType = 'OWNED' "
                  "AND v.QualityStatus = 'ACCEPTED' AND "
                  "v.IsAvailable = 1),0), i.LifecycleStatus FROM "
                  "inv.ItemSubstitute s JOIN inv.Item i ON "
                  "i.CompanyCode = s.CompanyCode AND i.ItemCode = "
                  "s.SubstituteItemCode WHERE s.CompanyCode = ? AND "
                  "s.ItemCode = ? AND s."
                  FUNCTION TRIM(WS-CTX-COLUMN)
                  " = 1 AND s.ValidFrom <= CONVERT(date, ?, 112) AND"
                  " (s.ValidTo IS NULL OR s.ValidTo >= CONVERT(date, "
                  "?, 112)) ORDER BY s.SubstituteItemCode"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE SB-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE SB-AS-AT TO WS-DATE-N
           MOVE WS-DATE-N TO SQLIO-PARM-VAL(3)
           MOVE WS-DATE-N TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE 9 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 1000-EXIT
           END-IF
           MOVE 0 TO SB-LINE-CNT
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-AT-END
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA ITM-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO WS-DONE
               ELSE
                   IF SB-LINE-CNT < 15
                       ADD 1 TO SB-LINE-CNT
                       MOVE ITM-COL(1) TO SB-L-ITEM(SB-LINE-CNT)
                       MOVE ITM-COL(2) TO SB-L-DESC(SB-LINE-CNT)
                       COMPUTE SB-L-RATIO(SB-LINE-CNT) =
                           FUNCTION NUMVAL(ITM-COL(3))
                       MOVE SB-CONTEXT TO SB-L-CONTEXTS(SB-LINE-CNT)
                       COMPUTE SB-L-FROM(SB-LINE-CNT) =
                           FUNCTION NUMVAL(ITM-COL(4))
                       IF ITM-COL(5) = SPACES
                           MOVE 0 TO SB-L-TO(SB-LINE-CNT)
                       ELSE
                           COMPUTE SB-L-TO(SB-LINE-CNT) =
                               FUNCTION NUMVAL(ITM-COL(5))
                       END-IF
                       MOVE ITM-COL(6) TO SB-L-APPROVER(SB-LINE-CNT)
                       IF ITM-COL(7)(1:1) = "1"
                           MOVE "Y" TO SB-L-AGREEMENT(SB-LINE-CNT)
                       ELSE
                           MOVE "N" TO SB-L-AGREEMENT(SB-LINE-CNT)
                       END-IF
                       COMPUTE SB-L-FREE-QTY(SB-LINE-CNT) =
                           FUNCTION NUMVAL(ITM-COL(8))
                   END-IF
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SB-LINE-CNT = 0
               MOVE 16 TO CA-RETURN-CODE
               MOVE "SUB0001" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "No approved substitute for "
                      FUNCTION TRIM(SB-ITEM-CODE) " in context "
                      FUNCTION TRIM(SB-CONTEXT)
                   DELIMITED BY SIZE INTO CA-MESSAGE
           ELSE
               MOVE SB-LINE-CNT TO WS-INT-ED
               MOVE SPACES TO CA-MESSAGE
               STRING FUNCTION TRIM(WS-INT-ED)
                      " approved substitute(s); each needs its own "
                      "decision"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF.
       1000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000  MAY THIS PARTICULAR SUBSTITUTE BE USED HERE?
      *>----------------------------------------------------------------
       2000-CHECK SECTION.
       2000-START.
           IF SB-SUBSTITUTE = SPACES
               MOVE "SUB0001" TO CA-REASON-CODE
               MOVE "Name the substitute being proposed" TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 2000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT s.ConversionRatio, CONVERT(char(8), "
                  "s.ValidFrom, 112), ISNULL(CONVERT(char(8), "
                  "s.ValidTo, 112),''), ISNULL(s.ApprovedBy,''), "
                  "s.RequiresCustomerAgreement, "
                  "ISNULL(s.Restrictions,''), s.ContextPurchase, "
                  "s.ContextSales, s.ContextKit, s.ContextBom, "
                  "i.LifecycleStatus, i.ItemClass, i.Description "
                  "FROM inv.ItemSubstitute s JOIN inv.Item i ON "
                  "i.CompanyCode = s.CompanyCode AND i.ItemCode = "
                  "s.SubstituteItemCode WHERE s.CompanyCode = ? AND "
                  "s.ItemCode = ? AND s.SubstituteItemCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE SB-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE SB-SUBSTITUTE TO SQLIO-PARM-VAL(3)
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 3
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE 13 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 2000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "SUB0001" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING FUNCTION TRIM(SB-SUBSTITUTE)
                      " is not an approved substitute for "
                      FUNCTION TRIM(SB-ITEM-CODE)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 2000-EXIT
           END-IF
           COMPUTE SB-RATIO = FUNCTION NUMVAL(ITM-COL(1))
           COMPUTE SB-VALID-FROM = FUNCTION NUMVAL(ITM-COL(2))
           IF ITM-COL(3) = SPACES
               MOVE 0 TO SB-VALID-TO
           ELSE
               COMPUTE SB-VALID-TO = FUNCTION NUMVAL(ITM-COL(3))
           END-IF
           MOVE ITM-COL(4) TO SB-APPROVED-BY
           IF ITM-COL(5)(1:1) = "1"
               MOVE "Y" TO SB-NEEDS-AGREEMENT
           ELSE
               MOVE "N" TO SB-NEEDS-AGREEMENT
           END-IF
           MOVE ITM-COL(6) TO SB-RESTRICTIONS
           MOVE ITM-COL(11) TO WS-SUB-LIFECYCLE
           MOVE ITM-COL(12) TO WS-SUB-CLASS
           MOVE ITM-COL(13) TO WS-SUB-DESC
           PERFORM 2100-CONTEXT-TEST
           IF CA-FAILED
               GO TO 2000-EXIT
           END-IF
           PERFORM 2200-DATE-TEST
           IF CA-FAILED
               GO TO 2000-EXIT
           END-IF
           PERFORM 2300-APPROVAL-TEST
           IF CA-FAILED
               GO TO 2000-EXIT
           END-IF
           COMPUTE SB-QTY-SUBSTITUTE ROUNDED =
               SB-QTY-REQUIRED * SB-RATIO
           MOVE "Y" TO SB-PERMITTED
           MOVE SB-QTY-SUBSTITUTE TO WS-QTY-ED
           MOVE SPACES TO WS-TEXT
           STRING FUNCTION TRIM(WS-QTY-ED) " of "
                  FUNCTION TRIM(SB-SUBSTITUTE) " replace "
               DELIMITED BY SIZE INTO WS-TEXT
           MOVE SB-QTY-REQUIRED TO WS-QTY-ED
           MOVE SPACES TO CA-MESSAGE
           STRING FUNCTION TRIM(WS-TEXT) FUNCTION TRIM(WS-QTY-ED)
                  " of " FUNCTION TRIM(SB-ITEM-CODE)
                  " on this document only"
               DELIMITED BY SIZE INTO CA-MESSAGE.
       2000-EXIT.
           EXIT.

       2100-CONTEXT-TEST SECTION.
       2100-START.
           MOVE "N" TO WS-FLAG
           EVALUATE SB-CONTEXT
               WHEN "PURCHASE" MOVE ITM-COL(7)(1:1) TO WS-FLAG
               WHEN "SALES"    MOVE ITM-COL(8)(1:1) TO WS-FLAG
               WHEN "KIT"      MOVE ITM-COL(9)(1:1) TO WS-FLAG
               WHEN "BOM"      MOVE ITM-COL(10)(1:1) TO WS-FLAG
           END-EVALUATE
           IF WS-FLAG NOT = "1"
               MOVE "SUB0002" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The substitution rule does not cover "
                      FUNCTION TRIM(SB-CONTEXT)
                      "; a separate approval is needed"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
           END-IF.
       2100-EXIT.
           EXIT.

       2200-DATE-TEST SECTION.
       2200-START.
           IF SB-VALID-FROM > SB-AS-AT
              OR (SB-VALID-TO > 0 AND SB-VALID-TO < SB-AS-AT)
               MOVE "SUB0003" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The substitution rule is not valid on that "
                      "date"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
           END-IF.
       2200-EXIT.
           EXIT.

       2300-APPROVAL-TEST SECTION.
       2300-START.
           IF SB-APPROVED-BY = SPACES
               MOVE "SUB0006" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The substitution rule has not been approved "
                      "by an engineering or quality authority"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 2300-EXIT
           END-IF
           IF WS-SUB-LIFECYCLE NOT = "RELEASED"
               MOVE "SUB0004" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Substitute " FUNCTION TRIM(SB-SUBSTITUTE)
                      " is " FUNCTION TRIM(WS-SUB-LIFECYCLE)
                      " and may not be used"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 2300-EXIT
           END-IF
      *>   CR1320.  A CUSTOMER SPECIFIED PART IS NEVER SWAPPED
      *>   WITHOUT THE CUSTOMER'S RECORDED AGREEMENT.
           IF SB-NEEDS-AGREEMENT = "Y"
              AND SB-AGREEMENT-REF = SPACES
               MOVE "SUB0005" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Substituting " FUNCTION TRIM(SB-SUBSTITUTE)
                      " needs the customer's recorded agreement "
                      "before it may be used"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE 6 TO CA-RETURN-CODE
               MOVE "Y" TO WS-ERROR
               ADD 1 TO CA-MSG-COUNT
           END-IF.
       2300-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000  EQUIVALENT REVISIONS.  AN INTERCHANGEABLE REVISION OF
      *>       THE SAME ITEM IS NOT A SUBSTITUTION, BUT IT STILL HAS
      *>       TO BE DECLARED INTERCHANGEABLE.
      *>----------------------------------------------------------------
       3000-REVISION-CHECK SECTION.
       3000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT Status, IsInterchangeable, CONVERT(char(8)"
                  ", EffectiveFrom, 112), ISNULL(CONVERT(char(8), "
                  "EffectiveTo, 112),'') FROM inv.ItemRevision WHERE"
                  " CompanyCode = ? AND ItemCode = ? AND Revision = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 3 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE SB-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE SB-SUBSTITUTE-REV TO SQLIO-PARM-VAL(3)
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 3
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE 4 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 3000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "SUB0007" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Revision " FUNCTION TRIM(SB-SUBSTITUTE-REV)
                      " of " FUNCTION TRIM(SB-ITEM-CODE)
                      " is not recorded"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE 16 TO CA-RETURN-CODE
               ADD 1 TO CA-MSG-COUNT
               GO TO 3000-EXIT
           END-IF
           COMPUTE SB-VALID-FROM = FUNCTION NUMVAL(ITM-COL(3))
           IF ITM-COL(4) = SPACES
               MOVE 0 TO SB-VALID-TO
           ELSE
               COMPUTE SB-VALID-TO = FUNCTION NUMVAL(ITM-COL(4))
           END-IF
           IF ITM-COL(2)(1:1) = "1"
               MOVE "Y" TO SB-INTERCHANGEABLE
               MOVE "Y" TO SB-PERMITTED
               MOVE 1 TO SB-RATIO
               MOVE SB-QTY-REQUIRED TO SB-QTY-SUBSTITUTE
               MOVE SPACES TO CA-MESSAGE
               STRING "Revision " FUNCTION TRIM(SB-SUBSTITUTE-REV)
                      " is interchangeable with the demanded "
                      "revision"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           ELSE
               MOVE "N" TO SB-INTERCHANGEABLE
               MOVE "SUB0007" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Revision " FUNCTION TRIM(SB-SUBSTITUTE-REV)
                      " is not declared interchangeable; it needs "
                      "an engineering decision"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
           END-IF.
       3000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 4000  RECORD AN ACCEPTED SUBSTITUTION AS EVIDENCE AGAINST THE
      *>       DOCUMENT.  THE STANDARD BILL IS NOT REWRITTEN.
      *>----------------------------------------------------------------
       4000-APPLY SECTION.
       4000-START.
           PERFORM 2000-CHECK
           IF CA-FAILED
               GO TO 4000-EXIT
           END-IF
           INITIALIZE ITA-BLOCK
           MOVE CA-COMPANY TO ITA-COMPANY
           MOVE CA-USER TO ITA-USER
           MOVE "inv.ItemSubstitute" TO ITA-ENTITY-TYPE
           MOVE SPACES TO ITA-ENTITY-KEY
           STRING FUNCTION TRIM(SB-ITEM-CODE) "/"
                  FUNCTION TRIM(SB-SUBSTITUTE)
               DELIMITED BY SIZE INTO ITA-ENTITY-KEY
           MOVE "SUBSTITUTE" TO ITA-ACTION
           MOVE SB-CONTEXT TO ITA-FIELD-NAME
           MOVE SB-ITEM-CODE TO ITA-OLD-VALUE
           MOVE SB-SUBSTITUTE TO ITA-NEW-VALUE
           MOVE SPACES TO ITA-REASON
           STRING "Accepted on " FUNCTION TRIM(SB-DOCUMENT-TYPE) " "
                  FUNCTION TRIM(SB-DOCUMENT-REF)
                  "; the standard bill is unchanged"
               DELIMITED BY SIZE INTO ITA-REASON
           MOVE SB-AGREEMENT-REF TO ITA-AUTHORITY-REF
           MOVE CA-CORRELATION-ID TO ITA-CORRELATION
           CALL "ITMAUDIT" USING ITA-BLOCK
           IF ITA-RC > 4
               MOVE "SUB0008" TO CA-REASON-CODE
               MOVE ITA-MSG TO CA-MESSAGE
               MOVE 12 TO CA-RETURN-CODE
               ADD 1 TO CA-MSG-COUNT
               GO TO 4000-EXIT
           END-IF
           MOVE SPACES TO CA-MESSAGE
           STRING "Substitution of " FUNCTION TRIM(SB-SUBSTITUTE)
                  " recorded against "
                  FUNCTION TRIM(SB-DOCUMENT-REF)
               DELIMITED BY SIZE INTO CA-MESSAGE.
       4000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       9000-REJECT SECTION.
       9000-START.
           MOVE "Y" TO WS-ERROR
           MOVE "N" TO SB-PERMITTED
           MOVE 8 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION SECTION.
       9100-START.
           MOVE "SUB0008" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "SUBSTCHK does not support function "
                  FUNCTION TRIM(CA-FUNCTION)
               DELIMITED BY SIZE INTO CA-MESSAGE
           PERFORM 9000-REJECT.
       9100-EXIT.
           EXIT.

       9300-SQL-FAILED SECTION.
       9300-START.
           MOVE "Y" TO WS-ERROR WS-SQL-FAILED
           MOVE SQLIO-ERRMSG(1:200) TO CA-MESSAGE
           IF SQLIO-ERRMSG(1:512) = SPACES
               MOVE "SQL failure in SUBSTCHK" TO CA-MESSAGE
           END-IF
           MOVE "SQL0001" TO CA-REASON-CODE
           MOVE 12 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9300-EXIT.
           EXIT.
