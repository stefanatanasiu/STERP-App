      *>================================================================
      *> PROGRAM-ID : ITMCOMPL
      *> SYSTEM     : STERP - Stefan's ERP
      *> MODULE     : ITEM AND PART MAINTENANCE
      *> PURPOSE    : UK VAT CLASSIFICATION, TRADE AND CUSTOMS DATA AND
      *>              PRODUCT COMPLIANCE FOR AN ITEM.  A VAT LIABILITY
      *>              APPLIES BY TAX POINT TO NEW DOCUMENTS ONLY AND AN
      *>              ITEM WITHOUT A CLASSIFICATION CANNOT BE INVOICED.
      *>              AN INTERNATIONAL MOVEMENT WITHOUT A VALID
      *>              COMMODITY CODE OR ORIGIN IS A BLOCKING EXCEPTION
      *>              FOR THE CUSTOMS HAND OFF, AND DANGEROUS GOODS
      *>              MUST SHOW THEIR CLASSIFICATION ON DESPATCH
      *>              DOCUMENTS.
      *> CALLED BY  : SALES INVOICING, PURCHASE INVOICE MATCHING,
      *>              DESPATCH, THE CUSTOMS HAND OFF, PKGRPT,
      *>              THE .NET CLIENT
      *> INTERFACE  : CALL "ITMCOMPL" USING WS-ITMCOMPL-CA
      *>              COPYBOOK ITMCOMPLCA
      *> CALLS      : SQLIO (inv.Item, inv.ItemVatClass,
      *>              inv.ItemCustoms, inv.ItemPackaging, fin.VatCode)
      *>----------------------------------------------------------------
      *> CHANGE LOG
      *> 1996-01-30 RJH  CR0402 VAT LIABILITY BY ITEM
      *> 2007-06-01 KMP  CR1760 MOBILE PHONES AND COMPUTER CHIPS -
      *>                 DOMESTIC REVERSE CHARGE SPECIFIED GOODS
      *> 2021-01-01 ANW  CR4410 BREXIT - UK GLOBAL TARIFF, UKCA
      *> 2022-04-01 ANW  CR4702 PLASTIC PACKAGING TAX
      *> 2026-10-05 STERP ITM-025 TO ITM-027
      *>================================================================
      *> REQ: ITM-025, ITM-026, ITM-027
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ITMCOMPL.
       AUTHOR. R J HOLLIS.
       DATE-WRITTEN. 30/01/1996.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY SQLIOCA.
       COPY ITMWORK.
       01  WS-CONTROL.
           05  WS-ERROR                PIC X VALUE "N".
               88  WS-INPUT-BAD                  VALUE "Y".
           05  WS-DIGITS                PIC X(10) VALUE SPACES.
           05  WS-BAD-DIGIT            PIC X VALUE "N".
       LINKAGE SECTION.
       COPY ITMCOMPLCA.
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
               WHEN "VATCODE"  PERFORM 1000-VAT-CODE
               WHEN "VATSET"   PERFORM 2000-VAT-SET
               WHEN "CUSTOMS"  PERFORM 3000-CUSTOMS
               WHEN "CUSTCHK"  PERFORM 3500-CUSTOMS-CHECK
               WHEN "COMPLY"   PERFORM 4000-COMPLIANCE
               WHEN "DGCHECK"  PERFORM 4500-DANGEROUS-GOODS
               WHEN "PACKAGE"  PERFORM 5000-PACKAGING
               WHEN OTHER      PERFORM 9100-BAD-FUNCTION
           END-EVALUATE.
       0000-EXIT.
           GOBACK.

      *>----------------------------------------------------------------
       0100-INITIALISE SECTION.
       0100-START.
           MOVE "N" TO WS-ERROR WS-SQL-FAILED
           MOVE "ITMCOMPL" TO CA-PROGRAM
           MOVE 0 TO CA-RETURN-CODE CA-MSG-COUNT
           MOVE SPACES TO CA-REASON-CODE CA-MESSAGE
           MOVE "ITM-025" TO CA-REQ-REF
           MOVE 0 TO CO-VAT-RATE CO-VAT-FROM CO-VAT-TO CO-DUTY-RATE
           MOVE 0 TO CO-NET-WEIGHT-KG CO-CUSTOMS-FROM CO-LINE-CNT
           MOVE 0 TO CO-FAULT-CNT CO-PKG-TOTAL-G CO-PKG-PLASTIC-G
           MOVE 0 TO CO-PKG-RECYCLED-G
           MOVE SPACES TO CO-VAT-CODE CO-VAT-EVIDENCE CO-DRC-WORDING
           MOVE SPACES TO CO-COMMODITY-CODE CO-ORIGIN-COUNTRY
           MOVE SPACES TO CO-PREF-EVIDENCE CO-EXPORT-LICENCE
           MOVE SPACES TO CO-SUPP-UNIT CO-CONFORMITY-REF
           MOVE SPACES TO CO-DG-UN-NUMBER CO-DG-CLASS
           MOVE SPACES TO CO-DG-PACK-GROUP CO-REACH-STATUS
           MOVE SPACES TO CO-WEEE-CATEGORY CO-BATTERY-CATEGORY
           MOVE "N" TO CO-VALID CO-PREFERENTIAL CO-EXPORT-CONTROL
           MOVE "N" TO CO-UKCA-MARKED CO-DRC-GOODS CO-PPT-LIABLE
           MOVE FUNCTION UPPER-CASE(CO-ITEM-CODE) TO CO-ITEM-CODE
           IF CO-DIRECTION = SPACES
               MOVE "SALE" TO CO-DIRECTION
           END-IF
           IF CO-TAX-POINT = 0
               MOVE CA-BUSINESS-DATE TO CO-TAX-POINT
           END-IF
           IF CO-TAX-POINT = 0
               MOVE 20261005 TO CO-TAX-POINT
           END-IF
           IF CO-DESPATCH-QTY = 0
               MOVE 1 TO CO-DESPATCH-QTY
           END-IF
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 24
               MOVE 100 TO SQLIO-COL-WIDTH(WS-IX)
           END-PERFORM.
       0100-EXIT.
           EXIT.

       0200-VALIDATE SECTION.
       0200-START.
           IF CA-COMPANY = SPACES OR CO-ITEM-CODE = SPACES
               MOVE "CMP0001" TO CA-REASON-CODE
               MOVE "A company and an item are required" TO CA-MESSAGE
               PERFORM 9000-REJECT
           END-IF.
       0200-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 1000  THE VAT LIABILITY EFFECTIVE AT THE TAX POINT (ITM-025).
      *>       A LATER CHANGE OF LIABILITY DOES NOT REACH BACK: THE
      *>       SELECT IS BOUNDED BY THE TAX POINT, NOT BY TODAY.
      *>----------------------------------------------------------------
       1000-VAT-CODE SECTION.
       1000-START.
           PERFORM 1100-READ-DRC-FLAG
           IF CA-FAILED
               GO TO 1000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 v.VatCode, ISNULL(c.RatePct,0), "
                  "ISNULL(v.EvidenceRef,''), CONVERT(char(8), "
                  "v.EffectiveFrom, 112), ISNULL(CONVERT(char(8), "
                  "v.EffectiveTo, 112),'') FROM inv.ItemVatClass v "
                  "LEFT JOIN fin.VatCode c ON c.VatCode = v.VatCode "
                  "WHERE v.CompanyCode = ? AND v.ItemCode = ? AND "
                  "v.Direction = ? AND v.EffectiveFrom <= "
                  "CONVERT(date, ?, 112) AND (v.EffectiveTo IS NULL "
                  "OR v.EffectiveTo >= CONVERT(date, ?, 112)) ORDER "
                  "BY v.EffectiveFrom DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 5 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE CO-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE CO-DIRECTION TO SQLIO-PARM-VAL(3)
           MOVE CO-TAX-POINT TO WS-DATE-N
           MOVE WS-DATE-N TO SQLIO-PARM-VAL(4)
           MOVE WS-DATE-N TO SQLIO-PARM-VAL(5)
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 5
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE 5 TO SQLIO-COL-CNT
           MOVE 1 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 1000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "CMP0002" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Item " FUNCTION TRIM(CO-ITEM-CODE)
                      " has no UK VAT classification at that tax "
                      "point, so it cannot be invoiced"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 1000-EXIT
           END-IF
           MOVE ITM-COL(1) TO CO-VAT-CODE
           COMPUTE CO-VAT-RATE = FUNCTION NUMVAL(ITM-COL(2))
           MOVE ITM-COL(3) TO CO-VAT-EVIDENCE
           COMPUTE CO-VAT-FROM = FUNCTION NUMVAL(ITM-COL(4))
           IF ITM-COL(5) = SPACES
               MOVE 0 TO CO-VAT-TO
           ELSE
               COMPUTE CO-VAT-TO = FUNCTION NUMVAL(ITM-COL(5))
           END-IF
      *>   CR1760.  SPECIFIED GOODS SOLD TO A VAT REGISTERED BUSINESS
      *>   CARRY THE DOMESTIC REVERSE CHARGE AND NO VAT IS CHARGED.
           IF CO-DRC-GOODS = "Y" AND CO-DIRECTION = "SALE"
               MOVE "DRC" TO CO-VAT-CODE
               MOVE 0 TO CO-VAT-RATE
               MOVE SPACES TO CO-DRC-WORDING
               STRING "Reverse charge: customer to pay the VAT to "
                      "HMRC (VAT Act 1994 section 55A)"
                   DELIMITED BY SIZE INTO CO-DRC-WORDING
           END-IF
           IF CO-VAT-CODE NOT = "S" AND CO-VAT-EVIDENCE = SPACES
               MOVE 4 TO CA-RETURN-CODE
               MOVE "CMP0003" TO CA-REASON-CODE
               ADD 1 TO CA-MSG-COUNT
           END-IF
           MOVE "Y" TO CO-VALID
           MOVE CO-VAT-RATE TO WS-NUM-ED
           MOVE SPACES TO CA-MESSAGE
           STRING "VAT code " FUNCTION TRIM(CO-VAT-CODE) " at "
                  FUNCTION TRIM(WS-NUM-ED) "% for "
                  FUNCTION TRIM(CO-DIRECTION)
               DELIMITED BY SIZE INTO CA-MESSAGE.
       1000-EXIT.
           EXIT.

       1100-READ-DRC-FLAG SECTION.
       1100-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT IsDrcSpecifiedGoods, UkcaMarked, "
                  "ISNULL(ConformityDocRef,''), "
                  "ISNULL(DangerousGoodsUn,''), "
                  "ISNULL(DangerousGoodsClass,''), "
                  "ISNULL(DangerousGoodsPackGroup,''), "
                  "ISNULL(ReachStatus,''), ISNULL(WeeeCategory,''), "
                  "ISNULL(BatteryCategory,''), ISNULL(NetWeightKg,0)"
                  " FROM inv.Item WHERE CompanyCode = ? AND "
                  "ItemCode = ?"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE CO-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 10 TO SQLIO-COL-CNT
           MOVE 2 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 1100-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "CMP0001" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Item " FUNCTION TRIM(CO-ITEM-CODE)
                      " is not held in company "
                      FUNCTION TRIM(CA-COMPANY)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE 16 TO CA-RETURN-CODE
               MOVE "Y" TO WS-ERROR
               ADD 1 TO CA-MSG-COUNT
               GO TO 1100-EXIT
           END-IF
           IF ITM-COL(1)(1:1) = "1"
               MOVE "Y" TO CO-DRC-GOODS
           END-IF
           IF ITM-COL(2)(1:1) = "1"
               MOVE "Y" TO CO-UKCA-MARKED
           END-IF
           MOVE ITM-COL(3) TO CO-CONFORMITY-REF
           MOVE ITM-COL(4) TO CO-DG-UN-NUMBER
           MOVE ITM-COL(5) TO CO-DG-CLASS
           MOVE ITM-COL(6) TO CO-DG-PACK-GROUP
           MOVE ITM-COL(7) TO CO-REACH-STATUS
           MOVE ITM-COL(8) TO CO-WEEE-CATEGORY
           MOVE ITM-COL(9) TO CO-BATTERY-CATEGORY
           COMPUTE CO-NET-WEIGHT-KG = FUNCTION NUMVAL(ITM-COL(10)).
       1100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 2000  RECORD AN EFFECTIVE DATED LIABILITY WITH ITS EVIDENCE.
      *>       A NON STANDARD LIABILITY WITHOUT EVIDENCE IS REFUSED.
      *>----------------------------------------------------------------
       2000-VAT-SET SECTION.
       2000-START.
           IF CO-VAT-CODE = SPACES
               MOVE "CMP0002" TO CA-REASON-CODE
               MOVE "A VAT code is required" TO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 2000-EXIT
           END-IF
           IF CO-VAT-CODE NOT = "S" AND CO-VAT-EVIDENCE = SPACES
               MOVE "CMP0003" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A non standard VAT liability needs the "
                      "evidence or HMRC reference that supports it"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 2000-EXIT
           END-IF
           IF CO-VAT-FROM = 0
               MOVE CO-TAX-POINT TO CO-VAT-FROM
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "MERGE inv.ItemVatClass AS t USING (SELECT ? AS C,"
                  " ? AS I, ? AS D, CONVERT(date, ?, 112) AS F) AS s"
                  " ON t.CompanyCode = s.C AND t.ItemCode = s.I AND "
                  "t.Direction = s.D AND t.EffectiveFrom = s.F WHEN "
                  "MATCHED THEN UPDATE SET VatCode = ?, EvidenceRef "
                  "= NULLIF(?,''), ModifiedBy = ?, ModifiedAt = "
                  "SYSDATETIME() WHEN NOT MATCHED THEN INSERT "
                  "(CompanyCode, ItemCode, Direction, EffectiveFrom,"
                  " VatCode, EvidenceRef, ApprovedBy, CreatedBy) "
                  "VALUES (s.C, s.I, s.D, s.F, ?, NULLIF(?,''), ?, ?)"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 11 TO SQLIO-PARM-CNT
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 11
               MOVE SPACES TO SQLIO-PARM-VAL(WS-JX)
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE CO-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE CO-DIRECTION TO SQLIO-PARM-VAL(3)
           MOVE CO-VAT-FROM TO WS-DATE-N
           MOVE WS-DATE-N TO SQLIO-PARM-VAL(4)
           MOVE CO-VAT-CODE TO SQLIO-PARM-VAL(5)
           MOVE CO-VAT-EVIDENCE TO SQLIO-PARM-VAL(6)
           MOVE CA-USER TO SQLIO-PARM-VAL(7)
           MOVE CO-VAT-CODE TO SQLIO-PARM-VAL(8)
           MOVE CO-VAT-EVIDENCE TO SQLIO-PARM-VAL(9)
           MOVE CA-USER TO SQLIO-PARM-VAL(10)
           MOVE CA-USER TO SQLIO-PARM-VAL(11)
           MOVE 0 TO SQLIO-COL-CNT
           MOVE 3 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 2000-EXIT
           END-IF
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           MOVE "Y" TO CO-VALID
           MOVE CO-VAT-FROM TO WS-DATE-N
           MOVE SPACES TO CA-MESSAGE
           STRING "VAT code " FUNCTION TRIM(CO-VAT-CODE)
                  " applies to " FUNCTION TRIM(CO-DIRECTION)
                  " documents from " WS-DATE-DD "/" WS-DATE-MM "/"
                  WS-DATE-YYYY
               DELIMITED BY SIZE INTO CA-MESSAGE.
       2000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3000  CUSTOMS DATA FOR A DECLARATION (ITM-026).
      *>----------------------------------------------------------------
       3000-CUSTOMS SECTION.
       3000-START.
           PERFORM 1100-READ-DRC-FLAG
           IF CA-FAILED
               GO TO 3000-EXIT
           END-IF
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT TOP 1 CommodityCode, CountryOfOrigin, "
                  "PreferentialOrigin, ISNULL(PreferenceEvidenceRef,"
                  "''), ISNULL(DutyRatePct,0), ExportControlled, "
                  "ISNULL(ExportLicenceRef,''), "
                  "ISNULL(SupplementaryUnit,''), "
                  "ISNULL(NetWeightKgPerStockUnit,0), "
                  "CONVERT(char(8), EffectiveFrom, 112) FROM "
                  "inv.ItemCustoms WHERE CompanyCode = ? AND "
                  "ItemCode = ? AND EffectiveFrom <= CONVERT(date, "
                  "?, 112) AND (EffectiveTo IS NULL OR EffectiveTo "
                  ">= CONVERT(date, ?, 112)) ORDER BY EffectiveFrom "
                  "DESC"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 4 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE CO-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE CO-TAX-POINT TO WS-DATE-N
           MOVE WS-DATE-N TO SQLIO-PARM-VAL(3)
           MOVE WS-DATE-N TO SQLIO-PARM-VAL(4)
           PERFORM VARYING WS-JX FROM 1 BY 1 UNTIL WS-JX > 4
               MOVE "N" TO SQLIO-PARM-NULL(WS-JX)
           END-PERFORM
           MOVE 10 TO SQLIO-COL-CNT
           MOVE 4 TO SQLIO-CURSOR
           MOVE "SNGL" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 3000-EXIT
           END-IF
           IF SQLIO-NOTFOUND
               MOVE "CMP0004" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Item " FUNCTION TRIM(CO-ITEM-CODE)
                      " has no customs data effective at that date"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE 16 TO CA-RETURN-CODE
               MOVE "Y" TO WS-ERROR
               ADD 1 TO CA-MSG-COUNT
               GO TO 3000-EXIT
           END-IF
           MOVE ITM-COL(1) TO CO-COMMODITY-CODE
           MOVE ITM-COL(2)(1:2) TO CO-ORIGIN-COUNTRY
           IF ITM-COL(3)(1:1) = "1"
               MOVE "Y" TO CO-PREFERENTIAL
           END-IF
           MOVE ITM-COL(4) TO CO-PREF-EVIDENCE
           COMPUTE CO-DUTY-RATE = FUNCTION NUMVAL(ITM-COL(5))
           IF ITM-COL(6)(1:1) = "1"
               MOVE "Y" TO CO-EXPORT-CONTROL
           END-IF
           MOVE ITM-COL(7) TO CO-EXPORT-LICENCE
           MOVE ITM-COL(8) TO CO-SUPP-UNIT
           IF FUNCTION NUMVAL(ITM-COL(9)) > 0
               COMPUTE CO-NET-WEIGHT-KG = FUNCTION NUMVAL(ITM-COL(9))
           END-IF
           COMPUTE CO-CUSTOMS-FROM = FUNCTION NUMVAL(ITM-COL(10))
           MOVE "Y" TO CO-VALID
           MOVE SPACES TO CA-MESSAGE
           STRING "Commodity " FUNCTION TRIM(CO-COMMODITY-CODE)
                  ", origin " CO-ORIGIN-COUNTRY
               DELIMITED BY SIZE INTO CA-MESSAGE.
       3000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 3500  VALIDATE BEFORE AN INTERNATIONAL MOVEMENT.  A MISSING
      *>       OR BADLY FORMED COMMODITY CODE OR ORIGIN BLOCKS THE
      *>       CUSTOMS HAND OFF (AC-129).
      *>----------------------------------------------------------------
       3500-CUSTOMS-CHECK SECTION.
       3500-START.
           MOVE 0 TO CO-FAULT-CNT
           PERFORM 3000-CUSTOMS
           IF CA-NOT-FOUND
               MOVE "CMP0005" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The customs hand off is blocked: item "
                      FUNCTION TRIM(CO-ITEM-CODE)
                      " has no commodity code or country of origin"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               MOVE 1 TO CO-FAULT-CNT
               PERFORM 9000-REJECT
               GO TO 3500-EXIT
           END-IF
           IF CA-FAILED
               GO TO 3500-EXIT
           END-IF
           MOVE "N" TO CO-VALID
           PERFORM 3600-CHECK-COMMODITY
           IF CA-FAILED
               GO TO 3500-EXIT
           END-IF
           IF CO-ORIGIN-COUNTRY = SPACES
              OR CO-ORIGIN-COUNTRY(1:1) = " "
               MOVE "CMP0006" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The country of origin of "
                      FUNCTION TRIM(CO-ITEM-CODE)
                      " is missing; the declaration cannot be made"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CO-FAULT-CNT
               PERFORM 9000-REJECT
               GO TO 3500-EXIT
           END-IF
           IF CO-PREFERENTIAL = "Y" AND CO-PREF-EVIDENCE = SPACES
               MOVE "CMP0004" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Preferential origin is claimed for "
                      FUNCTION TRIM(CO-ITEM-CODE)
                      " without evidence on file"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CO-FAULT-CNT
               PERFORM 9000-REJECT
               GO TO 3500-EXIT
           END-IF
           IF CO-EXPORT-CONTROL = "Y" AND CO-EXPORT-LICENCE = SPACES
               MOVE "CMP0007" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Item " FUNCTION TRIM(CO-ITEM-CODE)
                      " is export controlled and no licence "
                      "reference is recorded"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CO-FAULT-CNT
               MOVE 6 TO CA-RETURN-CODE
               MOVE "Y" TO WS-ERROR
               ADD 1 TO CA-MSG-COUNT
               GO TO 3500-EXIT
           END-IF
           MOVE "Y" TO CO-VALID
           MOVE SPACES TO CA-MESSAGE
           STRING "Customs data is complete: "
                  FUNCTION TRIM(CO-COMMODITY-CODE) " / "
                  CO-ORIGIN-COUNTRY
               DELIMITED BY SIZE INTO CA-MESSAGE.
       3500-EXIT.
           EXIT.

       3600-CHECK-COMMODITY SECTION.
       3600-START.
      *>   A UK GLOBAL TARIFF COMMODITY CODE IS EIGHT OR TEN DIGITS.
           MOVE "N" TO WS-BAD-DIGIT
           MOVE 0 TO WS-KX
           PERFORM VARYING WS-IX FROM 1 BY 1 UNTIL WS-IX > 10
               IF CO-COMMODITY-CODE(WS-IX:1) NOT = " "
                   ADD 1 TO WS-KX
                   IF CO-COMMODITY-CODE(WS-IX:1) < "0"
                      OR CO-COMMODITY-CODE(WS-IX:1) > "9"
                       MOVE "Y" TO WS-BAD-DIGIT
                   END-IF
               END-IF
           END-PERFORM
           IF WS-KX = 0 OR WS-BAD-DIGIT = "Y"
              OR (WS-KX NOT = 8 AND WS-KX NOT = 10)
               MOVE "CMP0005" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Commodity code "
                      FUNCTION TRIM(CO-COMMODITY-CODE)
                      " is not a valid UK Global Tariff code of "
                      "eight or ten digits"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               ADD 1 TO CO-FAULT-CNT
               PERFORM 9000-REJECT
           END-IF.
       3600-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 4000  PRODUCT COMPLIANCE (ITM-027).
      *>----------------------------------------------------------------
       4000-COMPLIANCE SECTION.
       4000-START.
           PERFORM 1100-READ-DRC-FLAG
           IF CA-FAILED
               GO TO 4000-EXIT
           END-IF
           MOVE 0 TO CO-FAULT-CNT
           MOVE "Y" TO CO-VALID
           IF CO-UKCA-MARKED = "Y" AND CO-CONFORMITY-REF = SPACES
               ADD 1 TO CO-FAULT-CNT
               MOVE "N" TO CO-VALID
               MOVE "CMP0008" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "A UKCA marked item must hold its declaration"
                      " of conformity reference"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF
           IF CO-DG-UN-NUMBER NOT = SPACES
              AND (CO-DG-CLASS = SPACES OR CO-DG-PACK-GROUP = SPACES)
               ADD 1 TO CO-FAULT-CNT
               MOVE "N" TO CO-VALID
               MOVE "CMP0008" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Dangerous goods need a class and a packing "
                      "group as well as the UN number"
                   DELIMITED BY SIZE INTO CA-MESSAGE
           END-IF
           IF CO-FAULT-CNT > 0
               PERFORM 9000-REJECT
               GO TO 4000-EXIT
           END-IF
           MOVE SPACES TO CA-MESSAGE
           STRING "Compliance data complete for "
                  FUNCTION TRIM(CO-ITEM-CODE)
               DELIMITED BY SIZE INTO CA-MESSAGE.
       4000-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 4500  WHAT A DESPATCH NOTE MUST SHOW FOR DANGEROUS GOODS.
      *>----------------------------------------------------------------
       4500-DANGEROUS-GOODS SECTION.
       4500-START.
           PERFORM 1100-READ-DRC-FLAG
           IF CA-FAILED
               GO TO 4500-EXIT
           END-IF
           IF CO-DG-UN-NUMBER = SPACES
               MOVE 16 TO CA-RETURN-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "Item " FUNCTION TRIM(CO-ITEM-CODE)
                      " is not classified as dangerous goods"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 4500-EXIT
           END-IF
           IF CO-DG-CLASS = SPACES OR CO-DG-PACK-GROUP = SPACES
               MOVE "CMP0008" TO CA-REASON-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "The dangerous goods classification of "
                      FUNCTION TRIM(CO-ITEM-CODE)
                      " is incomplete, so it cannot be despatched"
                   DELIMITED BY SIZE INTO CA-MESSAGE
               PERFORM 9000-REJECT
               GO TO 4500-EXIT
           END-IF
           MOVE "Y" TO CO-VALID
           MOVE SPACES TO CA-MESSAGE
           STRING FUNCTION TRIM(CO-DG-UN-NUMBER) ", class "
                  FUNCTION TRIM(CO-DG-CLASS) ", packing group "
                  FUNCTION TRIM(CO-DG-PACK-GROUP)
               DELIMITED BY SIZE INTO CA-MESSAGE.
       4500-EXIT.
           EXIT.

      *>----------------------------------------------------------------
      *> 5000  PACKAGING COMPOSITION FOR THE PACKAGING DATA REPORT AND
      *>       FOR PLASTIC PACKAGING TAX.  AC-132: 1,000 UNITS EACH
      *>       WITH 50 g OF PLASTIC AT 30% RECYCLED CONTENT GIVE
      *>       50,000 g (50 kg) OF PLASTIC INCLUDING 15,000 g (15 kg)
      *>       OF RECYCLED MATERIAL.
      *>----------------------------------------------------------------
       5000-PACKAGING SECTION.
       5000-START.
           MOVE SPACES TO SQLIO-STMT
           STRING "SELECT PackagingMaterial, PackagingType, "
                  "WeightGrams, ISNULL(RecycledContentPct,0), "
                  "IsPlasticPackagingTax FROM inv.ItemPackaging "
                  "WHERE CompanyCode = ? AND ItemCode = ? ORDER BY "
                  "PackagingMaterial, PackagingType"
               DELIMITED BY SIZE INTO SQLIO-STMT
           MOVE 2 TO SQLIO-PARM-CNT
           MOVE CA-COMPANY TO SQLIO-PARM-VAL(1)
           MOVE "N" TO SQLIO-PARM-NULL(1)
           MOVE CO-ITEM-CODE TO SQLIO-PARM-VAL(2)
           MOVE "N" TO SQLIO-PARM-NULL(2)
           MOVE 5 TO SQLIO-COL-CNT
           MOVE 5 TO SQLIO-CURSOR
           MOVE "EXEC" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF SQLIO-ERROR
               PERFORM 9300-SQL-FAILED
               GO TO 5000-EXIT
           END-IF
           MOVE 0 TO CO-LINE-CNT
           MOVE "N" TO WS-DONE
           PERFORM UNTIL WS-AT-END
               MOVE "FTCH" TO SQLIO-FUNC
               CALL "SQLIO" USING SQLIO-CA ITM-ROW
               IF SQLIO-NOTFOUND OR SQLIO-ERROR
                   MOVE "Y" TO WS-DONE
               ELSE
                   PERFORM 5100-ADD-PACKAGING
               END-IF
           END-PERFORM
           MOVE "CLOS" TO SQLIO-FUNC
           CALL "SQLIO" USING SQLIO-CA ITM-ROW
           IF CO-LINE-CNT = 0
               MOVE 16 TO CA-RETURN-CODE
               MOVE SPACES TO CA-MESSAGE
               STRING "No packaging composition is recorded for "
                      FUNCTION TRIM(CO-ITEM-CODE)
                   DELIMITED BY SIZE INTO CA-MESSAGE
               GO TO 5000-EXIT
           END-IF
           MOVE "Y" TO CO-VALID
           COMPUTE WS-QTY = CO-PKG-PLASTIC-G / 1000
           MOVE WS-QTY TO WS-QTY-ED
           MOVE SPACES TO WS-TEXT
           STRING FUNCTION TRIM(WS-QTY-ED) " kg of plastic, of which"
               DELIMITED BY SIZE INTO WS-TEXT
           COMPUTE WS-QTY = CO-PKG-RECYCLED-G / 1000
           MOVE WS-QTY TO WS-QTY-ED
           MOVE SPACES TO CA-MESSAGE
           STRING FUNCTION TRIM(WS-TEXT) " "
                  FUNCTION TRIM(WS-QTY-ED) " kg is recycled"
               DELIMITED BY SIZE INTO CA-MESSAGE.
       5000-EXIT.
           EXIT.

       5100-ADD-PACKAGING SECTION.
       5100-START.
           IF CO-LINE-CNT >= 12
               GO TO 5100-EXIT
           END-IF
           ADD 1 TO CO-LINE-CNT
           MOVE ITM-COL(1) TO CO-L-MATERIAL(CO-LINE-CNT)
           MOVE ITM-COL(2) TO CO-L-TYPE(CO-LINE-CNT)
           COMPUTE CO-L-WEIGHT-G(CO-LINE-CNT) =
               FUNCTION NUMVAL(ITM-COL(3))
           COMPUTE CO-L-RECYCLED(CO-LINE-CNT) =
               FUNCTION NUMVAL(ITM-COL(4))
           COMPUTE CO-L-TOTAL-G(CO-LINE-CNT) ROUNDED =
               CO-L-WEIGHT-G(CO-LINE-CNT) * CO-DESPATCH-QTY
           COMPUTE CO-L-RECYC-G(CO-LINE-CNT) ROUNDED =
               CO-L-TOTAL-G(CO-LINE-CNT)
               * CO-L-RECYCLED(CO-LINE-CNT) / 100
           IF ITM-COL(5)(1:1) = "1"
               MOVE "Y" TO CO-L-PPT(CO-LINE-CNT)
               MOVE "Y" TO CO-PPT-LIABLE
           ELSE
               MOVE "N" TO CO-L-PPT(CO-LINE-CNT)
           END-IF
           ADD CO-L-TOTAL-G(CO-LINE-CNT) TO CO-PKG-TOTAL-G
           IF CO-L-MATERIAL(CO-LINE-CNT) = "PLASTIC"
               ADD CO-L-TOTAL-G(CO-LINE-CNT) TO CO-PKG-PLASTIC-G
               ADD CO-L-RECYC-G(CO-LINE-CNT) TO CO-PKG-RECYCLED-G
           END-IF.
       5100-EXIT.
           EXIT.

      *>----------------------------------------------------------------
       9000-REJECT SECTION.
       9000-START.
           MOVE "Y" TO WS-ERROR
           MOVE "N" TO CO-VALID
           MOVE 8 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9000-EXIT.
           EXIT.

       9100-BAD-FUNCTION SECTION.
       9100-START.
           MOVE "CMP0009" TO CA-REASON-CODE
           MOVE SPACES TO CA-MESSAGE
           STRING "ITMCOMPL does not support function "
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
               MOVE "SQL failure in ITMCOMPL" TO CA-MESSAGE
           END-IF
           MOVE "SQL0001" TO CA-REASON-CODE
           MOVE 12 TO CA-RETURN-CODE
           ADD 1 TO CA-MSG-COUNT.
       9300-EXIT.
           EXIT.
