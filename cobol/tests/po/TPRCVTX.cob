      *> Native RECEIPT commit proof. Mutates only a STERP_QA_ database;
      *> committed ZZ-RCTX fixtures are removed after reconnect proof.
      *> REQ: GUI-028, PO-009, PO-017, PO-024, COM-015
       identification division.
       program-id. TPRCVTX.
       data division.
       working-storage section.
       copy TESTKITW.
       copy SQLIOCA.
       copy RECEIPTCA replacing ==DFHCOMMAREA== by ==Ws-Receipt==.
       copy STKMOVECA replacing ==DFHCOMMAREA== by ==Ws-Stock==
           leading ==CA-== by ==SK-==.
       01 Probe                       pic x(60).
       01 Receipt-Reference           pic x(30).
       01 Scenario                    binary-long.
       01 Query-Buffer                pic x(4000).
       procedure division.
       Main.
           move "native RECEIPT transaction commit" to TK-SUITE
           perform TK-BEGIN
           move spaces to SQLIO-STMT
           string "SELECT CASE WHEN DB_NAME() LIKE 'STERP_QA[_]%' "
             "THEN 1 ELSE 0 END"
             delimited by size into SQLIO-STMT
           perform Read-Scalar
           if Probe(1:1) not = "1"
               move "native committed receipt proof" to TK-NAME
               move "requires isolated STERP_QA_ database"
                 to TK-DETAIL
               perform TK-SKIP
               perform TK-END
               stop run
           end-if
           perform Cleanup
           move spaces to SQLIO-STMT
           string "INSERT inv.Item (CompanyCode,ItemCode,"
             "Description,ItemClass,StockUomCode,QuantumQty,"
             "IsIndivisible,TraceabilityType,MakeBuy,ValuationGroup,"
             "CostMethod,LifecycleStatus,CreatedBy) VALUES "
             "('STC','ZZ-RCTX','Receipt transaction','BOUGHT','EA',"
             "1,1,'LOT','BUY','ZZRCT','MWA','RELEASED','TEST'); "
             "INSERT po.PurchaseOrder (CompanyCode,SiteCode,"
             "OrderNumber,SupplierCode,OrderDate,Status) VALUES "
             "('STC','BHM','ZZ-RCTX-PO','S0004','20261012','ISSUED');"
             "INSERT po.PurchaseOrderLine (PurchaseOrderId,"
             "CompanyCode,OrderNumber,LineNum,ItemCode,Revision,"
             "Description,OrderedQty,UomCode,UnitPrice) SELECT "
             "PurchaseOrderId,'STC','ZZ-RCTX-PO',1,'ZZ-RCTX','A',"
             "'Receipt transaction',8,'EA',2.5 FROM "
             "po.PurchaseOrder WHERE CompanyCode='STC' AND "
             "OrderNumber='ZZ-RCTX-PO'; INSERT po.PurchaseSchedule "
             "(PurchaseOrderLineId,CompanyCode,OrderNumber,"
             "LineNum,ScheduleNo,Quantity,RequestedDate) SELECT "
             "PurchaseOrderLineId,'STC','ZZ-RCTX-PO',1,1,8,"
             "'20261012' FROM po.PurchaseOrderLine WHERE "
             "CompanyCode='STC' AND OrderNumber='ZZ-RCTX-PO'"
             delimited by size into SQLIO-STMT
           perform Execute-Sql
           perform varying Scenario from 1 by 1 until Scenario > 2
               if Scenario = 1
                   move "ZZ-RCTX-G1" to Receipt-Reference
               else
                   move "ZZ-RCTX-G2" to Receipt-Reference
                   move "BEGN" to SQLIO-FUNC
                   call "SQLIO" using SQLIO-CA Probe
               end-if
               perform Receipt-Setup
               call "RECEIPT" using Ws-Receipt
               move "native receipt confirms stock and GRNI"
                 to TK-NAME
               if CA-RETURN-CODE < 8 and RC-STATUS = "CONFIRMED"
                  and RC-L-MOVEMENT(1) > 0
                  and RC-GRNI-GBP = 10
                   perform TK-PASS
               else
                   move CA-MESSAGE to TK-DETAIL
                   perform TK-FAIL
               end-if
               move "TSTA" to SQLIO-FUNC
               call "SQLIO" using SQLIO-CA Probe
               move "receipt preserves driver transaction ownership"
                 to TK-NAME
               if (Scenario = 1 and SQLIO-ROWCOUNT = 1)
                  or (Scenario = 2 and SQLIO-ROWCOUNT = 0)
                   perform TK-PASS
               else
                   perform TK-FAIL
               end-if
               if Scenario = 2
                   move "SELECT @@TRANCOUNT" to SQLIO-STMT
                   perform Read-Scalar
                   move "receipt leaves caller SQL UOW open"
                     to TK-NAME
                   if function numval(Probe) > 0
                       perform TK-PASS
                   else
                       perform TK-FAIL
                   end-if
                   move "COMT" to SQLIO-FUNC
                   call "SQLIO" using SQLIO-CA Probe
               end-if
               move "DISC" to SQLIO-FUNC
               call "SQLIO" using SQLIO-CA Probe
               perform Assert-Receipt
           end-perform
           move "ZZ-RCTX-G3" to Receipt-Reference
           perform Receipt-Setup
           move "ZZ-NO-LOCATION" to RC-L-LOCATION(1)
           call "RECEIPT" using Ws-Receipt
           move "invalid stock destination rejects native receipt"
             to TK-NAME
           if CA-RETURN-CODE >= 8
               perform TK-PASS
           else
               perform TK-FAIL
           end-if
           move "DISC" to SQLIO-FUNC
           call "SQLIO" using SQLIO-CA Probe
           move spaces to SQLIO-STMT
           string "SELECT CASE WHEN NOT EXISTS (SELECT 1 FROM "
             "po.Receipt WHERE CompanyCode='STC' AND "
             "ReceiptRef='ZZ-RCTX-G3') AND NOT EXISTS (SELECT 1 "
             "FROM inv.StockMovement WHERE CompanyCode='STC' "
             "AND SourceDocRef='ZZ-RCTX-G3') AND EXISTS "
             "(SELECT 1 FROM cst.ItemCost WHERE CompanyCode='STC' "
             "AND ItemCode='ZZ-RCTX' AND QtyOnHand=8 AND "
             "CarryingValueGBP=20) THEN 1 ELSE 0 END"
             delimited by size into SQLIO-STMT
           move "failed receipt leaves no draft, movement or value"
             to TK-NAME
           perform Assert-Sql
           move "BEGN" to SQLIO-FUNC
           call "SQLIO" using SQLIO-CA Probe
           move "UPDATE fin.Account SET Status='RETIRED' "
             to SQLIO-STMT
           move spaces to Query-Buffer
           string function trim(SQLIO-STMT)
             " WHERE CompanyCode='STC' AND AccountCode='1100'"
             delimited by size into Query-Buffer
           move Query-Buffer to SQLIO-STMT
           perform Execute-Sql
           move "ZZ-RCTX-G4" to Receipt-Reference
           perform Receipt-Setup
           call "RECEIPT" using Ws-Receipt
           move "GRNI failure occurs after native stock posting"
             to TK-NAME
           if CA-RETURN-CODE >= 8 and RC-L-MOVEMENT(1) > 0
               perform TK-PASS
           else
               move CA-MESSAGE to TK-DETAIL
               perform TK-FAIL
           end-if
           move "TSTA" to SQLIO-FUNC
           call "SQLIO" using SQLIO-CA Probe
           move "failed receipt does not commit caller UOW"
             to TK-NAME
           if SQLIO-ROWCOUNT = 0
               perform TK-PASS
           else
               perform TK-FAIL
           end-if
           move "ROLB" to SQLIO-FUNC
           call "SQLIO" using SQLIO-CA Probe
           move "DISC" to SQLIO-FUNC
           call "SQLIO" using SQLIO-CA Probe
           move spaces to SQLIO-STMT
           string "SELECT CASE WHEN NOT EXISTS (SELECT 1 FROM "
             "po.Receipt WHERE CompanyCode='STC' AND "
             "ReceiptRef='ZZ-RCTX-G4') AND NOT EXISTS (SELECT 1 "
             "FROM inv.StockMovement WHERE CompanyCode='STC' "
             "AND SourceDocRef='ZZ-RCTX-G4') AND EXISTS "
             "(SELECT 1 FROM cst.ItemCost WHERE CompanyCode='STC' "
             "AND ItemCode='ZZ-RCTX' AND QtyOnHand=8 AND "
             "CarryingValueGBP=20) AND EXISTS (SELECT 1 FROM "
             "fin.Account WHERE CompanyCode='STC' AND "
             "AccountCode='1100' AND Status='ACTIVE') "
             "THEN 1 ELSE 0 END"
             delimited by size into SQLIO-STMT
           move "caller rollback restores stock, header and account"
             to TK-NAME
           perform Assert-Sql
           initialize Ws-Stock
           move "RECEIPT" to SK-FUNCTION
           move "STC" to SK-COMPANY
           move "BHM" to SK-SITE SM-SITE
           move "WHSE" to SK-USER
           move 20261012 to SK-BUSINESS-DATE SM-MOVEMENT-DATE
           move "ZZ-RCTX" to SM-ITEM-CODE
           move "A" to SM-REVISION
           move "MAIN" to SM-WAREHOUSE
           move "GOODSIN" to SM-LOCATION
           move "ZZ-RCTX-SM" to SM-LOT-CODE SM-SOURCE-DOC-REF
           move "OWNED" to SM-OWNER-TYPE
           move "ACCEPTED" to SM-CONDITION
           move "EA" to SM-UOM-CODE
           move 4 to SM-QUANTITY
           move 2.5 to SM-UNIT-COST
           move 10 to SM-VALUE-GBP
           call "STKMOVE" using Ws-Stock
           move "standalone stock movement commits driver UOW"
             to TK-NAME
           if SK-RETURN-CODE < 8 and SM-MOVEMENT-ID > 0
              and SM-VALUE-GBP = 10
               perform TK-PASS
           else
               move SK-MESSAGE to TK-DETAIL
               perform TK-FAIL
           end-if
           move "DISC" to SQLIO-FUNC
           call "SQLIO" using SQLIO-CA Probe
           move spaces to SQLIO-STMT
           string "SELECT CASE WHEN EXISTS (SELECT 1 FROM "
             "cst.ItemCost WHERE CompanyCode='STC' AND "
             "ItemCode='ZZ-RCTX' AND QtyOnHand=12 AND "
             "CarryingValueGBP=30) AND (SELECT SUM(QtyOnHand) "
             "FROM inv.StockBalance WHERE CompanyCode='STC' "
             "AND ItemCode='ZZ-RCTX')=12 THEN 1 ELSE 0 END"
             delimited by size into SQLIO-STMT
           move "standalone physical and valued stock survive reconnect"
             to TK-NAME
           perform Assert-Sql
           perform Cleanup
           perform TK-END
           stop run.

       Receipt-Setup.
           initialize Ws-Receipt
           move "CONFIRM" to CA-FUNCTION
           move "STC" to CA-COMPANY
           move "BHM" to CA-SITE
           move "WHSE" to CA-USER RC-RECEIVED-BY
           move 20261012 to CA-BUSINESS-DATE RC-PHYSICAL-DATE
             RC-POSTING-DATE
           move Receipt-Reference to RC-RECEIPT-REF
             RC-DELIVERY-NOTE CA-CORRELATION-ID RC-L-LOT(1)
           move "ZZ-RCTX-PO" to RC-ORDER-NUMBER
           move "S0004" to RC-SUPPLIER
           move "MAIN" to RC-WAREHOUSE
           move "RECEIPT" to RC-OWNERSHIP
           move "GBP" to RC-CURRENCY
           move 1 to RC-FX-RATE RC-LINE-CNT RC-L-NUM(1)
             RC-L-PO-LINE(1) RC-L-SCHEDULE(1) RC-L-FACTOR(1)
           move "ZZ-RCTX" to RC-L-ITEM(1)
           move "A" to RC-L-REVISION(1)
           move "Receipt transaction" to RC-L-DESC(1)
           move 8 to RC-L-ORDERED(1)
           if Scenario = 2
               move 4 to RC-L-COUNTED(1)
           end-if
           move 4 to RC-L-RECEIVED(1)
           move "EA" to RC-L-UOM(1)
           move "GOODSIN" to RC-L-LOCATION(1)
           move "LOT" to RC-L-TRACE(1)
           move "Y" to RC-L-STOCKED(1)
           move 2.5 to RC-L-PRICE(1).

       Assert-Receipt.
           move spaces to SQLIO-STMT
           string "SELECT COUNT(*) FROM po.Receipt r JOIN "
             "fin.Journal j ON j.JournalId=r.JournalId WHERE "
             "r.CompanyCode='STC' AND r.ReceiptRef='"
             function trim(Receipt-Reference)
             "' AND r.Status='CONFIRMED' AND r.GrniValueGBP=10 "
             "AND j.Status='POSTED' AND j.TotalDebitGBP=10 "
             "AND j.TotalCreditGBP=10 AND EXISTS (SELECT 1 FROM "
             "fin.JournalLine jl WHERE jl.JournalId=j.JournalId "
             "AND jl.AccountCode='1100' AND jl.DebitAmountGBP=10) "
             "AND EXISTS (SELECT 1 FROM fin.JournalLine jl WHERE "
             "jl.JournalId=j.JournalId AND jl.AccountCode='2010' "
             "AND jl.CreditAmountGBP=10)"
             delimited by size into SQLIO-STMT
           move "reconnected receipt has committed balanced GRNI"
             to TK-NAME
           perform Assert-Sql
           move spaces to SQLIO-STMT
           string "SELECT COUNT(*) FROM po.Receipt r JOIN "
             "po.ReceiptLine l ON l.ReceiptId=r.ReceiptId JOIN "
             "po.PurchaseOrderLine p ON "
             "p.PurchaseOrderLineId=l.PurchaseOrderLineId "
             "JOIN inv.StockMovement m ON m.MovementId=l.MovementId "
             "JOIN cst.ItemCostHistory h ON h.MovementId=m.MovementId "
             "WHERE r.CompanyCode='STC' AND r.ReceiptRef='"
             function trim(Receipt-Reference)
             "' AND l.ReceivedQty=4 AND l.AcceptedQty=4 "
             "AND l.StockQty=4 "
             "AND l.ProvisionalValueGBP=10 AND m.Quantity=4 "
             "AND m.ValueGBP=10 AND h.ValueChangeGBP=10 "
             "AND h.QtyAfter-h.QtyBefore=4"
             delimited by size into SQLIO-STMT
           move "committed line FK, quantity, cost and history agree"
             to TK-NAME
           perform Assert-Sql
           move spaces to SQLIO-STMT
           string "SELECT COUNT(*) FROM po.PurchaseOrder o JOIN "
             "po.PurchaseOrderLine l ON "
             "l.PurchaseOrderId=o.PurchaseOrderId JOIN "
             "po.PurchaseSchedule s ON "
             "s.PurchaseOrderLineId=l.PurchaseOrderLineId "
             "WHERE o.CompanyCode='STC' AND "
             "o.OrderNumber='ZZ-RCTX-PO' AND l.ReceivedQty="
             delimited by size into SQLIO-STMT
           if Scenario = 1
               move spaces to Query-Buffer
               string function trim(SQLIO-STMT) "4 AND "
                 "l.AcceptedQty=4 AND s.ReceivedQty=4 AND "
                 "s.Status='PARTIAL' AND "
                 "o.DeliveryStatus='PARTRECEIVED'"
                 delimited by size into Query-Buffer
           else
               move spaces to Query-Buffer
               string function trim(SQLIO-STMT) "8 AND "
                 "l.AcceptedQty=8 AND s.ReceivedQty=8 AND "
                 "s.Status='COMPLETE' AND "
                 "o.DeliveryStatus='FULLRECEIVED'"
                 delimited by size into Query-Buffer
           end-if
           move Query-Buffer to SQLIO-STMT
           move "committed order and schedule delivery agree"
             to TK-NAME
           perform Assert-Sql.

       Read-Scalar.
           move 0 to SQLIO-PARM-CNT
           move 1 to SQLIO-COL-CNT
           move 60 to SQLIO-COL-WIDTH(1)
           move 7 to SQLIO-CURSOR
           move "SNGL" to SQLIO-FUNC
           move spaces to Probe
           call "SQLIO" using SQLIO-CA Probe.

       Assert-Sql.
           perform Read-Scalar
           move 1 to TK-EXPECTED-N
           compute TK-ACTUAL-N = function numval(Probe)
           if SQLIO-ERROR
               move SQLIO-ERRMSG to TK-DETAIL
               perform TK-FAIL
           else
               perform TK-ASSERT-NUM
           end-if.

       Execute-Sql.
           move 0 to SQLIO-PARM-CNT SQLIO-COL-CNT
           move 7 to SQLIO-CURSOR
           move "EXEC" to SQLIO-FUNC
           call "SQLIO" using SQLIO-CA Probe
           if SQLIO-ERROR
               move "receipt fixture SQL executes" to TK-NAME
               move SQLIO-ERRMSG to TK-DETAIL
               perform TK-FAIL
           end-if
           move "CLOS" to SQLIO-FUNC
           call "SQLIO" using SQLIO-CA Probe.

       Cleanup.
           move spaces to SQLIO-STMT
           string "DELETE l FROM po.ReceiptLine l JOIN po.Receipt r "
             "ON r.ReceiptId=l.ReceiptId WHERE "
             "r.CompanyCode='STC' AND r.ReceiptRef LIKE 'ZZ-RCTX%';"
             "DELETE po.Receipt WHERE CompanyCode='STC' AND "
             "ReceiptRef LIKE 'ZZ-RCTX%'; DELETE l FROM "
             "fin.JournalLine l JOIN fin.Journal j ON "
             "j.JournalId=l.JournalId WHERE j.CompanyCode='STC' "
             "AND j.SourceRef LIKE 'ZZ-RCTX%'; DELETE fin.Journal "
             "WHERE CompanyCode='STC' AND SourceRef LIKE 'ZZ-RCTX%';"
             "DELETE cst.ItemCostHistory WHERE CompanyCode='STC' "
             "AND ItemCode='ZZ-RCTX'; DELETE cst.CostLayer WHERE "
             "CompanyCode='STC' AND ItemCode='ZZ-RCTX'; DELETE "
             "cst.ItemCost WHERE CompanyCode='STC' AND "
             "ItemCode='ZZ-RCTX'; DELETE inv.StockMovement WHERE "
             "CompanyCode='STC' AND ItemCode='ZZ-RCTX'; DELETE "
             "inv.StockBalance WHERE CompanyCode='STC' AND "
             "ItemCode='ZZ-RCTX'; DELETE inv.Lot WHERE "
             "CompanyCode='STC' AND ItemCode='ZZ-RCTX'; DELETE "
             "core.IdempotencyKey WHERE CorrelationId LIKE "
             "'ZZ-RCTX%'; DELETE po.PurchaseSchedule WHERE "
             "CompanyCode='STC' AND OrderNumber='ZZ-RCTX-PO'; "
             "DELETE po.PurchaseOrderLine WHERE CompanyCode='STC' "
             "AND OrderNumber='ZZ-RCTX-PO'; DELETE po.PurchaseOrder "
             "WHERE CompanyCode='STC' AND OrderNumber='ZZ-RCTX-PO';"
             "DELETE inv.Item WHERE CompanyCode='STC' AND "
             "ItemCode='ZZ-RCTX'"
             delimited by size into SQLIO-STMT
           perform Execute-Sql.
       copy TESTKITP.
