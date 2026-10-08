/*  Development cleardown for the PODEMO purchasing demonstration.  Not deployed by build-db -
    run it by hand (sqlcmd -S . -d STERP -E -C -i run\podemo-reset.sql) when a partial PODEMO run
    has to be rebuilt from nothing.  PODEMO itself refuses to rebuild while its own orders exist. */
DELETE FROM po.PurchaseSchedule;
DELETE FROM po.PurchaseOrderLine;
DELETE FROM po.OrderAmendment;
DELETE FROM po.BlanketBalance;
DELETE FROM po.Commitment;
DELETE FROM po.ServiceAcceptance;
DELETE FROM po.ReceiptLine;
DELETE FROM po.Receipt;
DELETE FROM po.SupplierReturnLine;
DELETE FROM po.SupplierReturn;
DELETE FROM po.ImportEntryLine;
DELETE FROM po.ImportEntry;
DELETE FROM po.PurchaseOrder;
DELETE FROM po.RequisitionLine;
DELETE FROM po.Requisition;
DELETE FROM po.SourcingQuote;
DELETE FROM po.SourcingEvent;
DELETE FROM core.Approval WHERE DocumentType IN ('PURCHASEORDER', 'REQUISITION');
DELETE FROM core.DocumentNumber WHERE CorrelationId LIKE 'PODEMO-%';
DELETE FROM inv.StockMovement WHERE CorrelationId LIKE 'PODEMO-%';
/* The stock service is idempotent on the correlation identifier: leaving the
   keys behind makes every later run replay as a duplicate and post no stock,
   so the receipts appear to work but no balance ever moves. */
DELETE FROM core.IdempotencyKey WHERE CorrelationId LIKE 'PODEMO-%';
/* Balances and lots the demonstration receipts left in goods-in. */
DELETE FROM inv.StockBalance WHERE LotCode LIKE 'PODEMO-%';
DELETE FROM inv.Lot WHERE LotCode LIKE 'PODEMO-%';
DELETE b FROM inv.StockBalance b
 WHERE b.LocationCode = 'GOODSIN'
   AND NOT EXISTS (SELECT 1 FROM inv.StockMovement m
                    WHERE m.CompanyCode = b.CompanyCode AND m.SiteCode = b.SiteCode
                      AND m.WarehouseCode = b.WarehouseCode AND m.LocationCode = b.LocationCode
                      AND m.ItemCode = b.ItemCode AND m.LotCode = b.LotCode);
GO
