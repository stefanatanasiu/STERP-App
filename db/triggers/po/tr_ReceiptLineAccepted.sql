SET NOCOUNT ON;
GO
CREATE OR ALTER TRIGGER po.tr_ReceiptLineAccepted
ON po.ReceiptLine AFTER UPDATE
AS
BEGIN
    SET NOCOUNT ON;
    IF NOT UPDATE(AcceptedQty) RETURN;
    ;WITH changes AS (
        SELECT i.PurchaseOrderLineId, Delta = SUM(i.AcceptedQty - d.AcceptedQty)
          FROM inserted i JOIN deleted d ON d.ReceiptLineId = i.ReceiptLineId
          JOIN po.Receipt r ON r.ReceiptId = i.ReceiptId AND r.Status = 'CONFIRMED'
         WHERE i.PurchaseOrderLineId IS NOT NULL
         GROUP BY i.PurchaseOrderLineId)
    UPDATE p SET AcceptedQty = p.AcceptedQty + c.Delta
      FROM po.PurchaseOrderLine p JOIN changes c ON c.PurchaseOrderLineId = p.PurchaseOrderLineId;
END
GO
