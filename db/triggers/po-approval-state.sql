/* The generic approvals inbox and POAPPRV decide the same versioned request.
   Apply its purchase-order state atomically, regardless of the entrypoint. */
CREATE OR ALTER TRIGGER core.trg_ApprovalPurchaseOrder ON core.Approval
AFTER UPDATE
AS
BEGIN
    SET NOCOUNT ON;
    IF EXISTS (
        SELECT 1
        FROM inserted i
        JOIN deleted d ON d.ApprovalId = i.ApprovalId
        JOIN po.PurchaseOrder p ON p.CompanyCode = i.CompanyCode AND p.OrderNumber = i.DocumentRef
        WHERE i.DocumentType = 'PURCHASEORDER' AND d.Status = 'PENDING'
          AND i.Status IN ('APPROVED', 'REJECTED')
          AND ISNULL(p.ContentHash, '') <> i.ContentHash
    )
        THROW 50021, 'STERP-E-0021: The purchase order has changed since this approval was requested. Request approval for its current content.', 1;

    UPDATE p
       SET Status = CASE WHEN i.Status = 'APPROVED' THEN 'APPROVED' ELSE 'DRAFT' END,
           ApprovalId = i.ApprovalId,
           ApprovedBy = i.DecidedBy,
           ApprovedAt = CASE WHEN i.Status = 'APPROVED' THEN i.DecidedAt ELSE NULL END,
           ModifiedBy = i.DecidedBy,
           ModifiedAt = SYSDATETIME()
    FROM po.PurchaseOrder p
    JOIN inserted i ON i.CompanyCode = p.CompanyCode AND i.DocumentRef = p.OrderNumber
    JOIN deleted d ON d.ApprovalId = i.ApprovalId
    WHERE i.DocumentType = 'PURCHASEORDER' AND d.Status = 'PENDING'
      AND i.Status IN ('APPROVED', 'REJECTED')
      AND p.Status IN ('DRAFT', 'AWAITAPPROVAL', 'AMENDED')
      AND ISNULL(p.ContentHash, '') = i.ContentHash;
END;
GO
