SET NOCOUNT ON;
PRINT '# po-approval-sync';
GO
BEGIN TRANSACTION;
DECLARE @id bigint;
SELECT TOP (1) @id = ApprovalId FROM core.Approval
WHERE CompanyCode = 'STC' AND DocumentType = 'PURCHASEORDER' AND DocumentRef = 'PO00000002';
UPDATE po.PurchaseOrder SET Status = 'AWAITAPPROVAL', ContentHash = 'QA-APPROVAL-CURRENT',
    ApprovedBy = NULL, ApprovedAt = NULL WHERE CompanyCode = 'STC' AND OrderNumber = 'PO00000002';
UPDATE core.Approval SET Status = 'PENDING', RequestedBy = 'BUYER1', ContentHash = 'QA-APPROVAL-CURRENT'
WHERE ApprovalId = @id;
EXEC core.usp_DecideApproval @id, 'PURMGR', 'APPROVED', 'QA inbox approval';
IF EXISTS (SELECT 1 FROM po.PurchaseOrder WHERE CompanyCode = 'STC' AND OrderNumber = 'PO00000002'
    AND Status = 'APPROVED' AND ApprovalId = @id AND ApprovedBy = 'PURMGR' AND ApprovedAt IS NOT NULL)
    PRINT 'ok 1 - generic inbox approval transitions the exact purchase-order version';
ELSE PRINT 'not ok 1 - generic inbox approval did not update the order';
UPDATE po.PurchaseOrder SET Status = 'AWAITAPPROVAL' WHERE CompanyCode = 'STC' AND OrderNumber = 'PO00000002';
UPDATE core.Approval SET Status = 'PENDING' WHERE ApprovalId = @id;
EXEC core.usp_DecideApproval @id, 'PURMGR', 'REJECTED', 'QA inbox rejection';
IF EXISTS (SELECT 1 FROM po.PurchaseOrder WHERE CompanyCode = 'STC' AND OrderNumber = 'PO00000002'
    AND Status = 'DRAFT' AND ApprovedBy = 'PURMGR' AND ApprovedAt IS NULL)
    PRINT 'ok 2 - rejection returns the pending order to draft without an approval timestamp';
ELSE PRINT 'not ok 2 - rejection left an approved or awaiting-approval order';
ROLLBACK TRANSACTION;
GO
DECLARE @id bigint, @error nvarchar(4000) = '';
BEGIN TRY
    BEGIN TRANSACTION;
    SELECT TOP (1) @id = ApprovalId FROM core.Approval
    WHERE CompanyCode = 'STC' AND DocumentType = 'PURCHASEORDER' AND DocumentRef = 'PO00000002';
    UPDATE po.PurchaseOrder SET Status = 'AWAITAPPROVAL', ContentHash = 'QA-APPROVAL-NEW'
    WHERE CompanyCode = 'STC' AND OrderNumber = 'PO00000002';
    UPDATE core.Approval SET Status = 'PENDING', RequestedBy = 'BUYER1', ContentHash = 'QA-APPROVAL-OLD'
    WHERE ApprovalId = @id;
    EXEC core.usp_DecideApproval @id, 'PURMGR', 'APPROVED', 'QA stale approval';
END TRY
BEGIN CATCH
    SET @error = ERROR_MESSAGE();
END CATCH;
IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
IF @error LIKE '%purchase order has changed%'
    PRINT 'ok 3 - a stale inbox request cannot approve changed purchase-order content';
ELSE PRINT 'not ok 3 - stale approval result: ' + @error;
GO
