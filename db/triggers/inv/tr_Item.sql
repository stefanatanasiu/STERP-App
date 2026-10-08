/*  Audit trigger on the item master - classification, units, traceability, VAT class and
    lifecycle are all protected data.  REQ: COM-013, ITM-004, ITM-010, ITM-020 */
SET NOCOUNT ON;
GO
CREATE OR ALTER TRIGGER inv.tr_Item_Audit ON inv.Item AFTER INSERT, UPDATE, DELETE AS
BEGIN
    SET NOCOUNT ON;
    INSERT core.AuditLog (UserCode, CompanyCode, EntityType, EntityKey, Action, OldValue, NewValue, Reason, Source)
    SELECT COALESCE(i.ModifiedBy, i.CreatedBy, d.ModifiedBy, SUSER_SNAME()), COALESCE(i.CompanyCode, d.CompanyCode), 'inv.Item', COALESCE(i.ItemCode, d.ItemCode),
           CASE WHEN d.ItemCode IS NULL THEN 'INSERT' WHEN i.ItemCode IS NULL THEN 'DELETE' ELSE 'UPDATE' END,
           CONCAT_WS('|', d.Description, d.ItemClass, d.StockUomCode, d.TraceabilityType, d.ValuationGroup, d.CostMethod, d.LifecycleStatus),
           CONCAT_WS('|', i.Description, i.ItemClass, i.StockUomCode, i.TraceabilityType, i.ValuationGroup, i.CostMethod, i.LifecycleStatus),
           COALESCE(i.StatusReason, d.StatusReason), 'TRIGGER'
      FROM inserted i FULL OUTER JOIN deleted d ON d.CompanyCode = i.CompanyCode AND d.ItemCode = i.ItemCode;
END
GO
