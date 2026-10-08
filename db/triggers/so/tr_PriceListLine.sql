/*  Audit trigger on selling prices - a price change is a protected commercial change.
    REQ: COM-013, SO-004, COM-003 */
SET NOCOUNT ON;
GO
CREATE OR ALTER TRIGGER so.tr_PriceListLine_Audit ON so.PriceListLine AFTER INSERT, UPDATE, DELETE AS
BEGIN
    SET NOCOUNT ON;
    INSERT core.AuditLog (UserCode, CompanyCode, EntityType, EntityKey, Action, FieldName, OldValue, NewValue, Source)
    SELECT COALESCE(i.ModifiedBy, i.CreatedBy, d.ModifiedBy, SUSER_SNAME()), COALESCE(i.CompanyCode, d.CompanyCode), 'so.PriceListLine',
           CONCAT_WS('/', COALESCE(i.PriceListCode, d.PriceListCode), COALESCE(i.ItemCode, d.ItemCode), COALESCE(i.QtyBreak, d.QtyBreak)),
           CASE WHEN d.PriceLineId IS NULL THEN 'INSERT' WHEN i.PriceLineId IS NULL THEN 'DELETE' ELSE 'UPDATE' END, 'UnitPrice',
           CONCAT_WS('|', d.CurrencyCode, d.UnitPrice, d.DiscountPct, d.ValidFrom, d.ValidTo),
           CONCAT_WS('|', i.CurrencyCode, i.UnitPrice, i.DiscountPct, i.ValidFrom, i.ValidTo), 'TRIGGER'
      FROM inserted i FULL OUTER JOIN deleted d ON d.PriceLineId = i.PriceLineId;
END
GO
