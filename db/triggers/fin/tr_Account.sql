/*  Audit trigger on the chart of accounts - a referenced account cannot be reclassified or
    retired without an attributable history.  REQ: COM-013, FIN-001 */
SET NOCOUNT ON;
GO
CREATE OR ALTER TRIGGER fin.tr_Account_Audit ON fin.Account AFTER INSERT, UPDATE, DELETE AS
BEGIN
    SET NOCOUNT ON;
    INSERT core.AuditLog (UserCode, CompanyCode, EntityType, EntityKey, Action, OldValue, NewValue, Source)
    SELECT COALESCE(i.ModifiedBy, i.CreatedBy, d.ModifiedBy, SUSER_SNAME()), COALESCE(i.CompanyCode, d.CompanyCode), 'fin.Account', COALESCE(i.AccountCode, d.AccountCode),
           CASE WHEN d.AccountCode IS NULL THEN 'INSERT' WHEN i.AccountCode IS NULL THEN 'DELETE' ELSE 'UPDATE' END,
           CONCAT_WS('|', d.Name, d.ClassCode, d.AccountKind, d.ControlType, d.StatutoryLineCode, d.Status),
           CONCAT_WS('|', i.Name, i.ClassCode, i.AccountKind, i.ControlType, i.StatutoryLineCode, i.Status), 'TRIGGER'
      FROM inserted i FULL OUTER JOIN deleted d ON d.CompanyCode = i.CompanyCode AND d.AccountCode = i.AccountCode;
END
GO
