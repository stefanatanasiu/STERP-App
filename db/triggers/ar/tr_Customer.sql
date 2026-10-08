/*  Audit triggers on the customer master and its credit policy.  REQ: COM-013, AR-001, AR-008, D-06 */
SET NOCOUNT ON;
GO
CREATE OR ALTER TRIGGER ar.tr_Customer_Audit ON ar.Customer AFTER INSERT, UPDATE, DELETE AS
BEGIN
    SET NOCOUNT ON;
    INSERT core.AuditLog (UserCode, CompanyCode, EntityType, EntityKey, Action, OldValue, NewValue, Source, IsSensitive)
    SELECT COALESCE(i.ModifiedBy, i.CreatedBy, d.ModifiedBy, SUSER_SNAME()), COALESCE(i.CompanyCode, d.CompanyCode), 'ar.Customer', COALESCE(i.CustomerCode, d.CustomerCode),
           CASE WHEN d.CustomerCode IS NULL THEN 'INSERT' WHEN i.CustomerCode IS NULL THEN 'DELETE' ELSE 'UPDATE' END,
           CONCAT_WS('|', d.LegalName, d.VatRegNo, d.VatLocation, d.TermsCode, d.CurrencyCode, d.Status, d.SanctionsStatus),
           CONCAT_WS('|', i.LegalName, i.VatRegNo, i.VatLocation, i.TermsCode, i.CurrencyCode, i.Status, i.SanctionsStatus), 'TRIGGER',
           CASE WHEN COALESCE(i.IsPersonalData, d.IsPersonalData) = 1 THEN 1 ELSE 0 END
      FROM inserted i FULL OUTER JOIN deleted d ON d.CompanyCode = i.CompanyCode AND d.CustomerCode = i.CustomerCode;
END
GO
CREATE OR ALTER TRIGGER ar.tr_CustomerCredit_Audit ON ar.CustomerCredit AFTER INSERT, UPDATE, DELETE AS
BEGIN
    SET NOCOUNT ON;
    INSERT core.AuditLog (UserCode, CompanyCode, EntityType, EntityKey, Action, FieldName, OldValue, NewValue, Reason, AuthorityRef, Source)
    SELECT COALESCE(i.ModifiedBy, i.CreatedBy, d.ModifiedBy, SUSER_SNAME()), COALESCE(i.CompanyCode, d.CompanyCode), 'ar.CustomerCredit', COALESCE(i.CustomerCode, d.CustomerCode),
           CASE WHEN d.CustomerCode IS NULL THEN 'INSERT' WHEN i.CustomerCode IS NULL THEN 'DELETE' ELSE 'UPDATE' END, 'CreditLimitGBP',
           CONCAT_WS('|', d.CreditLimitGBP, d.HoldFlag, d.RiskClass), CONCAT_WS('|', i.CreditLimitGBP, i.HoldFlag, i.RiskClass),
           COALESCE(i.HoldReason, d.HoldReason), COALESCE(i.ApprovedBy, d.ApprovedBy), 'TRIGGER'
      FROM inserted i FULL OUTER JOIN deleted d ON d.CompanyCode = i.CompanyCode AND d.CustomerCode = i.CustomerCode;
END
GO
