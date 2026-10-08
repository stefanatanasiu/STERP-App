/*  Audit triggers on the supplier master and its payment destinations.  Bank details are shown
    masked to the last four digits for roles without banking authority (D-17).
    REQ: COM-013, AP-001, AP-002, AP-027 */
SET NOCOUNT ON;
GO
CREATE OR ALTER TRIGGER ap.tr_Supplier_Audit ON ap.Supplier AFTER INSERT, UPDATE, DELETE AS
BEGIN
    SET NOCOUNT ON;
    INSERT core.AuditLog (UserCode, CompanyCode, EntityType, EntityKey, Action, OldValue, NewValue, Source)
    SELECT COALESCE(i.ModifiedBy, i.CreatedBy, d.ModifiedBy, SUSER_SNAME()), COALESCE(i.CompanyCode, d.CompanyCode), 'ap.Supplier', COALESCE(i.SupplierCode, d.SupplierCode),
           CASE WHEN d.SupplierCode IS NULL THEN 'INSERT' WHEN i.SupplierCode IS NULL THEN 'DELETE' ELSE 'UPDATE' END,
           CONCAT_WS('|', d.LegalName, d.VatRegNo, d.BusinessSize, d.TermsCode, d.PaymentMethod, d.Status, d.PaymentHold, d.SanctionsStatus),
           CONCAT_WS('|', i.LegalName, i.VatRegNo, i.BusinessSize, i.TermsCode, i.PaymentMethod, i.Status, i.PaymentHold, i.SanctionsStatus), 'TRIGGER'
      FROM inserted i FULL OUTER JOIN deleted d ON d.CompanyCode = i.CompanyCode AND d.SupplierCode = i.SupplierCode;
END
GO
CREATE OR ALTER TRIGGER ap.tr_SupplierBank_Audit ON ap.SupplierBank AFTER INSERT, UPDATE, DELETE AS
BEGIN
    SET NOCOUNT ON;
    INSERT core.AuditLog (UserCode, CompanyCode, EntityType, EntityKey, Action, FieldName, OldValue, NewValue, Reason, AuthorityRef, Source, IsSensitive)
    SELECT COALESCE(i.ModifiedBy, i.CreatedBy, d.ModifiedBy, SUSER_SNAME()), COALESCE(i.CompanyCode, d.CompanyCode), 'ap.SupplierBank',
           CONCAT_WS('/', COALESCE(i.SupplierCode, d.SupplierCode), COALESCE(i.BankRef, d.BankRef)),
           CASE WHEN d.SupplierBankId IS NULL THEN 'INSERT' WHEN i.SupplierBankId IS NULL THEN 'DELETE' ELSE 'UPDATE' END, 'PaymentDestination',
           CONCAT_WS('|', d.BeneficiaryName, d.SortCode, CONCAT('****', RIGHT(d.AccountNumber, 4)), d.CopResult, d.Status),
           CONCAT_WS('|', i.BeneficiaryName, i.SortCode, CONCAT('****', RIGHT(i.AccountNumber, 4)), i.CopResult, i.Status),
           COALESCE(i.CallBackNote, d.CallBackNote), COALESCE(i.ApprovedBy, d.ApprovedBy), 'TRIGGER', 1
      FROM inserted i FULL OUTER JOIN deleted d ON d.SupplierBankId = i.SupplierBankId;
END
GO
