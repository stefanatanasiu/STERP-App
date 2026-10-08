/*  Audit triggers on core master data: company statutory profile, users, roles, role permissions
    and policy parameters.  Every create, change and removal leaves an attributable history.
    REQ: COM-013, COM-021, COM-025 */
SET NOCOUNT ON;
GO
CREATE OR ALTER TRIGGER core.tr_Company_Audit ON core.Company AFTER INSERT, UPDATE, DELETE AS
BEGIN
    SET NOCOUNT ON;
    INSERT core.AuditLog (UserCode, CompanyCode, EntityType, EntityKey, Action, OldValue, NewValue, Source)
    SELECT COALESCE(i.ModifiedBy, i.CreatedBy, d.ModifiedBy, SUSER_SNAME()), COALESCE(i.CompanyCode, d.CompanyCode), 'core.Company', COALESCE(i.CompanyCode, d.CompanyCode),
           CASE WHEN d.CompanyCode IS NULL THEN 'INSERT' WHEN i.CompanyCode IS NULL THEN 'DELETE' ELSE 'UPDATE' END,
           CONCAT_WS('|', d.Name, d.RegisteredNumber, d.VatRegNo, d.VatScheme, d.AccountingFramework, d.IsActive),
           CONCAT_WS('|', i.Name, i.RegisteredNumber, i.VatRegNo, i.VatScheme, i.AccountingFramework, i.IsActive), 'TRIGGER'
      FROM inserted i FULL OUTER JOIN deleted d ON d.CompanyCode = i.CompanyCode;
END
GO
CREATE OR ALTER TRIGGER core.tr_AppUser_Audit ON core.AppUser AFTER INSERT, UPDATE, DELETE AS
BEGIN
    SET NOCOUNT ON;
    INSERT core.AuditLog (UserCode, EntityType, EntityKey, Action, OldValue, NewValue, Source, IsSensitive)
    SELECT COALESCE(i.ModifiedBy, i.CreatedBy, d.ModifiedBy, SUSER_SNAME()), 'core.AppUser', COALESCE(i.UserCode, d.UserCode),
           CASE WHEN d.UserCode IS NULL THEN 'INSERT' WHEN i.UserCode IS NULL THEN 'DELETE' ELSE 'UPDATE' END,
           CONCAT_WS('|', d.DisplayName, d.WindowsLogin, d.IsReadOnly, d.IsActive), CONCAT_WS('|', i.DisplayName, i.WindowsLogin, i.IsReadOnly, i.IsActive), 'TRIGGER', 1
      FROM inserted i FULL OUTER JOIN deleted d ON d.UserCode = i.UserCode;
END
GO
CREATE OR ALTER TRIGGER core.tr_UserRole_Audit ON core.UserRole AFTER INSERT, UPDATE, DELETE AS
BEGIN
    SET NOCOUNT ON;
    INSERT core.AuditLog (UserCode, CompanyCode, EntityType, EntityKey, Action, OldValue, NewValue, Source, IsSensitive)
    SELECT COALESCE(i.ModifiedBy, i.CreatedBy, d.ModifiedBy, SUSER_SNAME()), COALESCE(i.CompanyCode, d.CompanyCode), 'core.UserRole',
           CONCAT_WS('/', COALESCE(i.UserCode, d.UserCode), COALESCE(i.RoleCode, d.RoleCode), COALESCE(i.CompanyCode, d.CompanyCode), COALESCE(i.SiteCode, d.SiteCode)),
           CASE WHEN d.UserCode IS NULL THEN 'GRANT' WHEN i.UserCode IS NULL THEN 'REVOKE' ELSE 'AMEND' END,
           CONCAT_WS('|', d.ValidFrom, d.ValidTo), CONCAT_WS('|', i.ValidFrom, i.ValidTo), 'TRIGGER', 1
      FROM inserted i FULL OUTER JOIN deleted d ON d.UserCode = i.UserCode AND d.RoleCode = i.RoleCode AND d.CompanyCode = i.CompanyCode AND d.SiteCode = i.SiteCode;
END
GO
CREATE OR ALTER TRIGGER core.tr_RolePermission_Audit ON core.RolePermission AFTER INSERT, UPDATE, DELETE AS
BEGIN
    SET NOCOUNT ON;
    INSERT core.AuditLog (UserCode, EntityType, EntityKey, Action, OldValue, NewValue, Source, IsSensitive)
    SELECT COALESCE(i.ModifiedBy, i.CreatedBy, d.ModifiedBy, SUSER_SNAME()), 'core.RolePermission', CONCAT_WS('/', COALESCE(i.RoleCode, d.RoleCode), COALESCE(i.PermissionCode, d.PermissionCode)),
           CASE WHEN d.RoleCode IS NULL THEN 'GRANT' WHEN i.RoleCode IS NULL THEN 'REVOKE' ELSE 'AMEND' END,
           d.PermissionCode, i.PermissionCode, 'TRIGGER', 1
      FROM inserted i FULL OUTER JOIN deleted d ON d.RoleCode = i.RoleCode AND d.PermissionCode = i.PermissionCode;
END
GO
CREATE OR ALTER TRIGGER core.tr_Parameter_Audit ON core.Parameter AFTER INSERT, UPDATE, DELETE AS
BEGIN
    SET NOCOUNT ON;
    INSERT core.AuditLog (UserCode, CompanyCode, EntityType, EntityKey, Action, FieldName, OldValue, NewValue, AuthorityRef, Source)
    SELECT COALESCE(i.ModifiedBy, i.CreatedBy, d.ModifiedBy, SUSER_SNAME()), COALESCE(i.CompanyCode, d.CompanyCode), 'core.Parameter',
           CONCAT_WS('/', COALESCE(i.ParamCode, d.ParamCode), COALESCE(i.CompanyCode, d.CompanyCode), CONVERT(varchar(10), COALESCE(i.EffectiveFrom, d.EffectiveFrom), 23)),
           CASE WHEN d.ParamCode IS NULL THEN 'INSERT' WHEN i.ParamCode IS NULL THEN 'DELETE' ELSE 'UPDATE' END,
           'Value', d.Value, i.Value, COALESCE(i.DecisionRef, d.DecisionRef), 'TRIGGER'
      FROM inserted i FULL OUTER JOIN deleted d ON d.ParamCode = i.ParamCode AND d.CompanyCode = i.CompanyCode AND d.EffectiveFrom = i.EffectiveFrom;
END
GO
