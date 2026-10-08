/*  SYSADM holds the whole permission catalogue (03-core-roles-users.sql).  That grant runs
    before the module seeds register their own permissions (the SO price list, rebate, invoice
    run and document print permissions arrive in 72/73), so on a fresh build the administrator
    was refused the screens those permissions guard.  Running last, this grants SYSADM anything
    a later seed added.  Re-runnable.  REQ: COM-002 */
SET NOCOUNT ON;
GO
INSERT core.RolePermission (RoleCode, PermissionCode, CreatedBy)
SELECT 'SYSADM', p.PermissionCode, 'SEED' FROM core.Permission p
 WHERE NOT EXISTS (SELECT 1 FROM core.RolePermission r WHERE r.RoleCode = 'SYSADM' AND r.PermissionCode = p.PermissionCode);
GO
