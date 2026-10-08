/*  96-xmd-jobs.sql  -  JCL style job streams in the report / job catalogue.
    The streams in jobs\*.cmd are registered in core.ReportDef with the reserved
    ReportCode prefix 'JOB.' so that the operator console (STERPOPS) and the
    Windows client launch them with jobs\<ProgramName>.cmd instead of
    bin\x86\<ProgramName>.exe.  An operator user is seeded as well so the night
    shift can sign on to the console without borrowing a clerk's code.
    Re-runnable: every statement is a MERGE or an existence test.
    REQ: XMD-005, XMD-007, RPT-015, COM-018, COM-002
    2026-10-06 STERP  original                                              */
SET NOCOUNT ON;
GO
/* ---------------------------------------------------------------- operator */
MERGE core.AppUser AS t
USING (VALUES
    ('OPS',      'Night Operations', 'Operations controller', 'STC', 'RDG', 0),
    ('OPSCAL',   'Caledon Operator', 'Operations controller', 'CAL', 'GLW', 0),
    ('OPSVIEW',  'Operations Viewer','Service desk (read only)', 'STC', 'RDG', 1)
) AS s (UserCode, DisplayName, JobTitle, DefaultCompanyCode, DefaultSiteCode, IsReadOnly)
   ON t.UserCode = s.UserCode
WHEN MATCHED THEN UPDATE SET DisplayName = s.DisplayName, JobTitle = s.JobTitle,
     DefaultCompanyCode = s.DefaultCompanyCode, DefaultSiteCode = s.DefaultSiteCode,
     IsReadOnly = s.IsReadOnly, IsActive = 1, ModifiedBy = 'SEED', ModifiedAt = SYSDATETIME()
WHEN NOT MATCHED THEN INSERT (UserCode, DisplayName, JobTitle, DefaultCompanyCode,
     DefaultSiteCode, IsReadOnly, IsActive, CreatedBy)
     VALUES (s.UserCode, s.DisplayName, s.JobTitle, s.DefaultCompanyCode,
             s.DefaultSiteCode, s.IsReadOnly, 1, 'SEED');
GO
/*  the operator holds the batch submission role where one exists - no new
    permissions are invented here, the existing BATCH/CLERK roles are reused.
    VATOWNER is included because the VAT return stream (jobs\VATRET.cmd) runs
    MTDJSON, which checks FIN.VAT.PREPARE on the submitting user.             */
INSERT core.UserRole (UserCode, RoleCode, CompanyCode, SiteCode, CreatedBy)
SELECT u.UserCode, r.RoleCode, c.CompanyCode, '*', 'SEED'
  FROM (VALUES ('OPS','STC'), ('OPSCAL','CAL'), ('OPSVIEW','STC')) AS u (UserCode, CompanyCode)
  JOIN core.Company c ON c.CompanyCode = u.CompanyCode
  JOIN core.Role r ON r.RoleCode IN ('BATCH','OPERATOR','GLCLERK','VATOWNER')
 WHERE NOT EXISTS (SELECT 1 FROM core.UserRole x
                    WHERE x.UserCode = u.UserCode AND x.RoleCode = r.RoleCode
                      AND x.CompanyCode = c.CompanyCode AND x.SiteCode = '*');
GO
/* ------------------------------------------------------------- job streams */
MERGE core.ReportDef AS t
USING (VALUES
 ('JOB.NIGHTLY',  'XMD', 'Nightly service stream',
  'NIGHTLY',  'CO,USER,MODE,ASAT', NULL, 'XMD-005,XMD-007,COM-017',
  'STEP010 exception ageing, STEP020 recurring journals, STEP030 aged debt, STEP040 aged creditors, STEP050 six month VAT, STEP060 FX staleness. COND 0/4 continue, 8 stops.'),
 ('JOB.MONTHEND', 'XMD', 'Period end stream',
  'MONTHEND', 'CO,YEAR,PERIOD,MODE,USER,RESTART', NULL, 'XMD-005,XMD-002,RPT-016',
  'Revaluation, accrual release, inventory valuation, control reconciliations, trial balance, profit and loss, balance sheet. RESTART=STEPnnn resumes.'),
 ('JOB.PAYRUN',   'XMD', 'Supplier payment run stream',
  'PAYRUN',   'CO,RUN,USER,MODE', 'AP.PAYMENT.RUN', 'XMD-005,AP-015,AP-016',
  'Locates the proposal, refuses anything not approved, then Bacs file and remittances.'),
 ('JOB.DDCOLL',   'XMD', 'Direct debit collection stream',
  'DDCOLL',   'CO,ASAT,SUN,MODE,USER', NULL, 'XMD-005,AR-019,AR-020',
  'AR direct debit collection schedule and Bacs collection file.'),
 ('JOB.VATRET',   'XMD', 'VAT return and MTD extract stream',
  'VATRET',   'CO,VATPER,MODE,USER', NULL, 'XMD-005,FIN-025,FIN-026',
  'Nine box return for the VAT period and the Making Tax Digital submission body.'),
 ('JOB.STMTS',    'XMD', 'Statements and dunning stream',
  'STMTS',    'CO,ASAT,STYLE,STAGE,MODE,USER', NULL, 'XMD-005,AR-012,AR-013',
  'Customer statements followed by the collection reminder run.'),
 ('JOB.YEAREND',  'XMD', 'Year end close stream',
  'YEAREND',  'CO,YEAR,MODE,USER,RESTART', NULL, 'XMD-005,FIN-027,RPT-001',
  'Final statutory set, close to retained earnings, open the new year.')
) AS s (ReportCode, Module, Title, ProgramName, ParamSpec, PermissionCode,
        RequirementRefs, Description)
   ON t.ReportCode = s.ReportCode
WHEN MATCHED THEN UPDATE SET Module = s.Module, Title = s.Title,
     ProgramName = s.ProgramName, ParamSpec = s.ParamSpec,
     PermissionCode = CASE WHEN EXISTS (SELECT 1 FROM core.Permission p
                                         WHERE p.PermissionCode = s.PermissionCode)
                           THEN s.PermissionCode ELSE NULL END,
     RequirementRefs = s.RequirementRefs, Description = s.Description,
     IsActive = 1, ModifiedBy = 'SEED', ModifiedAt = SYSDATETIME()
WHEN NOT MATCHED THEN INSERT (ReportCode, Module, Title, ProgramName, ParamSpec,
     PermissionCode, RequirementRefs, Description, IsActive, CreatedBy)
     VALUES (s.ReportCode, s.Module, s.Title, s.ProgramName, s.ParamSpec,
             CASE WHEN EXISTS (SELECT 1 FROM core.Permission p
                                WHERE p.PermissionCode = s.PermissionCode)
                  THEN s.PermissionCode ELSE NULL END,
             s.RequirementRefs, s.Description, 1, 'SEED');
GO
/* ---------------------------------------------- menu rows for the operator */
/*  the streams hang under the existing operations branch when there is one,
    otherwise under the root - the shell tolerates either                     */
IF EXISTS (SELECT 1 FROM sys.columns WHERE object_id = OBJECT_ID('core.MenuItem') AND name = 'ParentMenuCode')
   AND NOT EXISTS (SELECT 1 FROM core.MenuItem WHERE MenuCode = 'XMD.JOBS')
BEGIN
    DECLARE @parent varchar(20) =
        (SELECT TOP 1 MenuCode FROM core.MenuItem WHERE MenuCode IN ('SYS','SYSTEM','OPS','TOOLS') ORDER BY MenuCode);
    INSERT core.MenuItem (MenuCode, ParentMenuCode, Caption, SortOrder, IsActive, CreatedBy)
    SELECT 'XMD.JOBS', @parent, 'Job streams', 900, 1, 'SEED'
     WHERE @parent IS NOT NULL;
END
GO
PRINT 'STERP-I-SEED 96-xmd-jobs.sql applied';
GO
