/*  85-kit-menu.sql - the Kits menu, the report catalogue and the generic maintenance definition
    for commercial kit definitions.  Re-runnable (MERGE).

    The menu follows the shape of the work: define the kit, prove it can be built (trial), then
    execute it (reserve, pick, issue, complete), then the enquiries that answer awkward questions
    afterwards - genealogy for a recall, cost for the close.

    REQ: KIT-001, KIT-006, KIT-007, KIT-008, KIT-019, KIT-020, KIT-021, KIT-022, KIT-023, RPT-013
*/
SET NOCOUNT ON;
GO
IF OBJECT_ID(N'core.MenuItem') IS NOT NULL
BEGIN
    DECLARE @m TABLE (MenuCode varchar(40), ParentMenuCode varchar(40), Caption varchar(80),
                      FormClass varchar(60), FormArgument varchar(60), PermissionCode varchar(40),
                      SortOrder int, Shortcut varchar(20));
    INSERT @m VALUES
      ('KIT',            NULL,  '&Kits',                     NULL,                NULL,          NULL,          75, NULL),
      ('KIT.MAINT',      'KIT', 'Kit &Maintenance...',        'frmKitMaint',       NULL,          'KIT.EXEC.CREATE',   10, NULL),
      ('KIT.DEFS',       'KIT', 'Commercial Kit &Definitions...','frmGenericMaint','KIT.DEF',     'KIT.DEF.MAINT',   20, NULL),
      ('KIT.TRIAL',      'KIT', '&Trial Kitting...',          'frmKitTrial',       NULL,          'KIT.TRIAL.RUN',   40, 'Ctrl+T'),
      ('KIT.EXEC',       'KIT', 'Kit &Execution...',          'frmKitExecution',   NULL,          'KIT.ISSUE.POST',   50, NULL),
      ('KIT.GENEALOGY',  'KIT', '&Genealogy Enquiry...',      'frmKitGenealogy',   NULL,          'KIT.ENQUIRY.VIEW',    60, NULL),
      ('KIT.DISASSY',    'KIT', '&Disassembly...',            'frmGenericMaint',   'KIT.DISASSY', 'KIT.DISASSEMBLE.POST', 70, NULL),
      ('KIT.RPT.READY',  'KIT', 'Kit &Readiness...',          'frmReportLauncher', 'KITRP010',    'KIT.ENQUIRY.VIEW',    90, NULL),
      ('KIT.RPT.EXEC',   'KIT', 'Kit E&xecution Position...', 'frmReportLauncher', 'KITRP020',    'KIT.ENQUIRY.VIEW',   100, NULL),
      ('KIT.RPT.SHORT',  'KIT', 'Kit &Shortage List...',      'frmReportLauncher', 'KITRP030',    'KIT.ENQUIRY.VIEW',   110, NULL);

    MERGE core.MenuItem AS t USING @m AS s ON t.MenuCode = s.MenuCode
    WHEN MATCHED THEN UPDATE SET ParentMenuCode = s.ParentMenuCode, Caption = s.Caption,
            FormClass = s.FormClass, FormArgument = s.FormArgument, PermissionCode = s.PermissionCode,
            SortOrder = s.SortOrder, Shortcut = s.Shortcut, IsActive = 1
    WHEN NOT MATCHED BY TARGET THEN
        INSERT (MenuCode, ParentMenuCode, Caption, FormClass, FormArgument, PermissionCode, SortOrder, Shortcut, IsActive)
        VALUES (s.MenuCode, s.ParentMenuCode, s.Caption, s.FormClass, s.FormArgument, s.PermissionCode, s.SortOrder, s.Shortcut, 1);

    PRINT 'Seeded core.MenuItem (kits)';
END
GO
IF OBJECT_ID(N'core.ReportDef') IS NOT NULL
BEGIN
    DECLARE @r TABLE (ReportCode varchar(20), Module varchar(10), Title varchar(100), ProgramName varchar(40),
                      ParamSpec varchar(200), PermissionCode varchar(40), RequirementRefs varchar(200));
    INSERT @r VALUES
      ('KITRP010', 'KIT', 'Kit readiness to pick (RPT-013)', 'KITRDY',
       'CO,SITE,STATUS,ASAT,MODE=READY,USER', 'KIT.ENQUIRY.VIEW', 'KIT-008,KIT-009,KIT-022'),
      ('KITRP020', 'KIT', 'Kit execution position (RPT-013)', 'KITRDY',
       'CO,SITE,STATUS,ASAT,MODE=EXEC,USER', 'KIT.ENQUIRY.VIEW', 'KIT-010,KIT-013,KIT-014,KIT-015,KIT-022'),
      ('KITRP030', 'KIT', 'Kit shortage list', 'KITRDY',
       'CO,SITE,STATUS,ASAT,MODE=SHORT,USER', 'KIT.ENQUIRY.VIEW', 'KIT-006,KIT-009,KIT-022'),
      ('KITRP040', 'KIT', 'Kit picking document / reprint (RPT-014)', 'KITDOCP',
       'CO,SITE,KIT,MODE=ORIGINAL,COPY,REASON,USER', 'KIT.ISSUE.POST', 'KIT-022');

    MERGE core.ReportDef AS t USING @r AS s ON t.ReportCode = s.ReportCode
    WHEN MATCHED THEN UPDATE SET Module = s.Module, Title = s.Title, ProgramName = s.ProgramName,
            ParamSpec = s.ParamSpec, PermissionCode = s.PermissionCode, RequirementRefs = s.RequirementRefs
    WHEN NOT MATCHED BY TARGET THEN
        INSERT (ReportCode, Module, Title, ProgramName, ParamSpec, PermissionCode, RequirementRefs)
        VALUES (s.ReportCode, s.Module, s.Title, s.ProgramName, s.ParamSpec, s.PermissionCode, s.RequirementRefs);

    PRINT 'Seeded core.ReportDef (kits)';
END
GO
