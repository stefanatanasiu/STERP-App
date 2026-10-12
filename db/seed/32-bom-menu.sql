/*  32-bom-menu.sql - the Bills of Material menu, the report catalogue and the generic maintenance
    definition for bill headers.  Re-runnable (MERGE).

    The menu follows the classic layout: maintenance first, then the enquiries an engineer uses all
    day (explosion, where-used, comparison), then the change control entries, then the reports.
    Every entry is permission guarded with the BOM permissions seeded by 02-core-permissions.sql.

    REQ: BOM-001, BOM-014, BOM-020, BOM-021, BOM-023, BOM-024, BOM-025, BOM-032, RPT-001
*/
SET NOCOUNT ON;
GO
IF OBJECT_ID(N'core.MenuItem') IS NOT NULL
BEGIN
    DECLARE @m TABLE (MenuCode varchar(40), ParentMenuCode varchar(40), Caption varchar(80),
                      FormClass varchar(60), FormArgument varchar(60), PermissionCode varchar(40),
                      SortOrder int, Shortcut varchar(20));
    INSERT @m VALUES
      ('BOM',            NULL,  '&Bills of Material',        NULL,                NULL,            NULL,                  70, NULL),
      ('BOM.EDIT',       'BOM', '&Bill Maintenance...',      'frmBomEditor',      NULL,            'BOM.MAINT',           10, NULL),
      ('BOM.HEADERS',    'BOM', 'Bill &Headers (list)...',   'frmGenericMaint',   'BOM.HEADER',    'BOM.MAINT',           20, NULL),
      ('BOM.EXPLODE',    'BOM', '&Explosion Enquiry...',     'frmBomExplosion',   NULL,            'BOM.EXPLODE.RUN',     40, 'Ctrl+E'),
      ('BOM.WHEREUSED',  'BOM', '&Where Used Enquiry...',    'frmBomWhereUsed',   NULL,            'BOM.WHEREUSED.VIEW',  50, NULL),
      ('BOM.COMPARE',    'BOM', 'Revision &Comparison...',   'frmBomExplosion',   'COMPARE',       'BOM.VIEW',            60, NULL),
      ('BOM.CHANGE',     'BOM', 'Engineering &Changes...',   'frmGenericMaint',   'BOM.CHANGE',    'BOM.CHANGE.APPROVE',  70, NULL),
      ('BOM.CHANGE.PREVIEW','BOM','Bulk Change &Preview...', 'frmBomBulkPreview', NULL,            'BOM.VIEW',            75, NULL),
      ('BOM.RPT.INDENT', 'BOM', 'Indented Parts &List...',   'frmReportLauncher', 'BOMRP010',      'BOM.VIEW',            90, NULL),
      ('BOM.RPT.COSTED', 'BOM', 'Costed E&xplosion...',      'frmReportLauncher', 'BOMRP020',      'BOM.COST.ROLLUP',    100, NULL);

    MERGE core.MenuItem AS t USING @m AS s ON t.MenuCode = s.MenuCode
    WHEN MATCHED THEN UPDATE SET ParentMenuCode = s.ParentMenuCode, Caption = s.Caption,
            FormClass = s.FormClass, FormArgument = s.FormArgument, PermissionCode = s.PermissionCode,
            SortOrder = s.SortOrder, Shortcut = s.Shortcut, IsActive = 1
    WHEN NOT MATCHED BY TARGET THEN
        INSERT (MenuCode, ParentMenuCode, Caption, FormClass, FormArgument, PermissionCode, SortOrder, Shortcut, IsActive)
        VALUES (s.MenuCode, s.ParentMenuCode, s.Caption, s.FormClass, s.FormArgument, s.PermissionCode, s.SortOrder, s.Shortcut, 1);

    PRINT 'Seeded core.MenuItem (bills of material)';
END
GO
IF OBJECT_ID(N'core.ReportDef') IS NOT NULL
BEGIN
    DECLARE @r TABLE (ReportCode varchar(20), Module varchar(10), Title varchar(100), ProgramName varchar(40),
                      ParamSpec varchar(200), PermissionCode varchar(40), RequirementRefs varchar(200));
    INSERT @r VALUES
      ('BOMRP010', 'BOM', 'Indented explosion (parts list)', 'BOMRP010',
       'CO,ITEM,REV,QTY,ASAT,LEVELS,MODE,USER', 'BOM.VIEW',
       'BOM-006,BOM-007,BOM-008,BOM-009,BOM-010,BOM-011,BOM-014,BOM-017,BOM-032'),
      ('BOMRP020', 'BOM', 'Costed explosion roll-up', 'BOMRP010',
       'CO,ITEM,REV,QTY,ASAT,COSTSET,MODE=COSTED,USER', 'BOM.COST.ROLLUP',
       'BOM-021,BOM-026,BOM-027,BOM-032'),
      ('BOMRP030', 'BOM', 'Where used (direct and indirect)', 'BOMRP030',
       'CO,ITEM,ASAT,LEVELS,MODE,USER', 'BOM.WHEREUSED.VIEW', 'BOM-023,BOM-028,BOM-031'),
      ('BOMRP040', 'BOM', 'Revision comparison', 'BOMRP030',
       'CO,BOMREF,FROMREV,TOREV,COSTSET,MODE=COMPARE,USER', 'BOM.VIEW', 'BOM-025,BOM-029');

    MERGE core.ReportDef AS t USING @r AS s ON t.ReportCode = s.ReportCode
    WHEN MATCHED THEN UPDATE SET Module = s.Module, Title = s.Title, ProgramName = s.ProgramName,
            ParamSpec = s.ParamSpec, PermissionCode = s.PermissionCode, RequirementRefs = s.RequirementRefs
    WHEN NOT MATCHED BY TARGET THEN
        INSERT (ReportCode, Module, Title, ProgramName, ParamSpec, PermissionCode, RequirementRefs)
        VALUES (s.ReportCode, s.Module, s.Title, s.ProgramName, s.ParamSpec, s.PermissionCode, s.RequirementRefs);

    PRINT 'Seeded core.ReportDef (bills of material)';
END
GO
