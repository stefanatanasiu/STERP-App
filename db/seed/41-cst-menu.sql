/*  41-cst-menu.sql - the Costing menu, the report catalogue and the generic maintenance
    definitions for the simple costing masters.  Re-runnable (MERGE).

    The menu is laid out the way a cost accountant works through the month: look a cost up,
    maintain the standards, run the roll-up, then jobs and work in progress, then the period
    end valuation and its reconciliation, and the reports last.  Every entry is guarded by a
    CST permission seeded in 02-core-permissions.sql - activation and write-down approval are
    separate permissions because they are separate decisions.

    REQ: CST-001, CST-007, CST-010, CST-012, CST-014, CST-018, CST-019, CST-021, CST-022,
         CST-023, CST-026, RPT-011, RPT-014
*/
SET NOCOUNT ON;
GO
IF OBJECT_ID(N'core.MenuItem') IS NOT NULL
BEGIN
    DECLARE @m TABLE (MenuCode varchar(40), ParentMenuCode varchar(40), Caption varchar(80),
                      FormClass varchar(60), FormArgument varchar(60), PermissionCode varchar(40),
                      SortOrder int, Shortcut varchar(20));
    INSERT @m VALUES
      ('CST',             NULL,  '&Costing',                    NULL,                NULL,          NULL,                     90, NULL),
      ('CST.ENQUIRY',     'CST', 'Item Cost &Enquiry...',       'frmCostEnquiry',    NULL,          'CST.COST.VIEW',          10, 'Ctrl+K'),
      ('CST.COSTSET',     'CST', '&Cost Sets...',               'frmCostSet',        NULL,          'CST.STANDARD.MAINT',     20, NULL),
      ('CST.ROLLUP',      'CST', '&Roll-up and What-if...',     'frmCostRollup',     NULL,          'CST.STANDARD.MAINT',     30, 'Ctrl+R'),
      ('CST.RATES',       'CST', '&Work Centre Rates...',       'frmGenericMaint',   'CST.RATES',   'CST.RATE.MAINT',         40, NULL),
      ('CST.ELEMENTS',    'CST', 'Cost &Elements...',           'frmGenericMaint',   'CST.ELEMENT', 'CST.RATE.MAINT',         50, NULL),
      ('CST.POLICY',      'CST', 'Valuation &Policy...',        'frmGenericMaint',   'CST.POLICY',  'CST.RATE.MAINT',         60, NULL),
      ('CST.JOB',         'CST', '&Job Cost Enquiry...',        'frmJobCost',        NULL,          'CST.JOB.MAINT',          70, 'Ctrl+J'),
      ('CST.WIP',         'CST', 'Work in &Progress...',        'frmJobCost',        'WIP',         'CST.WIP.VIEW',           80, NULL),
      ('CST.VARIANCE',    'CST', '&Variance Analysis...',       'frmReportLauncher', 'CSTRP030',    'CST.VARIANCE.VIEW',      90, NULL),
      ('CST.LANDED',      'CST', '&Landed Cost Allocation...',  'frmGenericMaint',   'CST.LANDED',  'CST.LANDED.MAINT',      100, NULL),
      ('CST.NRV',         'CST', '&Net Realisable Value...',    'frmGenericMaint',   'CST.NRV',     'CST.NRV.POST',          110, NULL),
      ('CST.RPT.VAL',     'CST', 'Inventory Va&luation...',     'frmReportLauncher', 'CSTRP010',    'CST.COST.VIEW',         130, NULL),
      ('CST.RPT.JOB',     'CST', 'Job Profita&bility...',       'frmReportLauncher', 'CSTRP020',    'CST.JOB.MAINT',         140, NULL),
      ('CST.RPT.COMPARE', 'CST', 'Cost Set Co&mparison...',     'frmReportLauncher', 'CSTRP040',    'CST.STANDARD.MAINT',    150, NULL);

    MERGE core.MenuItem AS t USING @m AS s ON t.MenuCode = s.MenuCode
    WHEN MATCHED THEN UPDATE SET ParentMenuCode = s.ParentMenuCode, Caption = s.Caption,
            FormClass = s.FormClass, FormArgument = s.FormArgument, PermissionCode = s.PermissionCode,
            SortOrder = s.SortOrder, Shortcut = s.Shortcut, IsActive = 1
    WHEN NOT MATCHED BY TARGET THEN
        INSERT (MenuCode, ParentMenuCode, Caption, FormClass, FormArgument, PermissionCode, SortOrder, Shortcut, IsActive)
        VALUES (s.MenuCode, s.ParentMenuCode, s.Caption, s.FormClass, s.FormArgument, s.PermissionCode, s.SortOrder, s.Shortcut, 1);

    PRINT 'Seeded core.MenuItem (costing)';
END
GO
IF OBJECT_ID(N'core.ReportDef') IS NOT NULL
BEGIN
    DECLARE @r TABLE (ReportCode varchar(20), Module varchar(10), Title varchar(100), ProgramName varchar(40),
                      ParamSpec varchar(200), PermissionCode varchar(40), RequirementRefs varchar(200));
    INSERT @r VALUES
      /*  RPT-011 - the valuation is printed with the ledger balance beside it and the
          difference stated.  A valuation that does not reconcile is still printed; it is
          labelled, because hiding it would not make it go away (CST-022). */
      ('CSTRP010', 'CST', 'Inventory valuation and GL reconciliation', 'CSTINVAL',
       'CO,ASAT,SITE,GROUP,METHOD,DETAIL,MODE,USER', 'CST.COST.VIEW',
       'CST-005,CST-006,CST-021,CST-022,RPT-011'),
      /*  RPT-014 - budget, committed, actual and realised margin for each job, with sub-jobs
          shown under their parent and counted once (D-13). */
      ('CSTRP020', 'CST', 'Job profitability and work in progress', 'CSTJOBPR',
       'CO,PERIOD,JOB,STATUS,CUSTOMER,MODE,USER', 'CST.JOB.MAINT',
       'CST-013,CST-014,CST-016,CST-017,CST-018,RPT-014'),
      /*  The quantity and rate split with the baseline quantity printed next to it - a
          variance without its allowance is an opinion (CST-019). */
      ('CSTRP030', 'CST', 'Variance analysis by type and owner', 'CSTVARRP',
       'CO,PERIOD,TYPE,JOB,ITEM,MODE,USER', 'CST.VARIANCE.VIEW',
       'CST-009,CST-019,CST-026'),
      /*  CST-012 - what a proposed set would do to the stock value before anybody activates
          it.  Items in only one of the two sets are listed, never dropped. */
      ('CSTRP040', 'CST', 'Cost set comparison and revaluation impact', 'CSTSETCM',
       'CO,COSTSET,COMPARETO,THRESHOLD,MODE,USER', 'CST.STANDARD.MAINT',
       'CST-007,CST-010,CST-012,CST-023');

    MERGE core.ReportDef AS t USING @r AS s ON t.ReportCode = s.ReportCode
    WHEN MATCHED THEN UPDATE SET Module = s.Module, Title = s.Title, ProgramName = s.ProgramName,
            ParamSpec = s.ParamSpec, PermissionCode = s.PermissionCode, RequirementRefs = s.RequirementRefs
    WHEN NOT MATCHED BY TARGET THEN
        INSERT (ReportCode, Module, Title, ProgramName, ParamSpec, PermissionCode, RequirementRefs)
        VALUES (s.ReportCode, s.Module, s.Title, s.ProgramName, s.ParamSpec, s.PermissionCode, s.RequirementRefs);

    PRINT 'Seeded core.ReportDef (costing)';
END
GO
