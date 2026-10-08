/*  Costing - work in progress roll-forward report (CST-018).

    CSTRP050 prints, for every job that is not financially closed, the opening work in
    progress, what went in during the period and what came out, and the closing balance that
    cst.WipBalance carries.  The figures come from the WIPCALC engine, not from a sum written
    inside the report, so what is printed is what MODE=POST would write.

    A completion transfer takes value OUT of work in progress.  It does not leave the same
    money in both the job balance and finished stock - that is the double count this report
    exists to make visible.

    Separate from 41-cst-menu.sql because that file was already deployed when the roll-forward
    was written; both are idempotent and may be run in either order.
    REQ: CST-018, CST-020  */
SET NOCOUNT ON;
GO
IF OBJECT_ID(N'core.ReportDef') IS NOT NULL
BEGIN
    DECLARE @r TABLE (ReportCode varchar(20), Module varchar(10), Title varchar(100), ProgramName varchar(40),
                      ParamSpec varchar(200), PermissionCode varchar(40), RequirementRefs varchar(200));
    INSERT @r VALUES
      /*  RPT-015 - opening + material issues - returns + labour + overhead + subcontract
          - completion transfers - scrap - write off = closing, one job at a time. */
      ('CSTRP050', 'CST', 'Work in progress roll-forward', 'CSTWIPRF',
       'CO,PERIOD,JOB,MODE,USER', 'CST.JOB.MAINT',
       'CST-014,CST-018,CST-020,RPT-015');

    MERGE core.ReportDef AS t USING @r AS s ON t.ReportCode = s.ReportCode
    WHEN MATCHED THEN UPDATE SET Module = s.Module, Title = s.Title, ProgramName = s.ProgramName,
            ParamSpec = s.ParamSpec, PermissionCode = s.PermissionCode, RequirementRefs = s.RequirementRefs
    WHEN NOT MATCHED BY TARGET THEN
        INSERT (ReportCode, Module, Title, ProgramName, ParamSpec, PermissionCode, RequirementRefs)
        VALUES (s.ReportCode, s.Module, s.Title, s.ProgramName, s.ParamSpec, s.PermissionCode, s.RequirementRefs);

    PRINT 'Seeded core.ReportDef CSTRP050 (WIP roll-forward)';
END
GO
IF OBJECT_ID(N'core.MenuItem') IS NOT NULL
BEGIN
    DECLARE @m TABLE (MenuCode varchar(40), ParentMenuCode varchar(40), Caption varchar(80), FormClass varchar(60),
                      FormArgument varchar(60), PermissionCode varchar(40), SortOrder int, Shortcut varchar(20));
    INSERT @m VALUES
      ('CST.RPT.WIP',     'CST', '&Work in Progress Roll-forward...', 'frmReportLauncher', 'CSTRP050',    'CST.JOB.MAINT',         145, NULL);

    MERGE core.MenuItem AS t USING @m AS s ON t.MenuCode = s.MenuCode
    WHEN MATCHED THEN UPDATE SET ParentMenuCode = s.ParentMenuCode, Caption = s.Caption, FormClass = s.FormClass,
            FormArgument = s.FormArgument, PermissionCode = s.PermissionCode, SortOrder = s.SortOrder,
            Shortcut = s.Shortcut, IsActive = 1
    WHEN NOT MATCHED BY TARGET THEN
        INSERT (MenuCode, ParentMenuCode, Caption, FormClass, FormArgument, PermissionCode, SortOrder, Shortcut, IsActive)
        VALUES (s.MenuCode, s.ParentMenuCode, s.Caption, s.FormClass, s.FormArgument, s.PermissionCode, s.SortOrder, s.Shortcut, 1);

    PRINT 'Seeded core.MenuItem CST.RPT.WIP (WIP roll-forward)';
END
GO
