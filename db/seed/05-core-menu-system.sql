/* ---------------------------------------------------------------------------
   05-core-menu-system - the FILE / SYSTEM / REPORTS / WINDOW / HELP menus of the
   WinForms client.  Module teams add their own branch with their own seed file.
   FormClass is the VB class name instantiated by reflection; FormArgument is
   handed to the form (for frmGenericMaint it is the core.MaintDef code).  A
   FormClass of NULL with a FormArgument starting with $ is a built-in shell
   command (see docs\NET-FRAMEWORK.md).                     Re-runnable (MERGE).
   Top level SortOrder values match the module taxonomy in 07-core-numbering-terms.sql
   (FILE 10, SYSTEM 20, modules 30-110, RPT 120, WINDOW 130, HELP 140) so that the
   Window and Help menus stay last on the bar as a classic Windows MDI app expects.
   --------------------------------------------------------------------------- */
SET NOCOUNT ON;
GO
IF OBJECT_ID(N'core.MenuItem') IS NOT NULL
BEGIN
    DECLARE @m table (MenuCode varchar(40), ParentMenuCode varchar(40) NULL, Caption varchar(60),
                      FormClass varchar(80) NULL, FormArgument varchar(60) NULL,
                      PermissionCode varchar(60) NULL, SortOrder int, Shortcut varchar(20) NULL);
    INSERT @m VALUES
      -- top level bar ---------------------------------------------------------
      ('FILE',   NULL, '&File',    NULL, NULL, NULL, 10, NULL),
      ('SYSTEM', NULL, '&System',  NULL, NULL, NULL, 20, NULL),
      ('RPT',    NULL, '&Reports', NULL, NULL, NULL, 120, NULL),
      ('WINDOW', NULL, '&Window',  NULL, NULL, NULL, 130, NULL),
      ('HELP',   NULL, '&Help',    NULL, NULL, NULL, 140, NULL),
      -- File ------------------------------------------------------------------
      ('FILE.COMPANY',  'FILE', 'Select &Company / Site...', NULL, '$COMPANY',    NULL, 10, NULL),
      ('FILE.RUNAS',    'FILE', 'Continue as &User...',      NULL, '$RUNAS',      NULL, 20, NULL),
      ('FILE.SPOOL',    'FILE', 'View &Spool File...',       'frmSpoolViewer', NULL, NULL, 30, 'Ctrl+O'),
      ('FILE.PRINTSET', 'FILE', 'P&rinter Setup...',         NULL, '$PRINTSETUP', NULL, 40, NULL),
      ('FILE.EXIT',     'FILE', 'E&xit',                     NULL, '$EXIT',       NULL, 90, 'Alt+F4'),
      -- System ----------------------------------------------------------------
      ('SYS.PARAM',     'SYSTEM', 'System &Parameters...',   'frmGenericMaint', 'CORE.PARAM',    'SYS.PARAM.MAINT',    10, NULL),
      ('SYS.COMPANY',   'SYSTEM', 'C&ompanies...',           'frmGenericMaint', 'CORE.COMPANY',  'SYS.COMPANY.MAINT',  20, NULL),
      ('SYS.SITE',      'SYSTEM', '&Sites...',               'frmGenericMaint', 'CORE.SITE',     'SYS.SITE.MAINT',     30, NULL),
      ('SYS.USER',      'SYSTEM', '&Users...',               'frmGenericMaint', 'CORE.USER',     'SYS.USER.MAINT',     40, NULL),
      ('SYS.ROLE',      'SYSTEM', '&Roles...',               'frmGenericMaint', 'CORE.ROLE',     'SYS.ROLE.MAINT',     50, NULL),
      ('SYS.USERROLE',  'SYSTEM', 'User Ro&les...',          'frmGenericMaint', 'CORE.USERROLE', 'SYS.ROLE.MAINT',     60, NULL),
      ('SYS.PERM',      'SYSTEM', 'Per&missions...',         'frmGenericMaint', 'CORE.PERM',     'SYS.ROLE.MAINT',     70, NULL),
      ('SYS.CURRENCY',  'SYSTEM', 'Curre&ncies...',          'frmGenericMaint', 'CORE.CURRENCY', 'SYS.FX.MAINT',       80, NULL),
      ('SYS.FXRATE',    'SYSTEM', 'E&xchange Rates...',      'frmGenericMaint', 'CORE.FXRATE',   'SYS.FX.MAINT',       90, NULL),
      ('SYS.PAYTERMS',  'SYSTEM', 'Pa&yment Terms...',       'frmGenericMaint', 'CORE.PAYTERMS', 'SYS.PARAM.MAINT',   100, NULL),
      ('SYS.CALENDAR',  'SYSTEM', 'Calen&dars...',           'frmGenericMaint', 'CORE.CALENDAR', 'SYS.CALENDAR.MAINT',110, NULL),
      ('SYS.HOLIDAY',   'SYSTEM', 'Calendar &Holidays...',   'frmGenericMaint', 'CORE.HOLIDAY',  'SYS.CALENDAR.MAINT',120, NULL),
      ('SYS.NUMSER',    'SYSTEM', 'Num&ber Series...',       'frmGenericMaint', 'CORE.NUMSER',   'SYS.NUMBER.MAINT',  130, NULL),
      ('SYS.MAINT',     'SYSTEM', 'File Main&tenance...',    'frmMaintLauncher', NULL,           NULL,                140, NULL),
      ('SYS.APPROVAL',  'SYSTEM', '&Approvals Inbox...',     'frmApprovals',     NULL,           'SYS.APPROVAL.VIEW', 200, 'F8'),
      ('SYS.EXCEPTION', 'SYSTEM', '&Exception Worklist...',  'frmExceptions',    NULL,           'SYS.EXCEPTION.VIEW',210, 'F9'),
      ('SYS.AUDIT',     'SYSTEM', 'Audit En&quiry...',       'frmAudit',         NULL,           'SYS.AUDIT.VIEW',    220, NULL),
      ('SYS.JOBS',      'SYSTEM', '&Job Monitor...',         'frmJobMonitor',    NULL,           'SYS.JOB.VIEW',      230, NULL),
      ('SYS.ENGINE',    'SYSTEM', 'Engine Dia&gnostics...',  'frmEngineDiag',    NULL,           'SYS.ADMIN',         240, NULL),
      -- Reports ---------------------------------------------------------------
      ('RPT.LAUNCH',    'RPT', '&Run Report...',             'frmReportLauncher', NULL, 'RPT.RUN',      10, 'Ctrl+R'),
      ('RPT.SPOOL',     'RPT', '&Spool File Viewer...',      'frmSpoolViewer',    NULL, NULL,           20, NULL),
      ('RPT.JOBS',      'RPT', '&Job Monitor...',            'frmJobMonitor',     NULL, 'SYS.JOB.VIEW', 30, NULL),
      -- Window ----------------------------------------------------------------
      ('WIN.CASCADE',   'WINDOW', '&Cascade',                NULL, '$CASCADE',  NULL, 10, NULL),
      ('WIN.TILEH',     'WINDOW', 'Tile &Horizontally',      NULL, '$TILEH',    NULL, 20, NULL),
      ('WIN.TILEV',     'WINDOW', 'Tile &Vertically',        NULL, '$TILEV',    NULL, 30, NULL),
      ('WIN.ARRANGE',   'WINDOW', '&Arrange Icons',          NULL, '$ARRANGE',  NULL, 40, NULL),
      ('WIN.CLOSEALL',  'WINDOW', 'C&lose All',              NULL, '$CLOSEALL', NULL, 50, NULL),
      -- Help ------------------------------------------------------------------
      ('HELP.CONTENTS', 'HELP', '&Contents',                 NULL, '$HELP',     NULL, 10, 'F1'),
      ('HELP.ABOUT',    'HELP', '&About STERP - Stefan''s ERP...', NULL, '$ABOUT', NULL, 90, NULL);

    MERGE core.MenuItem AS t USING @m AS s ON t.MenuCode = s.MenuCode
    WHEN MATCHED THEN UPDATE SET ParentMenuCode = s.ParentMenuCode, Caption = s.Caption,
            FormClass = s.FormClass, FormArgument = s.FormArgument, PermissionCode = s.PermissionCode,
            SortOrder = s.SortOrder, Shortcut = s.Shortcut, IsActive = 1
    WHEN NOT MATCHED BY TARGET THEN
        INSERT (MenuCode, ParentMenuCode, Caption, FormClass, FormArgument, PermissionCode, SortOrder, Shortcut, IsActive)
        VALUES (s.MenuCode, s.ParentMenuCode, s.Caption, s.FormClass, s.FormArgument, s.PermissionCode, s.SortOrder, s.Shortcut, 1);

    PRINT 'Seeded core.MenuItem (system menus)';
END
GO
