/*  Sales order module user interface: the Sales menu branch, the two permission codes the sales
    screens need that the core catalogue does not already carry, the SO batch report catalogue and
    the generic maintenance definition behind the Returns entry.  The top level 'SO' bar entry is
    created by 07-core-numbering-terms.sql; only its children belong here.
    Re-runnable (MERGE / IF NOT EXISTS).
    REQ: SO-002, SO-003, SO-010, SO-011, SO-012, SO-013, SO-014, SO-017, SO-018, SO-022,
    REQ: RPT-008, COM-002, COM-019, COM-024 */
SET NOCOUNT ON;
GO
/*  Permissions the core catalogue does not carry yet.  Everything else the sales screens use
    (SO.ORDER.ENTER, SO.ORDER.RELEASE, SO.DESPATCH.CONFIRM, SO.RETURN.AUTHORISE, SO.ENQUIRY.VIEW,
    SO.PRICE.OVERRIDE, SO.MARGIN.OVERRIDE) is already seeded by 02-core-permissions.sql and is
    deliberately not repeated here.  REQ: COM-002 */
MERGE core.Permission AS t
USING (VALUES
 ('SO.INVOICE.RUN','SO','Run the sales invoice run (SOINVRUN) in preview or commit mode',0,0),
 ('SO.DOCUMENT.PRINT','SO','Print sales order acknowledgements and despatch documents',0,0)
) AS s (PermissionCode, Module, Name, IsSensitive, IsApproval)
ON t.PermissionCode = s.PermissionCode
WHEN MATCHED AND EXISTS (SELECT s.Module, s.Name, s.IsSensitive, s.IsApproval EXCEPT SELECT t.Module, t.Name, t.IsSensitive, t.IsApproval)
    THEN UPDATE SET Module = s.Module, Name = s.Name, IsSensitive = s.IsSensitive, IsApproval = s.IsApproval, ModifiedBy = 'SEED', ModifiedAt = SYSDATETIME()
WHEN NOT MATCHED THEN INSERT (PermissionCode, Module, Name, IsSensitive, IsApproval, CreatedBy)
     VALUES (s.PermissionCode, s.Module, s.Name, s.IsSensitive, s.IsApproval, 'SEED');
GO
/*  Who holds them.  The invoice run is opened by the sales manager and by the AR clerk; only
    the AR clerk also holds AR.INVOICE.POST, which ARINVPST demands before a COMMIT raises an
    invoice, so a sales manager can preview the billing queue but not bill it (GUI-006 - the
    order-to-cash acceptance test bills as ARCLERK).  Printing an acknowledgement or a despatch
    note is routine for the sales desk and the warehouse alike.  The warehouse also gains the
    read-only sales enquiry so it can see the backlog it has to pick (SO-022).  Printing is
    evidence, never a release (SO-011).
    REQ: COM-002, SO-011, SO-014, SO-022 */
INSERT core.RolePermission (RoleCode, PermissionCode, CreatedBy)
SELECT v.RoleCode, v.PermissionCode, 'SEED'
  FROM (VALUES ('SALESMGR','SO.INVOICE.RUN'), ('ARCLERK','SO.INVOICE.RUN'),
               ('SALESREP','SO.DOCUMENT.PRINT'), ('SALESMGR','SO.DOCUMENT.PRINT'), ('WHSE','SO.DOCUMENT.PRINT'),
               ('WHSE','SO.ENQUIRY.VIEW')) AS v (RoleCode, PermissionCode)
 WHERE EXISTS (SELECT 1 FROM core.Role r WHERE r.RoleCode = v.RoleCode)
   AND EXISTS (SELECT 1 FROM core.Permission p WHERE p.PermissionCode = v.PermissionCode)
   AND NOT EXISTS (SELECT 1 FROM core.RolePermission x WHERE x.RoleCode = v.RoleCode AND x.PermissionCode = v.PermissionCode);
GO
/*  The Sales menu.  Three hand written screens - frmSalesOrderEntry, frmDespatchConfirm and
    frmSalesInvoiceRun - plus the generic maintenance shell for returns and the report launcher
    for the two batch reports.  FormArgument on a report launcher entry is the core.ReportDef
    module, which is what core.usp_UiReportList filters on.  REQ: COM-024 */
IF OBJECT_ID(N'core.MenuItem') IS NOT NULL
BEGIN
    DECLARE @m table (MenuCode varchar(40), ParentMenuCode varchar(40), Caption varchar(60), FormClass varchar(80) NULL,
                      FormArgument varchar(60) NULL, PermissionCode varchar(60) NULL, SortOrder int, Shortcut varchar(20) NULL);
    INSERT @m VALUES
      ('SO.ORDER.ENTRY',    'SO', 'Sales &Order Entry...',          'frmSalesOrderEntry', NULL,       'SO.ORDER.ENTER',      10, 'Ctrl+Shift+O'),
      ('SO.DESPATCH.CONF',  'SO', '&Despatch Confirmation...',      'frmDespatchConfirm', NULL,       'SO.DESPATCH.CONFIRM', 20, 'Ctrl+Shift+D'),
      ('SO.INVOICE.RUN',    'SO', 'Sales &Invoice Run...',          'frmSalesInvoiceRun', NULL,       'SO.INVOICE.RUN',      30, NULL),
      ('SO.RETURN.MAINT',   'SO', '&Returns and Authorisations...', 'frmGenericMaint',    'SO.RETURN','SO.RETURN.AUTHORISE', 40, NULL),
      ('SO.RPT',            'SO', 'Sales Repor&ts',                 NULL,                 NULL,       NULL,                  90, NULL),
      ('SO.RPT.BACKLOG',    'SO.RPT', 'Sales &Backlog and Fulfilment...', 'frmReportLauncher', 'SO',  'SO.ENQUIRY.VIEW',     10, NULL),
      ('SO.RPT.ANALYSIS',   'SO.RPT', 'Sales &Analysis...',               'frmReportLauncher', 'SO',  'SO.ENQUIRY.VIEW',     20, NULL);

    MERGE core.MenuItem AS t USING @m AS s ON t.MenuCode = s.MenuCode
    WHEN MATCHED THEN UPDATE SET ParentMenuCode = s.ParentMenuCode, Caption = s.Caption, FormClass = s.FormClass,
            FormArgument = s.FormArgument, PermissionCode = s.PermissionCode, SortOrder = s.SortOrder, Shortcut = s.Shortcut, IsActive = 1
    WHEN NOT MATCHED BY TARGET THEN
        INSERT (MenuCode, ParentMenuCode, Caption, FormClass, FormArgument, PermissionCode, SortOrder, Shortcut, IsActive)
        VALUES (s.MenuCode, s.ParentMenuCode, s.Caption, s.FormClass, s.FormArgument, s.PermissionCode, s.SortOrder, s.Shortcut, 1);
    PRINT 'Seeded core.MenuItem (Sales)';
END
GO
/*  Batch reports.  ParamSpec is the prompt list the report launcher builds its dialogue from;
    the launcher pre-fills CO, SITE, USER, ASAT, FROM, TO, PERIOD and MODE.  SOINVRUN defaults to
    MODE=PREVIEW, which lists what would be billed and has no business effect (SO-014).
    REQ: SO-011, SO-013, SO-014, SO-022, SO-024, RPT-008, COM-019 */
MERGE core.ReportDef AS t
USING (VALUES
 ('RPT-008','SO','Sales backlog and fulfilment','SOBACKLG','CO,SITE,FROM,TO','SO.ENQUIRY.VIEW','RPT-008, SO-022',
  'Accepted, cancelled, despatched and unbilled quantities by order, line and schedule, with requested, original and current promise dates.'),
 ('SOANAL','SO','Sales analysis','SOANAL','CO,SITE,FROM,TO,PERIOD','SO.ENQUIRY.VIEW','RPT-008, SO-024',
  'Sales by customer, item, salesperson and period on the ex-VAT net, with margin against the comparable cost that was captured.'),
 ('SOINVRUN','SO','Sales invoice run','SOINVRUN','CO,SITE,FROM,TO,MODE','SO.INVOICE.RUN','SO-014',
  'Bills confirmed despatches. MODE=PREVIEW reports what would be billed and has no effect; MODE=COMMIT raises the invoices.'),
 ('SOACKDOC','SO','Sales order acknowledgement and despatch documents','SOACKDOC','CO,SITE,ORDER','SO.DOCUMENT.PRINT','SO-011, SO-013',
  'Order acknowledgement, pick note and despatch note. Printing or reprinting a document never releases a held order.')
) AS s (ReportCode, Module, Title, ProgramName, ParamSpec, PermissionCode, RequirementRefs, Description)
ON t.ReportCode = s.ReportCode
WHEN MATCHED THEN UPDATE SET Module = s.Module, Title = s.Title, ProgramName = s.ProgramName, ParamSpec = s.ParamSpec,
        PermissionCode = s.PermissionCode, RequirementRefs = s.RequirementRefs, Description = s.Description, IsActive = 1,
        ModifiedBy = 'SEED', ModifiedAt = SYSDATETIME()
WHEN NOT MATCHED THEN INSERT (ReportCode, Module, Title, ProgramName, ParamSpec, PermissionCode, RequirementRefs, Description, CreatedBy)
     VALUES (s.ReportCode, s.Module, s.Title, s.ProgramName, s.ParamSpec, s.PermissionCode, s.RequirementRefs, s.Description, 'SEED');
GO
/*  Generic maintenance behind the Returns menu entry.  A return authorisation is flat enough for
    the generic shell; the receipt, inspection and disposition of the goods belong to RETURNS and
    to the stock module, not to this screen.  There is no hard delete - a return is rejected or
    closed, never erased.  REQ: SO-017, SO-018, D-12 */
IF OBJECT_ID(N'core.MaintDef') IS NOT NULL AND OBJECT_ID(N'core.MaintColumn') IS NOT NULL
BEGIN
    MERGE core.MaintDef AS t
    USING (VALUES ('SO.RETURN', 'Customer returns and authorisations', 'so', 'so', 'ReturnAuthorisation', 1, NULL, 'SO.RETURN.AUTHORISE',
                   'Customer return authorisations. Authorising a return does not credit the customer; the credit note follows inspection and disposition.')
    ) AS s (MaintCode, Title, ModuleCode, SchemaName, TableName, CompanyScoped, RetireColumn, PermissionCode, HelpText)
    ON t.MaintCode = s.MaintCode
    WHEN MATCHED THEN UPDATE SET Title = s.Title, ModuleCode = s.ModuleCode, SchemaName = s.SchemaName, TableName = s.TableName,
            CompanyScoped = s.CompanyScoped, RetireColumn = s.RetireColumn, PermissionCode = s.PermissionCode,
            HelpText = s.HelpText, IsActive = 1, ModifiedBy = 'SEED', ModifiedAt = SYSDATETIME()
    WHEN NOT MATCHED BY TARGET THEN
        INSERT (MaintCode, Title, ModuleCode, SchemaName, TableName, CompanyScoped, RetireColumn, PermissionCode, HelpText, CreatedBy)
        VALUES (s.MaintCode, s.Title, s.ModuleCode, s.SchemaName, s.TableName, s.CompanyScoped, s.RetireColumn, s.PermissionCode, s.HelpText, 'SEED');

    DECLARE @c table (MaintCode varchar(30), ColumnName sysname, Caption varchar(60), DataType varchar(10), SortOrder int, Width int,
                      IsKey bit, IsRequired bit, IsReadOnly bit, IsInList bit, IsInEdit bit, IsSearchable bit,
                      LookupProc varchar(128) NULL, LookupArg varchar(60) NULL);
    INSERT @c VALUES
      ('SO.RETURN','ReturnId','Return',                      'INT',  10, 70,1,0,1,1,0,0,NULL,NULL),
      ('SO.RETURN','CompanyCode','Company',                  'CODE', 20, 80,0,1,0,1,1,1,'core.usp_UiLookup','CORE.COMPANY'),
      ('SO.RETURN','SiteCode','Site',                        'CODE', 30, 70,0,1,0,1,1,1,'core.usp_UiLookup','CORE.SITE'),
      ('SO.RETURN','ReturnRef','Return reference',           'CODE', 40,120,0,1,0,1,1,1,NULL,NULL),
      ('SO.RETURN','CustomerCode','Customer',                'CODE', 50,100,0,1,0,1,1,1,NULL,NULL),
      ('SO.RETURN','OriginalOrderNumber','Original order',   'CODE', 60,120,0,0,0,1,1,1,NULL,NULL),
      ('SO.RETURN','OriginalDespatchRef','Original despatch','CODE', 70,120,0,0,0,0,1,1,NULL,NULL),
      ('SO.RETURN','OriginalInvoiceNumber','Original invoice','CODE',80,120,0,0,0,1,1,1,NULL,NULL),
      ('SO.RETURN','RequestedDate','Requested',              'DATE', 90,100,0,1,0,1,1,0,NULL,NULL),
      ('SO.RETURN','AuthorisedDate','Authorised',            'DATE',100,100,0,0,0,1,1,0,NULL,NULL),
      ('SO.RETURN','ReceivedDate','Received',                'DATE',110,100,0,0,0,1,1,0,NULL,NULL),
      ('SO.RETURN','InspectedDate','Inspected',              'DATE',120,100,0,0,0,0,1,0,NULL,NULL),
      ('SO.RETURN','ReasonCode','Reason code',               'CODE',130,100,0,1,0,1,1,1,NULL,NULL),
      ('SO.RETURN','Reason','Reason',                        'TEXT',140,260,0,0,0,1,1,1,NULL,NULL),
      ('SO.RETURN','IsFaulty','Faulty',                      'BOOL',150, 60,0,0,0,1,1,0,NULL,NULL),
      ('SO.RETURN','WithinReturnWindow','In window',         'BOOL',160, 70,0,0,0,1,1,0,NULL,NULL),
      ('SO.RETURN','WarrantyClaim','Warranty',               'BOOL',170, 70,0,0,0,0,1,0,NULL,NULL),
      ('SO.RETURN','ReplacementDecision','Decision',         'CODE',180, 90,0,0,0,1,1,1,NULL,NULL),
      ('SO.RETURN','ReplacementOrderNumber','Replacement order','CODE',190,120,0,0,0,0,1,1,NULL,NULL),
      ('SO.RETURN','CreditNoteNumber','Credit note',         'CODE',200,120,0,0,1,1,1,1,NULL,NULL),
      ('SO.RETURN','AuthorisedBy','Authorised by',           'CODE',210, 90,0,0,1,1,1,1,'core.usp_UiLookup','CORE.USER'),
      ('SO.RETURN','Status','Status',                        'CODE',220, 90,0,0,0,1,1,1,NULL,NULL);

    MERGE core.MaintColumn AS t USING @c AS s ON t.MaintCode = s.MaintCode AND t.ColumnName = s.ColumnName
    WHEN MATCHED THEN UPDATE SET Caption = s.Caption, DataType = s.DataType, SortOrder = s.SortOrder, Width = s.Width,
            IsKey = s.IsKey, IsRequired = s.IsRequired, IsReadOnly = s.IsReadOnly, IsInList = s.IsInList, IsInEdit = s.IsInEdit,
            IsSearchable = s.IsSearchable, LookupProc = s.LookupProc, LookupArg = s.LookupArg
    WHEN NOT MATCHED BY TARGET THEN
        INSERT (MaintCode, ColumnName, Caption, DataType, SortOrder, Width, IsKey, IsRequired, IsReadOnly, IsInList, IsInEdit,
                IsSearchable, LookupProc, LookupArg)
        VALUES (s.MaintCode, s.ColumnName, s.Caption, s.DataType, s.SortOrder, s.Width, s.IsKey, s.IsRequired, s.IsReadOnly,
                s.IsInList, s.IsInEdit, s.IsSearchable, s.LookupProc, s.LookupArg);
    PRINT 'Seeded core.MaintDef / core.MaintColumn (Sales returns)';
END
GO
