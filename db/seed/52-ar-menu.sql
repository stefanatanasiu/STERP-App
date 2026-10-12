/*  Debtors module user interface: the Debtors menu branch, the batch report catalogue, the generic
    maintenance definitions for the simple sales-ledger masters, and the permission codes the
    debtors screens need that the core catalogue does not already carry.  The top level 'AR' bar
    entry is created by 07-core-numbering-terms.sql; only its children belong here.
    Re-runnable (MERGE / IF NOT EXISTS).
    REQ: AR-007, AR-008, AR-009, AR-010, AR-016, AR-017, AR-018, AR-021, AR-022, AR-025, AR-029,
    COM-002, COM-019, COM-024, D-16 */
SET NOCOUNT ON;
GO
/*  Permissions the core catalogue does not carry yet.  Everything else the debtors screens use
    (AR.CUSTOMER.MAINT, AR.INVOICE.POST, AR.RECEIPT.POST, AR.WRITEOFF.APPROVE, AR.CREDIT.OVERRIDE)
    is already seeded by 02-core-permissions.sql and is deliberately not repeated.  REQ: COM-002 */
MERGE core.Permission AS t
USING (VALUES
 ('FIN.CONTROL.OVERRIDE','AR','Approve credit overrides above the controller threshold',1,1),
 ('AR.ENQUIRY','AR','Enquire on debtor accounts, ageing and statements',0,0),
 ('AR.ALLOC.CORRECT','AR','Reverse and re-apply a cash or credit allocation',0,0),
 ('AR.DD.MANAGE','AR','Manage Direct Debit mandates and collections',1,0),
 ('AR.CUSTOMER.ENQUIRE','AR','Enquire on one debtor account (ARENQ)',0,0),
 ('AR.ADJUST.CORRECTION','AR','Post a correcting ledger adjustment',0,0),
 ('AR.ADJUST.DISCOUNT','AR','Post a settlement discount inside the stated terms',0,0),
 ('AR.ADJUST.WRITEOFF.MINOR','AR','Write off a difference inside the D-22 clerk tolerance',0,0),
 ('AR.ADJUST.WRITEOFF.APPROVE','AR','Approve a write off above the clerk tolerance',1,1),
 ('AR.ADJUST.CREDIT','AR','Post a credit adjustment to a debtor balance',1,0),
 ('AR.ADJUST.TRANSFER','AR','Transfer a balance between debtor accounts',1,0),
 ('AR.ADJUST.PROVISION','AR','Raise or release a doubtful debt provision',1,0),
 ('AR.DISPUTE.RAISE','AR','Raise or amend a disputed amount',0,0),
 ('AR.DISPUTE.RESOLVE','AR','Resolve a dispute case',0,0),
 ('AR.COLLECTION.ACTION','AR','Record a collection contact or suppression',0,0),
 ('AR.COLLECTION.PROMISE','AR','Record a promise to pay',0,0),
 ('AR.INSOLVENCY.RECORD','AR','Record an insolvency event on a debtor',1,0),
 ('AR.RECOVERY.PAP','AR','Evidence a Pre-Action Protocol letter',0,0),
 ('AR.RECOVERY.HANDOFF','AR','Hand a debt off to an agency or solicitor',1,0)
) AS s (PermissionCode, Module, Name, IsSensitive, IsApproval)
ON t.PermissionCode = s.PermissionCode
WHEN MATCHED AND EXISTS (SELECT s.Module, s.Name, s.IsSensitive, s.IsApproval EXCEPT SELECT t.Module, t.Name, t.IsSensitive, t.IsApproval)
    THEN UPDATE SET Module = s.Module, Name = s.Name, IsSensitive = s.IsSensitive, IsApproval = s.IsApproval, ModifiedBy = 'SEED', ModifiedAt = SYSDATETIME()
WHEN NOT MATCHED THEN INSERT (PermissionCode, Module, Name, IsSensitive, IsApproval, CreatedBy)
     VALUES (s.PermissionCode, s.Module, s.Name, s.IsSensitive, s.IsApproval, 'SEED');
GO
/*  Grant each new code to the roles that already hold its nearest existing equivalent, so that the
    security model stays one definition rather than two lists that drift apart.  REQ: COM-002 */
INSERT core.RolePermission (RoleCode, PermissionCode, CreatedBy)
SELECT r.RoleCode, g.NewCode, 'SEED'
  FROM (VALUES ('AR.ENQUIRY','AR.ENQUIRY.VIEW'), ('AR.ALLOC.CORRECT','AR.ALLOCATE.RUN'), ('AR.DD.MANAGE','AR.DD.MANDATE.MAINT'),
               ('AR.CUSTOMER.ENQUIRE','AR.CUSTOMER.VIEW'), ('AR.ADJUST.CORRECTION','AR.ALLOCATE.RUN'),
               ('AR.ADJUST.DISCOUNT','AR.ALLOCATE.RUN'), ('AR.ADJUST.WRITEOFF.MINOR','AR.WRITEOFF.POST'),
               ('AR.ADJUST.WRITEOFF.APPROVE','AR.WRITEOFF.APPROVE'), ('AR.ADJUST.CREDIT','AR.CREDITNOTE.APPROVE'),
               ('AR.ADJUST.TRANSFER','AR.WRITEOFF.APPROVE'), ('AR.ADJUST.PROVISION','AR.WRITEOFF.APPROVE'),
               ('AR.DISPUTE.RAISE','AR.ENQUIRY.VIEW'), ('AR.DISPUTE.RESOLVE','AR.HOLD.RELEASE'),
               ('AR.COLLECTION.ACTION','AR.DUNNING.RUN'), ('AR.COLLECTION.PROMISE','AR.DUNNING.RUN'),
               ('AR.INSOLVENCY.RECORD','AR.HOLD.SET'), ('AR.RECOVERY.PAP','AR.DUNNING.RUN'),
               ('AR.RECOVERY.HANDOFF','AR.HOLD.SET')) AS g (NewCode, LikeCode)
  JOIN core.RolePermission r ON r.PermissionCode = g.LikeCode
 WHERE NOT EXISTS (SELECT 1 FROM core.RolePermission x WHERE x.RoleCode = r.RoleCode AND x.PermissionCode = g.NewCode);
INSERT core.RolePermission (RoleCode, PermissionCode, CreatedBy)
SELECT v.RoleCode, v.PermissionCode, 'SEED'
  FROM (VALUES ('CREDMGR','AR.ALLOC.CORRECT'), ('FINCTRL','AR.ALLOC.CORRECT'), ('TREASURY','AR.DD.MANAGE'),
               ('FINCTRL','FIN.CONTROL.OVERRIDE')) AS v (RoleCode, PermissionCode)
 WHERE EXISTS (SELECT 1 FROM core.Role r WHERE r.RoleCode = v.RoleCode)
   AND NOT EXISTS (SELECT 1 FROM core.RolePermission x WHERE x.RoleCode = v.RoleCode AND x.PermissionCode = v.PermissionCode);
GO
/*  The Debtors menu and sales credit-request entry. Credit approval uses frmCreditOverride,
    not writable generic maintenance. Reports use the launcher primed with a ReportDef code.
    REQ: COM-024 */
IF OBJECT_ID(N'core.MenuItem') IS NOT NULL
BEGIN
    DECLARE @m table (MenuCode varchar(40), ParentMenuCode varchar(40), Caption varchar(60), FormClass varchar(80) NULL,
                      FormArgument varchar(60) NULL, PermissionCode varchar(60) NULL, SortOrder int, Shortcut varchar(20) NULL);
    INSERT @m VALUES
      ('AR.CUSTOMER.ENQ',   'AR', 'Customer &Enquiry...',          'frmCustomerEnquiry', NULL,         'AR.ENQUIRY',         10, 'F5'),
      ('AR.RECEIPT.ENTRY',  'AR', '&Receipt Entry...',             'frmReceiptEntry',    NULL,         'AR.RECEIPT.ENTER',   20, 'F6'),
      ('AR.CREDIT.WORKLIST','AR', 'Credit Control &Worklist...',   'frmCreditControl',   NULL,         'AR.ENQUIRY',         30, 'F7'),
      ('AR.CREDIT.MAINT',   'AR', '&Credit Limits and Holds...',   'frmGenericMaint',    'AR.CREDIT',  'AR.CREDIT.SET',      40, NULL),
      ('AR.OVERRIDE.MAINT', 'AR', 'Credit &Overrides...',          'frmCreditOverride',  NULL,         'AR.CREDIT.OVERRIDE', 50, NULL),
      ('AR.OVERRIDE.REQUEST','SO','Request Credit &Override...',  'frmCreditOverride',  'REQUEST',    'SO.ORDER.ENTER',     15, NULL),
      ('AR.DD.MAINT',       'AR', '&Direct Debit Mandates...',     'frmGenericMaint',    'AR.DDMANDATE','AR.DD.MANAGE',      60, NULL),
      ('AR.RPT',            'AR', 'Debtors Repor&ts',              NULL,                 NULL,          NULL,                90, NULL),
      ('AR.RPT.AGED',       'AR.RPT', '&Aged Debt...',             'frmReportLauncher',  'AGEDDEBT',   'AR.ENQUIRY',         10, NULL),
      ('AR.RPT.STATEMENT',  'AR.RPT', 'Customer &Statements...',   'frmReportLauncher',  'STATEMNT',   'AR.STATEMENT.RUN',   20, NULL),
      ('AR.RPT.DUNNING',    'AR.RPT', '&Dunning Letters...',       'frmReportLauncher',  'DUNNING',    'AR.DUNNING.RUN',     30, NULL),
      ('AR.RPT.DDCOLL',     'AR.RPT', 'Direct Debit &Collections...','frmReportLauncher','DDCOLL',     'AR.DD.COLLECT.RUN',  40, NULL),
      ('AR.RPT.RECON',      'AR.RPT', 'Debtors to Ledger &Reconciliation...','frmReportLauncher','ARRECON','AR.ENQUIRY',     50, NULL),
      ('AR.RPT.EINV',       'AR.RPT', '&Electronic Invoice Export...','frmReportLauncher','EINVUBL',   'AR.INVOICE.POST',    60, NULL);

    MERGE core.MenuItem AS t USING @m AS s ON t.MenuCode = s.MenuCode
    WHEN MATCHED THEN UPDATE SET ParentMenuCode = s.ParentMenuCode, Caption = s.Caption, FormClass = s.FormClass,
            FormArgument = s.FormArgument, PermissionCode = s.PermissionCode, SortOrder = s.SortOrder, Shortcut = s.Shortcut, IsActive = 1
    WHEN NOT MATCHED BY TARGET THEN
        INSERT (MenuCode, ParentMenuCode, Caption, FormClass, FormArgument, PermissionCode, SortOrder, Shortcut, IsActive)
        VALUES (s.MenuCode, s.ParentMenuCode, s.Caption, s.FormClass, s.FormArgument, s.PermissionCode, s.SortOrder, s.Shortcut, 1);
    PRINT 'Seeded core.MenuItem (Debtors)';
END
GO
/*  Batch reports.  ParamSpec is the prompt list the report launcher builds its dialogue from.
    REQ: AR-017, AR-018, AR-021, AR-025, AR-029, COM-019 */
MERGE core.ReportDef AS t
USING (VALUES
 ('AGEDDEBT','AR','Aged debt analysis','AGEDDEBT','CO,ASAT','AR.ENQUIRY','AR-013, AR-017, AR-021','Open item ageing as at a chosen date with credits shown separately.'),
 ('STATEMNT','AR','Customer statements','STATEMNT','CO,ASAT,CUST,STYLE','AR.STATEMENT.RUN','AR-018, AR-027','Open item or transaction statements with opening, movement and closing balances.'),
 ('DUNNING','AR','Collection reminders and letters','DUNNING','CO,ASAT,STAGE,OWNER','AR.DUNNING.RUN','AR-016, AR-024, AR-028','The baseline collection sequence, suppressed for disputes, settled items and contact restrictions.'),
 ('DDCOLL','AR','Direct Debit collection schedule','DDCOLL','CO,ASAT,SUN','AR.DD.COLLECT.RUN','AR-025','Advance notice, submission and unpaid position for Bacs Direct Debit collections.'),
 ('ARRECON','AR','Debtors to general ledger reconciliation','ARRECON','CO,PERIOD,ASAT,USER','AR.ENQUIRY','AR-021, FIN-022','Open item detail reconciled to the trade debtors control account. ARRECON takes PERIOD=yyyypp or ASAT=yyyymmdd, never a separate YEAR.'),
 ('EINVUBL','AR','Electronic invoice export','EINVUBL','CO,FROM,TO,CUST','AR.INVOICE.POST','AR-029','UBL electronic invoice file for customers enabled for e-invoicing.')
) AS s (ReportCode, Module, Title, ProgramName, ParamSpec, PermissionCode, RequirementRefs, Description)
ON t.ReportCode = s.ReportCode
WHEN MATCHED THEN UPDATE SET Module = s.Module, Title = s.Title, ProgramName = s.ProgramName, ParamSpec = s.ParamSpec,
        PermissionCode = s.PermissionCode, RequirementRefs = s.RequirementRefs, Description = s.Description, IsActive = 1,
        ModifiedBy = 'SEED', ModifiedAt = SYSDATETIME()
WHEN NOT MATCHED THEN INSERT (ReportCode, Module, Title, ProgramName, ParamSpec, PermissionCode, RequirementRefs, Description, CreatedBy)
     VALUES (s.ReportCode, s.Module, s.Title, s.ProgramName, s.ParamSpec, s.PermissionCode, s.RequirementRefs, s.Description, 'SEED');
GO
/*  Generic maintenance for the simple sales-ledger masters.  Customers and invoices are far too
    rich for the generic shell and have their own hand written screens; credit policy, scoped
    overrides and Direct Debit mandates are flat enough for it.  REQ: AR-007, AR-008, AR-025, D-16 */
IF OBJECT_ID(N'core.MaintDef') IS NOT NULL AND OBJECT_ID(N'core.MaintColumn') IS NOT NULL
BEGIN
    DECLARE @d table (MaintCode varchar(30), Title varchar(80), ModuleCode varchar(10), SchemaName sysname, TableName sysname,
                      CompanyScoped bit, RetireColumn sysname NULL, PermissionCode varchar(60) NULL, HelpText varchar(400) NULL);
    INSERT @d VALUES
      ('AR.CREDIT',    'Customer credit policy',  'ar', 'ar', 'CustomerCredit',     1, NULL, 'AR.CREDIT.SET',      'Credit limit, overdue trigger, risk class and account hold for a customer. A dispute does not reduce exposure.'),
      ('AR.OVERRIDE',  'Credit overrides',        'ar', 'ar', 'CreditOverride',     1, NULL, 'AR.CREDIT.OVERRIDE', 'Scoped, expiring credit overrides granted by an independent authority (D-06).'),
      ('AR.DDMANDATE', 'Direct Debit mandates',   'ar', 'ar', 'DirectDebitMandate', 1, NULL, 'AR.DD.MANAGE',       'Bacs Direct Debit mandates. Bank details are sensitive data (D-16).');

    MERGE core.MaintDef AS t USING @d AS s ON t.MaintCode = s.MaintCode
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
      ('AR.CREDIT','CompanyCode','Company',                  'CODE', 10, 80,1,1,0,1,1,1,'core.usp_UiLookup','CORE.COMPANY'),
      ('AR.CREDIT','CustomerCode','Customer',                'CODE', 20,100,1,1,0,1,1,1,NULL,NULL),
      ('AR.CREDIT','CreditLimitGBP','Credit limit',          'MONEY',30,110,0,1,0,1,1,0,NULL,NULL),
      ('AR.CREDIT','OverdueThresholdGBP','Overdue trigger',  'MONEY',40,110,0,0,0,1,1,0,NULL,NULL),
      ('AR.CREDIT','OverdueDaysTrigger','Overdue days',      'INT',  50, 90,0,0,0,1,1,0,NULL,NULL),
      ('AR.CREDIT','RiskClass','Risk class',                 'CODE', 60, 90,0,0,0,1,1,1,NULL,NULL),
      ('AR.CREDIT','CreditScore','Score',                    'INT',  70, 70,0,0,0,1,1,0,NULL,NULL),
      ('AR.CREDIT','CreditInsuranceCoverGBP','Insured cover','MONEY',80,110,0,0,0,0,1,0,NULL,NULL),
      ('AR.CREDIT','ReviewDate','Review due',                'DATE', 90,100,0,0,0,1,1,0,NULL,NULL),
      ('AR.CREDIT','ControllerUser','Controller',            'CODE',100, 90,0,0,0,1,1,1,'core.usp_UiLookup','CORE.USER'),
      ('AR.CREDIT','HoldFlag','On hold',                     'BOOL',110, 60,0,0,0,1,1,0,NULL,NULL),
      ('AR.CREDIT','HoldReason','Hold reason',               'TEXT',120,240,0,0,0,1,1,0,NULL,NULL),
      ('AR.CREDIT','RequiresPrepayment','Prepay',            'BOOL',130, 60,0,0,0,0,1,0,NULL,NULL),
      ('AR.CREDIT','ProFormaOnly','Pro forma only',          'BOOL',140, 70,0,0,0,0,1,0,NULL,NULL),
      ('AR.OVERRIDE','OverrideId','Override',                'INT',  10, 70,1,0,1,1,0,0,NULL,NULL),
      ('AR.OVERRIDE','CompanyCode','Company',                'CODE', 20, 80,0,1,0,1,1,1,'core.usp_UiLookup','CORE.COMPANY'),
      ('AR.OVERRIDE','CustomerCode','Customer',              'CODE', 30,100,0,1,0,1,1,1,NULL,NULL),
      ('AR.OVERRIDE','Scope','Scope',                        'CODE', 40, 90,0,1,0,1,1,0,NULL,NULL),
      ('AR.OVERRIDE','DocumentRef','Document',               'TEXT', 50,120,0,0,0,1,1,1,NULL,NULL),
      ('AR.OVERRIDE','OverrideAmountGBP','Override amount',  'MONEY',60,110,0,1,0,1,1,0,NULL,NULL),
      ('AR.OVERRIDE','Reason','Reason',                      'TEXT', 70,280,0,1,0,1,1,0,NULL,NULL),
      ('AR.OVERRIDE','RequestedBy','Requested by',           'CODE', 80, 90,0,1,0,1,1,1,'core.usp_UiLookup','CORE.USER'),
      ('AR.OVERRIDE','ApprovedBy','Approved by',             'CODE', 90, 90,0,0,1,1,1,1,'core.usp_UiLookup','CORE.USER'),
      ('AR.OVERRIDE','ValidFrom','Valid from',               'DATE',100,100,0,1,0,1,1,0,NULL,NULL),
      ('AR.OVERRIDE','ExpiresOn','Expires on',               'DATE',110,100,0,1,0,1,1,0,NULL,NULL),
      ('AR.OVERRIDE','Status','Status',                      'CODE',120, 90,0,0,0,1,1,1,NULL,NULL),
      ('AR.DDMANDATE','MandateId','Mandate',                 'INT',  10, 70,1,0,1,1,0,0,NULL,NULL),
      ('AR.DDMANDATE','CompanyCode','Company',               'CODE', 20, 80,0,1,0,1,1,1,'core.usp_UiLookup','CORE.COMPANY'),
      ('AR.DDMANDATE','CustomerCode','Customer',             'CODE', 30,100,0,1,0,1,1,1,NULL,NULL),
      ('AR.DDMANDATE','MandateReference','Mandate reference','TEXT', 40,150,0,1,0,1,1,1,NULL,NULL),
      ('AR.DDMANDATE','ServiceUserNumber','SUN',             'TEXT', 50, 70,0,1,0,1,1,0,NULL,NULL),
      ('AR.DDMANDATE','PayerName','Payer name',              'TEXT', 60,200,0,1,0,1,1,1,NULL,NULL),
      ('AR.DDMANDATE','SortCode','Sort code',                'TEXT', 70, 80,0,1,0,0,1,0,NULL,NULL),
      ('AR.DDMANDATE','AccountNumber','Account number',      'TEXT', 80, 90,0,1,0,0,1,0,NULL,NULL),
      ('AR.DDMANDATE','SignedDate','Signed',                 'DATE', 90,100,0,1,0,1,1,0,NULL,NULL),
      ('AR.DDMANDATE','LodgedDate','Lodged',                 'DATE',100,100,0,0,0,1,1,0,NULL,NULL),
      ('AR.DDMANDATE','FirstCollectionDate','First collection','DATE',110,110,0,0,0,0,1,0,NULL,NULL),
      ('AR.DDMANDATE','AdvanceNoticeDays','Advance notice',  'INT', 120, 90,0,1,0,1,1,0,NULL,NULL),
      ('AR.DDMANDATE','Status','Status',                     'CODE',130, 90,0,0,0,1,1,1,NULL,NULL);

    MERGE core.MaintColumn AS t USING @c AS s ON t.MaintCode = s.MaintCode AND t.ColumnName = s.ColumnName
    WHEN MATCHED THEN UPDATE SET Caption = s.Caption, DataType = s.DataType, SortOrder = s.SortOrder, Width = s.Width,
            IsKey = s.IsKey, IsRequired = s.IsRequired, IsReadOnly = s.IsReadOnly, IsInList = s.IsInList, IsInEdit = s.IsInEdit,
            IsSearchable = s.IsSearchable, LookupProc = s.LookupProc, LookupArg = s.LookupArg
    WHEN NOT MATCHED BY TARGET THEN
        INSERT (MaintCode, ColumnName, Caption, DataType, SortOrder, Width, IsKey, IsRequired, IsReadOnly, IsInList, IsInEdit,
                IsSearchable, LookupProc, LookupArg)
        VALUES (s.MaintCode, s.ColumnName, s.Caption, s.DataType, s.SortOrder, s.Width, s.IsKey, s.IsRequired, s.IsReadOnly,
                s.IsInList, s.IsInEdit, s.IsSearchable, s.LookupProc, s.LookupArg);
    PRINT 'Seeded core.MaintDef / core.MaintColumn (Debtors)';
END
GO

/*  COM-005 / AR-022 - the document number series the collection and direct debit programs ask
    for.  ARDISPUT and ARINSOLV request ARCASE when a dispute or insolvency case is opened;
    DDMAND requests DDMAN for a mandate and DDCOL for a collection instruction.  Without these
    rows the programs refuse with DSP0061 "No active number series for this company and
    document type", so a dispute cannot be raised at all. */
DECLARE @arns TABLE (SeriesCode varchar(20), DocumentType varchar(20), Description varchar(100), Prefix varchar(4), NumberLength tinyint);
INSERT @arns VALUES
 ('ARCASE','COLLECTIONCASE','Collection, dispute and insolvency case', 'CC', 8),
 ('DDMAN', 'DDMANDATE',     'Direct Debit mandate',                    'DM', 8),
 ('DDCOL', 'DDCOLLECTION',  'Direct Debit collection instruction',     'DC', 8);
INSERT core.NumberSeries (CompanyCode, SeriesCode, DocumentType, Description, Prefix, NumberLength,
        NextNumber, MinNumber, MaxNumber, IsLegalSequence, IsActive, CreatedBy)
SELECT c.CompanyCode, n.SeriesCode, n.DocumentType, n.Description, n.Prefix + LEFT(c.CompanyCode, 1),
       n.NumberLength, 1, 1, 99999999, 0, 1, 'SEED'
  FROM core.Company c CROSS JOIN @arns n
 WHERE NOT EXISTS (SELECT 1 FROM core.NumberSeries s
                    WHERE s.CompanyCode = c.CompanyCode AND s.SeriesCode = n.SeriesCode);
UPDATE s
   SET IsActive = 1, ModifiedBy = 'SEED', ModifiedAt = SYSDATETIME()
  FROM core.NumberSeries s
 WHERE s.SeriesCode IN ('ARCASE', 'DDMAN', 'DDCOL') AND s.IsActive = 0;
PRINT 'Seeded core.NumberSeries (ARCASE, DDMAN, DDCOL)';
GO
