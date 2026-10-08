/*  Finance module user interface: the Finance menu branch, the batch and report catalogue for the
    fin batch programs, the generic maintenance definitions for the simple finance masters, and the
    VAT return periods with their statutory filing obligations.  The top level 'FIN' bar entry is
    created by 07-core-numbering-terms.sql; only its children belong here.
    Re-runnable (MERGE / IF NOT EXISTS).
    REQ: FIN-001, FIN-002, FIN-003, FIN-008, FIN-009, FIN-010, FIN-011, FIN-012, FIN-013, FIN-016,
    FIN-017, FIN-018, FIN-019, FIN-020, FIN-021, FIN-022, FIN-023, FIN-025, FIN-026, FIN-028,
    COM-002, COM-019, COM-024, D-15, D-16 */
SET NOCOUNT ON;
GO
/*  The finance controller is the named approver for journals (SOD-JNL) and for the VAT return
    (SOD-VAT, SOD-VATE), so the role must never also hold the matching origination codes: once one
    role holds both sides of a blocking segregation of duties rule, core.usp_DecideApproval refuses
    every decision with STERP-E-0007 and nothing can be approved at all.  Keying a journal belongs
    to GLCLERK and preparing the return to VATOWNER (03-core-roles-users.sql); only the submission
    code, which no rule pairs with an approval, is added here.  The delete repairs databases that
    were seeded with the earlier, conflicting grants.  REQ: COM-002, COM-003, D-07 */
IF OBJECT_ID(N'core.SoDRule') IS NOT NULL
    DELETE rp
      FROM core.RolePermission rp
      JOIN core.SoDRule s ON s.IsActive = 1 AND s.Severity = 'BLOCK' AND s.PermissionA = rp.PermissionCode
     WHERE rp.RoleCode = 'FINCTRL'
       AND EXISTS (SELECT 1 FROM core.RolePermission x WHERE x.RoleCode = rp.RoleCode AND x.PermissionCode = s.PermissionB);
GO
INSERT core.RolePermission (RoleCode, PermissionCode, CreatedBy)
SELECT v.RoleCode, v.PermissionCode, 'SEED'
  FROM (VALUES ('FINCTRL','FIN.VAT.SUBMIT')) AS v (RoleCode, PermissionCode)
 WHERE EXISTS (SELECT 1 FROM core.Role r WHERE r.RoleCode = v.RoleCode)
   AND EXISTS (SELECT 1 FROM core.Permission p WHERE p.PermissionCode = v.PermissionCode)
   AND NOT EXISTS (SELECT 1 FROM core.RolePermission x WHERE x.RoleCode = v.RoleCode AND x.PermissionCode = v.PermissionCode)
   AND NOT EXISTS (SELECT 1 FROM core.SoDRule s WHERE s.IsActive = 1 AND s.Severity = 'BLOCK' AND s.PermissionA = v.PermissionCode
                     AND EXISTS (SELECT 1 FROM core.RolePermission y WHERE y.RoleCode = v.RoleCode AND y.PermissionCode = s.PermissionB));
GO
/*  The Finance menu.  The five hand written forms are frmJournalEntry, frmJournalEnquiry,
    frmPeriodClose, frmVatReturn and frmBankRecon; everything else is either the generic
    maintenance shell primed with a core.MaintDef code or the report launcher primed with a
    core.ReportDef code.  REQ: COM-024, FIN-002, FIN-010, FIN-016, FIN-018 */
IF OBJECT_ID(N'core.MenuItem') IS NOT NULL
BEGIN
    DECLARE @m table (MenuCode varchar(40), ParentMenuCode varchar(40), Caption varchar(60), FormClass varchar(60) NULL,
                      FormArgument varchar(60) NULL, PermissionCode varchar(40) NULL, SortOrder int, Shortcut varchar(20) NULL);
    INSERT @m VALUES
      ('FIN.JOURNAL.ENTRY', 'FIN', '&Journal Entry...',             'frmJournalEntry',   NULL,            'FIN.JOURNAL.ENTER',  10, 'F5'),
      ('FIN.JOURNAL.ENQ',   'FIN', 'Journal &Enquiry...',           'frmJournalEnquiry', NULL,            'FIN.ENQUIRY.VIEW',   20, 'F6'),
      ('FIN.PERIOD.CLOSE',  'FIN', '&Period Close Checklist...',    'frmPeriodClose',    NULL,            'FIN.PERIOD.VIEW',    30, 'F7'),
      ('FIN.VAT.RETURN',    'FIN', '&VAT Return...',                'frmVatReturn',      NULL,            'FIN.VAT.PREPARE',    40, 'F8'),
      ('FIN.BANK.RECON',    'FIN', '&Bank Reconciliation...',       'frmBankRecon',      NULL,            'FIN.BANK.RECONCILE', 50, 'F9'),
      ('FIN.MAINT',         'FIN', 'Finance &Maintenance',          NULL,                NULL,             NULL,                70, NULL),
      ('FIN.MAINT.ACCOUNT', 'FIN.MAINT', 'Chart of &Accounts...',   'frmGenericMaint',   'FIN.ACCOUNT',   'FIN.ACCOUNT.MAINT',  10, NULL),
      ('FIN.MAINT.ACCLASS', 'FIN.MAINT', 'Account &Classes...',     'frmGenericMaint',   'FIN.ACCLASS',   'FIN.ACCOUNT.MAINT',  20, NULL),
      ('FIN.MAINT.VATCODE', 'FIN.MAINT', '&VAT Codes...',           'frmGenericMaint',   'FIN.VATCODE',   'FIN.VAT.APPROVE',    30, NULL),
      ('FIN.MAINT.VATRATE', 'FIN.MAINT', 'VAT &Rates...',           'frmGenericMaint',   'FIN.VATRATE',   'FIN.VAT.APPROVE',    40, NULL),
      ('FIN.MAINT.BUDGET',  'FIN.MAINT', '&Budget Versions...',     'frmGenericMaint',   'FIN.BUDGET',    'FIN.BUDGET.MAINT',   50, NULL),
      ('FIN.MAINT.BUDGLINE','FIN.MAINT', 'Budget &Lines...',        'frmGenericMaint',   'FIN.BUDGETLINE','FIN.BUDGET.MAINT',   60, NULL),
      ('FIN.MAINT.BANK',    'FIN.MAINT', 'Bank A&ccounts...',       'frmGenericMaint',   'FIN.BANK',      'FIN.BANK.MAINT',     70, NULL),
      ('FIN.RUN',           'FIN', 'Finance &Runs',                 NULL,                NULL,             NULL,                80, NULL),
      ('FIN.RUN.RECUR',     'FIN.RUN', '&Recurring Journals...',    'frmReportLauncher', 'RECURJNL',      'FIN.RECURRING.MAINT',10, NULL),
      ('FIN.RUN.REVAL',     'FIN.RUN', 'Currency Re&valuation...',  'frmReportLauncher', 'REVALRUN',      'FIN.REVAL.RUN',      20, NULL),
      ('FIN.RUN.YEAREND',   'FIN.RUN', '&Year End...',              'frmReportLauncher', 'YEAREND',       'FIN.YEAREND.RUN',    30, NULL),
      ('FIN.RUN.MTD',       'FIN.RUN', '&MTD Submission File...',   'frmReportLauncher', 'MTDJSON',       'FIN.VAT.SUBMIT',     40, NULL),
      ('FIN.RPT',           'FIN', 'Finance Repor&ts',              NULL,                NULL,             NULL,                90, NULL),
      ('FIN.RPT.TB',        'FIN.RPT', '&Trial Balance...',         'frmReportLauncher', 'TBRPT',         'FIN.ENQUIRY.VIEW',   10, NULL),
      ('FIN.RPT.PL',        'FIN.RPT', '&Profit and Loss...',       'frmReportLauncher', 'PLRPT',         'FIN.ENQUIRY.VIEW',   20, NULL),
      ('FIN.RPT.BS',        'FIN.RPT', '&Balance Sheet...',         'frmReportLauncher', 'BSRPT',         'FIN.ENQUIRY.VIEW',   30, NULL),
      ('FIN.RPT.CASHBOOK',  'FIN.RPT', '&Cash Book...',             'frmReportLauncher', 'CASHBOOK',      'FIN.BANK.RECONCILE', 40, NULL),
      ('FIN.RPT.BUDGET',    'FIN.RPT', 'Budget &Variance...',       'frmReportLauncher', 'BUDGRPT',       'FIN.BUDGET.MAINT',   50, NULL);

    MERGE core.MenuItem AS t USING @m AS s ON t.MenuCode = s.MenuCode
    WHEN MATCHED THEN UPDATE SET ParentMenuCode = s.ParentMenuCode, Caption = s.Caption, FormClass = s.FormClass,
            FormArgument = s.FormArgument, PermissionCode = s.PermissionCode, SortOrder = s.SortOrder, Shortcut = s.Shortcut, IsActive = 1
    WHEN NOT MATCHED BY TARGET THEN
        INSERT (MenuCode, ParentMenuCode, Caption, FormClass, FormArgument, PermissionCode, SortOrder, Shortcut, IsActive)
        VALUES (s.MenuCode, s.ParentMenuCode, s.Caption, s.FormClass, s.FormArgument, s.PermissionCode, s.SortOrder, s.Shortcut, 1);
    PRINT 'Seeded core.MenuItem (Finance)';
END
GO
/*  The fin batch catalogue.  ParamSpec is the prompt list the report launcher builds its dialogue
    from; ProgramName is the COBOL batch program in cobol\batch\fin, run with KEY=VALUE arguments.
    FINDEMO loads demonstration data and is deliberately inactive so it never appears on a menu.
    REQ: FIN-011, FIN-012, FIN-016, FIN-019, FIN-020, FIN-021, FIN-022, FIN-025, FIN-026, COM-019 */
MERGE core.ReportDef AS t
USING (VALUES
 ('TBRPT','fin','Trial balance','TBRPT','CO,YEAR,PERFROM,PERTO,ZEROS','FIN.ENQUIRY.VIEW','FIN-021, FIN-023, FIN-030','Brought forward, period movement and carried forward by posting account, with the debit and credit totals proved equal.'),
 ('PLRPT','fin','Profit and loss account','PLRPT','CO,YEAR,PERFROM,PERTO,COMPARE','FIN.ENQUIRY.VIEW','FIN-021, FIN-022','Statutory format profit and loss for a period range with the prior year comparative and a year to date column.'),
 ('BSRPT','fin','Balance sheet','BSRPT','CO,YEAR,PERIOD,COMPARE','FIN.ENQUIRY.VIEW','FIN-021, FIN-022','Statutory format balance sheet as at a period end, proving that assets equal liabilities plus equity.'),
 ('CASHBOOK','fin','Cash book','CASHBOOK','CO,BANK,FROM,TO','FIN.BANK.RECONCILE','FIN-017, FIN-018','Receipts and payments through a bank account for a date range with the running book balance and the unreconciled items.'),
 ('BUDGRPT','fin','Budget variance','BUDGRPT','CO,YEAR,PERIOD,BUDGET','FIN.BUDGET.MAINT','FIN-019, FIN-020','Actual against budget by account for the period and year to date, with the variance and the variance percentage.'),
 ('MTDJSON','fin','Making Tax Digital submission file','MTDJSON','CO,VATPERIOD','FIN.VAT.SUBMIT','FIN-016, FIN-028','Writes the nine box VAT return as an MTD JSON payload to the mtd folder of the run directory, ready to transmit.'),
 ('RECURJNL','fin','Recurring and reversing journals','RECURJNL','CO,YEAR,PERIOD,MODE','FIN.RECURRING.MAINT','FIN-006, FIN-007','Generates the journals due in a period from the recurring templates and reverses the accruals raised in the prior period.'),
 ('REVALRUN','fin','Foreign currency revaluation','REVALRUN','CO,ASAT,MODE,RATE,USER','FIN.REVAL.RUN','FIN-019, FIN-020','Revalues monetary balances held in foreign currency at the period end rate and posts the unrealised difference.'),
 ('YEAREND','fin','Year end close','YEAREND','CO,YEAR','FIN.YEAREND.RUN','FIN-012','Closes the profit and loss accounts to retained earnings, carries the balance sheet forward and opens the new fiscal year.'),
 ('FINDEMO','fin','Finance demonstration data','FINDEMO','CO','FIN.JOURNAL.POST','FIN-029','Loads the demonstration journals, bank statement lines and budget used by the finance screens and reports.')
) AS s (ReportCode, Module, Title, ProgramName, ParamSpec, PermissionCode, RequirementRefs, Description)
ON t.ReportCode = s.ReportCode
WHEN MATCHED THEN UPDATE SET Module = s.Module, Title = s.Title, ProgramName = s.ProgramName, ParamSpec = s.ParamSpec,
        PermissionCode = s.PermissionCode, RequirementRefs = s.RequirementRefs, Description = s.Description,
        IsActive = CASE WHEN s.ReportCode = 'FINDEMO' THEN 0 ELSE 1 END, ModifiedBy = 'SEED', ModifiedAt = SYSDATETIME()
WHEN NOT MATCHED THEN INSERT (ReportCode, Module, Title, ProgramName, ParamSpec, PermissionCode, RequirementRefs, Description, IsActive, CreatedBy)
     VALUES (s.ReportCode, s.Module, s.Title, s.ProgramName, s.ParamSpec, s.PermissionCode, s.RequirementRefs, s.Description,
             CASE WHEN s.ReportCode = 'FINDEMO' THEN 0 ELSE 1 END, 'SEED');
GO
/*  Generic maintenance for the simple finance masters.  These tables are keyed and edited through
    this metadata by frmGenericMaint, with no hand written form.  Balances, postings and anything
    derived from a transaction are never editable here.  REQ: FIN-001, FIN-013, FIN-019, FIN-017 */
IF OBJECT_ID(N'core.MaintDef') IS NOT NULL AND OBJECT_ID(N'core.MaintColumn') IS NOT NULL
BEGIN
    DECLARE @d table (MaintCode varchar(30), Title varchar(80), ModuleCode varchar(10), SchemaName sysname,
                      TableName sysname, CompanyScoped bit, RetireColumn sysname NULL,
                      PermissionCode varchar(60) NULL, HelpText varchar(400) NULL);
    INSERT @d VALUES
      ('FIN.ACCOUNT',   'Chart of accounts',      'fin', 'fin', 'Account',     1, NULL,       'FIN.ACCOUNT.MAINT', 'The governed chart of accounts. An account is blocked or retired, never deleted, because posted history must keep its account. A control account is maintained by its own subledger and refuses a manual journal.'),
      ('FIN.ACCLASS',   'Account classes',        'fin', 'fin', 'AccountClass',0, NULL,       'FIN.ACCOUNT.MAINT', 'The class decides the statement an account appears on and its normal balance. Changing a class after a year has been posted changes the shape of the comparatives, so it is an approval grade change.'),
      ('FIN.VATCODE',   'VAT codes',              'fin', 'fin', 'VatCode',     0, 'IsActive', 'FIN.VAT.APPROVE',   'VAT liability, direction and mechanism with the nine box mapping. Boxes 2, 8 and 9 stay empty because Northern Ireland to European Union goods movements are out of scope. A code is deactivated, never deleted.'),
      ('FIN.VATRATE',   'VAT rates',              'fin', 'fin', 'VatRate',     0, NULL,       'FIN.VAT.APPROVE',   'Effective dated percentage for a VAT code. A rate change creates a new row with a new effective date so that a document is always rated at its tax point, never retrospectively.'),
      ('FIN.BUDGET',    'Budget versions',        'fin', 'fin', 'Budget',      1, NULL,       'FIN.BUDGET.MAINT',  'A named budget or forecast version for a fiscal year. Only one version may be the baseline. An approved version is superseded rather than edited, so the variance reported last month can still be reproduced.'),
      ('FIN.BUDGETLINE','Budget lines',           'fin', 'fin', 'BudgetLine',  1, NULL,       'FIN.BUDGET.MAINT',  'The budget amount for one account, period and analysis combination. Phasing is held period by period rather than as a year total divided by twelve.'),
      ('FIN.BANK',      'Bank accounts',          'fin', 'fin', 'BankAccount', 1, NULL,       'FIN.BANK.MAINT',    'Company bank accounts and the general ledger account each one posts to. Sort code and account number are sensitive: a change is audited and the payment runs are blocked until it is approved.');

    MERGE core.MaintDef AS t USING @d AS s ON t.MaintCode = s.MaintCode
    WHEN MATCHED THEN UPDATE SET Title = s.Title, ModuleCode = s.ModuleCode, SchemaName = s.SchemaName,
            TableName = s.TableName, CompanyScoped = s.CompanyScoped, RetireColumn = s.RetireColumn,
            PermissionCode = s.PermissionCode, HelpText = s.HelpText, IsActive = 1, ModifiedBy = 'SEED', ModifiedAt = SYSDATETIME()
    WHEN NOT MATCHED BY TARGET THEN
        INSERT (MaintCode, Title, ModuleCode, SchemaName, TableName, CompanyScoped, RetireColumn, PermissionCode, HelpText, CreatedBy)
        VALUES (s.MaintCode, s.Title, s.ModuleCode, s.SchemaName, s.TableName, s.CompanyScoped, s.RetireColumn, s.PermissionCode, s.HelpText, 'SEED');

    DECLARE @c table (MaintCode varchar(30), ColumnName sysname, Caption varchar(60), DataType varchar(10), SortOrder int,
                      Width int, IsKey bit, IsRequired bit, IsReadOnly bit, IsInList bit, IsInEdit bit, IsSearchable bit,
                      LookupProc varchar(128) NULL, LookupArg varchar(60) NULL);
    INSERT @c VALUES
      -- Chart of accounts ----------------------------------------------------------------------------
      ('FIN.ACCOUNT','CompanyCode','Company','CODE',             10, 80,1,1,0,1,1,0,NULL,NULL),
      ('FIN.ACCOUNT','AccountCode','Account','CODE',             20,100,1,1,0,1,1,1,NULL,NULL),
      ('FIN.ACCOUNT','Name','Account name','TEXT',               30,240,0,1,0,1,1,1,NULL,NULL),
      ('FIN.ACCOUNT','ClassCode','Class','CODE',                 40, 90,0,1,0,1,1,1,NULL,NULL),
      ('FIN.ACCOUNT','AccountKind','Kind','CODE',                50, 90,0,1,0,1,1,0,NULL,NULL),
      ('FIN.ACCOUNT','ControlType','Control','CODE',             60, 90,0,0,0,1,1,0,NULL,NULL),
      ('FIN.ACCOUNT','AllowManualJournal','Manual journal','BOOL',70,110,0,0,0,1,1,0,NULL,NULL),
      ('FIN.ACCOUNT','CurrencyCode','Currency','CODE',           80, 80,0,0,0,1,1,0,NULL,NULL),
      ('FIN.ACCOUNT','IsVatEligible','VAT eligible','BOOL',      90,100,0,0,0,1,1,0,NULL,NULL),
      ('FIN.ACCOUNT','ManagementGroup','Management group','TEXT',100,150,0,0,0,1,1,1,NULL,NULL),
      ('FIN.ACCOUNT','StatutoryLineCode','Statutory line','CODE',110,120,0,0,0,0,1,0,NULL,NULL),
      ('FIN.ACCOUNT','ValidFrom','Valid from','DATE',            120, 90,0,1,0,0,1,0,NULL,NULL),
      ('FIN.ACCOUNT','ValidTo','Valid to','DATE',                130, 90,0,0,0,0,1,0,NULL,NULL),
      ('FIN.ACCOUNT','Status','Status','CODE',                   140, 80,0,1,0,1,1,0,NULL,NULL),
      -- Account classes ------------------------------------------------------------------------------
      ('FIN.ACCLASS','ClassCode','Class','CODE',                 10, 90,1,1,0,1,1,1,NULL,NULL),
      ('FIN.ACCLASS','Name','Class name','TEXT',                 20,220,0,1,0,1,1,1,NULL,NULL),
      ('FIN.ACCLASS','ClassType','Type','CODE',                  30,100,0,1,0,1,1,0,NULL,NULL),
      ('FIN.ACCLASS','NormalBalance','Normal balance','CODE',    40, 90,0,1,0,1,1,0,NULL,NULL),
      ('FIN.ACCLASS','Statement','Statement','CODE',             50, 80,0,1,0,1,1,0,NULL,NULL),
      ('FIN.ACCLASS','SortOrder','Sequence','INT',               60, 70,0,0,0,1,1,0,NULL,NULL),
      -- VAT codes ------------------------------------------------------------------------------------
      ('FIN.VATCODE','VatCode','VAT code','CODE',                10, 70,1,1,0,1,1,1,NULL,NULL),
      ('FIN.VATCODE','Name','Description','TEXT',                20,240,0,1,0,1,1,1,NULL,NULL),
      ('FIN.VATCODE','Liability','Liability','CODE',             30,100,0,1,0,1,1,0,NULL,NULL),
      ('FIN.VATCODE','Direction','Direction','CODE',             40, 80,0,1,0,1,1,0,NULL,NULL),
      ('FIN.VATCODE','Mechanism','Mechanism','CODE',             50,100,0,1,0,1,1,0,NULL,NULL),
      ('FIN.VATCODE','IsRecoverable','Recoverable','BOOL',       60, 90,0,0,0,1,1,0,NULL,NULL),
      ('FIN.VATCODE','SelfAccounts','Self accounts','BOOL',      70, 90,0,0,0,1,1,0,NULL,NULL),
      ('FIN.VATCODE','OutputVatBox','Output VAT box','INT',      80, 90,0,0,0,1,1,0,NULL,NULL),
      ('FIN.VATCODE','InputVatBox','Input VAT box','INT',        90, 90,0,0,0,1,1,0,NULL,NULL),
      ('FIN.VATCODE','OutputNetBox','Output net box','INT',     100, 90,0,0,0,0,1,0,NULL,NULL),
      ('FIN.VATCODE','InputNetBox','Input net box','INT',       110, 90,0,0,0,0,1,0,NULL,NULL),
      ('FIN.VATCODE','InvoiceWording','Invoice wording','TEXT', 120,260,0,0,0,0,1,0,NULL,NULL),
      ('FIN.VATCODE','IsActive','In use','BOOL',                130, 70,0,0,0,1,1,0,NULL,NULL),
      -- VAT rates ------------------------------------------------------------------------------------
      ('FIN.VATRATE','VatCode','VAT code','CODE',                10, 70,1,1,0,1,1,1,NULL,NULL),
      ('FIN.VATRATE','EffectiveFrom','Effective from','DATE',    20,100,1,1,0,1,1,0,NULL,NULL),
      ('FIN.VATRATE','RatePct','Rate per cent','PCT',            30, 90,0,1,0,1,1,0,NULL,NULL),
      ('FIN.VATRATE','ApprovedBy','Approved by','CODE',          40, 90,0,0,0,1,1,0,NULL,NULL),
      ('FIN.VATRATE','StatutoryRef','Statutory reference','TEXT',50,240,0,0,0,1,1,0,NULL,NULL),
      -- Budget versions ------------------------------------------------------------------------------
      ('FIN.BUDGET','CompanyCode','Company','CODE',              10, 80,1,1,0,1,1,0,NULL,NULL),
      ('FIN.BUDGET','BudgetCode','Budget','CODE',                20,110,1,1,0,1,1,1,NULL,NULL),
      ('FIN.BUDGET','FiscalYear','Year','INT',                   30, 70,1,1,0,1,1,0,NULL,NULL),
      ('FIN.BUDGET','Version','Version','INT',                   40, 70,1,1,0,1,1,0,NULL,NULL),
      ('FIN.BUDGET','Name','Budget name','TEXT',                 50,220,0,1,0,1,1,1,NULL,NULL),
      ('FIN.BUDGET','BudgetType','Type','CODE',                  60, 90,0,1,0,1,1,0,NULL,NULL),
      ('FIN.BUDGET','CurrencyCode','Currency','CODE',            70, 80,0,1,0,1,1,0,NULL,NULL),
      ('FIN.BUDGET','IsBaseline','Baseline','BOOL',              80, 80,0,0,0,1,1,0,NULL,NULL),
      ('FIN.BUDGET','OwnerUser','Owner','CODE',                  90, 90,0,0,0,1,1,0,NULL,NULL),
      ('FIN.BUDGET','Status','Status','CODE',                   100, 90,0,1,0,1,1,0,NULL,NULL),
      ('FIN.BUDGET','ApprovedBy','Approved by','CODE',          110, 90,0,0,1,1,1,0,NULL,NULL),
      -- Budget lines ---------------------------------------------------------------------------------
      ('FIN.BUDGETLINE','CompanyCode','Company','CODE',          10, 80,0,1,0,1,1,0,NULL,NULL),
      ('FIN.BUDGETLINE','BudgetLineId','Line','INT',             20, 70,1,1,1,1,0,0,NULL,NULL),
      ('FIN.BUDGETLINE','BudgetCode','Budget','CODE',            30,110,0,1,0,1,1,1,NULL,NULL),
      ('FIN.BUDGETLINE','FiscalYear','Year','INT',               40, 70,0,1,0,1,1,0,NULL,NULL),
      ('FIN.BUDGETLINE','Version','Version','INT',               50, 70,0,1,0,1,1,0,NULL,NULL),
      ('FIN.BUDGETLINE','PeriodNo','Period','INT',               60, 70,0,1,0,1,1,0,NULL,NULL),
      ('FIN.BUDGETLINE','AccountCode','Account','CODE',          70,100,0,1,0,1,1,1,NULL,NULL),
      ('FIN.BUDGETLINE','DimCostCentre','Cost centre','CODE',    80, 90,0,0,0,1,1,1,NULL,NULL),
      ('FIN.BUDGETLINE','DimProject','Project','CODE',           90, 90,0,0,0,1,1,1,NULL,NULL),
      ('FIN.BUDGETLINE','Amount','Amount','MONEY',              100,110,0,1,0,1,1,0,NULL,NULL),
      ('FIN.BUDGETLINE','Narrative','Narrative','TEXT',         110,220,0,0,0,0,1,0,NULL,NULL),
      -- Bank accounts --------------------------------------------------------------------------------
      ('FIN.BANK','CompanyCode','Company','CODE',                10, 80,1,1,0,1,1,0,NULL,NULL),
      ('FIN.BANK','BankAccountCode','Bank account','CODE',       20,110,1,1,0,1,1,1,NULL,NULL),
      ('FIN.BANK','Name','Account name','TEXT',                  30,220,0,1,0,1,1,1,NULL,NULL),
      ('FIN.BANK','AccountCode','Ledger account','CODE',         40,100,0,1,0,1,1,1,NULL,NULL),
      ('FIN.BANK','CurrencyCode','Currency','CODE',              50, 80,0,1,0,1,1,0,NULL,NULL),
      ('FIN.BANK','BankName','Bank','TEXT',                      60,160,0,0,0,1,1,1,NULL,NULL),
      ('FIN.BANK','SortCode','Sort code','CODE',                 70, 90,0,0,0,1,1,0,NULL,NULL),
      ('FIN.BANK','AccountNumber','Account number','CODE',       80,110,0,0,0,1,1,0,NULL,NULL),
      ('FIN.BANK','Iban','IBAN','TEXT',                          90,200,0,0,0,0,1,0,NULL,NULL),
      ('FIN.BANK','CalendarCode','Calendar','CODE',             100, 90,0,1,0,0,1,0,NULL,NULL),
      ('FIN.BANK','StatementBalance','Statement balance','MONEY',110,120,0,0,1,1,1,0,NULL,NULL),
      ('FIN.BANK','StatementDate','Statement date','DATE',      120,100,0,0,1,1,1,0,NULL,NULL),
      ('FIN.BANK','LastReconciledDate','Last reconciled','DATE',130,110,0,0,1,1,1,0,NULL,NULL),
      ('FIN.BANK','Status','Status','CODE',                     140, 80,0,1,0,1,1,0,NULL,NULL);

    MERGE core.MaintColumn AS t
    USING @c AS s ON t.MaintCode = s.MaintCode AND t.ColumnName = s.ColumnName
    WHEN MATCHED THEN UPDATE SET Caption = s.Caption, DataType = s.DataType, SortOrder = s.SortOrder,
            Width = s.Width, IsKey = s.IsKey, IsRequired = s.IsRequired, IsReadOnly = s.IsReadOnly,
            IsInList = s.IsInList, IsInEdit = s.IsInEdit, IsSearchable = s.IsSearchable,
            LookupProc = s.LookupProc, LookupArg = s.LookupArg
    WHEN NOT MATCHED BY TARGET AND s.MaintCode LIKE 'FIN.%' THEN
        INSERT (MaintCode, ColumnName, Caption, DataType, SortOrder, Width, IsKey, IsRequired, IsReadOnly,
                IsInList, IsInEdit, IsSearchable, LookupProc, LookupArg)
        VALUES (s.MaintCode, s.ColumnName, s.Caption, s.DataType, s.SortOrder, s.Width, s.IsKey, s.IsRequired,
                s.IsReadOnly, s.IsInList, s.IsInEdit, s.IsSearchable, s.LookupProc, s.LookupArg);

    PRINT 'Seeded core.MaintDef / core.MaintColumn (Finance masters)';
END
GO
/*  VAT return periods for the prior year.  Both companies file quarterly on stagger group three,
    so the quarters end in March, June, September and December.  The return is due one calendar
    month and seven days after the period end, and payment falls on the same day (D-15).
    11-fin-eventmap.sql already seeds the current year quarters 26A1 onwards; this adds the four
    FY2025/26 quarters so that the VAT return screen has a filed prior period to compare against
    and an earlier period to post an error correction into.  Keys follow the HMRC convention of
    the two digit year the quarter falls in followed by A and the quarter number, and the guard is
    on the date range as well as the key so that a second key for the same quarter is impossible.
    REQ: FIN-016, FIN-018, FIN-028, D-15 */
INSERT fin.VatPeriod (CompanyCode, VatPeriodKey, VatRegNo, StartDate, EndDate, FilingDueDate, PaymentDueDate,
        Frequency, Status, SubmissionRef, FinalisedBy, FinalisedAt)
SELECT c.CompanyCode, q.VatPeriodKey, c.VatRegNo, q.StartDate, q.EndDate,
       DATEADD(day, 7, DATEADD(month, 1, q.EndDate)), DATEADD(day, 7, DATEADD(month, 1, q.EndDate)),
       'QUARTERLY', 'SUBMITTED', 'HMRC-' + c.CompanyCode + '-' + q.VatPeriodKey, 'FINCTRL',
       CAST(DATEADD(day, 5, DATEADD(month, 1, q.EndDate)) AS datetime2(3))
  FROM core.Company c
 CROSS JOIN (VALUES
    ('25A1','2025-04-01','2025-06-30'), ('25A2','2025-07-01','2025-09-30'),
    ('25A3','2025-10-01','2025-12-31'), ('25A4','2026-01-01','2026-03-31')
 ) AS q (VatPeriodKey, StartDate, EndDate)
 WHERE c.VatRegNo IS NOT NULL
   AND NOT EXISTS (SELECT 1 FROM fin.VatPeriod p
                    WHERE p.CompanyCode = c.CompanyCode
                      AND (p.VatPeriodKey = q.VatPeriodKey OR (p.StartDate = q.StartDate AND p.EndDate = q.EndDate)));
GO
/*  Statutory filing obligations.  Every VAT quarter raises a VATRETURN obligation on its filing
    due date, and the FY2026 accounting reference date raises the Companies House and corporation
    tax obligations: accounts nine months after the period end, corporation tax payment nine months
    and one day after it, and the return twelve months after it.  An obligation that is still open
    seven days before it is due escalates.  REQ: FIN-026, FIN-027, FIN-028 */
WITH q AS (
    SELECT p.CompanyCode, p.EndDate, p.FilingDueDate, p.Status, p.SubmissionRef,
           ROW_NUMBER() OVER (PARTITION BY p.CompanyCode, p.EndDate ORDER BY p.VatPeriodKey) AS Seq
      FROM fin.VatPeriod p
)
INSERT fin.StatutoryObligation (CompanyCode, PeriodEndDate, ObligationType, Description, DueDate, EscalateFrom,
        ResponsibleRole, Status, Reference, Note)
SELECT q.CompanyCode, q.EndDate, 'VATRETURN',
       'VAT return for the quarter ended ' + FORMAT(q.EndDate, 'd MMMM yyyy', 'en-GB'),
       q.FilingDueDate, DATEADD(day, -7, q.FilingDueDate), 'FINCTRL',
       CASE WHEN q.Status IN ('SUBMITTED', 'AMENDED') THEN 'COMPLETE' ELSE 'OPEN' END,
       q.SubmissionRef, 'Nine box return filed under Making Tax Digital.'
  FROM q
 WHERE q.Seq = 1
   AND NOT EXISTS (SELECT 1 FROM fin.StatutoryObligation x
                    WHERE x.CompanyCode = q.CompanyCode AND x.PeriodEndDate = q.EndDate AND x.ObligationType = 'VATRETURN');

INSERT fin.StatutoryObligation (CompanyCode, PeriodEndDate, ObligationType, Description, DueDate, EscalateFrom,
        ResponsibleRole, Status, Note)
SELECT c.CompanyCode, o.PeriodEndDate, o.ObligationType, o.Description, o.DueDate, DATEADD(day, -28, o.DueDate),
       o.ResponsibleRole, 'OPEN', o.Note
  FROM core.Company c
 CROSS JOIN (VALUES
    ('2027-03-31','ACCOUNTS','Annual accounts for the year ended 31 March 2027','2027-12-31','FINCTRL','Filed at Companies House under FRS 102 Section 1A.'),
    ('2027-03-31','CTPAYMENT','Corporation tax payment for the year ended 31 March 2027','2028-01-01','FINCTRL','Due nine months and one day after the end of the accounting period.'),
    ('2027-03-31','CTRETURN','Corporation tax return CT600 for the year ended 31 March 2027','2028-03-31','FINCTRL','Filed with HMRC twelve months after the end of the accounting period.'),
    ('2027-03-31','CONFIRMATION','Confirmation statement','2027-04-14','FINCTRL','Confirms the registered office, officers and people with significant control.')
 ) AS o (PeriodEndDate, ObligationType, Description, DueDate, ResponsibleRole, Note)
 WHERE NOT EXISTS (SELECT 1 FROM fin.StatutoryObligation x
                    WHERE x.CompanyCode = c.CompanyCode AND x.PeriodEndDate = o.PeriodEndDate AND x.ObligationType = o.ObligationType);
PRINT 'Seeded fin.VatPeriod / fin.StatutoryObligation';
GO
