/*  Creditors module seed: the menu the purchase ledger is driven from and the report
    definitions the launcher primes.  Three hand written forms carry the work that needs
    judgement - supplier and bank verification, the invoice register with matching, and the
    payment proposal and run; everything else is the report launcher.
    Re-runnable (MERGE / IF NOT EXISTS).
    REQ: AP-001, AP-002, AP-003, AP-006, AP-011, AP-012, AP-013, AP-015, AP-020, AP-025,
         AP-027, AP-028, COM-019, COM-024, RPT-018 */
SET NOCOUNT ON;
GO
/*  The purchase ledger menu.  Payment release and approval are separated from preparation,
    because the person who prepares a run may not authorise it (SOD-PAY).  REQ: COM-024, D-16 */
IF OBJECT_ID(N'core.MenuItem') IS NOT NULL
BEGIN
    DECLARE @m table (MenuCode varchar(40), ParentMenuCode varchar(40), Caption varchar(60), FormClass varchar(80) NULL,
                      FormArgument varchar(60) NULL, PermissionCode varchar(60) NULL, SortOrder int, Shortcut varchar(20) NULL);
    INSERT @m VALUES
      ('AP.SUPPLIER.VERIFY','AP', '&Supplier and Bank Verification...', 'frmSupplierVerify',  NULL,      'AP.SUPPLIER.VIEW',    10, 'F5'),
      ('AP.INVOICE.REG',    'AP', '&Invoice Register and Matching...',  'frmInvoiceRegister', NULL,      'AP.INVOICE.ENTER',    20, 'F6'),
      ('AP.PAYMENT.RUN',    'AP', '&Payment Proposal and Run...',       'frmPaymentRun',      NULL,      'AP.PAYMENT.PROPOSE',  30, 'F7'),
      ('AP.RPT',            'AP', 'Creditors Repor&ts',                 NULL,                 NULL,       NULL,                 90, NULL),
      ('AP.RPT.AGED',       'AP.RPT', '&Aged Creditors...',             'frmReportLauncher',  'APAGED',  'AP.ENQUIRY.VIEW',     10, NULL),
      ('AP.RPT.REMIT',      'AP.RPT', '&Remittance Advices...',         'frmReportLauncher',  'APREMIT', 'AP.PAYMENT.RELEASE',  20, NULL),
      ('AP.RPT.BACS',       'AP.RPT', '&Bacs Payment File...',          'frmReportLauncher',  'APBACS',  'AP.PAYMENT.RELEASE',  30, NULL),
      ('AP.RPT.PPR',        'AP.RPT', '&Payment Practices Report...',   'frmReportLauncher',  'APPPR',   'AP.PRACTICES.REPORT', 40, NULL),
      ('AP.RPT.VAT6M',      'AP.RPT', 'Six Month &Unpaid Input VAT...', 'frmReportLauncher',  'APVAT6M', 'AP.ENQUIRY.VIEW',     50, NULL);

    MERGE core.MenuItem AS t USING @m AS s ON t.MenuCode = s.MenuCode
    WHEN MATCHED THEN UPDATE SET ParentMenuCode = s.ParentMenuCode, Caption = s.Caption, FormClass = s.FormClass,
            FormArgument = s.FormArgument, PermissionCode = s.PermissionCode, SortOrder = s.SortOrder, Shortcut = s.Shortcut, IsActive = 1
    WHEN NOT MATCHED BY TARGET THEN
        INSERT (MenuCode, ParentMenuCode, Caption, FormClass, FormArgument, PermissionCode, SortOrder, Shortcut, IsActive)
        VALUES (s.MenuCode, s.ParentMenuCode, s.Caption, s.FormClass, s.FormArgument, s.PermissionCode, s.SortOrder, s.Shortcut, 1);
    PRINT 'Seeded core.MenuItem (Creditors)';
END
GO
/*  Batch reports.  ProgramName is the COBOL batch program in cobol\batch\ap and ParamSpec is
    the prompt list the report launcher builds its dialogue from.  The Bacs file and the
    remittances are produced from an approved payment run only, which is why they carry the
    release permission rather than the enquiry one.
    REQ: AP-013, AP-015, AP-020, AP-025, AP-028, RPT-018, COM-019 */
MERGE core.ReportDef AS t
USING (VALUES
 ('APAGED','AP','Aged creditors and cash requirement','APAGED','CO,ASAT,SUPP','AP.ENQUIRY.VIEW','AP-020, AP-022',
  'Open item ageing as at a chosen date with blocked, disputed and in-flight amounts shown separately, and the cash requirement by due band.'),
 ('APREMIT','AP','Remittance advices','APREMIT','CO,RUN','AP.PAYMENT.RELEASE','AP-015',
  'Remittance advice for every payment in an approved run, showing the invoices, credits and deductions settled. Reissue has no cash effect.'),
 ('APBACS','AP','Bacs payment file, Standard 18','APBACS','CO,RUN,ENTRYDATE','AP.PAYMENT.RELEASE','AP-013, AP-026',
  'Bacs Standard 18 payment file for an approved run, written to run\bacs with its hash totals and processing calendar.'),
 ('APPPR','AP','Payment practices and performance','APPPR','CO,FROM,TO','AP.PRACTICES.REPORT','AP-028, AP-030, RPT-018',
  'Half-yearly reporting duty: average days to pay, the 30, 31-60 and 61 day bands, the proportion not paid within terms and the disputed share.'),
 ('APVAT6M','AP','Six month unpaid input VAT','APVAT6M','CO,ASAT,MODE','AP.ENQUIRY.VIEW','AP-025, AP-029',
  'Input VAT on invoices still unpaid more than six months after the due date, to be repaid on the return covering that date and reclaimed once paid.')
) AS s (ReportCode, Module, Title, ProgramName, ParamSpec, PermissionCode, RequirementRefs, Description)
ON t.ReportCode = s.ReportCode
WHEN MATCHED THEN UPDATE SET Module = s.Module, Title = s.Title, ProgramName = s.ProgramName, ParamSpec = s.ParamSpec,
        PermissionCode = s.PermissionCode, RequirementRefs = s.RequirementRefs, Description = s.Description, IsActive = 1,
        ModifiedBy = 'SEED', ModifiedAt = SYSDATETIME()
WHEN NOT MATCHED THEN INSERT (ReportCode, Module, Title, ProgramName, ParamSpec, PermissionCode, RequirementRefs, Description, CreatedBy)
     VALUES (s.ReportCode, s.Module, s.Title, s.ProgramName, s.ParamSpec, s.PermissionCode, s.RequirementRefs, s.Description, 'SEED');
GO
PRINT 'Seeded AP menu and report definitions';
GO
