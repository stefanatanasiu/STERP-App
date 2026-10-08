/*  Purchasing module user interface: the Purchasing menu branch, the batch report catalogue for the
    five purchasing reports, the generic maintenance definitions for the simple purchasing masters,
    and the four permission codes the purchasing screens need that the core catalogue does not
    already carry.  The top level 'PO' bar entry is created by 07-core-numbering-terms.sql; only its
    children belong here.
    The three hand written screens are frmRequisition, frmPurchaseOrder and frmGoodsReceipt; the
    rest of the branch is the generic maintenance shell or the report launcher primed with a
    core.ReportDef code.
    Re-runnable (MERGE / IF NOT EXISTS).
    REQ: PO-001, PO-002, PO-004, PO-008, PO-009, PO-010, PO-016, PO-022, PO-023, PO-024, PO-026,
    COM-002, COM-019, COM-024, D-08, D-19 */
SET NOCOUNT ON;
GO
/*  Permissions the core catalogue does not carry yet.  Everything else the purchasing screens use
    (PO.REQ.ENTER, PO.REQ.APPROVE, PO.ORDER.ENTER, PO.ORDER.AMEND, PO.ORDER.APPROVE,
    PO.ORDER.RELEASE, PO.ORDER.CLOSE, PO.RECEIPT.CONFIRM, PO.RETURN.AUTHORISE, PO.SCHEDULE.MAINT,
    PO.IMPORT.ENTER, PO.SERVICE.ACCEPT, PO.ENQUIRY.VIEW) is already seeded by
    02-core-permissions.sql and is deliberately not repeated.  REQ: COM-002 */
MERGE core.Permission AS t
USING (VALUES
 ('PO.SOURCE.MAINT','PO','Maintain approved supplier sources for an item',0,0),
 ('PO.PRICE.MAINT','PO','Maintain contract, quote and list purchase prices',0,0),
 ('PO.TOLERANCE.MAINT','PO','Maintain receipt, price and charge tolerance rules',1,0),
 ('PO.INSPECT.DISPOSE','PO','Dispose of quarantined goods after inspection',0,0)
) AS s (PermissionCode, Module, Name, IsSensitive, IsApproval)
ON t.PermissionCode = s.PermissionCode
WHEN MATCHED AND EXISTS (SELECT s.Module, s.Name, s.IsSensitive, s.IsApproval EXCEPT SELECT t.Module, t.Name, t.IsSensitive, t.IsApproval)
    THEN UPDATE SET Module = s.Module, Name = s.Name, IsSensitive = s.IsSensitive, IsApproval = s.IsApproval, ModifiedBy = 'SEED', ModifiedAt = SYSDATETIME()
WHEN NOT MATCHED THEN INSERT (PermissionCode, Module, Name, IsSensitive, IsApproval, CreatedBy)
     VALUES (s.PermissionCode, s.Module, s.Name, s.IsSensitive, s.IsApproval, 'SEED');
GO
/*  Grant each new code to the roles that already hold its nearest existing equivalent, so that the
    security model stays one definition rather than two lists that drift apart.  A tolerance rule
    decides what passes without a human looking at it, so it is held more tightly than a price: it
    is granted only where the order approval authority already sits.  REQ: COM-002, D-08 */
INSERT core.RolePermission (RoleCode, PermissionCode, CreatedBy)
SELECT r.RoleCode, g.NewCode, 'SEED'
  FROM (VALUES ('PO.SOURCE.MAINT','PO.ORDER.ENTER'), ('PO.PRICE.MAINT','PO.ORDER.ENTER'),
               ('PO.TOLERANCE.MAINT','PO.ORDER.APPROVE.HIGH'), ('PO.INSPECT.DISPOSE','PO.RECEIPT.CONFIRM')) AS g (NewCode, LikeCode)
  JOIN core.RolePermission r ON r.PermissionCode = g.LikeCode
 WHERE NOT EXISTS (SELECT 1 FROM core.RolePermission x WHERE x.RoleCode = r.RoleCode AND x.PermissionCode = g.NewCode);
GO
/*  The Purchasing menu.  REQ: COM-024 */
IF OBJECT_ID(N'core.MenuItem') IS NOT NULL
BEGIN
    DECLARE @m table (MenuCode varchar(40), ParentMenuCode varchar(40), Caption varchar(60), FormClass varchar(80) NULL,
                      FormArgument varchar(60) NULL, PermissionCode varchar(60) NULL, SortOrder int, Shortcut varchar(20) NULL);
    INSERT @m VALUES
      ('PO.REQ.ENTRY',    'PO', '&Requisitions...',              'frmRequisition',    NULL,          'PO.REQ.ENTER',        10, 'F5'),
      ('PO.ORDER.ENTRY',  'PO', '&Purchase Orders...',           'frmPurchaseOrder',  NULL,          'PO.ORDER.ENTER',      20, 'F6'),
      ('PO.RECEIPT.ENTRY','PO', '&Goods Receipt and Inspection...','frmGoodsReceipt', NULL,          'PO.RECEIPT.CONFIRM',  30, 'F7'),
      ('PO.SOURCE.MAINT', 'PO', '&Supplier Sources...',          'frmGenericMaint',   'PO.SOURCE',   'PO.SOURCE.MAINT',     40, NULL),
      ('PO.PRICE.MAINT',  'PO', 'Purchase P&rices...',           'frmGenericMaint',   'PO.PRICE',    'PO.PRICE.MAINT',      50, NULL),
      ('PO.TOL.MAINT',    'PO', '&Tolerance Rules...',           'frmGenericMaint',   'PO.TOLERANCE','PO.TOLERANCE.MAINT',  60, NULL),
      ('PO.RPT',          'PO', 'Purchasing Repor&ts',           NULL,                NULL,          NULL,                  90, NULL),
      ('PO.RPT.PIPE',     'PO.RPT', 'Pipeline and &GRNI...',     'frmReportLauncher', 'POPIPE',      'PO.ENQUIRY.VIEW',     10, NULL),
      ('PO.RPT.OVERDUE',  'PO.RPT', '&Overdue Orders...',        'frmReportLauncher', 'OVERDUE',     'PO.ENQUIRY.VIEW',     20, NULL),
      ('PO.RPT.SUPPPERF', 'PO.RPT', 'Supplier &Performance...',  'frmReportLauncher', 'SUPPPERF',    'PO.ENQUIRY.VIEW',     30, NULL),
      ('PO.RPT.PODOC',    'PO.RPT', 'Print Purchase Order &Document...','frmReportLauncher','PODOC',  'PO.ORDER.RELEASE',    40, NULL),
      ('PO.RPT.REQGEN',   'PO.RPT', '&Requisition Generation...','frmReportLauncher', 'REQGEN',      'PO.REQ.ENTER',        50, NULL);

    MERGE core.MenuItem AS t USING @m AS s ON t.MenuCode = s.MenuCode
    WHEN MATCHED THEN UPDATE SET ParentMenuCode = s.ParentMenuCode, Caption = s.Caption, FormClass = s.FormClass,
            FormArgument = s.FormArgument, PermissionCode = s.PermissionCode, SortOrder = s.SortOrder, Shortcut = s.Shortcut, IsActive = 1
    WHEN NOT MATCHED BY TARGET THEN
        INSERT (MenuCode, ParentMenuCode, Caption, FormClass, FormArgument, PermissionCode, SortOrder, Shortcut, IsActive)
        VALUES (s.MenuCode, s.ParentMenuCode, s.Caption, s.FormClass, s.FormArgument, s.PermissionCode, s.SortOrder, s.Shortcut, 1);
    PRINT 'Seeded core.MenuItem (Purchasing)';
END
GO
/*  Batch reports.  ParamSpec is the prompt list the report launcher builds its dialogue from, and
    it must match the keywords the COBOL programs read through CMDARGS - CO, SITE, SUPP, BUYER,
    DAYS, ASAT, FROM, TO, ORDER, ITEM and MODE.  Anything else is silently ignored by the program,
    which is worse than refusing it, so the two lists are kept in step here.
    REQ: PO-001, PO-017, PO-020, PO-022, PO-023, PO-024, COM-019, RPT-009 */
MERGE core.ReportDef AS t
USING (VALUES
 ('POPIPE','PO','Purchasing pipeline and GRNI','POPIPE','CO,SITE,SUPP,ASAT','PO.ENQUIRY.VIEW','PO-017, PO-020, PO-022, RPT-009',
  'Every open order line with committed, accrued and invoiced shown as separate stages, never added together, and both the original and the current promised date.'),
 ('OVERDUE','PO','Overdue purchase orders and exceptions','OVERDUE','CO,SITE,SUPP,BUYER,DAYS,ASAT','PO.ENQUIRY.VIEW','PO-021, PO-022',
  'Late schedules by owning buyer, measured against both the original and the current promise, with the last chase note and the sales order or job that is waiting.'),
 ('SUPPPERF','PO','Supplier performance (OTIF, quality, price, lead time)','SUPPPERF','CO,SUPP,FROM,TO,MODE','PO.ENQUIRY.VIEW','PO-023, D-19',
  'On time, in full, OTIF, quality acceptance, price variance and lead time, on the D-19 original-promise basis and on the current-promise basis as separate rows. MODE=PREVIEW reports without storing.'),
 ('PODOC','PO','Purchase order document','PODOC','CO,ORDER','PO.ORDER.RELEASE','PO-024',
  'The approved version of one purchase order with its content hash and approver, plus the receipts, returns, import entries, landed cost and amendments that explain it. A reprint is marked as such.'),
 ('REQGEN','PO','Requisition generation (replenishment and shortage)','REQGEN','CO,SITE,ITEM,MODE','PO.REQ.ENTER','PO-001',
  'Proposes requisition lines where free stock plus goods on order has fallen to the re-order point, or reservations exceed stock. MODE=PREVIEW proposes only; MODE=COMMIT raises them through POREQ. No supplier commitment is created either way.'),
 ('PODEMO','PO','Purchasing demonstration data','PODEMO','CO,MODE','PO.ORDER.ENTER','PO-001, PO-029',
  'Builds the purchasing demonstration transactions for a company by driving POREQ, SOURCSEL, POORDER, POAPPRV, RECEIPT, INSPECT, SUPPRET, SVCACC, IMPVAT and POCLOSE. Every document carries the correlation id PODEMO-<CO>-<nnn>, so a second run builds nothing new.')
) AS s (ReportCode, Module, Title, ProgramName, ParamSpec, PermissionCode, RequirementRefs, Description)
ON t.ReportCode = s.ReportCode
WHEN MATCHED THEN UPDATE SET Module = s.Module, Title = s.Title, ProgramName = s.ProgramName, ParamSpec = s.ParamSpec,
        PermissionCode = s.PermissionCode, RequirementRefs = s.RequirementRefs, Description = s.Description,
        IsActive = CASE WHEN s.ReportCode = 'PODEMO' THEN 0 ELSE 1 END,
        ModifiedBy = 'SEED', ModifiedAt = SYSDATETIME()
WHEN NOT MATCHED THEN INSERT (ReportCode, Module, Title, ProgramName, ParamSpec, PermissionCode, RequirementRefs, Description, IsActive, CreatedBy)
     VALUES (s.ReportCode, s.Module, s.Title, s.ProgramName, s.ParamSpec, s.PermissionCode, s.RequirementRefs, s.Description,
             CASE WHEN s.ReportCode = 'PODEMO' THEN 0 ELSE 1 END, 'SEED');
GO
/*  Generic maintenance for the simple purchasing masters.  Requisitions, orders and receipts are
    far too rich for the generic shell and have their own hand written screens; approved sources,
    purchase prices and tolerance rules are flat reference data and are maintained generically.
    REQ: PO-002, PO-004, PO-026, D-08 */
IF OBJECT_ID(N'core.MaintDef') IS NOT NULL AND OBJECT_ID(N'core.MaintColumn') IS NOT NULL
BEGIN
    DECLARE @d table (MaintCode varchar(30), Title varchar(80), ModuleCode varchar(10), SchemaName sysname, TableName sysname,
                      CompanyScoped bit, RetireColumn sysname NULL, PermissionCode varchar(60) NULL, HelpText varchar(400) NULL);
    INSERT @d VALUES
      ('PO.SOURCE',   'Approved supplier sources','po', 'po', 'SupplierSource', 1, 'IsActive', 'PO.SOURCE.MAINT',
       'Which suppliers may be used for an item, in what priority, on what lead time and pack size. An UNAPPROVED or SUSPENDED source may still be chosen, but only by a named authoriser (PO-002).'),
      ('PO.PRICE',    'Purchase prices',          'po', 'po', 'PurchasePrice',  1, 'IsActive', 'PO.PRICE.MAINT',
       'Contract, quote and list prices with their effective dates. The price that applies is the one effective on the order date, not the one in force today (PO-004).'),
      ('PO.TOLERANCE','Receipt and price tolerances','po','po','ToleranceRule', 1, 'IsActive', 'PO.TOLERANCE.MAINT',
       'What a receipt or an invoice may differ by before a human has to look at it. The company row is the baseline; a supplier or item row overrides it (D-08).');

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
      ('PO.SOURCE','SupplierSourceId','Source',             'INT',  10, 70,1,0,1,1,0,0,NULL,NULL),
      ('PO.SOURCE','CompanyCode','Company',                 'CODE', 20, 80,0,1,0,1,1,1,'core.usp_UiLookup','CORE.COMPANY'),
      ('PO.SOURCE','ItemCode','Item',                       'CODE', 30,140,0,1,0,1,1,1,NULL,NULL),
      ('PO.SOURCE','SupplierCode','Supplier',               'CODE', 40,100,0,1,0,1,1,1,NULL,NULL),
      ('PO.SOURCE','SupplierPartNumber','Supplier part',    'TEXT', 50,140,0,0,0,1,1,1,NULL,NULL),
      ('PO.SOURCE','Priority','Priority',                   'INT',  60, 70,0,1,0,1,1,0,NULL,NULL),
      ('PO.SOURCE','ApprovalStatus','Approval',             'CODE', 70,100,0,1,0,1,1,1,NULL,NULL),
      ('PO.SOURCE','ApprovalEvidence','Evidence',           'TEXT', 80,200,0,0,0,0,1,0,NULL,NULL),
      ('PO.SOURCE','ContractRef','Contract',                'TEXT', 90,120,0,0,0,1,1,1,NULL,NULL),
      ('PO.SOURCE','LeadTimeDays','Lead time (days)',       'INT', 100, 90,0,1,0,1,1,0,NULL,NULL),
      ('PO.SOURCE','MinOrderQty','Minimum order',           'QTY', 110,100,0,0,0,1,1,0,NULL,NULL),
      ('PO.SOURCE','PackQty','Pack quantity',               'QTY', 120,100,0,0,0,1,1,0,NULL,NULL),
      ('PO.SOURCE','OrderUomCode','Order UOM',              'CODE',130, 80,0,0,0,1,1,0,NULL,NULL),
      ('PO.SOURCE','CurrencyCode','Currency',               'CODE',140, 70,0,1,0,1,1,0,NULL,NULL),
      ('PO.SOURCE','OverDeliveryTolerancePct','Over-delivery %','PCT',150, 90,0,0,0,0,1,0,NULL,NULL),
      ('PO.SOURCE','CommercialRestriction','Restriction',   'TEXT',160,220,0,0,0,0,1,0,NULL,NULL),
      ('PO.SOURCE','EffectiveFrom','Effective from',        'DATE',170,100,0,1,0,1,1,0,NULL,NULL),
      ('PO.SOURCE','EffectiveTo','Effective to',            'DATE',180,100,0,0,0,1,1,0,NULL,NULL),
      ('PO.SOURCE','IsActive','Active',                     'BOOL',190, 60,0,0,0,1,1,0,NULL,NULL),
      ('PO.PRICE','PurchasePriceId','Price',                'INT',  10, 70,1,0,1,1,0,0,NULL,NULL),
      ('PO.PRICE','CompanyCode','Company',                  'CODE', 20, 80,0,1,0,1,1,1,'core.usp_UiLookup','CORE.COMPANY'),
      ('PO.PRICE','SupplierCode','Supplier',                'CODE', 30,100,0,1,0,1,1,1,NULL,NULL),
      ('PO.PRICE','ItemCode','Item',                        'CODE', 40,140,0,0,0,1,1,1,NULL,NULL),
      ('PO.PRICE','ItemGroupCode','Item group',             'CODE', 50,100,0,0,0,1,1,1,NULL,NULL),
      ('PO.PRICE','PriceBasis','Basis',                     'CODE', 60, 90,0,1,0,1,1,1,NULL,NULL),
      ('PO.PRICE','AgreementRef','Agreement',               'TEXT', 70,120,0,0,0,1,1,1,NULL,NULL),
      ('PO.PRICE','CurrencyCode','Currency',                'CODE', 80, 70,0,1,0,1,1,0,NULL,NULL),
      ('PO.PRICE','UomCode','Price UOM',                    'CODE', 90, 80,0,0,0,1,1,0,NULL,NULL),
      ('PO.PRICE','UnitPrice','Unit price',                 'MONEY',100,110,0,1,0,1,1,0,NULL,NULL),
      ('PO.PRICE','PricePerQty','Price per',                'QTY', 110, 90,0,1,0,1,1,0,NULL,NULL),
      ('PO.PRICE','DiscountPct','Discount %',               'PCT', 120, 80,0,0,0,1,1,0,NULL,NULL),
      ('PO.PRICE','SettlementDiscountPct','Settlement %',   'PCT', 130, 90,0,0,0,0,1,0,NULL,NULL),
      ('PO.PRICE','EffectiveFrom','Effective from',         'DATE',140,100,0,1,0,1,1,0,NULL,NULL),
      ('PO.PRICE','EffectiveTo','Effective to',             'DATE',150,100,0,0,0,1,1,0,NULL,NULL),
      ('PO.PRICE','Priority','Priority',                    'INT', 160, 70,0,1,0,1,1,0,NULL,NULL),
      ('PO.PRICE','IsActive','Active',                      'BOOL',170, 60,0,0,0,1,1,0,NULL,NULL),
      ('PO.TOLERANCE','ToleranceRuleId','Rule',             'INT',  10, 70,1,0,1,1,0,0,NULL,NULL),
      ('PO.TOLERANCE','CompanyCode','Company',              'CODE', 20, 80,0,1,0,1,1,1,'core.usp_UiLookup','CORE.COMPANY'),
      ('PO.TOLERANCE','ScopeType','Scope',                  'CODE', 30, 90,0,1,0,1,1,1,NULL,NULL),
      ('PO.TOLERANCE','ScopeCode','Scope value',            'TEXT', 40,140,0,1,0,1,1,1,NULL,NULL),
      ('PO.TOLERANCE','OverDeliveryPct','Over delivery %',  'PCT',  50, 90,0,0,0,1,1,0,NULL,NULL),
      ('PO.TOLERANCE','OverDeliveryQty','Over delivery qty','QTY',  60,100,0,0,0,0,1,0,NULL,NULL),
      ('PO.TOLERANCE','UnderDeliveryPct','Under delivery %','PCT',  70, 90,0,0,0,0,1,0,NULL,NULL),
      ('PO.TOLERANCE','PriceVariancePct','Price variance %','PCT',  80, 90,0,0,0,1,1,0,NULL,NULL),
      ('PO.TOLERANCE','PriceVarianceAbs','Price variance',  'MONEY',90,100,0,0,0,1,1,0,NULL,NULL),
      ('PO.TOLERANCE','ChargeVarianceAbs','Charge variance','MONEY',100,100,0,0,0,1,1,0,NULL,NULL),
      ('PO.TOLERANCE','EarlyDeliveryDays','Early days',     'INT', 110, 80,0,0,0,1,1,0,NULL,NULL),
      ('PO.TOLERANCE','LateDeliveryDays','Late days',       'INT', 120, 80,0,0,0,1,1,0,NULL,NULL),
      ('PO.TOLERANCE','ActionOverDelivery','Over delivery action','CODE',130,90,0,1,0,1,1,1,NULL,NULL),
      ('PO.TOLERANCE','ApproverRole','Approver role',       'CODE',140, 90,0,0,0,1,1,1,NULL,NULL),
      ('PO.TOLERANCE','IsActive','Active',                  'BOOL',150, 60,0,0,0,1,1,0,NULL,NULL);

    MERGE core.MaintColumn AS t USING @c AS s ON t.MaintCode = s.MaintCode AND t.ColumnName = s.ColumnName
    WHEN MATCHED THEN UPDATE SET Caption = s.Caption, DataType = s.DataType, SortOrder = s.SortOrder, Width = s.Width,
            IsKey = s.IsKey, IsRequired = s.IsRequired, IsReadOnly = s.IsReadOnly, IsInList = s.IsInList, IsInEdit = s.IsInEdit,
            IsSearchable = s.IsSearchable, LookupProc = s.LookupProc, LookupArg = s.LookupArg
    WHEN NOT MATCHED BY TARGET THEN
        INSERT (MaintCode, ColumnName, Caption, DataType, SortOrder, Width, IsKey, IsRequired, IsReadOnly, IsInList, IsInEdit,
                IsSearchable, LookupProc, LookupArg)
        VALUES (s.MaintCode, s.ColumnName, s.Caption, s.DataType, s.SortOrder, s.Width, s.IsKey, s.IsRequired, s.IsReadOnly,
                s.IsInList, s.IsInEdit, s.IsSearchable, s.LookupProc, s.LookupArg);
    PRINT 'Seeded core.MaintDef / core.MaintColumn (Purchasing)';
END
GO
/*  A report definition that names a program nobody has built is a broken menu entry waiting to be
    found by a user.  Fail the deploy instead.  REQ: COM-019 */
IF EXISTS (SELECT 1 FROM core.ReportDef WHERE Module = 'PO' AND ProgramName NOT IN
           ('POPIPE','OVERDUE','SUPPPERF','PODOC','REQGEN','PODEMO'))
    THROW 50810, 'A purchasing core.ReportDef names a program that is not one of the six built purchasing batch programs.', 1;
GO

/*  ---------------------------------------------------------------------------------------------
    Number series for the purchasing documents.  POREQ, POORDER and SOURCSEL ask NUMGEN for the
    series codes POREQ, POORD and POSRC; the foundation seed only carries the three-letter
    document series (PREQ, PORD), so the module seeds its own.  Idempotent.  REQ: PO-001, PO-003
    --------------------------------------------------------------------------------------------- */
INSERT INTO core.NumberSeries (CompanyCode, SeriesCode, DocumentType, Description, Prefix,
                               NumberLength, NextNumber, MinNumber, MaxNumber, IsLegalSequence,
                               IsActive, CreatedBy)
SELECT c.CompanyCode, s.SeriesCode, s.DocumentType, s.Description, s.Prefix,
       8, 1, 1, 99999999, 0, 1, 'SEED'
FROM   core.Company c
CROSS JOIN (VALUES ('POREQ', 'REQUISITION', 'Purchase requisition', 'PR'),
                   ('POORD', 'PURCHASEORDER', 'Purchase order', 'PO'),
                   ('PORCP', 'GOODSRECEIPT', 'Goods received note', 'GRN'),
                   ('PORTN', 'SUPPLIERRETURN', 'Supplier return note', 'SRN'),
                   ('POSRC', 'SOURCINGEVENT', 'Sourcing event', 'SE'))
           AS s(SeriesCode, DocumentType, Description, Prefix)
WHERE NOT EXISTS (SELECT 1 FROM core.NumberSeries n
                  WHERE n.CompanyCode = c.CompanyCode AND n.SeriesCode = s.SeriesCode);
PRINT 'Seeded core.NumberSeries for purchasing (POREQ/POORD/POSRC)';
GO
