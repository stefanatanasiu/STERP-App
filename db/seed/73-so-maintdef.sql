/*  Generic maintenance for the sales masters that are flat enough for the framework shell:
    price lists and their quantity break lines, and rebate agreements.  Everything that needs a
    decision - which price applies, what the discount sequence does, whether a rebate accrues -
    stays in COBOL; these screens only maintain the data those programs read.  A price list line
    is never edited in place once it has been used: the effectivity dates are the audit trail,
    so a new price is a new line with a new ValidFrom (SO-004).
    Re-runnable (MERGE / IF NOT EXISTS).  REQ: SO-004, SO-005, SO-027, COM-002, COM-024 */
SET NOCOUNT ON;
GO
MERGE core.Permission AS t
USING (VALUES
 ('SO.PRICELIST.MAINTAIN','SO','Maintain sales price lists and quantity breaks',0,0),
 ('SO.REBATE.MAINTAIN','SO','Maintain customer rebate agreements',0,0)
) AS s (PermissionCode, Module, Name, IsSensitive, IsApproval)
ON t.PermissionCode = s.PermissionCode
WHEN MATCHED AND EXISTS (SELECT s.Module, s.Name, s.IsSensitive, s.IsApproval EXCEPT SELECT t.Module, t.Name, t.IsSensitive, t.IsApproval)
    THEN UPDATE SET Module = s.Module, Name = s.Name, IsSensitive = s.IsSensitive, IsApproval = s.IsApproval,
                    ModifiedBy = 'SEED', ModifiedAt = SYSDATETIME()
WHEN NOT MATCHED THEN INSERT (PermissionCode, Module, Name, IsSensitive, IsApproval, CreatedBy)
     VALUES (s.PermissionCode, s.Module, s.Name, s.IsSensitive, s.IsApproval, 'SEED');
GO
/*  Prices and rebates are commercial policy, so the sales manager owns them; a sales
    representative may read them through the price enquiry but not change them.  REQ: COM-002 */
INSERT core.RolePermission (RoleCode, PermissionCode, CreatedBy)
SELECT v.RoleCode, v.PermissionCode, 'SEED'
  FROM (VALUES ('SALESMGR','SO.PRICELIST.MAINTAIN'), ('SALESMGR','SO.REBATE.MAINTAIN'),
               ('FINCTRL','SO.REBATE.MAINTAIN')) AS v (RoleCode, PermissionCode)
 WHERE EXISTS (SELECT 1 FROM core.Role r WHERE r.RoleCode = v.RoleCode)
   AND EXISTS (SELECT 1 FROM core.Permission p WHERE p.PermissionCode = v.PermissionCode)
   AND NOT EXISTS (SELECT 1 FROM core.RolePermission x WHERE x.RoleCode = v.RoleCode AND x.PermissionCode = v.PermissionCode);
GO
IF OBJECT_ID(N'core.MaintDef') IS NOT NULL AND OBJECT_ID(N'core.MaintColumn') IS NOT NULL
BEGIN
    MERGE core.MaintDef AS t
    USING (VALUES
     ('SO.PRICELIST','Sales price lists','so','so','PriceList',1,'IsActive','SO.PRICELIST.MAINTAIN',
      'Price list headers. The level and scope decide precedence: 2 contract, 3 customer, 4 group, 5 standard. Retiring a list clears IsActive; it is never deleted, because orders priced from it keep its code.'),
     ('SO.PRICELINE','Sales price list lines and breaks','so','so','PriceListLine',1,NULL,'SO.PRICELIST.MAINTAIN',
      'Price per item with quantity breaks. Break 0 is the base price; a break applies from that quantity upwards. A price change is a new line with a new valid-from date, not an edit of the old one.'),
     ('SO.REBATE','Customer rebate agreements','so','so','RebateAgreement',1,NULL,'SO.REBATE.MAINTAIN',
      'Retrospective volume rebates. The accrual is a reduction of revenue, raised by the REBATE batch program; this screen only holds the agreement.')
    ) AS s (MaintCode, Title, ModuleCode, SchemaName, TableName, CompanyScoped, RetireColumn, PermissionCode, HelpText)
    ON t.MaintCode = s.MaintCode
    WHEN MATCHED THEN UPDATE SET Title = s.Title, ModuleCode = s.ModuleCode, SchemaName = s.SchemaName,
            TableName = s.TableName, CompanyScoped = s.CompanyScoped, RetireColumn = s.RetireColumn,
            PermissionCode = s.PermissionCode, HelpText = s.HelpText, IsActive = 1,
            ModifiedBy = 'SEED', ModifiedAt = SYSDATETIME()
    WHEN NOT MATCHED BY TARGET THEN
        INSERT (MaintCode, Title, ModuleCode, SchemaName, TableName, CompanyScoped, RetireColumn,
                PermissionCode, HelpText, CreatedBy)
        VALUES (s.MaintCode, s.Title, s.ModuleCode, s.SchemaName, s.TableName, s.CompanyScoped,
                s.RetireColumn, s.PermissionCode, s.HelpText, 'SEED');

    DECLARE @c table (MaintCode varchar(30), ColumnName sysname, Caption varchar(60), DataType varchar(10), SortOrder int,
                      Width int, IsKey bit, IsRequired bit, IsReadOnly bit, IsInList bit, IsInEdit bit, IsSearchable bit,
                      LookupProc varchar(128) NULL, LookupArg varchar(60) NULL);
    INSERT @c VALUES
      ('SO.PRICELIST','CompanyCode','Company',           'CODE', 10, 80,1,1,0,1,1,1,'core.usp_UiLookup','CORE.COMPANY'),
      ('SO.PRICELIST','PriceListCode','Price list',      'CODE', 20,110,1,1,0,1,1,1,NULL,NULL),
      ('SO.PRICELIST','Name','Name',                     'TEXT', 30,240,0,1,0,1,1,1,NULL,NULL),
      ('SO.PRICELIST','PriceLevel','Level',              'INT',  40, 60,0,1,0,1,1,0,NULL,NULL),
      ('SO.PRICELIST','Scope','Scope',                   'CODE', 50,100,0,1,0,1,1,1,NULL,NULL),
      ('SO.PRICELIST','CurrencyCode','Currency',         'CODE', 60, 70,0,1,0,1,1,1,'core.usp_UiLookup','CORE.CURRENCY'),
      ('SO.PRICELIST','CustomerCode','Customer',         'CODE', 70,100,0,0,0,1,1,1,NULL,NULL),
      ('SO.PRICELIST','PriceGroup','Price group',        'CODE', 80,100,0,0,0,1,1,1,NULL,NULL),
      ('SO.PRICELIST','ContractRef','Contract',          'CODE', 90,140,0,0,0,0,1,1,NULL,NULL),
      ('SO.PRICELIST','ValidFrom','Valid from',          'DATE',100,100,0,1,0,1,1,0,NULL,NULL),
      ('SO.PRICELIST','ValidTo','Valid to',              'DATE',110,100,0,0,0,1,1,0,NULL,NULL),
      ('SO.PRICELIST','IsActive','Active',               'BOOL',120, 60,0,0,0,1,1,0,NULL,NULL),
      ('SO.PRICELINE','PriceLineId','Line',              'INT',  10, 70,1,0,1,1,0,0,NULL,NULL),
      ('SO.PRICELINE','CompanyCode','Company',           'CODE', 20, 80,0,1,0,1,1,1,'core.usp_UiLookup','CORE.COMPANY'),
      ('SO.PRICELINE','PriceListCode','Price list',      'CODE', 30,110,0,1,0,1,1,1,NULL,NULL),
      ('SO.PRICELINE','ItemCode','Item',                 'CODE', 40,160,0,1,0,1,1,1,'core.usp_UiLookup','ITM.ITEM'),
      ('SO.PRICELINE','UomCode','Unit',                  'CODE', 50, 60,0,0,0,1,1,0,NULL,NULL),
      ('SO.PRICELINE','QtyBreak','Break from',           'QTY',  60, 90,0,1,0,1,1,0,NULL,NULL),
      ('SO.PRICELINE','CurrencyCode','Currency',         'CODE', 70, 70,0,1,0,1,1,1,'core.usp_UiLookup','CORE.CURRENCY'),
      ('SO.PRICELINE','UnitPrice','Unit price',          'DEC',  80,100,0,1,0,1,1,0,NULL,NULL),
      ('SO.PRICELINE','PricePerQty','Price per',         'QTY',  90, 80,0,1,0,1,1,0,NULL,NULL),
      ('SO.PRICELINE','DiscountPct','List discount %',   'PCT', 100, 90,0,0,0,1,1,0,NULL,NULL),
      ('SO.PRICELINE','MinimumMarginPct','Min margin %', 'PCT', 110, 90,0,0,0,1,1,0,NULL,NULL),
      ('SO.PRICELINE','ValidFrom','Valid from',          'DATE',120,100,0,1,0,1,1,0,NULL,NULL),
      ('SO.PRICELINE','ValidTo','Valid to',              'DATE',130,100,0,0,0,1,1,0,NULL,NULL),
      ('SO.REBATE','RebateId','Rebate',                  'INT',  10, 70,1,0,1,1,0,0,NULL,NULL),
      ('SO.REBATE','CompanyCode','Company',              'CODE', 20, 80,0,1,0,1,1,1,'core.usp_UiLookup','CORE.COMPANY'),
      ('SO.REBATE','RebateRef','Agreement',              'CODE', 30,130,0,1,0,1,1,1,NULL,NULL),
      ('SO.REBATE','CustomerCode','Customer',            'CODE', 40,100,0,1,0,1,1,1,NULL,NULL),
      ('SO.REBATE','Description','Description',          'TEXT', 50,260,0,0,0,1,1,1,NULL,NULL),
      ('SO.REBATE','ItemCode','Item',                    'CODE', 60,140,0,0,0,0,1,1,'core.usp_UiLookup','ITM.ITEM'),
      ('SO.REBATE','ProductGroup','Product group',       'CODE', 70,110,0,0,0,1,1,1,NULL,NULL),
      ('SO.REBATE','Basis','Basis',                      'CODE', 80, 90,0,1,0,1,1,1,NULL,NULL),
      ('SO.REBATE','ThresholdValue','Threshold',         'DEC',  90,110,0,0,0,1,1,0,NULL,NULL),
      ('SO.REBATE','RebatePct','Rebate %',               'PCT', 100, 80,0,0,0,1,1,0,NULL,NULL),
      ('SO.REBATE','RebatePerUnit','Per unit',           'DEC', 110, 90,0,0,0,1,1,0,NULL,NULL),
      ('SO.REBATE','PeriodStart','Period from',          'DATE',120,100,0,1,0,1,1,0,NULL,NULL),
      ('SO.REBATE','PeriodEnd','Period to',              'DATE',130,100,0,1,0,1,1,0,NULL,NULL),
      ('SO.REBATE','CurrencyCode','Currency',            'CODE',140, 70,0,1,0,0,1,1,'core.usp_UiLookup','CORE.CURRENCY'),
      ('SO.REBATE','Status','Status',                    'CODE',150, 90,0,1,0,1,1,1,NULL,NULL),
      ('SO.REBATE','ApprovedBy','Approved by',           'CODE',160, 90,0,0,0,0,1,1,'core.usp_UiLookup','CORE.USER'),
      ('SO.REBATE','SettlementCreditNote','Credit note', 'CODE',170,120,0,0,1,1,1,1,NULL,NULL);

    MERGE core.MaintColumn AS t USING @c AS s ON t.MaintCode = s.MaintCode AND t.ColumnName = s.ColumnName
    WHEN MATCHED THEN UPDATE SET Caption = s.Caption, DataType = s.DataType, SortOrder = s.SortOrder, Width = s.Width,
            IsKey = s.IsKey, IsRequired = s.IsRequired, IsReadOnly = s.IsReadOnly, IsInList = s.IsInList,
            IsInEdit = s.IsInEdit, IsSearchable = s.IsSearchable, LookupProc = s.LookupProc, LookupArg = s.LookupArg
    WHEN NOT MATCHED BY TARGET THEN
        INSERT (MaintCode, ColumnName, Caption, DataType, SortOrder, Width, IsKey, IsRequired, IsReadOnly,
                IsInList, IsInEdit, IsSearchable, LookupProc, LookupArg)
        VALUES (s.MaintCode, s.ColumnName, s.Caption, s.DataType, s.SortOrder, s.Width, s.IsKey, s.IsRequired,
                s.IsReadOnly, s.IsInList, s.IsInEdit, s.IsSearchable, s.LookupProc, s.LookupArg);
    PRINT 'Seeded core.MaintDef / core.MaintColumn (Sales prices and rebates)';
END
GO
/*  Menu entries for the two maintenance screens, under the existing Sales bar.  REQ: COM-024 */
IF OBJECT_ID(N'core.MenuItem') IS NOT NULL
BEGIN
    DECLARE @m table (MenuCode varchar(40), ParentMenuCode varchar(40), Caption varchar(60), FormClass varchar(80) NULL,
                      FormArgument varchar(60) NULL, PermissionCode varchar(60) NULL, SortOrder int, Shortcut varchar(20) NULL);
    INSERT @m VALUES
      ('SO.PRICE.MAINT',    'SO', '&Price Lists...',            'frmGenericMaint', 'SO.PRICELIST', 'SO.PRICELIST.MAINTAIN', 50, NULL),
      ('SO.PRICELINE.MAINT','SO', 'Price List &Breaks...',      'frmGenericMaint', 'SO.PRICELINE', 'SO.PRICELIST.MAINTAIN', 60, NULL),
      ('SO.REBATE.MAINT',   'SO', 'Re&bate Agreements...',      'frmGenericMaint', 'SO.REBATE',    'SO.REBATE.MAINTAIN',    70, NULL);

    MERGE core.MenuItem AS t USING @m AS s ON t.MenuCode = s.MenuCode
    WHEN MATCHED THEN UPDATE SET ParentMenuCode = s.ParentMenuCode, Caption = s.Caption, FormClass = s.FormClass,
            FormArgument = s.FormArgument, PermissionCode = s.PermissionCode, SortOrder = s.SortOrder,
            Shortcut = s.Shortcut, IsActive = 1
    WHEN NOT MATCHED BY TARGET THEN
        INSERT (MenuCode, ParentMenuCode, Caption, FormClass, FormArgument, PermissionCode, SortOrder, Shortcut, IsActive)
        VALUES (s.MenuCode, s.ParentMenuCode, s.Caption, s.FormClass, s.FormArgument, s.PermissionCode, s.SortOrder, s.Shortcut, 1);
    PRINT 'Seeded core.MenuItem (Sales price and rebate maintenance)';
END
GO
