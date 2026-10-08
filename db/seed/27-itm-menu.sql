/* ---------------------------------------------------------------------------
   27-itm-menu - the Stock Control (ITM) drop-down of the WinForms client, the
   generic file maintenance definitions for the simple inventory masters, and
   the batch report catalogue for the module.  The four hand written screens are
   frmItemMaster, frmStockEnquiry, frmStockMovement and frmStockCount; every
   other master is maintained by frmGenericMaint from the metadata below, so the
   module adds almost no .NET code.                         Re-runnable (MERGE).
   Top level SortOrder for ITM is 80 (07-core-numbering-terms.sql) - never
   re-numbered here.                       REQ: ITM-024, COM-013, COM-019, D-21
   --------------------------------------------------------------------------- */
SET NOCOUNT ON;
GO
IF OBJECT_ID(N'core.MenuItem') IS NOT NULL
BEGIN
    DECLARE @m table (MenuCode varchar(40), ParentMenuCode varchar(40) NULL, Caption varchar(60),
                      FormClass varchar(80) NULL, FormArgument varchar(60) NULL,
                      PermissionCode varchar(60) NULL, SortOrder int, Shortcut varchar(20) NULL);
    INSERT @m VALUES
      ('ITM', NULL, 'Stoc&k Control', NULL, NULL, NULL, 80, NULL),
      -- the four hand written screens ------------------------------------------
      ('ITM.ITEM',      'ITM', '&Item and Part Maintenance...', 'frmItemMaster',    NULL, 'ITM.ITEM.VIEW',   10, 'Ctrl+I'),
      ('ITM.STOCKENQ',  'ITM', 'Stock &Enquiry...',             'frmStockEnquiry',  NULL, 'ITM.STOCK.VIEW',  20, 'Ctrl+K'),
      ('ITM.STOCKMOVE', 'ITM', 'Stock &Movement Entry...',      'frmStockMovement', NULL, 'ITM.STOCK.ADJUST',30, NULL),
      ('ITM.STOCKCOUNT','ITM', 'Stock &Count and Adjustment...','frmStockCount',    NULL, 'ITM.COUNT.ENTER', 40, NULL),
      -- generic file maintenance ------------------------------------------------
      ('ITM.UOM',       'ITM', '&Units of Measure...',          'frmGenericMaint', 'ITM.UOM',       'ITM.UOM.MAINT',     100, NULL),
      ('ITM.ITEMUOM',   'ITM', 'Item Unit Con&versions...',     'frmGenericMaint', 'ITM.ITEMUOM',   'ITM.UOM.MAINT',     110, NULL),
      ('ITM.ITEMREV',   'ITM', 'Item &Revisions...',            'frmGenericMaint', 'ITM.ITEMREV',   'ITM.ITEM.MAINT',    120, NULL),
      ('ITM.ITEMSITE',  'ITM', 'Item &Site Planning Data...',   'frmGenericMaint', 'ITM.ITEMSITE',  'ITM.ITEM.MAINT',    130, NULL),
      ('ITM.PARTNER',   'ITM', '&Partner Part Numbers...',      'frmGenericMaint', 'ITM.PARTNER',   'ITM.ITEM.MAINT',    140, NULL),
      ('ITM.SUBST',     'ITM', 'Approved Su&bstitutes...',      'frmGenericMaint', 'ITM.SUBST',     'ITM.ITEM.MAINT',    150, NULL),
      ('ITM.WAREHOUSE', 'ITM', '&Warehouses...',                'frmGenericMaint', 'ITM.WAREHOUSE', 'ITM.LOCATION.MAINT',160, NULL),
      ('ITM.LOCATION',  'ITM', 'Stock &Locations...',           'frmGenericMaint', 'ITM.LOCATION',  'ITM.LOCATION.MAINT',170, NULL),
      ('ITM.LOT',       'ITM', 'Lo&ts...',                      'frmGenericMaint', 'ITM.LOT',       'ITM.LOT.MAINT',     180, NULL),
      ('ITM.SERIAL',    'ITM', 'Serial &Numbers...',            'frmGenericMaint', 'ITM.SERIAL',    'ITM.LOT.MAINT',     190, NULL),
      ('ITM.VATCLASS',  'ITM', 'Item V&AT Liability...',        'frmGenericMaint', 'ITM.VATCLASS',  'ITM.ITEM.MAINT',    200, NULL),
      ('ITM.CUSTOMS',   'ITM', 'Commodit&y Codes and Origin...','frmGenericMaint', 'ITM.CUSTOMS',   'ITM.CUSTOMS.MAINT', 210, NULL),
      ('ITM.PACKAGING', 'ITM', 'Packa&ging Composition...',     'frmGenericMaint', 'ITM.PACKAGING', 'ITM.CUSTOMS.MAINT', 220, NULL),
      ('ITM.COUNTLINE', 'ITM', 'Count Lin&e Entry...',          'frmGenericMaint', 'ITM.COUNTLINE', 'ITM.COUNT.ENTER',   230, NULL),
      -- batch reports (run through the standard report launcher) -----------------
      ('ITM.REPORTS',   'ITM', 'Stock Re&ports...',             'frmReportLauncher', 'itm',         'RPT.RUN',           300, NULL);

    MERGE core.MenuItem AS t USING @m AS s ON t.MenuCode = s.MenuCode
    WHEN MATCHED THEN UPDATE SET ParentMenuCode = s.ParentMenuCode, Caption = s.Caption,
            FormClass = s.FormClass, FormArgument = s.FormArgument, PermissionCode = s.PermissionCode,
            SortOrder = s.SortOrder, Shortcut = s.Shortcut, IsActive = 1
    WHEN NOT MATCHED BY TARGET THEN
        INSERT (MenuCode, ParentMenuCode, Caption, FormClass, FormArgument, PermissionCode, SortOrder, Shortcut, IsActive)
        VALUES (s.MenuCode, s.ParentMenuCode, s.Caption, s.FormClass, s.FormArgument, s.PermissionCode, s.SortOrder, s.Shortcut, 1);

    PRINT 'Seeded core.MenuItem (Stock Control menu)';
END
GO
/*  Batch reports.  ProgramName is the executable in bin\x86 without .exe; the launcher draws
    one box per ParamSpec key and pre-fills the ones it knows (CO, SITE, USER, ASAT, FROM, TO,
    PERIOD, MODE).  RPT-019 keeps the report code the reporting catalogue reserved for it. */
IF OBJECT_ID(N'core.ReportDef') IS NOT NULL
BEGIN
    DECLARE @r table (ReportCode varchar(20), Module varchar(6), Title varchar(100), ProgramName varchar(8),
                      ParamSpec varchar(100), PermissionCode varchar(40), RequirementRefs varchar(200),
                      Description varchar(300));
    INSERT @r VALUES
      ('ITM.STKSTAT',  'itm', 'Stock status by item and location', 'STKSTAT',  'CO,SITE,ITEM,ASAT,MODE', 'ITM.STOCK.VIEW',
       'ITM-012,ITM-013,ITM-023', 'Physical, eligible, reserved and free stock by site, location, lot and quality condition, with third party owned and in transit quantities shown separately.'),
      ('ITM.STKVALF',  'itm', 'Stock valuation',                   'STKVALF',  'CO,SITE,ASAT,MODE',      'ITM.STOCK.VIEW',
       'ITM-012,FIN-022,CST-012', 'Carrying value of stock at a date on the item cost method, reconciled to the stock control accounts.'),
      ('ITM.LOTTRACE', 'itm', 'Lot and serial traceability',       'LOTTRACE', 'CO,ITEM,LOT,FROM,TO,MODE','ITM.LOT.MAINT',
       'ITM-011,ITM-016', 'Forward and backward traceability for a lot or serial number: receipts, issues, transfers, quality holds and despatches.'),
      ('ITM.COUNTADJ', 'itm', 'Stock count sheet and adjustment',  'COUNTADJ', 'CO,SITE,COUNT,USER,MODE','ITM.COUNT.ENTER',
       'ITM-018,FIN-022', 'PREVIEW prints the count sheet and the variance list; COMMIT posts the approved adjustment movements. Committing needs ITM.COUNT.APPROVE.'),
      ('RPT-019',      'itm', 'Compliance and packaging report',   'PKGRPT',   'CO,FROM,TO,MODE',        'ITM.CUSTOMS.MAINT',
       'ITM-025,ITM-026,ITM-027,RPT-019,D-20', 'Packaging composition despatched in the period for extended producer responsibility and plastic packaging tax, with UKCA, REACH, WEEE, battery and dangerous goods data.'),
      ('ITM.OPENSTK',  'itm', 'Opening stock load',                'OPENSTK',  'CO,SITE,ASAT,MODE',      'ITM.STOCK.ADJUST',
       'ITM-012,FIN-021,XMD-012', 'Loads cut over opening stock as OPENING movements so that every balance has a movement behind it. PREVIEW validates the file, COMMIT posts it.');

    MERGE core.ReportDef AS t USING @r AS s ON t.ReportCode = s.ReportCode
    WHEN MATCHED THEN UPDATE SET Module = s.Module, Title = s.Title, ProgramName = s.ProgramName,
            ParamSpec = s.ParamSpec, PermissionCode = s.PermissionCode, RequirementRefs = s.RequirementRefs,
            Description = s.Description, IsActive = 1, ModifiedBy = 'SEED', ModifiedAt = SYSDATETIME()
    WHEN NOT MATCHED BY TARGET THEN
        INSERT (ReportCode, Module, Title, ProgramName, ParamSpec, PermissionCode, RequirementRefs,
                Description, IsActive, CreatedBy)
        VALUES (s.ReportCode, s.Module, s.Title, s.ProgramName, s.ParamSpec, s.PermissionCode,
                s.RequirementRefs, s.Description, 1, 'SEED');

    PRINT 'Seeded core.ReportDef (ITM batch reports)';
END
GO
/*  Generic file maintenance.  Every simple inventory master is a list + edit screen built from
    this metadata by frmGenericMaint, with no hand written form.  Retiring clears the in-use
    flag; core.usp_UiMaintSave never deletes a row (COM-013). */
IF OBJECT_ID(N'core.MaintDef') IS NOT NULL AND OBJECT_ID(N'core.MaintColumn') IS NOT NULL
BEGIN
    DECLARE @d table (MaintCode varchar(30), Title varchar(80), ModuleCode varchar(10), SchemaName sysname,
                      TableName sysname, CompanyScoped bit, RetireColumn sysname NULL,
                      PermissionCode varchar(60) NULL, HelpText varchar(400) NULL);
    INSERT @d VALUES
      ('ITM.ITEM',      'Items and parts',          'itm', 'inv', 'Item',           1, NULL,       'ITM.ITEM.MAINT',     'The governed catalogue identity. Balances, costs and demand are derived from transactions and are never edited here. An item is blocked, made obsolete or retired, never deleted.'),
      ('ITM.UOM',       'Units of measure',         'itm', 'inv', 'Uom',            0, 'IsActive', 'ITM.UOM.MAINT',      'Units and their factor to the base unit of the dimension. A missing or zero factor is invalid, never an assumed one for one.'),
      ('ITM.ITEMUOM',   'Item unit conversions',    'itm', 'inv', 'ItemUom',        1, NULL,       'ITM.UOM.MAINT',      'Stock units per entered unit, by usage and effective date. Changing a pack size creates a new effective dated row.'),
      ('ITM.ITEMREV',   'Item revisions',           'itm', 'inv', 'ItemRevision',   1, NULL,       'ITM.ITEM.MAINT',     'Revision history with interchangeability. A non interchangeable revision may not be substituted for its predecessor.'),
      ('ITM.ITEMSITE',  'Item site planning data',  'itm', 'inv', 'ItemSite',       1, 'IsActive', 'ITM.ITEM.MAINT',     'Lead time, order quantities, reorder point, safety stock and ABC class for one item at one site.'),
      ('ITM.PARTNER',   'Partner part numbers',     'itm', 'inv', 'ItemPartner',    1, NULL,       'ITM.ITEM.MAINT',     'Supplier, customer and manufacturer part numbers with supply terms and price breaks. An alias never replaces the internal identity.'),
      ('ITM.SUBST',     'Approved substitutes',     'itm', 'inv', 'ItemSubstitute', 1, NULL,       'ITM.ITEM.MAINT',     'Approved substitutes by context with the conversion ratio. Equal or higher specification only.'),
      ('ITM.WAREHOUSE', 'Warehouses',               'itm', 'inv', 'Warehouse',      1, 'IsActive', 'ITM.LOCATION.MAINT', 'Physical stores within a site.'),
      ('ITM.LOCATION',  'Stock locations',          'itm', 'inv', 'Location',       1, 'IsActive', 'ITM.LOCATION.MAINT', 'Stock, quarantine, inspection, work in progress, transit, scrap and subcontract locations. Only available owned locations hold eligible stock.'),
      ('ITM.LOT',       'Lots',                     'itm', 'inv', 'Lot',            1, NULL,       'ITM.LOT.MAINT',      'Lot identities with expiry, certificate evidence and quality condition.'),
      ('ITM.SERIAL',    'Serial numbers',           'itm', 'inv', 'Serial',         1, NULL,       'ITM.LOT.MAINT',      'Serial identities and where each one is.'),
      ('ITM.VATCLASS',  'Item VAT liability',       'itm', 'inv', 'ItemVatClass',   1, NULL,       'ITM.ITEM.MAINT',     'Effective dated VAT liability by direction with the evidence that supports a non standard rate.'),
      ('ITM.CUSTOMS',   'Commodity codes',          'itm', 'inv', 'ItemCustoms',    1, NULL,       'ITM.CUSTOMS.MAINT',  'Commodity code, country of origin, preference evidence and duty rate, effective dated.'),
      ('ITM.PACKAGING', 'Packaging composition',    'itm', 'inv', 'ItemPackaging',  1, NULL,       'ITM.CUSTOMS.MAINT',  'Packaging materials and weights per stock unit for extended producer responsibility and plastic packaging tax.'),
      ('ITM.COUNTLINE', 'Stock count lines',        'itm', 'inv', 'StockCountLine', 0, NULL,       'ITM.COUNT.ENTER',    'Counted quantities against the frozen book quantity. The variance is only posted by the approved COUNTADJ adjustment run.');

    MERGE core.MaintDef AS t USING @d AS s ON t.MaintCode = s.MaintCode
    WHEN MATCHED THEN UPDATE SET Title = s.Title, ModuleCode = s.ModuleCode, SchemaName = s.SchemaName,
            TableName = s.TableName, CompanyScoped = s.CompanyScoped, RetireColumn = s.RetireColumn,
            PermissionCode = s.PermissionCode, HelpText = s.HelpText, IsActive = 1,
            ModifiedBy = 'SEED', ModifiedAt = SYSDATETIME()
    WHEN NOT MATCHED BY TARGET THEN
        INSERT (MaintCode, Title, ModuleCode, SchemaName, TableName, CompanyScoped, RetireColumn,
                PermissionCode, HelpText, CreatedBy)
        VALUES (s.MaintCode, s.Title, s.ModuleCode, s.SchemaName, s.TableName, s.CompanyScoped,
                s.RetireColumn, s.PermissionCode, s.HelpText, 'SEED');

    DECLARE @c table (MaintCode varchar(30), ColumnName sysname, Caption varchar(60), DataType varchar(10),
                      SortOrder int, Width int, IsKey bit, IsRequired bit, IsReadOnly bit,
                      IsInList bit, IsInEdit bit, IsSearchable bit, LookupProc varchar(128) NULL, LookupArg varchar(60) NULL);
    INSERT @c VALUES
      -- Items -------------------------------------------------------------------
      ('ITM.ITEM','CompanyCode','Company','CODE',             10, 70,1,1,0,0,1,0,'core.usp_UiLookup','CORE.COMPANY'),
      ('ITM.ITEM','ItemCode','Item code','CODE',              20,180,1,1,0,1,1,1,NULL,NULL),
      ('ITM.ITEM','Description','Description','TEXT',         30,240,0,1,0,1,1,1,NULL,NULL),
      ('ITM.ITEM','ItemClass','Class','CODE',                 40, 90,0,1,0,1,1,1,NULL,NULL),
      ('ITM.ITEM','Category','Category','CODE',               50, 90,0,0,0,1,1,1,NULL,NULL),
      ('ITM.ITEM','StockUomCode','Stock unit','CODE',         60, 70,0,1,0,1,1,0,'core.usp_UiLookup','ITM.UOM'),
      ('ITM.ITEM','CurrentRevision','Revision','CODE',        70, 60,0,0,0,1,1,0,NULL,NULL),
      ('ITM.ITEM','TraceabilityType','Traceability','CODE',   80, 90,0,0,0,1,1,0,NULL,NULL),
      ('ITM.ITEM','ShelfLifeDays','Shelf life (days)','INT',  90, 90,0,0,0,0,1,0,NULL,NULL),
      ('ITM.ITEM','MakeBuy','Make / buy','CODE',             100, 70,0,0,0,1,1,0,NULL,NULL),
      ('ITM.ITEM','CostMethod','Cost method','CODE',         110, 80,0,0,0,1,1,0,NULL,NULL),
      ('ITM.ITEM','StewardUser','Steward','CODE',            120, 90,0,0,0,1,1,1,'core.usp_UiLookup','CORE.USER'),
      ('ITM.ITEM','LifecycleStatus','Lifecycle','CODE',      130, 90,0,0,1,1,1,0,NULL,NULL),
      ('ITM.ITEM','SuccessorItemCode','Successor','CODE',    140,160,0,0,0,0,1,0,NULL,NULL),
      ('ITM.ITEM','StatusReason','Status reason','TEXT',     150,220,0,0,0,0,1,0,NULL,NULL),
      ('ITM.ITEM','IsStocked','Stocked','BOOL',              160, 60,0,0,0,1,1,0,NULL,NULL),
      -- Units of measure ---------------------------------------------------------
      ('ITM.UOM','UomCode','Unit','CODE',                     10, 70,1,1,0,1,1,1,NULL,NULL),
      ('ITM.UOM','Name','Name','TEXT',                        20,200,0,1,0,1,1,1,NULL,NULL),
      ('ITM.UOM','UomDimension','Dimension','CODE',           30, 90,0,1,0,1,1,1,NULL,NULL),
      ('ITM.UOM','BaseUomCode','Base unit','CODE',            40, 80,0,0,0,1,1,0,NULL,NULL),
      ('ITM.UOM','FactorToBase','Factor to base','DECIMAL',   50,120,0,1,0,1,1,0,NULL,NULL),
      ('ITM.UOM','DecimalPlaces','Decimals','INT',            60, 70,0,0,0,1,1,0,NULL,NULL),
      ('ITM.UOM','IsMetric','Metric','BOOL',                  70, 60,0,0,0,1,1,0,NULL,NULL),
      ('ITM.UOM','IsActive','In use','BOOL',                  90, 60,0,0,0,1,1,0,NULL,NULL),
      -- Item unit conversions ----------------------------------------------------
      ('ITM.ITEMUOM','CompanyCode','Company','CODE',          10, 70,1,1,0,0,1,0,'core.usp_UiLookup','CORE.COMPANY'),
      ('ITM.ITEMUOM','ItemCode','Item','CODE',                20,180,1,1,0,1,1,1,'core.usp_UiLookup','ITM.ITEM'),
      ('ITM.ITEMUOM','UomCode','Unit','CODE',                 30, 70,1,1,0,1,1,1,'core.usp_UiLookup','ITM.UOM'),
      ('ITM.ITEMUOM','UsageType','Usage','CODE',              40, 80,1,1,0,1,1,0,NULL,NULL),
      ('ITM.ITEMUOM','EffectiveFrom','From','DATE',           50, 90,1,1,0,1,1,0,NULL,NULL),
      ('ITM.ITEMUOM','StockUnitsPerUom','Stock units per unit','DECIMAL', 60,140,0,1,0,1,1,0,NULL,NULL),
      ('ITM.ITEMUOM','PackDescription','Pack','TEXT',         70,160,0,0,0,1,1,1,NULL,NULL),
      ('ITM.ITEMUOM','EffectiveTo','To','DATE',               80, 90,0,0,0,1,1,0,NULL,NULL),
      -- Item revisions -----------------------------------------------------------
      ('ITM.ITEMREV','CompanyCode','Company','CODE',          10, 70,1,1,0,0,1,0,'core.usp_UiLookup','CORE.COMPANY'),
      ('ITM.ITEMREV','ItemCode','Item','CODE',                20,180,1,1,0,1,1,1,'core.usp_UiLookup','ITM.ITEM'),
      ('ITM.ITEMREV','Revision','Revision','CODE',            30, 70,1,1,0,1,1,1,NULL,NULL),
      ('ITM.ITEMREV','Description','Description','TEXT',      40,220,0,0,0,1,1,1,NULL,NULL),
      ('ITM.ITEMREV','EffectiveFrom','From','DATE',           50, 90,0,1,0,1,1,0,NULL,NULL),
      ('ITM.ITEMREV','EffectiveTo','To','DATE',               60, 90,0,0,0,1,1,0,NULL,NULL),
      ('ITM.ITEMREV','Status','Status','CODE',                70, 90,0,1,0,1,1,0,NULL,NULL),
      ('ITM.ITEMREV','IsInterchangeable','Interchangeable','BOOL', 80,110,0,0,0,1,1,0,NULL,NULL),
      ('ITM.ITEMREV','DrawingRef','Drawing','TEXT',           90,120,0,0,0,1,1,1,NULL,NULL),
      ('ITM.ITEMREV','ChangeReason','Change reason','TEXT',  100,220,0,0,0,0,1,1,NULL,NULL),
      -- Item site planning data ---------------------------------------------------
      ('ITM.ITEMSITE','CompanyCode','Company','CODE',         10, 70,1,1,0,0,1,0,'core.usp_UiLookup','CORE.COMPANY'),
      ('ITM.ITEMSITE','SiteCode','Site','CODE',               20, 70,1,1,0,1,1,1,'core.usp_UiLookup','CORE.SITE'),
      ('ITM.ITEMSITE','ItemCode','Item','CODE',               30,180,1,1,0,1,1,1,'core.usp_UiLookup','ITM.ITEM'),
      ('ITM.ITEMSITE','DefaultWarehouseCode','Warehouse','CODE', 40, 90,0,0,0,1,1,0,NULL,NULL),
      ('ITM.ITEMSITE','DefaultLocationCode','Location','CODE',50, 90,0,0,0,1,1,0,NULL,NULL),
      ('ITM.ITEMSITE','LeadTimeDays','Lead time (days)','INT',60, 90,0,0,0,1,1,0,NULL,NULL),
      ('ITM.ITEMSITE','MinOrderQty','Minimum order','QTY',    70,100,0,0,0,1,1,0,NULL,NULL),
      ('ITM.ITEMSITE','PurchaseMultipleQty','Order multiple','QTY', 80,110,0,0,0,1,1,0,NULL,NULL),
      ('ITM.ITEMSITE','ReorderPointQty','Reorder point','QTY',90,110,0,0,0,1,1,0,NULL,NULL),
      ('ITM.ITEMSITE','SafetyStockQty','Safety stock','QTY', 100,110,0,0,0,1,1,0,NULL,NULL),
      ('ITM.ITEMSITE','MaxStockQty','Maximum','QTY',         110,100,0,0,0,1,1,0,NULL,NULL),
      ('ITM.ITEMSITE','AbcClass','ABC','CODE',               120, 50,0,0,0,1,1,0,NULL,NULL),
      ('ITM.ITEMSITE','IsActive','In use','BOOL',            130, 60,0,0,0,1,1,0,NULL,NULL),
      -- Partner part numbers --------------------------------------------------------
      ('ITM.PARTNER','CompanyCode','Company','CODE',          10, 70,1,1,0,0,1,0,'core.usp_UiLookup','CORE.COMPANY'),
      ('ITM.PARTNER','ItemCode','Item','CODE',                20,180,1,1,0,1,1,1,'core.usp_UiLookup','ITM.ITEM'),
      ('ITM.PARTNER','PartnerType','Partner type','CODE',     30, 90,1,1,0,1,1,0,NULL,NULL),
      ('ITM.PARTNER','PartnerCode','Partner','CODE',          40, 90,1,1,0,1,1,1,NULL,NULL),
      ('ITM.PARTNER','QtyBreak','Quantity break','QTY',       50,100,1,0,0,1,1,0,NULL,NULL),
      ('ITM.PARTNER','EffectiveFrom','From','DATE',           60, 90,1,1,0,1,1,0,NULL,NULL),
      ('ITM.PARTNER','PartnerItemCode','Their part number','TEXT', 70,160,0,1,0,1,1,1,NULL,NULL),
      ('ITM.PARTNER','PartnerDescription','Their description','TEXT', 80,200,0,0,0,1,1,1,NULL,NULL),
      ('ITM.PARTNER','UomCode','Unit','CODE',                 90, 60,0,0,0,1,1,0,'core.usp_UiLookup','ITM.UOM'),
      ('ITM.PARTNER','CurrencyCode','Currency','CODE',       100, 70,0,0,0,1,1,0,'core.usp_UiLookup','CORE.CURRENCY'),
      ('ITM.PARTNER','UnitPrice','Unit price','DECIMAL',     110,100,0,0,0,1,1,0,NULL,NULL),
      ('ITM.PARTNER','LeadTimeDays','Lead time (days)','INT',120, 90,0,0,0,1,1,0,NULL,NULL),
      ('ITM.PARTNER','PreferenceRank','Rank','INT',          130, 50,0,0,0,1,1,0,NULL,NULL),
      ('ITM.PARTNER','IsApprovedSource','Approved','BOOL',   140, 70,0,0,0,1,1,0,NULL,NULL),
      ('ITM.PARTNER','EffectiveTo','To','DATE',              150, 90,0,0,0,1,1,0,NULL,NULL),
      -- Approved substitutes ----------------------------------------------------------
      ('ITM.SUBST','CompanyCode','Company','CODE',            10, 70,1,1,0,0,1,0,'core.usp_UiLookup','CORE.COMPANY'),
      ('ITM.SUBST','ItemCode','Item','CODE',                  20,180,1,1,0,1,1,1,'core.usp_UiLookup','ITM.ITEM'),
      ('ITM.SUBST','SubstituteItemCode','Substitute','CODE',  30,180,1,1,0,1,1,1,'core.usp_UiLookup','ITM.ITEM'),
      ('ITM.SUBST','ConversionRatio','Ratio','DECIMAL',       40, 90,0,1,0,1,1,0,NULL,NULL),
      ('ITM.SUBST','ContextPurchase','Purchase','BOOL',       50, 70,0,0,0,1,1,0,NULL,NULL),
      ('ITM.SUBST','ContextSales','Sales','BOOL',             60, 60,0,0,0,1,1,0,NULL,NULL),
      ('ITM.SUBST','ContextKit','Kit','BOOL',                 70, 50,0,0,0,1,1,0,NULL,NULL),
      ('ITM.SUBST','ContextBom','Bill','BOOL',                80, 50,0,0,0,1,1,0,NULL,NULL),
      ('ITM.SUBST','RequiresCustomerAgreement','Customer agreement','BOOL', 90,120,0,0,0,1,1,0,NULL,NULL),
      ('ITM.SUBST','Restrictions','Restrictions','TEXT',     100,220,0,0,0,0,1,1,NULL,NULL),
      ('ITM.SUBST','ValidFrom','From','DATE',                110, 90,0,0,0,1,1,0,NULL,NULL),
      ('ITM.SUBST','ValidTo','To','DATE',                    120, 90,0,0,0,1,1,0,NULL,NULL),
      -- Warehouses ---------------------------------------------------------------------
      ('ITM.WAREHOUSE','CompanyCode','Company','CODE',        10, 70,1,1,0,0,1,0,'core.usp_UiLookup','CORE.COMPANY'),
      ('ITM.WAREHOUSE','SiteCode','Site','CODE',              20, 70,1,1,0,1,1,1,'core.usp_UiLookup','CORE.SITE'),
      ('ITM.WAREHOUSE','WarehouseCode','Warehouse','CODE',    30,100,1,1,0,1,1,1,NULL,NULL),
      ('ITM.WAREHOUSE','Name','Name','TEXT',                  40,220,0,1,0,1,1,1,NULL,NULL),
      ('ITM.WAREHOUSE','IsDefault','Default','BOOL',          50, 60,0,0,0,1,1,0,NULL,NULL),
      ('ITM.WAREHOUSE','IsActive','In use','BOOL',            60, 60,0,0,0,1,1,0,NULL,NULL),
      -- Stock locations ------------------------------------------------------------------
      ('ITM.LOCATION','CompanyCode','Company','CODE',         10, 70,1,1,0,0,1,0,'core.usp_UiLookup','CORE.COMPANY'),
      ('ITM.LOCATION','SiteCode','Site','CODE',               20, 70,1,1,0,1,1,1,'core.usp_UiLookup','CORE.SITE'),
      ('ITM.LOCATION','WarehouseCode','Warehouse','CODE',     30,100,1,1,0,1,1,1,'core.usp_UiLookup','ITM.WAREHOUSE'),
      ('ITM.LOCATION','LocationCode','Location','CODE',       40,100,1,1,0,1,1,1,NULL,NULL),
      ('ITM.LOCATION','Name','Name','TEXT',                   50,220,0,1,0,1,1,1,NULL,NULL),
      ('ITM.LOCATION','LocationType','Type','CODE',           60,100,0,1,0,1,1,0,NULL,NULL),
      ('ITM.LOCATION','IsAvailable','Available','BOOL',       70, 70,0,0,0,1,1,0,NULL,NULL),
      ('ITM.LOCATION','IsOwnedStock','Owned stock','BOOL',    80, 80,0,0,0,1,1,0,NULL,NULL),
      ('ITM.LOCATION','AllowNegative','Allow negative','BOOL',90, 90,0,0,0,1,1,0,NULL,NULL),
      ('ITM.LOCATION','PickSequence','Pick sequence','INT',  100, 90,0,0,0,1,1,0,NULL,NULL),
      ('ITM.LOCATION','IsActive','In use','BOOL',            110, 60,0,0,0,1,1,0,NULL,NULL),
      -- Lots -----------------------------------------------------------------------------
      ('ITM.LOT','CompanyCode','Company','CODE',              10, 70,1,1,0,0,1,0,'core.usp_UiLookup','CORE.COMPANY'),
      ('ITM.LOT','ItemCode','Item','CODE',                    20,180,1,1,0,1,1,1,'core.usp_UiLookup','ITM.ITEM'),
      ('ITM.LOT','LotCode','Lot','CODE',                      30,140,1,1,0,1,1,1,NULL,NULL),
      ('ITM.LOT','Revision','Revision','CODE',                40, 60,0,0,0,1,1,0,NULL,NULL),
      ('ITM.LOT','SupplierLotRef','Supplier lot','TEXT',      50,120,0,0,0,1,1,1,NULL,NULL),
      ('ITM.LOT','SupplierCode','Supplier','CODE',            60, 90,0,0,0,1,1,1,NULL,NULL),
      ('ITM.LOT','ManufacturedDate','Manufactured','DATE',    70, 90,0,0,0,1,1,0,NULL,NULL),
      ('ITM.LOT','ReceivedDate','Received','DATE',            80, 90,0,0,0,1,1,0,NULL,NULL),
      ('ITM.LOT','ExpiryDate','Expiry','DATE',                90, 90,0,0,0,1,1,0,NULL,NULL),
      ('ITM.LOT','QualityStatus','Condition','CODE',         100, 90,0,1,0,1,1,0,NULL,NULL),
      ('ITM.LOT','CertificateRef','Certificate','TEXT',      110,140,0,0,0,1,1,1,NULL,NULL),
      ('ITM.LOT','OriginCountry','Origin','CODE',            120, 60,0,0,0,1,1,0,NULL,NULL),
      -- Serial numbers ----------------------------------------------------------------------
      ('ITM.SERIAL','CompanyCode','Company','CODE',           10, 70,1,1,0,0,1,0,'core.usp_UiLookup','CORE.COMPANY'),
      ('ITM.SERIAL','ItemCode','Item','CODE',                 20,180,1,1,0,1,1,1,'core.usp_UiLookup','ITM.ITEM'),
      ('ITM.SERIAL','SerialNo','Serial number','CODE',        30,160,1,1,0,1,1,1,NULL,NULL),
      ('ITM.SERIAL','LotCode','Lot','CODE',                   40,120,0,0,0,1,1,1,NULL,NULL),
      ('ITM.SERIAL','Status','Status','CODE',                 50, 90,0,1,0,1,1,0,NULL,NULL),
      ('ITM.SERIAL','CurrentSiteCode','Site','CODE',          60, 60,0,0,0,1,1,0,NULL,NULL),
      ('ITM.SERIAL','CurrentLocationCode','Location','CODE',  70, 90,0,0,0,1,1,0,NULL,NULL),
      ('ITM.SERIAL','QualityStatus','Condition','CODE',       80, 90,0,0,0,1,1,0,NULL,NULL),
      ('ITM.SERIAL','WarrantyEndDate','Warranty to','DATE',   90, 90,0,0,0,1,1,0,NULL,NULL),
      -- Item VAT liability --------------------------------------------------------------------
      ('ITM.VATCLASS','CompanyCode','Company','CODE',         10, 70,1,1,0,0,1,0,'core.usp_UiLookup','CORE.COMPANY'),
      ('ITM.VATCLASS','ItemCode','Item','CODE',               20,180,1,1,0,1,1,1,'core.usp_UiLookup','ITM.ITEM'),
      ('ITM.VATCLASS','Direction','Direction','CODE',         30, 80,1,1,0,1,1,0,NULL,NULL),
      ('ITM.VATCLASS','EffectiveFrom','From','DATE',          40, 90,1,1,0,1,1,0,NULL,NULL),
      ('ITM.VATCLASS','VatCode','VAT code','CODE',            50, 80,0,1,0,1,1,1,NULL,NULL),
      ('ITM.VATCLASS','EffectiveTo','To','DATE',              60, 90,0,0,0,1,1,0,NULL,NULL),
      ('ITM.VATCLASS','EvidenceRef','Evidence','TEXT',        70,220,0,0,0,1,1,1,NULL,NULL),
      ('ITM.VATCLASS','ApprovedBy','Approved by','CODE',      80, 90,0,0,0,1,1,0,NULL,NULL),
      -- Commodity codes -------------------------------------------------------------------------
      ('ITM.CUSTOMS','CompanyCode','Company','CODE',          10, 70,1,1,0,0,1,0,'core.usp_UiLookup','CORE.COMPANY'),
      ('ITM.CUSTOMS','ItemCode','Item','CODE',                20,180,1,1,0,1,1,1,'core.usp_UiLookup','ITM.ITEM'),
      ('ITM.CUSTOMS','EffectiveFrom','From','DATE',           30, 90,1,1,0,1,1,0,NULL,NULL),
      ('ITM.CUSTOMS','CommodityCode','Commodity code','CODE', 40,110,0,1,0,1,1,1,NULL,NULL),
      ('ITM.CUSTOMS','CountryOfOrigin','Origin','CODE',       50, 60,0,1,0,1,1,1,NULL,NULL),
      ('ITM.CUSTOMS','PreferentialOrigin','Preferential','BOOL', 60, 90,0,0,0,1,1,0,NULL,NULL),
      ('ITM.CUSTOMS','PreferenceEvidenceRef','Preference evidence','TEXT', 70,200,0,0,0,1,1,1,NULL,NULL),
      ('ITM.CUSTOMS','DutyRatePct','Duty rate','PCT',         80, 80,0,0,0,1,1,0,NULL,NULL),
      ('ITM.CUSTOMS','NetWeightKgPerStockUnit','Net weight (kg)','QTY', 90,110,0,0,0,1,1,0,NULL,NULL),
      ('ITM.CUSTOMS','EffectiveTo','To','DATE',              100, 90,0,0,0,1,1,0,NULL,NULL),
      -- Packaging composition --------------------------------------------------------------------
      ('ITM.PACKAGING','CompanyCode','Company','CODE',        10, 70,1,1,0,0,1,0,'core.usp_UiLookup','CORE.COMPANY'),
      ('ITM.PACKAGING','ItemCode','Item','CODE',              20,180,1,1,0,1,1,1,'core.usp_UiLookup','ITM.ITEM'),
      ('ITM.PACKAGING','PackagingMaterial','Material','CODE', 30,100,1,1,0,1,1,1,NULL,NULL),
      ('ITM.PACKAGING','PackagingType','Type','CODE',         40, 90,1,1,0,1,1,0,NULL,NULL),
      ('ITM.PACKAGING','WeightGrams','Weight (g)','QTY',      50, 90,0,1,0,1,1,0,NULL,NULL),
      ('ITM.PACKAGING','RecycledContentPct','Recycled','PCT', 60, 80,0,0,0,1,1,0,NULL,NULL),
      ('ITM.PACKAGING','NationOfSupply','Nation','CODE',      70, 60,0,0,0,1,1,0,NULL,NULL),
      ('ITM.PACKAGING','IsPlasticPackagingTax','Plastic packaging tax','BOOL', 80,130,0,0,0,1,1,0,NULL,NULL),
      -- Stock count lines --------------------------------------------------------------------------
      ('ITM.COUNTLINE','CountId','Count','INT',               10, 70,1,1,0,1,1,0,NULL,NULL),
      ('ITM.COUNTLINE','LineNum','Line','INT',                20, 50,1,1,0,1,1,0,NULL,NULL),
      ('ITM.COUNTLINE','ItemCode','Item','CODE',              30,180,0,1,1,1,1,1,NULL,NULL),
      ('ITM.COUNTLINE','WarehouseCode','Warehouse','CODE',    40, 90,0,0,1,1,1,0,NULL,NULL),
      ('ITM.COUNTLINE','LocationCode','Location','CODE',      50, 90,0,0,1,1,1,0,NULL,NULL),
      ('ITM.COUNTLINE','LotCode','Lot','CODE',                60,120,0,0,1,1,1,1,NULL,NULL),
      ('ITM.COUNTLINE','SerialNo','Serial','CODE',            70,120,0,0,1,1,1,1,NULL,NULL),
      ('ITM.COUNTLINE','ExpectedQty','Book quantity','QTY',   80,110,0,0,1,1,1,0,NULL,NULL),
      ('ITM.COUNTLINE','CountedQty','Counted quantity','QTY', 90,110,0,0,0,1,1,0,NULL,NULL),
      ('ITM.COUNTLINE','RecountQty','Recount','QTY',         100,100,0,0,0,1,1,0,NULL,NULL),
      ('ITM.COUNTLINE','AdjustmentQty','Adjustment','QTY',   110,100,0,0,1,1,1,0,NULL,NULL),
      ('ITM.COUNTLINE','ReasonCode','Reason code','CODE',    120, 90,0,0,0,1,1,1,NULL,NULL),
      ('ITM.COUNTLINE','Status','Status','CODE',             130, 80,0,0,0,1,1,0,NULL,NULL);

    MERGE core.MaintColumn AS t
    USING @c AS s ON t.MaintCode = s.MaintCode AND t.ColumnName = s.ColumnName
    WHEN MATCHED THEN UPDATE SET Caption = s.Caption, DataType = s.DataType, SortOrder = s.SortOrder,
            Width = s.Width, IsKey = s.IsKey, IsRequired = s.IsRequired, IsReadOnly = s.IsReadOnly,
            IsInList = s.IsInList, IsInEdit = s.IsInEdit, IsSearchable = s.IsSearchable,
            LookupProc = s.LookupProc, LookupArg = s.LookupArg
    WHEN NOT MATCHED BY TARGET AND s.MaintCode LIKE 'ITM.%' THEN
        INSERT (MaintCode, ColumnName, Caption, DataType, SortOrder, Width, IsKey, IsRequired, IsReadOnly,
                IsInList, IsInEdit, IsSearchable, LookupProc, LookupArg)
        VALUES (s.MaintCode, s.ColumnName, s.Caption, s.DataType, s.SortOrder, s.Width, s.IsKey, s.IsRequired,
                s.IsReadOnly, s.IsInList, s.IsInEdit, s.IsSearchable, s.LookupProc, s.LookupArg);

    PRINT 'Seeded core.MaintDef / core.MaintColumn (ITM masters)';
END
GO
