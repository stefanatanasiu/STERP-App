/*  86-kit-maintdef.sql - generic file maintenance for the two kit masters that do not need a
    hand written screen: commercial kit definitions (KIT.DEF) and controlled disassembly notes
    (KIT.DISASSY).  The menu rows in 85-kit-menu.sql point frmGenericMaint at these codes.

    Columns the kit module has not created yet are filtered out by core.usp_UiMaintDef, so this
    seed is safe to run at any point of the build.                     Re-runnable (MERGE).

    REQ: KIT-001, KIT-021, KIT-023, KIT-024, COM-013
*/
SET NOCOUNT ON;
GO
IF OBJECT_ID(N'core.MaintDef') IS NOT NULL AND OBJECT_ID(N'core.MaintColumn') IS NOT NULL
BEGIN
    DECLARE @d table (MaintCode varchar(30), Title varchar(80), ModuleCode varchar(10),
                      SchemaName sysname, TableName sysname, CompanyScoped bit,
                      RetireColumn sysname NULL, PermissionCode varchar(60) NULL, HelpText varchar(400) NULL);
    INSERT @d VALUES
      ('KIT.DEF',     'Commercial kit definitions', 'kit', 'kit', 'KitDefinition', 1, NULL, 'KIT.DEF.MAINT',
       'Commercial kits sold as one parent and delivered as components. A released version is '
       + 'superseded, never edited; the parent is not a stocked assembly and is never received as one.'),
      ('KIT.DISASSY', 'Disassembly notes',          'kit', 'kit', 'Disassembly',   1, NULL, 'KIT.DISASSEMBLE.POST',
       'Controlled disassembly of a parent back into components. Recovery can never create more '
       + 'carrying value than the parent plus approved disassembly costs, less recognised losses.');

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
      -- Commercial kit definitions (KIT-001, KIT-023) -------------------------
      ('KIT.DEF','CompanyCode','Company',                 'CODE', 10, 80,1,1,0,1,1,1,'core.usp_UiLookup','CORE.COMPANY'),
      ('KIT.DEF','KitItemCode','Kit item',                'CODE', 20,160,1,1,0,1,1,1,NULL,NULL),
      ('KIT.DEF','KitVersion','Version',                  'CODE', 30, 70,1,1,0,1,1,1,NULL,NULL),
      ('KIT.DEF','Name','Name',                           'TEXT', 40,220,0,1,0,1,1,1,NULL,NULL),
      ('KIT.DEF','Description','Description',             'TEXT', 50,260,0,0,0,0,1,1,NULL,NULL),
      ('KIT.DEF','SupplyKind','Supply kind',              'CODE', 60, 90,0,1,0,1,1,0,NULL,NULL),
      ('KIT.DEF','AllowPartialDespatch','Partial despatch','BOOL',70,110,0,0,0,1,1,0,NULL,NULL),
      ('KIT.DEF','EffectiveFrom','Effective from',        'DATE', 80,100,0,1,0,1,1,0,NULL,NULL),
      ('KIT.DEF','EffectiveTo','Effective to',            'DATE', 90,100,0,0,0,1,1,0,NULL,NULL),
      ('KIT.DEF','Status','Status',                       'CODE',100,100,0,1,0,1,1,1,NULL,NULL),
      ('KIT.DEF','VatOwnerDecisionBy','VAT decision by',  'TEXT',110,110,0,0,1,0,1,0,NULL,NULL),
      ('KIT.DEF','VatOwnerDecisionAt','VAT decided at',   'DATE',120,110,0,0,1,0,1,0,NULL,NULL),
      ('KIT.DEF','ApprovedBy','Approved by',              'TEXT',130,110,0,0,1,1,1,0,NULL,NULL),
      ('KIT.DEF','ApprovedAt','Approved at',              'DATE',140,110,0,0,1,0,1,0,NULL,NULL),
      -- Disassembly notes (KIT-021) -------------------------------------------
      ('KIT.DISASSY','CompanyCode','Company',             'CODE', 10, 80,1,1,0,1,1,1,'core.usp_UiLookup','CORE.COMPANY'),
      ('KIT.DISASSY','DisassemblyRef','Reference',        'CODE', 20,150,1,1,0,1,1,1,NULL,NULL),
      ('KIT.DISASSY','SiteCode','Site',                   'CODE', 30, 70,0,1,0,1,1,1,'core.usp_UiLookup','CORE.SITE'),
      ('KIT.DISASSY','ParentItemCode','Parent item',      'CODE', 40,160,0,1,0,1,1,1,NULL,NULL),
      ('KIT.DISASSY','Revision','Revision',               'CODE', 50, 70,0,0,0,1,1,0,NULL,NULL),
      ('KIT.DISASSY','LotCode','Lot',                     'CODE', 60,110,0,0,0,1,1,1,NULL,NULL),
      ('KIT.DISASSY','SerialNo','Serial',                 'CODE', 70,120,0,0,0,1,1,1,NULL,NULL),
      ('KIT.DISASSY','ConsumedQty','Consumed',            'QTY',  80,110,0,1,0,1,1,0,NULL,NULL),
      ('KIT.DISASSY','DisassemblyDate','Disassembled',    'DATE', 90,100,0,1,0,1,1,0,NULL,NULL),
      ('KIT.DISASSY','PostingDate','Posting date',        'DATE',100,100,0,1,0,1,1,0,NULL,NULL),
      ('KIT.DISASSY','AllocationBasis','Allocation basis','CODE',110,110,0,1,0,1,1,0,NULL,NULL),
      ('KIT.DISASSY','ParentCarryingValueGBP','Parent value','MONEY',120,110,0,0,1,1,1,0,NULL,NULL),
      ('KIT.DISASSY','ApprovedCostsGBP','Approved costs', 'MONEY',130,110,0,0,0,1,1,0,NULL,NULL),
      ('KIT.DISASSY','RecoveredValueGBP','Recovered',     'MONEY',140,110,0,0,1,1,1,0,NULL,NULL),
      ('KIT.DISASSY','LossGBP','Loss',                    'MONEY',150,100,0,0,1,1,1,0,NULL,NULL),
      ('KIT.DISASSY','Reason','Reason',                   'TEXT',160,260,0,0,0,0,1,1,NULL,NULL),
      ('KIT.DISASSY','Status','Status',                   'CODE',170,100,0,1,0,1,1,1,NULL,NULL),
      ('KIT.DISASSY','ApprovedBy','Approved by',          'TEXT',180,110,0,0,1,1,1,0,NULL,NULL);

    MERGE core.MaintColumn AS t USING @c AS s
        ON t.MaintCode = s.MaintCode AND t.ColumnName = s.ColumnName
    WHEN MATCHED THEN UPDATE SET Caption = s.Caption, DataType = s.DataType, SortOrder = s.SortOrder,
            Width = s.Width, IsKey = s.IsKey, IsRequired = s.IsRequired, IsReadOnly = s.IsReadOnly,
            IsInList = s.IsInList, IsInEdit = s.IsInEdit, IsSearchable = s.IsSearchable,
            LookupProc = s.LookupProc, LookupArg = s.LookupArg
    WHEN NOT MATCHED BY TARGET THEN
        INSERT (MaintCode, ColumnName, Caption, DataType, SortOrder, Width, IsKey, IsRequired,
                IsReadOnly, IsInList, IsInEdit, IsSearchable, LookupProc, LookupArg)
        VALUES (s.MaintCode, s.ColumnName, s.Caption, s.DataType, s.SortOrder, s.Width, s.IsKey,
                s.IsRequired, s.IsReadOnly, s.IsInList, s.IsInEdit, s.IsSearchable, s.LookupProc, s.LookupArg)
    WHEN NOT MATCHED BY SOURCE AND t.MaintCode IN ('KIT.DEF','KIT.DISASSY') THEN DELETE;

    PRINT 'Seeded core.MaintDef / core.MaintColumn (kits)';
END
GO
