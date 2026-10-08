/*  33-bom-maintdef.sql - generic file maintenance for the two bill of material masters that do
    not need a hand written screen: bill headers (BOM.HEADER) and engineering change proposals
    (BOM.CHANGE).  The menu rows BOM.HEADERS and BOM.CHANGE in 32-bom-menu.sql point
    frmGenericMaint at these codes.

    The header screen is a *list* maintenance view: the structure itself (components, by-products,
    alternates) is edited on frmBomEditor, and a released revision is protected by the trigger
    bom.tr_BomReleasedImmutable - the generic screen can read it but any engineering change to a
    released row is refused with STERP-E-BOM0413, which is the behaviour we want on this screen.

    Columns the module has not created yet are filtered out by core.usp_UiMaintDef, so this seed
    is safe to run at any point of the build.                            Re-runnable (MERGE).

    REQ: BOM-001, BOM-002, BOM-003, BOM-004, BOM-024, BOM-030, BOM-031, COM-013
*/
SET NOCOUNT ON;
GO
IF OBJECT_ID(N'core.MaintDef') IS NOT NULL AND OBJECT_ID(N'core.MaintColumn') IS NOT NULL
BEGIN
    DECLARE @d table (MaintCode varchar(30), Title varchar(80), ModuleCode varchar(10),
                      SchemaName sysname, TableName sysname, CompanyScoped bit,
                      RetireColumn sysname NULL, PermissionCode varchar(60) NULL, HelpText varchar(400) NULL);
    INSERT @d VALUES
      ('BOM.HEADER', 'Bill of material headers', 'bom', 'bom', 'BomHeader', 1, NULL, 'BOM.MAINT',
       'One revision of one bill. A revision is never edited once released - supersede it with a new '
       + 'revision instead. Nominal basis, yield and production quantum are the declared boundary the '
       + 'explosion rounds at; purpose, variant and job reference decide which revision is selected.'),
      ('BOM.CHANGE',  'Engineering changes',     'bom', 'bom', 'BomChange', 1, NULL, 'BOM.CHANGE.APPROVE',
       'An engineering change proposal. A bulk change is previewed by BOMBULK across every affected '
       + 'bill, every proposed revision is validated, and nothing is applied until the whole set passes '
       + 'and the change is approved by somebody other than the requester.');

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
      -- Bill headers (BOM-001, BOM-002, BOM-005..BOM-011) ---------------------
      ('BOM.HEADER','CompanyCode','Company',              'CODE', 10, 80,1,1,0,1,1,1,'core.usp_UiLookup','CORE.COMPANY'),
      ('BOM.HEADER','BomRef','Bill reference',            'CODE', 20,150,1,1,0,1,1,1,NULL,NULL),
      ('BOM.HEADER','BomRevision','Revision',             'CODE', 30, 70,1,1,0,1,1,1,NULL,NULL),
      ('BOM.HEADER','ParentItemCode','Parent item',       'CODE', 40,160,0,1,0,1,1,1,'core.usp_UiLookup','ITM.ITEM'),
      ('BOM.HEADER','ParentRevision','Parent revision',   'CODE', 50, 80,0,0,0,0,1,0,NULL,NULL),
      ('BOM.HEADER','SiteCode','Site',                    'CODE', 60, 70,0,0,0,1,1,1,'core.usp_UiLookup','CORE.SITE'),
      ('BOM.HEADER','Purpose','Purpose',                  'CODE', 70, 90,0,1,0,1,1,1,NULL,NULL),
      ('BOM.HEADER','JobRef','Job / order',               'CODE', 80,110,0,0,0,1,1,1,NULL,NULL),
      ('BOM.HEADER','VariantCode','Variant',              'CODE', 90,110,0,0,0,1,1,1,NULL,NULL),
      ('BOM.HEADER','Description','Description',          'TEXT',100,240,0,0,0,1,1,1,NULL,NULL),
      ('BOM.HEADER','NominalBasis','Nominal basis',       'QTY', 110,100,0,1,0,1,1,0,NULL,NULL),
      ('BOM.HEADER','YieldFactor','Yield factor',         'RATE',120,100,0,1,0,1,1,0,NULL,NULL),
      ('BOM.HEADER','ProductionQuantum','Quantum',        'QTY', 130,100,0,1,0,1,1,0,NULL,NULL),
      ('BOM.HEADER','BatchSize','Batch size',             'QTY', 140,100,0,0,0,0,1,0,NULL,NULL),
      ('BOM.HEADER','CostingLotSize','Costing lot size',  'QTY', 150,110,0,0,0,0,1,0,NULL,NULL),
      ('BOM.HEADER','IsPhantom','Phantom',                'BOOL',160, 70,0,0,0,1,1,0,NULL,NULL),
      ('BOM.HEADER','EffectiveFrom','Effective from',     'DATE',170,100,0,1,0,1,1,0,NULL,NULL),
      ('BOM.HEADER','EffectiveTo','Effective to',         'DATE',180,100,0,0,0,1,1,0,NULL,NULL),
      ('BOM.HEADER','Status','Status',                    'CODE',190, 90,0,1,0,1,1,1,NULL,NULL),
      ('BOM.HEADER','AuthorUser','Author',                'CODE',200,100,0,0,0,1,1,1,'core.usp_UiLookup','CORE.USER'),
      ('BOM.HEADER','ReviewerUser','Reviewer',            'CODE',210,100,0,0,0,1,1,1,'core.usp_UiLookup','CORE.USER'),
      ('BOM.HEADER','ReleasedBy','Released by',           'CODE',220,100,0,0,1,1,1,0,NULL,NULL),
      ('BOM.HEADER','ReleasedAt','Released at',           'DATE',230,110,0,0,1,1,1,0,NULL,NULL),
      ('BOM.HEADER','ChangeRef','Change reference',       'CODE',240,120,0,0,0,0,1,1,NULL,NULL),
      ('BOM.HEADER','ChangeReason','Change reason',       'TEXT',250,240,0,0,0,0,1,1,NULL,NULL),
      ('BOM.HEADER','EngineeringNotes','Engineering notes','TEXT',260,300,0,0,0,0,1,1,NULL,NULL),
      -- Engineering changes (BOM-024, BOM-030, BOM-031) -----------------------
      ('BOM.CHANGE','CompanyCode','Company',              'CODE', 10, 80,1,1,0,1,1,1,'core.usp_UiLookup','CORE.COMPANY'),
      ('BOM.CHANGE','ChangeRef','Change reference',       'CODE', 20,140,1,1,0,1,1,1,NULL,NULL),
      ('BOM.CHANGE','Title','Title',                      'TEXT', 30,220,0,1,0,1,1,1,NULL,NULL),
      ('BOM.CHANGE','Description','Description',          'TEXT', 40,300,0,0,0,0,1,1,NULL,NULL),
      ('BOM.CHANGE','ChangeType','Change type',           'CODE', 50,100,0,1,0,1,1,1,NULL,NULL),
      ('BOM.CHANGE','RequestedBy','Requested by',         'CODE', 60,110,0,1,0,1,1,1,'core.usp_UiLookup','CORE.USER'),
      ('BOM.CHANGE','RequestedAt','Requested at',         'DATE', 70,110,0,0,1,1,1,0,NULL,NULL),
      ('BOM.CHANGE','EffectiveDate','Effective date',     'DATE', 80,100,0,0,0,1,1,0,NULL,NULL),
      ('BOM.CHANGE','Status','Status',                    'CODE', 90,100,0,1,0,1,1,1,NULL,NULL),
      ('BOM.CHANGE','AffectedBills','Affected bills',     'INT', 100, 90,0,0,1,1,1,0,NULL,NULL),
      ('BOM.CHANGE','AppliedBills','Applied bills',       'INT', 110, 90,0,0,1,1,1,0,NULL,NULL),
      ('BOM.CHANGE','ApprovedBy','Approved by',           'CODE',120,110,0,0,0,1,1,1,'core.usp_UiLookup','CORE.USER'),
      ('BOM.CHANGE','ApprovedAt','Approved at',           'DATE',130,110,0,0,1,1,1,0,NULL,NULL),
      ('BOM.CHANGE','AffectedKits','Affected kits',       'TEXT',140,240,0,0,1,0,1,1,NULL,NULL),
      ('BOM.CHANGE','ImpactNotes','Impact notes',         'TEXT',150,300,0,0,0,0,1,1,NULL,NULL);

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
    WHEN NOT MATCHED BY SOURCE AND t.MaintCode IN ('BOM.HEADER','BOM.CHANGE') THEN DELETE;

    PRINT 'Seeded core.MaintDef / core.MaintColumn (bills of material)';
END
GO
