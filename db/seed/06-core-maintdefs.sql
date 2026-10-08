/* ---------------------------------------------------------------------------
   06-core-maintdefs - generic file-maintenance definitions for the simple core
   master files.  The WinForms client turns each of these into a list + edit
   screen (frmGenericMaint) with no hand written code.  Columns that the owning
   module has not created yet are filtered out by core.usp_UiMaintDef, so this
   seed is safe to run at any point of the build.          Re-runnable (MERGE).
   --------------------------------------------------------------------------- */
SET NOCOUNT ON;
GO
IF OBJECT_ID(N'core.MaintDef') IS NOT NULL AND OBJECT_ID(N'core.MaintColumn') IS NOT NULL
BEGIN
    DECLARE @d table (MaintCode varchar(30), Title varchar(80), ModuleCode varchar(10),
                      SchemaName sysname, TableName sysname, CompanyScoped bit,
                      RetireColumn sysname NULL, PermissionCode varchar(60) NULL, HelpText varchar(400) NULL);
    INSERT @d VALUES
      ('CORE.COMPANY',  'Companies',             'common', 'core', 'Company',      0, 'IsActive', 'SYS.COMPANY.MAINT',  'Legal entities. Retiring a company hides it from new transactions.'),
      ('CORE.SITE',     'Sites',                 'common', 'core', 'Site',         1, 'IsActive', 'SYS.SITE.MAINT',     'Warehouses and offices within a company.'),
      ('CORE.USER',     'Users',                 'common', 'core', 'AppUser',      0, 'IsActive', 'SYS.USER.MAINT',     'STERP user accounts mapped to Windows logins.'),
      ('CORE.ROLE',     'Roles',                 'common', 'core', 'Role',         0, 'IsActive', 'SYS.ROLE.MAINT',     'Security roles; permissions are granted to roles.'),
      ('CORE.USERROLE', 'User roles',            'common', 'core', 'UserRole',     1, NULL,       'SYS.ROLE.MAINT',     'Which role a user holds in a company (blank site = all sites).'),
      ('CORE.PERM',     'Permissions',           'common', 'core', 'Permission',   0, NULL,       'SYS.ROLE.MAINT',     'Permission codes such as AR.RECEIPT.POST.'),
      ('CORE.PARAM',    'System parameters',     'common', 'core', 'Parameter',    1, NULL,       'SYS.PARAM.MAINT',    'Effective dated parameters. Company * applies to every company.'),
      ('CORE.CURRENCY', 'Currencies',            'common', 'core', 'Currency',     0, 'IsActive', 'SYS.FX.MAINT',       'Currencies accepted by the system. Base currency is GBP.'),
      ('CORE.FXRATE',   'Exchange rates',        'common', 'core', 'FxRate',       0, NULL,       'SYS.FX.MAINT',       'Foreign units per GBP 1 (D-14).'),
      ('CORE.PAYTERMS', 'Payment terms',         'common', 'core', 'PaymentTerms', 0, 'IsActive', 'SYS.PARAM.MAINT',    'Settlement terms used by sales and purchase documents.'),
      ('CORE.CALENDAR', 'Calendars',             'common', 'core', 'Calendar',     0, 'IsActive', 'SYS.CALENDAR.MAINT', 'Working day calendars: ENG, SCT, NIR, BACS.'),
      ('CORE.HOLIDAY',  'Calendar holidays',     'common', 'core', 'Holiday',      0, NULL,       'SYS.CALENDAR.MAINT', 'Non working days of a calendar.'),
      ('CORE.NUMSER',   'Number series',         'common', 'core', 'NumberSeries', 1, 'IsActive', 'SYS.NUMBER.MAINT',   'Document numbering (COM-005).'),
      ('CORE.MAINTDEF', 'Maintenance screens',   'common', 'core', 'MaintDef',     0, 'IsActive', 'SYS.ADMIN',          'The generic file maintenance registry itself.'),
      ('CORE.MAINTCOL', 'Maintenance columns',   'common', 'core', 'MaintColumn',  0, NULL,       'SYS.ADMIN',          'Columns of a generic file maintenance screen.');

    MERGE core.MaintDef AS t USING @d AS s ON t.MaintCode = s.MaintCode
    WHEN MATCHED THEN UPDATE SET Title = s.Title, ModuleCode = s.ModuleCode, SchemaName = s.SchemaName,
            TableName = s.TableName, CompanyScoped = s.CompanyScoped, RetireColumn = s.RetireColumn,
            PermissionCode = s.PermissionCode,
            HelpText = s.HelpText, IsActive = 1, ModifiedBy = 'SEED', ModifiedAt = SYSDATETIME()
    WHEN NOT MATCHED BY TARGET THEN
        INSERT (MaintCode, Title, ModuleCode, SchemaName, TableName, CompanyScoped, RetireColumn, PermissionCode, HelpText, CreatedBy)
        VALUES (s.MaintCode, s.Title, s.ModuleCode, s.SchemaName, s.TableName, s.CompanyScoped, s.RetireColumn, s.PermissionCode, s.HelpText, 'SEED');

    DECLARE @c table (MaintCode varchar(30), ColumnName sysname, Caption varchar(60), DataType varchar(10),
                      SortOrder int, Width int, IsKey bit, IsRequired bit, IsReadOnly bit,
                      IsInList bit, IsInEdit bit, IsSearchable bit, LookupProc varchar(128) NULL, LookupArg varchar(60) NULL);
    INSERT @c VALUES
      -- Companies -------------------------------------------------------------
      ('CORE.COMPANY','CompanyCode','Company',            'CODE', 10, 80,1,1,0,1,1,1,NULL,NULL),
      ('CORE.COMPANY','Name','Registered name',           'TEXT', 20,240,0,1,0,1,1,1,NULL,NULL),
      ('CORE.COMPANY','RegisteredNumber','Company number','TEXT', 30,120,0,0,0,1,1,1,NULL,NULL),
      ('CORE.COMPANY','VatRegNo','VAT registration',      'TEXT', 40,130,0,0,0,1,1,1,NULL,NULL),
      ('CORE.COMPANY','Eori','EORI number',               'TEXT', 50,130,0,0,0,0,1,0,NULL,NULL),
      ('CORE.COMPANY','AccountingFramework','Framework',  'TEXT', 60,110,0,0,0,0,1,0,NULL,NULL),
      ('CORE.COMPANY','AccountingRefDate','Ref. date',    'DATE', 70,100,0,0,0,0,1,0,NULL,NULL),
      ('CORE.COMPANY','VatStagger','VAT stagger',         'TEXT', 80, 90,0,0,0,0,1,0,NULL,NULL),
      ('CORE.COMPANY','VatScheme','VAT scheme',           'TEXT', 90,110,0,0,0,1,1,0,NULL,NULL),
      ('CORE.COMPANY','BaseCurrency','Base currency',     'CODE',100, 90,0,0,0,1,1,0,'core.usp_UiLookup','CORE.CURRENCY'),
      ('CORE.COMPANY','IsActive','In use',                'BOOL',110, 60,0,0,0,1,1,0,NULL,NULL),
      -- Sites -----------------------------------------------------------------
      ('CORE.SITE','CompanyCode','Company',               'CODE', 10, 80,1,1,0,1,1,1,'core.usp_UiLookup','CORE.COMPANY'),
      ('CORE.SITE','SiteCode','Site',                     'CODE', 20, 80,1,1,0,1,1,1,NULL,NULL),
      ('CORE.SITE','Name','Site name',                    'TEXT', 30,220,0,1,0,1,1,1,NULL,NULL),
      ('CORE.SITE','Nation','Nation',                     'CODE', 40, 70,0,0,0,1,1,0,NULL,NULL),
      ('CORE.SITE','CalendarCode','Calendar',             'CODE', 50, 90,0,0,0,1,1,0,'core.usp_UiLookup','CORE.CALENDAR'),
      ('CORE.SITE','IsActive','In use',                   'BOOL', 60, 60,0,0,0,1,1,0,NULL,NULL),
      -- Users -----------------------------------------------------------------
      ('CORE.USER','UserCode','User',                     'CODE', 10, 90,1,1,0,1,1,1,NULL,NULL),
      ('CORE.USER','DisplayName','Name',                  'TEXT', 20,200,0,1,0,1,1,1,NULL,NULL),
      ('CORE.USER','WindowsLogin','Windows login',        'TEXT', 30,200,0,0,0,1,1,1,NULL,NULL),
      ('CORE.USER','EmailAddress','E-mail',               'TEXT', 40,200,0,0,0,1,1,1,NULL,NULL),
      ('CORE.USER','JobTitle','Job title',                'TEXT', 50,180,0,0,0,1,1,1,NULL,NULL),
      ('CORE.USER','DefaultCompanyCode','Default company','CODE', 60,110,0,0,0,1,1,0,'core.usp_UiLookup','CORE.COMPANY'),
      ('CORE.USER','DefaultSiteCode','Default site',      'CODE', 70, 90,0,0,0,1,1,0,NULL,NULL),
      ('CORE.USER','IsReadOnly','Read only',              'BOOL', 80, 70,0,0,0,1,1,0,NULL,NULL),
      ('CORE.USER','IsActive','In use',                   'BOOL', 90, 60,0,0,0,1,1,0,NULL,NULL),
      -- Roles -----------------------------------------------------------------
      ('CORE.ROLE','RoleCode','Role',                     'CODE', 10,100,1,1,0,1,1,1,NULL,NULL),
      ('CORE.ROLE','Name','Name',                         'TEXT', 20,200,0,0,0,1,1,1,NULL,NULL),
      ('CORE.ROLE','Description','Description',           'TEXT', 30,300,0,0,0,1,1,1,NULL,NULL),
      ('CORE.ROLE','IsActive','In use',                   'BOOL', 40, 60,0,0,0,1,1,0,NULL,NULL),
      -- User roles ------------------------------------------------------------
      ('CORE.USERROLE','UserCode','User',                 'CODE', 10, 90,1,1,0,1,1,1,'core.usp_UiLookup','CORE.USER'),
      ('CORE.USERROLE','RoleCode','Role',                 'CODE', 20,100,1,1,0,1,1,1,'core.usp_UiLookup','CORE.ROLE'),
      ('CORE.USERROLE','CompanyCode','Company',           'CODE', 30, 80,1,1,0,1,1,1,'core.usp_UiLookup','CORE.COMPANY'),
      ('CORE.USERROLE','SiteCode','Site (blank = all)',   'CODE', 40, 90,0,0,0,1,1,0,NULL,NULL),
      ('CORE.USERROLE','ValidFrom','Valid from',          'DATE', 50,100,0,0,0,1,1,0,NULL,NULL),
      ('CORE.USERROLE','ValidTo','Valid to',              'DATE', 60,100,0,0,0,1,1,0,NULL,NULL),
      -- Permissions -----------------------------------------------------------
      ('CORE.PERM','PermissionCode','Permission',         'CODE', 10,200,1,1,0,1,1,1,NULL,NULL),
      ('CORE.PERM','Description','Description',           'TEXT', 20,300,0,0,0,1,1,1,NULL,NULL),
      ('CORE.PERM','Name','Name',                         'TEXT', 25,220,0,0,0,1,1,1,NULL,NULL),
      ('CORE.PERM','IsSensitive','Sensitive',             'BOOL', 30, 70,0,0,0,1,1,0,NULL,NULL),
      -- Parameters ------------------------------------------------------------
      ('CORE.PARAM','ParamCode','Parameter',              'CODE', 10,180,1,1,0,1,1,1,NULL,NULL),
      ('CORE.PARAM','CompanyCode','Company (* = all)',    'CODE', 20, 90,1,1,0,1,1,1,NULL,NULL),
      ('CORE.PARAM','EffectiveFrom','Effective from',     'DATE', 30,110,1,1,0,1,1,0,NULL,NULL),
      ('CORE.PARAM','Value','Value',                      'TEXT', 40,200,0,1,0,1,1,1,NULL,NULL),
      ('CORE.PARAM','DataType','Data type',               'CODE', 50, 90,0,0,0,1,1,0,NULL,NULL),
      ('CORE.PARAM','Description','Description',          'TEXT', 60,280,0,0,0,1,1,1,NULL,NULL),
      ('CORE.PARAM','DecisionRef','Decision',             'TEXT', 70, 90,0,0,0,1,1,0,NULL,NULL),
      -- Currencies ------------------------------------------------------------
      ('CORE.CURRENCY','CurrencyCode','Currency',         'CODE', 10, 80,1,1,0,1,1,1,NULL,NULL),
      ('CORE.CURRENCY','Name','Name',                     'TEXT', 20,200,0,1,0,1,1,1,NULL,NULL),
      ('CORE.CURRENCY','Symbol','Symbol',                 'TEXT', 30, 60,0,0,0,1,1,0,NULL,NULL),
      ('CORE.CURRENCY','MinorUnits','Decimals',           'INT',  40, 70,0,0,0,1,1,0,NULL,NULL),
      ('CORE.CURRENCY','IsActive','In use',               'BOOL', 50, 60,0,0,0,1,1,0,NULL,NULL),
      -- FX rates --------------------------------------------------------------
      ('CORE.FXRATE','CurrencyCode','Currency',           'CODE', 10, 80,1,1,0,1,1,1,'core.usp_UiLookup','CORE.CURRENCY'),
      ('CORE.FXRATE','RateDate','Rate date',              'DATE', 20,100,1,1,0,1,1,0,NULL,NULL),
      ('CORE.FXRATE','RateType','Type',                   'CODE', 30, 80,1,1,0,1,1,1,NULL,NULL),
      ('CORE.FXRATE','UnitsPerGBP','Units per GBP 1',     'RATE', 40,130,0,1,0,1,1,0,NULL,NULL),
      ('CORE.FXRATE','Source','Source',                   'TEXT', 50,120,0,0,0,1,1,0,NULL,NULL),
      -- Payment terms ---------------------------------------------------------
      ('CORE.PAYTERMS','TermsCode','Terms',               'CODE', 10, 90,1,1,0,1,1,1,NULL,NULL),
      ('CORE.PAYTERMS','Name','Name',                     'TEXT', 20,200,0,1,0,1,1,1,NULL,NULL),
      ('CORE.PAYTERMS','Description','Description',       'TEXT', 25,240,0,0,0,1,1,1,NULL,NULL),
      ('CORE.PAYTERMS','BasisType','Basis',               'CODE', 30,120,0,1,0,1,1,0,NULL,NULL),
      ('CORE.PAYTERMS','Days','Days',                     'INT',  40, 60,0,0,0,1,1,0,NULL,NULL),
      ('CORE.PAYTERMS','DayOfMonth','Day of month',       'INT',  50, 90,0,0,0,1,1,0,NULL,NULL),
      ('CORE.PAYTERMS','DiscountDays','Disc. days',       'INT',  60, 80,0,0,0,1,1,0,NULL,NULL),
      ('CORE.PAYTERMS','DiscountPct','Discount %',        'PCT',  70, 90,0,0,0,1,1,0,NULL,NULL),
      ('CORE.PAYTERMS','IsActive','In use',               'BOOL', 80, 60,0,0,0,1,1,0,NULL,NULL),
      -- Calendars -------------------------------------------------------------
      ('CORE.CALENDAR','CalendarCode','Calendar',         'CODE', 10, 90,1,1,0,1,1,1,NULL,NULL),
      ('CORE.CALENDAR','Name','Name',                     'TEXT', 20,240,0,1,0,1,1,1,NULL,NULL),
      ('CORE.CALENDAR','Nation','Nation',                 'CODE', 25, 70,0,0,0,1,1,0,NULL,NULL),
      ('CORE.CALENDAR','CalendarType','Type',             'CODE', 27,100,0,0,0,1,1,0,NULL,NULL),
      ('CORE.CALENDAR','IsActive','In use',               'BOOL', 30, 60,0,0,0,1,1,0,NULL,NULL),
      -- Holidays --------------------------------------------------------------
      ('CORE.HOLIDAY','CalendarCode','Calendar',          'CODE', 10, 90,1,1,0,1,1,1,'core.usp_UiLookup','CORE.CALENDAR'),
      ('CORE.HOLIDAY','HolidayDate','Date',               'DATE', 20,100,1,1,0,1,1,0,NULL,NULL),
      ('CORE.HOLIDAY','Name','Description',               'TEXT', 30,240,0,0,0,1,1,1,NULL,NULL),
      ('CORE.HOLIDAY','IsSubstituteDay','Substitute day', 'BOOL', 35,100,0,0,0,1,1,0,NULL,NULL),
      -- Number series ---------------------------------------------------------
      ('CORE.NUMSER','CompanyCode','Company',             'CODE', 10, 80,1,1,0,1,1,1,'core.usp_UiLookup','CORE.COMPANY'),
      ('CORE.NUMSER','SeriesCode','Series',               'CODE', 20,110,1,1,0,1,1,1,NULL,NULL),
      ('CORE.NUMSER','DocumentType','Document type',      'CODE', 25,110,0,0,0,1,1,1,NULL,NULL),
      ('CORE.NUMSER','Description','Description',         'TEXT', 30,220,0,0,0,1,1,1,NULL,NULL),
      ('CORE.NUMSER','Prefix','Prefix',                   'TEXT', 40, 80,0,0,0,1,1,0,NULL,NULL),
      ('CORE.NUMSER','NextNumber','Next number',          'INT',  50,100,0,0,0,1,1,0,NULL,NULL),
      ('CORE.NUMSER','NumberLength','Length',             'INT',  60, 70,0,0,0,1,1,0,NULL,NULL),
      ('CORE.NUMSER','IsLegalSequence','Legal sequence',  'BOOL', 70,100,0,0,0,1,1,0,NULL,NULL),
      ('CORE.NUMSER','IsActive','In use',                 'BOOL', 80, 60,0,0,0,1,1,0,NULL,NULL),
      -- The maintenance registry maintains itself ------------------------------
      ('CORE.MAINTDEF','MaintCode','Screen',              'CODE', 10,120,1,1,0,1,1,1,NULL,NULL),
      ('CORE.MAINTDEF','Title','Title',                   'TEXT', 20,200,0,1,0,1,1,1,NULL,NULL),
      ('CORE.MAINTDEF','ModuleCode','Module',             'CODE', 30, 70,0,1,0,1,1,1,NULL,NULL),
      ('CORE.MAINTDEF','SchemaName','Schema',             'CODE', 40, 70,0,1,0,1,1,0,NULL,NULL),
      ('CORE.MAINTDEF','TableName','Table',               'CODE', 50,130,0,1,0,1,1,1,NULL,NULL),
      ('CORE.MAINTDEF','CompanyScoped','By company',      'BOOL', 60, 80,0,0,0,1,1,0,NULL,NULL),
      ('CORE.MAINTDEF','RetireColumn','Retire flag',      'CODE', 70,100,0,0,0,1,1,0,NULL,NULL),
      ('CORE.MAINTDEF','PermissionCode','Permission',     'CODE', 80,150,0,0,0,1,1,1,NULL,NULL),
      ('CORE.MAINTDEF','HelpText','Help text',            'TEXT', 90,300,0,0,0,0,1,0,NULL,NULL),
      ('CORE.MAINTDEF','IsActive','In use',               'BOOL',100, 60,0,0,0,1,1,0,NULL,NULL),
      ('CORE.MAINTCOL','MaintCode','Screen',              'CODE', 10,120,1,1,0,1,1,1,'core.usp_UiLookup','CORE.MAINTDEF'),
      ('CORE.MAINTCOL','ColumnName','Column',             'CODE', 20,140,1,1,0,1,1,1,NULL,NULL),
      ('CORE.MAINTCOL','Caption','Caption',               'TEXT', 30,160,0,1,0,1,1,1,NULL,NULL),
      ('CORE.MAINTCOL','DataType','Data type',            'CODE', 40, 80,0,1,0,1,1,0,NULL,NULL),
      ('CORE.MAINTCOL','SortOrder','Order',               'INT',  50, 60,0,0,0,1,1,0,NULL,NULL),
      ('CORE.MAINTCOL','Width','Width',                   'INT',  60, 60,0,0,0,1,1,0,NULL,NULL),
      ('CORE.MAINTCOL','IsKey','Key',                     'BOOL', 70, 50,0,0,0,1,1,0,NULL,NULL),
      ('CORE.MAINTCOL','IsRequired','Required',           'BOOL', 80, 70,0,0,0,1,1,0,NULL,NULL),
      ('CORE.MAINTCOL','IsReadOnly','Read only',          'BOOL', 90, 70,0,0,0,1,1,0,NULL,NULL),
      ('CORE.MAINTCOL','IsInList','In list',              'BOOL',100, 60,0,0,0,1,1,0,NULL,NULL),
      ('CORE.MAINTCOL','IsInEdit','In edit',              'BOOL',110, 60,0,0,0,1,1,0,NULL,NULL),
      ('CORE.MAINTCOL','IsSearchable','Searchable',       'BOOL',120, 80,0,0,0,1,1,0,NULL,NULL),
      ('CORE.MAINTCOL','LookupProc','Lookup procedure',   'TEXT',130,160,0,0,0,0,1,0,NULL,NULL),
      ('CORE.MAINTCOL','LookupArg','Lookup argument',     'TEXT',140,120,0,0,0,0,1,0,NULL,NULL),
      ('CORE.MAINTCOL','DefaultValue','Default',          'TEXT',150,100,0,0,0,0,1,0,NULL,NULL);

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
                s.IsRequired, s.IsReadOnly, s.IsInList, s.IsInEdit, s.IsSearchable, s.LookupProc, s.LookupArg);

    PRINT 'Seeded core.MaintDef / core.MaintColumn';
END
GO
