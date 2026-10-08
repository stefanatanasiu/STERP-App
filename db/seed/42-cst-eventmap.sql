/* ---------------------------------------------------------------------------
   42-cst-eventmap - the costing postings that the Finance seed (10-fin-eventmap.sql
   and 11-fin-eventmap.sql, never edited here) does not already map.  Finance maps
   CST.STDREVALUE, CST.WIPISSUE, CST.WIPLABOUR, CST.WIPOVERHEAD, CST.WIPSUBCON,
   CST.COMPLETION, CST.JOBCLOSE, the CST.VAR* family and INV.NRVWRITEDOWN; what had
   no mapping was the reversal of a write down, the landed cost settlement, the
   purchase and invoice price variances raised at standard, the valuation of a
   return after a standard change, the transfer of cost between jobs, a WIP write
   off and the period end disposal of under or over absorbed overhead.

   An unmapped posting is blocked rather than sent to a suspense account
   (fin.usp_PostJournal, STERP-E-0014), so every event the costing engine can raise
   has to exist here before the engine is allowed to post it.

   Re-runnable: each row is inserted only when it is not already there.
   REQ: CST-005, CST-006, CST-008, CST-010, CST-011, CST-017, CST-018, CST-021,
        CST-025, FIN-003, D-04, D-05
   --------------------------------------------------------------------------- */
SET NOCOUNT ON;
GO
IF OBJECT_ID(N'fin.EventAccountMap') IS NOT NULL
BEGIN
    DECLARE @e table (EventCode varchar(30), Module varchar(6), EventName varchar(100));
    INSERT @e VALUES
     ('CST.NRVREVERSAL','CST','Net realisable value write down reversed, capped at the original'),
     ('CST.LANDEDADJ','CST','Landed cost settled and absorbed into the stock value'),
     ('CST.VARPPV','CST','Purchase price variance at receipt against standard'),
     ('CST.VARINVPRICE','CST','Invoice price variance, incremental to the receipt variance'),
     ('CST.VARLANDED','CST','Landed cost variance - settlement differs from the estimate'),
     ('CST.VARRETURN','CST','Return valued at the standard that applied when it was sold'),
     ('CST.WIPTRANSFER','CST','Cost transferred between jobs'),
     ('CST.WIPWRITEOFF','CST','Work in progress written off on an abandoned job'),
     ('CST.OVHDISPOSAL','CST','Under or over absorbed overhead taken to profit at period end');

    DECLARE @l table (EventCode varchar(30), LegCode varchar(20), LegDescription varchar(100), DrCr char(2),
                      AccountCode varchar(12), CC varchar(20) NULL, PROJ varchar(20) NULL,
                      IsConditional bit, Cond varchar(60) NULL, SortOrder int);
    INSERT @l VALUES
     /*  CST-025.  A reversal releases the allowance and credits the same charge line that
         created it, so the cumulative charge in the profit and loss account can never go
         below zero.  The cap itself is enforced by NRVTEST (CST1105), not by the map.     */
     ('CST.NRVREVERSAL','ALLOW','Stock allowance, net realisable value','DR','1150',NULL,NULL,0,NULL,1),
     ('CST.NRVREVERSAL','CHARGE','Stock write down to net realisable value','CR','5310',NULL,NULL,0,NULL,2),
     /*  CST-008.  Freight, duty and handling become part of the stock value when they are
         settled; the absorption account is cleared, it is not a second cost.               */
     ('CST.LANDEDADJ','STOCK','Stock, raw materials and components','DR','1100',NULL,NULL,0,NULL,1),
     ('CST.LANDEDADJ','ABSORB','Landed cost absorbed','CR','5200',NULL,NULL,0,NULL,2),
     /*  CST-005 and worked example C3.  Stock goes in at standard, the difference against the
         order price is a variance of the period, and the goods received account carries the
         full ordered value until the supplier invoice arrives.                              */
     ('CST.VARPPV','VARIANCE','Purchase price variance','DR','5100',NULL,NULL,0,NULL,1),
     ('CST.VARPPV','GRNI','Goods received not invoiced','CR','2010',NULL,NULL,0,NULL,2),
     /*  CST-006.  Only the increment between the invoice and the receipt is taken, so the
         total adverse variance on C3 is 120.00 and not 220.00.                              */
     ('CST.VARINVPRICE','VARIANCE','Purchase price variance, invoice increment','DR','5100',NULL,NULL,0,NULL,1),
     ('CST.VARINVPRICE','GRNI','Goods received not invoiced','CR','2010',NULL,NULL,0,NULL,2),
     /*  CST-008.  When the carrier or the agent settles for more than was estimated the
         difference is a variance, not a quiet re-opening of the stock value.                */
     ('CST.VARLANDED','VARIANCE','Landed cost variance','DR','5100',NULL,NULL,0,NULL,1),
     ('CST.VARLANDED','ABSORB','Landed cost absorbed','CR','5200',NULL,NULL,0,NULL,2),
     /*  CST-011 and worked example C7.  A return comes back into stock at the current
         standard; the difference against the cost of sales originally relieved is the
         variance, so cost of sales is relieved by exactly what it was charged.              */
     ('CST.VARRETURN','STOCK','Stock returned at the current standard','DR','1100',NULL,NULL,0,NULL,1),
     ('CST.VARRETURN','VARIANCE','Return valuation variance','DR','5150',NULL,NULL,1,'AMOUNT<>0',2),
     ('CST.VARRETURN','COGS','Cost of goods sold, components','CR','5010',NULL,NULL,0,NULL,3),
     /*  CST-018.  Moving cost between jobs is two WIP legs and nothing else - it must never
         touch the profit and loss account, or a re-coding would look like production.       */
     ('CST.WIPTRANSFER','WIPIN','Work in progress, receiving job','DR','1110',NULL,NULL,0,NULL,1),
     ('CST.WIPTRANSFER','WIPOUT','Work in progress, giving job','CR','1110',NULL,NULL,0,NULL,2),
     /*  CST-017.  An abandoned job is a loss of the period.  Finance already maps the orderly
         close (CST.JOBCLOSE); this is the disorderly one.                                   */
     ('CST.WIPWRITEOFF','LOSS','Work in progress written off','DR','5520','PROD',NULL,0,NULL,1),
     ('CST.WIPWRITEOFF','WIP','Work in progress','CR','1110',NULL,NULL,0,NULL,2),
     /*  CST-010 and D-05.  Overhead is absorbed at a rate set on normal capacity, so there is
         always a balance left on the absorption account at period end.  It goes to profit;
         it is never spread back over the stock to make the rate look right.                 */
     ('CST.OVHDISPOSAL','VARIANCE','Overhead absorption variance','DR','5140','PROD',NULL,0,NULL,1),
     ('CST.OVHDISPOSAL','ABSORBED','Production overhead absorbed','CR','5510','PROD',NULL,0,NULL,2);

    INSERT fin.EventAccountMap (CompanyCode, EventCode, LegCode, Module, EventName, LegDescription, DrCr, AccountCode,
                                DimCostCentre, DimProject, IsConditional, Condition, SortOrder, IsActive, CreatedBy)
    SELECT c.CompanyCode, l.EventCode, l.LegCode, e.Module, e.EventName, l.LegDescription, l.DrCr, l.AccountCode,
           l.CC, l.PROJ, l.IsConditional, l.Cond, l.SortOrder, 1, 'SEED'
      FROM core.Company c CROSS JOIN @l l JOIN @e e ON e.EventCode = l.EventCode
     WHERE NOT EXISTS (SELECT 1 FROM fin.EventAccountMap m WHERE m.CompanyCode = c.CompanyCode
                         AND m.EventCode = l.EventCode AND m.LegCode = l.LegCode);

    PRINT 'Seeded fin.EventAccountMap (CST costing events)';
END
GO
/* ---------------------------------------------------------------------------
   Generic maintenance definitions.  41-cst-menu.sql already points frmGenericMaint
   at CST.RATES, CST.ELEMENT, CST.POLICY, CST.LANDED and CST.NRV; without these rows
   the menu items open an empty grid.  Simple masters are maintained generically so
   that a new column does not mean a new form (docs\NET-FRAMEWORK.md).
                                                            Re-runnable (MERGE).
   REQ: CST-002, CST-003, CST-004, CST-008, CST-021, CST-026, D-04, D-05
   --------------------------------------------------------------------------- */
IF OBJECT_ID(N'core.MaintDef') IS NOT NULL AND OBJECT_ID(N'core.MaintColumn') IS NOT NULL
BEGIN
    DECLARE @d table (MaintCode varchar(30), Title varchar(80), ModuleCode varchar(10),
                      SchemaName sysname, TableName sysname, CompanyScoped bit,
                      RetireColumn sysname NULL, PermissionCode varchar(60) NULL, HelpText varchar(400) NULL);
    INSERT @d VALUES
      /* PermissionCode must be a code that exists in core.Permission (02-core-permissions.sql):
         core.usp_UiMaintDef / core.usp_UiMaintList ask core.usp_UiCheckAccess, and a code the
         security module cannot answer for is denied (COM-002), which leaves the screen with no
         title, no columns and no rows.  These are the same codes the menu items use (41-cst-menu). */
      ('CST.ELEMENT','Cost elements',       'cst','cst','CostElement', 0, NULL,      'CST.RATE.MAINT',
       'The cost element catalogue (CST-002). Only inventoriable elements may be carried in stock.'),
      ('CST.POLICY', 'Costing policy',      'cst','cst','CostPolicy',  1, NULL,      'CST.RATE.MAINT',
       'Valuation method per valuation group (D-04). LIFO is prohibited; changing a method needs a conversion date.'),
      ('CST.RATES',  'Work centre rates',   'cst','cst','WorkCentre',  1, 'IsActive','CST.RATE.MAINT',
       'Labour, machine and overhead rates. Overhead is absorbed on normal capacity hours (D-05).'),
      ('CST.LANDED', 'Landed cost charges', 'cst','cst','LandedCost',  1, NULL,      'CST.LANDED.MAINT',
       'Freight, duty and handling to be allocated over a consignment (CST-008).'),
      ('CST.NRV',    'Impairment tests',    'cst','cst','Impairment',  1, NULL,      'CST.NRV.POST',
       'Net realisable value tests and write downs (CST-021). A reversal can never exceed the original write down.');

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
      -- Cost elements ---------------------------------------------------------
      ('CST.ELEMENT','ElementCode','Element',            'TEXT', 10, 90,1,1,0,1,1,1,NULL,NULL),
      ('CST.ELEMENT','Name','Name',                      'TEXT', 20,220,0,1,0,1,1,1,NULL,NULL),
      ('CST.ELEMENT','ElementType','Type',               'TEXT', 30,110,0,1,0,1,1,1,NULL,NULL),
      ('CST.ELEMENT','IsInventoriable','Inventoriable',  'BOOL', 40, 90,0,0,0,1,1,0,NULL,NULL),
      ('CST.ELEMENT','SortOrder','Order',                'INT',  50, 70,0,0,0,1,1,0,NULL,NULL),
      -- Costing policy --------------------------------------------------------
      ('CST.POLICY','CompanyCode','Company',             'TEXT', 10, 80,1,1,0,1,1,1,NULL,NULL),
      ('CST.POLICY','ValuationGroup','Valuation group',  'TEXT', 20,140,1,1,0,1,1,1,NULL,NULL),
      ('CST.POLICY','EffectiveFrom','Effective from',    'DATE', 30,100,1,1,0,1,1,0,NULL,NULL),
      ('CST.POLICY','CostMethod','Method',               'TEXT', 40, 70,0,1,0,1,1,1,NULL,NULL),
      ('CST.POLICY','PoolBySite','Pool by site',         'BOOL', 50, 80,0,0,0,1,1,0,NULL,NULL),
      ('CST.POLICY','PoolByRevision','Pool by revision', 'BOOL', 60, 90,0,0,0,1,1,0,NULL,NULL),
      ('CST.POLICY','IncludeLandedCost','Landed in cost','BOOL', 70, 90,0,0,0,1,1,0,NULL,NULL),
      ('CST.POLICY','ConversionDate','Converted on',     'DATE', 80,100,0,0,0,1,1,0,NULL,NULL),
      ('CST.POLICY','ApprovedBy','Approved by',          'TEXT', 90,100,0,0,0,1,1,0,NULL,NULL),
      ('CST.POLICY','Notes','Notes',                     'TEXT',100,240,0,0,0,0,1,0,NULL,NULL),
      -- Work centre rates -----------------------------------------------------
      ('CST.RATES','CompanyCode','Company',              'TEXT', 10, 80,1,1,0,1,1,1,NULL,NULL),
      ('CST.RATES','WorkCentreCode','Work centre',       'TEXT', 20,110,1,1,0,1,1,1,NULL,NULL),
      ('CST.RATES','EffectiveFrom','Effective from',     'DATE', 30,100,1,1,0,1,1,0,NULL,NULL),
      ('CST.RATES','Name','Name',                        'TEXT', 40,200,0,1,0,1,1,1,NULL,NULL),
      ('CST.RATES','SiteCode','Site',                    'TEXT', 50, 70,0,0,0,1,1,1,NULL,NULL),
      ('CST.RATES','SetupRatePerHour','Setup rate',      'MONEY',60,100,0,0,0,1,1,0,NULL,NULL),
      ('CST.RATES','RunRatePerHour','Run rate',          'MONEY',70,100,0,0,0,1,1,0,NULL,NULL),
      ('CST.RATES','MachineRatePerHour','Machine rate',  'MONEY',80,100,0,0,0,1,1,0,NULL,NULL),
      ('CST.RATES','OverheadRatePerHour','Overhead rate','MONEY',90,100,0,0,0,1,1,0,NULL,NULL),
      ('CST.RATES','OverheadBasis','Basis',              'TEXT',100, 90,0,1,0,1,1,0,NULL,NULL),
      ('CST.RATES','NormalCapacityHours','Normal hours', 'DEC', 110,100,0,0,0,1,1,0,NULL,NULL),
      ('CST.RATES','CostCentre','Cost centre',           'TEXT',120, 90,0,0,0,1,1,0,NULL,NULL),
      ('CST.RATES','ApprovedBy','Approved by',           'TEXT',130, 90,0,0,0,0,1,0,NULL,NULL),
      -- Landed cost charges ---------------------------------------------------
      ('CST.LANDED','LandedCostId','Id',                 'INT',  10, 70,1,0,1,1,0,0,NULL,NULL),
      ('CST.LANDED','CompanyCode','Company',             'TEXT', 20, 80,0,1,0,1,1,1,NULL,NULL),
      ('CST.LANDED','ChargeRef','Charge reference',      'TEXT', 30,120,0,1,0,1,1,1,NULL,NULL),
      ('CST.LANDED','ShipmentRef','Shipment',            'TEXT', 40,110,0,0,0,1,1,1,NULL,NULL),
      ('CST.LANDED','Category','Category',               'TEXT', 50,100,0,1,0,1,1,1,NULL,NULL),
      ('CST.LANDED','AllocationBasis','Basis',           'TEXT', 60, 90,0,1,0,1,1,0,NULL,NULL),
      ('CST.LANDED','IsEligibleToCapitalise','Capitalise','BOOL',70, 90,0,0,0,1,1,0,NULL,NULL),
      ('CST.LANDED','EstimatedAmountGBP','Estimated',    'MONEY',80,110,0,0,0,1,1,0,NULL,NULL),
      ('CST.LANDED','ActualAmountGBP','Actual',          'MONEY',90,110,0,0,0,1,1,0,NULL,NULL),
      ('CST.LANDED','AllocatedGBP','Allocated',          'MONEY',100,110,0,0,1,1,0,0,NULL,NULL),
      ('CST.LANDED','Status','Status',                   'TEXT',110, 90,0,1,0,1,1,1,NULL,NULL),
      -- Impairment tests ------------------------------------------------------
      ('CST.NRV','ImpairmentId','Id',                    'INT',  10, 70,1,0,1,1,0,0,NULL,NULL),
      ('CST.NRV','CompanyCode','Company',                'TEXT', 20, 80,0,1,0,1,1,1,NULL,NULL),
      ('CST.NRV','ItemCode','Item',                      'TEXT', 30,160,0,1,0,1,1,1,NULL,NULL),
      ('CST.NRV','AssessmentDate','Assessed on',         'DATE', 40,100,0,1,0,1,1,0,NULL,NULL),
      ('CST.NRV','Quantity','Quantity',                  'DEC',  50,110,0,1,0,1,1,0,NULL,NULL),
      ('CST.NRV','CarryingUnitCost','Unit cost',         'MONEY',60,110,0,1,0,1,1,0,NULL,NULL),
      ('CST.NRV','NrvUnitValue','Unit NRV',              'MONEY',70,110,0,1,0,1,1,0,NULL,NULL),
      ('CST.NRV','WriteDownGBP','Write down',            'MONEY',80,110,0,0,1,1,0,0,NULL,NULL),
      ('CST.NRV','IsReversal','Reversal',                'BOOL', 90, 80,0,0,1,1,0,0,NULL,NULL),
      ('CST.NRV','ReasonCode','Reason',                  'TEXT',100,110,0,0,0,1,1,1,NULL,NULL),
      ('CST.NRV','Evidence','Evidence',                  'TEXT',110,240,0,0,0,0,1,0,NULL,NULL);

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

    PRINT 'Seeded core.MaintDef / core.MaintColumn (costing masters)';
END
GO

