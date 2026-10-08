/*  40-cst-elements.sql - the costing reference data: the element catalogue every costing
    program reconciles against, the valuation policy per group (D-04), the work centres that
    carry the labour, machine and overhead rates (D-05), and the FY2026 standard cost set.

    Re-runnable (MERGE).  The element list is the backbone of CST-002: a total cost always
    equals the sum of its elements, so an element that is not in this table cannot be posted.

    REQ: CST-002, CST-004, CST-007, CST-011, CST-014, CST-018, CST-026, D-04, D-05
*/
SET NOCOUNT ON;
GO
IF OBJECT_ID(N'cst.CostElement') IS NOT NULL
BEGIN
    DECLARE @e TABLE (ElementCode varchar(10), Name varchar(60), ElementType varchar(12),
                      IsInventoriable bit, SortOrder int);
    INSERT @e VALUES
      ('MATERIAL',  'Material - purchased and issued components',   'MATERIAL', 1,  10),
      ('LABOUR',    'Direct labour at the work centre run rate',    'LABOUR',   1,  20),
      ('MACHINE',   'Machine time at the work centre machine rate', 'MACHINE',  1,  30),
      ('SETUP',     'Setup cost spread over the costing lot',       'LABOUR',   1,  35),
      ('SUBCON',    'Subcontract operations',                       'SUBCON',   1,  40),
      ('OVHDVAR',   'Variable production overhead absorbed',        'OVHDVAR',  1,  50),
      ('OVHDFIX',   'Fixed production overhead on normal capacity', 'OVHDFIX',  1,  60),
      ('LANDED',    'Attributable acquisition charges',             'LANDED',   1,  70),
      ('RECOVERY',  'Overhead recovery credit',                     'RECOVERY', 1,  80),
      ('VARIANCE',  'Cost variances taken to profit and loss',      'VARIANCE', 0,  90),
      /*  WIPCALC and JOBCOST bucket their transactions by these codes.  They are not
          inventoriable: they move value out of work in progress, they do not create it. */
      ('TRANSFER',  'Completion transfer out of work in progress',  'RECOVERY', 0, 100),
      ('MATRETURN', 'Material returned from the job to stores',     'MATERIAL', 0, 110),
      ('SCRAP',     'Scrap written off the job',                    'VARIANCE', 0, 120),
      ('WRITEOFF',  'Work in progress written off on review',       'VARIANCE', 0, 130),
      ('REVENUE',   'Revenue recognised against the job',           'RECOVERY', 0, 140);

    MERGE cst.CostElement AS t USING @e AS s ON t.ElementCode = s.ElementCode
    WHEN MATCHED THEN UPDATE SET Name = s.Name, ElementType = s.ElementType,
            IsInventoriable = s.IsInventoriable, SortOrder = s.SortOrder
    WHEN NOT MATCHED BY TARGET THEN
        INSERT (ElementCode, Name, ElementType, IsInventoriable, SortOrder)
        VALUES (s.ElementCode, s.Name, s.ElementType, s.IsInventoriable, s.SortOrder);

    PRINT 'Seeded cst.CostElement';
END
GO
/*  D-04: purchased stock on moving weighted average, manufactured stock at standard with the
    variances taken to profit and loss, and a FIFO group for the lot-traced chemicals where
    the auditors want to see the layers.  LIFO is not an option and never appears here. */
IF OBJECT_ID(N'cst.CostPolicy') IS NOT NULL
BEGIN
    DECLARE @p TABLE (CompanyCode varchar(10), ValuationGroup varchar(20), EffectiveFrom date,
                      CostMethod varchar(4), PoolBySite bit, PoolByRevision bit,
                      IncludeLandedCost bit, Notes varchar(200));
    INSERT @p VALUES
      ('STC', 'PURCHASED',   '2026-01-01', 'MWA',  0, 0, 1, 'Bought-in parts and consumables (D-04)'),
      ('STC', 'MANUFACTURED','2026-01-01', 'STD',  0, 1, 1, 'Made items valued at the active standard'),
      ('STC', 'CHEMICAL',    '2026-01-01', 'FIFO', 1, 0, 1, 'Lot traced - layers retained for audit'),
      ('STC', 'FINISHED',    '2026-01-01', 'STD',  0, 1, 1, 'Finished goods at standard, variances to P&L'),
      ('BRS', 'PURCHASED',   '2026-01-01', 'MWA',  0, 0, 1, 'Bristol follows group policy'),
      ('BRS', 'MANUFACTURED','2026-01-01', 'STD',  0, 1, 1, 'Bristol follows group policy');

    MERGE cst.CostPolicy AS t
     USING @p AS s ON t.CompanyCode = s.CompanyCode AND t.ValuationGroup = s.ValuationGroup
                  AND t.EffectiveFrom = s.EffectiveFrom
    WHEN MATCHED THEN UPDATE SET CostMethod = s.CostMethod, PoolBySite = s.PoolBySite,
            PoolByRevision = s.PoolByRevision, IncludeLandedCost = s.IncludeLandedCost,
            Notes = s.Notes, ModifiedBy = 'SEED', ModifiedAt = SYSDATETIME()
    WHEN NOT MATCHED BY TARGET THEN
        INSERT (CompanyCode, ValuationGroup, EffectiveFrom, CostMethod, PoolBySite,
                PoolByRevision, IncludeLandedCost, ApprovedBy, Notes, CreatedBy)
        VALUES (s.CompanyCode, s.ValuationGroup, s.EffectiveFrom, s.CostMethod, s.PoolBySite,
                s.PoolByRevision, s.IncludeLandedCost, 'FINCTRL', s.Notes, 'SEED');

    PRINT 'Seeded cst.CostPolicy';
END
GO
/*  D-05: overhead is absorbed on normal capacity, so NormalCapacityHours is stated for every
    centre.  When the hours actually worked fall short, LABOVH reports the under-recovery
    instead of quietly loading it onto the units that were made. */
IF OBJECT_ID(N'cst.WorkCentre') IS NOT NULL
BEGIN
    DECLARE @w TABLE (CompanyCode varchar(10), WorkCentreCode varchar(20), Name varchar(60),
                      SiteCode varchar(10), SetupRatePerHour decimal(19,6), RunRatePerHour decimal(19,6),
                      MachineRatePerHour decimal(19,6), OverheadRatePerHour decimal(19,6),
                      OverheadBasis varchar(10), NormalCapacityHours decimal(19,6), CostCentre varchar(20));
    INSERT @w VALUES
      ('STC', 'ASSY-01',  'Main assembly line',          'MAIN', 22.000000, 18.500000,  0.000000, 12.000000, 'LABOURHR', 1600.000000, 'CC-PROD'),
      ('STC', 'MACH-01',  'CNC machining cell',          'MAIN', 28.000000, 24.000000, 31.500000, 19.750000, 'MACHINEHR', 1450.000000, 'CC-PROD'),
      ('STC', 'WELD-01',  'Fabrication and welding bay', 'MAIN', 26.500000, 21.000000,  8.000000, 14.250000, 'LABOURHR', 1500.000000, 'CC-PROD'),
      ('STC', 'PAINT-01', 'Paint and finishing line',    'MAIN', 19.000000, 16.000000, 11.000000,  9.500000, 'MACHINEHR', 1200.000000, 'CC-FIN'),
      ('STC', 'TEST-01',  'Final test and inspection',   'MAIN', 24.000000, 20.000000,  0.000000, 15.000000, 'PCTCOST',   1100.000000, 'CC-QA'),
      ('BRS', 'ASSY-02',  'Bristol sub-assembly',        'BRS',  20.000000, 17.000000,  0.000000, 10.500000, 'LABOURHR', 1400.000000, 'CC-PROD');

    MERGE cst.WorkCentre AS t
     USING @w AS s ON t.CompanyCode = s.CompanyCode AND t.WorkCentreCode = s.WorkCentreCode
                  AND t.EffectiveFrom = '2026-01-01'
    WHEN MATCHED THEN UPDATE SET Name = s.Name, SiteCode = s.SiteCode,
            SetupRatePerHour = s.SetupRatePerHour, RunRatePerHour = s.RunRatePerHour,
            MachineRatePerHour = s.MachineRatePerHour, OverheadRatePerHour = s.OverheadRatePerHour,
            OverheadBasis = s.OverheadBasis, NormalCapacityHours = s.NormalCapacityHours,
            CostCentre = s.CostCentre, IsActive = 1, ModifiedBy = 'SEED', ModifiedAt = SYSDATETIME()
    WHEN NOT MATCHED BY TARGET THEN
        INSERT (CompanyCode, WorkCentreCode, Name, SiteCode, EffectiveFrom, SetupRatePerHour,
                RunRatePerHour, MachineRatePerHour, OverheadRatePerHour, OverheadBasis,
                NormalCapacityHours, CostCentre, ApprovedBy, IsActive, CreatedBy)
        VALUES (s.CompanyCode, s.WorkCentreCode, s.Name, s.SiteCode, '2026-01-01', s.SetupRatePerHour,
                s.RunRatePerHour, s.MachineRatePerHour, s.OverheadRatePerHour, s.OverheadBasis,
                s.NormalCapacityHours, s.CostCentre, 'FINCTRL', 1, 'SEED');

    PRINT 'Seeded cst.WorkCentre';
END
GO
/*  The cost sets.  FY2026STD is approved and active - it is what the ledger is posted on.
    FY2027DRAFT is the working set for next year's review, and FY2025STD is kept superseded
    so the comparison in STDCOST "COMPARE" has something real to compare against. */
IF OBJECT_ID(N'cst.CostSet') IS NOT NULL
BEGIN
    DECLARE @cs TABLE (CompanyCode varchar(10), CostSetCode varchar(20), Name varchar(80),
                       Purpose varchar(12), EffectiveFrom date, FiscalYear smallint,
                       Status varchar(10), ComparedToSetCode varchar(20), SourceEvidence varchar(200),
                       PreparedBy varchar(20), ReviewedBy varchar(20), ApprovedBy varchar(20));
    INSERT @cs VALUES
      ('STC', 'FY2025STD',   'Standard costs - year to 31 Dec 2025', 'STANDARD', '2025-01-01', 2025, 'SUPERSEDED', NULL,
       'Approved by the board 12 Dec 2024', 'COSTACC', 'FINMGR', 'FINCTRL'),
      ('STC', 'FY2026STD',   'Standard costs - year to 31 Dec 2026', 'STANDARD', '2026-01-01', 2026, 'ACTIVE', 'FY2025STD',
       'Supplier quotations Oct 2025 and agreed labour rates', 'COSTACC', 'FINMGR', 'FINCTRL'),
      ('STC', 'FY2027DRAFT', 'Standard costs - 2027 review (draft)', 'STANDARD', '2027-01-01', 2027, 'DRAFT', 'FY2026STD',
       'Under preparation - supplier quotations being gathered', 'COSTACC', NULL, NULL),
      ('STC', 'QUOTE2026',   'Quotation costing set', 'QUOTE', '2026-01-01', 2026, 'APPROVED', 'FY2026STD',
       'Used by sales for quotations only - never posts', 'COSTACC', 'FINMGR', 'FINCTRL'),
      ('BRS', 'FY2026STD',   'Bristol standard costs 2026', 'STANDARD', '2026-01-01', 2026, 'ACTIVE', NULL,
       'Group rates applied to Bristol volumes', 'COSTACC', 'FINMGR', 'FINCTRL');

    MERGE cst.CostSet AS t
     USING @cs AS s ON t.CompanyCode = s.CompanyCode AND t.CostSetCode = s.CostSetCode
    WHEN MATCHED THEN UPDATE SET Name = s.Name, Purpose = s.Purpose, EffectiveFrom = s.EffectiveFrom,
            FiscalYear = s.FiscalYear, Status = s.Status, ComparedToSetCode = s.ComparedToSetCode,
            SourceEvidence = s.SourceEvidence, PreparedBy = s.PreparedBy, ReviewedBy = s.ReviewedBy,
            ApprovedBy = s.ApprovedBy, ModifiedBy = 'SEED', ModifiedAt = SYSDATETIME()
    WHEN NOT MATCHED BY TARGET THEN
        INSERT (CompanyCode, CostSetCode, Name, Purpose, EffectiveFrom, CurrencyCode, FiscalYear,
                Status, ComparedToSetCode, SourceEvidence, PreparedBy, ReviewedBy, ApprovedBy,
                ApprovedAt, ActivatedAt, CreatedBy)
        VALUES (s.CompanyCode, s.CostSetCode, s.Name, s.Purpose, s.EffectiveFrom, 'GBP', s.FiscalYear,
                s.Status, s.ComparedToSetCode, s.SourceEvidence, s.PreparedBy, s.ReviewedBy, s.ApprovedBy,
                CASE WHEN s.ApprovedBy IS NULL THEN NULL ELSE DATEADD(day, -20, s.EffectiveFrom) END,
                CASE WHEN s.Status IN ('ACTIVE','SUPERSEDED') THEN s.EffectiveFrom ELSE NULL END, 'SEED');

    PRINT 'Seeded cst.CostSet';
END
GO
