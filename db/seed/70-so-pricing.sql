/*  Sales price lists, quantity breaks and rebate agreements for both companies.  The precedence
    PRICING walks is level 2 contract, 3 customer, 4 group, 5 standard, and within a level the
    largest quantity break the order reaches (SO-004).  The data is laid out so a demonstration
    can show all four levels on one item: FG-FRAME-ROAD-SL-54 is on the standard list, on the
    trade and distributor group lists, on Pennine Cycles' own customer list and on the Caxton
    OEM contract.  Prices are ex-VAT in the list currency; a GBP list never carries foreign
    currency lines, so the engine cannot mix currencies inside one list.
    Re-runnable (MERGE on the business key).  REQ: SO-004, SO-005, SO-006, SO-027, D-12 */
SET NOCOUNT ON;
GO
DECLARE @from date = '2026-04-01', @to date = '2027-03-31';

MERGE so.PriceList AS t
USING (VALUES
 ('STC','STD26',   'Stellar standard trade list 2026/27',      5,'STANDARD','GBP',NULL,   NULL,     NULL),
 ('STC','TRADE26', 'Stellar trade dealer list 2026/27',        4,'GROUP',   'GBP',NULL,   'TRADE26',NULL),
 ('STC','DIST26',  'Stellar distributor list 2026/27',         4,'GROUP',   'GBP',NULL,   'DIST26', NULL),
 ('STC','OEM26',   'Stellar OEM volume list 2026/27',          4,'GROUP',   'GBP',NULL,   'OEM26',  NULL),
 ('STC','CUSTC0001','Pennine Cycles negotiated list 2026/27',  3,'CUSTOMER','GBP','C0001',NULL,     NULL),
 ('STC','EURC0021','Velo Atlantique euro list 2026/27',        3,'CUSTOMER','EUR','C0021',NULL,     NULL),
 ('STC','USDC0027','Cascade Cycle dollar list 2026/27',        3,'CUSTOMER','USD','C0027',NULL,     NULL),
 ('STC','CONTC0007','Caxton OEM supply agreement STC/2026/07', 2,'CONTRACT','GBP','C0007',NULL,     'STC/2026/07'),
 ('CAL','CALSTD26','Caledon standard list 2026/27',            5,'STANDARD','GBP',NULL,   NULL,     NULL),
 ('CAL','CALTRD26','Caledon assembly trade list 2026/27',      4,'GROUP',   'GBP',NULL,   'TRADE26',NULL),
 ('CAL','CONTK0001','Clyde Cycle Co supply agreement CAL/26/1',2,'CONTRACT','GBP','K0001',NULL,     'CAL/2026/01')
) AS s (CompanyCode, PriceListCode, Name, PriceLevel, Scope, CurrencyCode, CustomerCode, PriceGroup, ContractRef)
ON t.CompanyCode = s.CompanyCode AND t.PriceListCode = s.PriceListCode
WHEN MATCHED THEN UPDATE SET Name = s.Name, PriceLevel = s.PriceLevel, Scope = s.Scope,
        CurrencyCode = s.CurrencyCode, CustomerCode = s.CustomerCode, PriceGroup = s.PriceGroup,
        ContractRef = s.ContractRef, ValidFrom = @from, ValidTo = @to, IsActive = 1,
        ModifiedBy = 'SEED', ModifiedAt = SYSDATETIME()
WHEN NOT MATCHED THEN
    INSERT (CompanyCode, PriceListCode, Name, PriceLevel, Scope, CurrencyCode, CustomerCode, PriceGroup,
            ContractRef, ValidFrom, ValidTo, IsActive, CreatedBy)
    VALUES (s.CompanyCode, s.PriceListCode, s.Name, s.PriceLevel, s.Scope, s.CurrencyCode, s.CustomerCode,
            s.PriceGroup, s.ContractRef, @from, @to, 1, 'SEED');
PRINT 'Seeded so.PriceList';
GO
/*  Price list lines.  QtyBreak 0 is the base price; a break row is the price from that quantity
    upwards, so a line of 25 frames takes the 25 break and a line of 24 takes the 10 break.
    The road frame carries the specification's worked example: 342.00 list, 10% line discount,
    307.80 net, 61.56 VAT, 369.36 gross (SO-005).  REQ: SO-004 */
DECLARE @from date = '2026-04-01', @to date = '2027-03-31';
DECLARE @l table (CompanyCode varchar(10), PriceListCode varchar(20), ItemCode varchar(40), UomCode varchar(10) NULL,
                  QtyBreak decimal(19,6), CurrencyCode char(3), UnitPrice decimal(19,6), PricePerQty decimal(19,6),
                  DiscountPct decimal(9,4), MinimumMarginPct decimal(9,4) NULL);
INSERT @l VALUES
 ('STC','STD26','FG-FRAME-ROAD-SL-54',  'EA',  0,'GBP', 342.000000,1, 0.0000,22.0000),
 ('STC','STD26','FG-FRAME-ROAD-SL-54',  'EA', 10,'GBP', 324.900000,1, 0.0000,20.0000),
 ('STC','STD26','FG-FRAME-ROAD-SL-54',  'EA', 25,'GBP', 307.800000,1, 0.0000,18.0000),
 ('STC','STD26','FG-FRAME-ROAD-SL-52',  'EA',  0,'GBP', 342.000000,1, 0.0000,22.0000),
 ('STC','STD26','FG-FRAME-GRAVEL-M',    'EA',  0,'GBP', 298.000000,1, 0.0000,22.0000),
 ('STC','STD26','FG-FRAME-GRAVEL-M',    'EA', 20,'GBP', 279.500000,1, 0.0000,19.0000),
 ('STC','STD26','FG-WHEELSET-700C-DISC','SET', 0,'GBP', 415.000000,1, 0.0000,24.0000),
 ('STC','STD26','FG-BIKE-COMMUTE-M',    'EA',  0,'GBP', 780.000000,1, 0.0000,25.0000),
 ('STC','STD26','SADDLE-COMP-143',      'EA',  0,'GBP',  42.500000,1, 0.0000,30.0000),
 ('STC','STD26','CASSETTE-11-34-11S',   'EA',  0,'GBP',  48.500000,1, 0.0000,28.0000),
 ('STC','STD26','CHAIN-116L-11S',       'EA',  0,'GBP',  26.400000,1, 0.0000,28.0000),
 ('STC','STD26','TYRE-700X32C',         'EA',  0,'GBP',  31.750000,1, 0.0000,26.0000),
 ('STC','STD26','BRAKE-DISC-160MM',     'EA',  0,'GBP',  18.900000,1, 0.0000,26.0000),
 ('STC','STD26','BOLT-M5X16-SS',        'EA',100,'GBP',  29.824600,100,0.0000,18.0000),
 ('STC','STD26','KIT-BUILD-ROAD-52',    'EA',  0,'GBP',1250.000000,1, 0.0000,20.0000),
 ('STC','STD26','KIT-EBIKE-CONV-250W',  'EA',  0,'GBP', 845.000000,1, 0.0000,20.0000),
 ('STC','STD26','BATTERY-36V-14AH',     'EA',  0,'GBP', 268.000000,1, 0.0000,18.0000),
 ('STC','STD26','MOTOR-HUB-250W',       'EA',  0,'GBP', 182.000000,1, 0.0000,18.0000),
 ('STC','STD26','SVC-CARRIAGE-UK',      'EA',  0,'GBP',   9.950000,1, 0.0000,NULL),
 ('STC','STD26','SVC-CARRIAGE-EXPORT',  'EA',  0,'GBP',  48.000000,1, 0.0000,NULL),
 ('STC','STD26','SVC-ASSEMBLY-HOUR',    'HR',  0,'GBP',  54.000000,1, 0.0000,NULL),
 ('STC','STD26','NS-SPARE-DECAL-SET',   'SET', 0,'GBP',  24.000000,1, 0.0000,NULL),
 ('STC','TRADE26','FG-FRAME-ROAD-SL-54','EA',  0,'GBP', 342.000000,1, 5.0000,20.0000),
 ('STC','TRADE26','FG-FRAME-GRAVEL-M',  'EA',  0,'GBP', 298.000000,1, 5.0000,20.0000),
 ('STC','TRADE26','FG-BIKE-COMMUTE-M',  'EA',  0,'GBP', 780.000000,1, 5.0000,22.0000),
 ('STC','TRADE26','SADDLE-COMP-143',    'EA',  0,'GBP',  42.500000,1, 5.0000,26.0000),
 ('STC','TRADE26','CASSETTE-11-34-11S', 'EA',  0,'GBP',  48.500000,1, 5.0000,25.0000),
 ('STC','TRADE26','TYRE-700X32C',       'EA',  0,'GBP',  31.750000,1, 5.0000,24.0000),
 ('STC','DIST26','FG-FRAME-ROAD-SL-54', 'EA',  0,'GBP', 318.000000,1, 0.0000,18.0000),
 ('STC','DIST26','FG-FRAME-ROAD-SL-54', 'EA', 25,'GBP', 299.000000,1, 0.0000,16.0000),
 ('STC','DIST26','FG-FRAME-GRAVEL-M',   'EA',  0,'GBP', 276.000000,1, 0.0000,18.0000),
 ('STC','DIST26','FG-WHEELSET-700C-DISC','SET',0,'GBP', 385.000000,1, 0.0000,20.0000),
 ('STC','DIST26','FG-BIKE-COMMUTE-M',   'EA',  0,'GBP', 712.000000,1, 0.0000,20.0000),
 ('STC','DIST26','SADDLE-COMP-143',     'EA', 50,'GBP',  36.000000,1, 0.0000,22.0000),
 ('STC','OEM26','FG-FRAME-ROAD-SL-54',  'EA', 50,'GBP', 284.000000,1, 0.0000,14.0000),
 ('STC','OEM26','FG-FRAME-GRAVEL-M',    'EA', 50,'GBP', 256.000000,1, 0.0000,14.0000),
 ('STC','OEM26','FG-WHEELSET-700C-DISC','SET',25,'GBP', 362.000000,1, 0.0000,16.0000),
 ('STC','CUSTC0001','FG-FRAME-ROAD-SL-54','EA',0,'GBP', 309.000000,1, 0.0000,16.0000),
 ('STC','CUSTC0001','FG-BIKE-COMMUTE-M','EA',  0,'GBP', 699.000000,1, 0.0000,18.0000),
 ('STC','CONTC0007','FG-FRAME-ROAD-SL-54','EA',0,'GBP', 272.500000,1, 0.0000,12.0000),
 ('STC','CONTC0007','FG-WHEELSET-700C-DISC','SET',0,'GBP',349.000000,1,0.0000,14.0000),
 ('STC','EURC0021','FG-FRAME-ROAD-SL-54','EA', 0,'EUR', 398.000000,1, 0.0000,16.0000),
 ('STC','EURC0021','FG-BIKE-COMMUTE-M', 'EA',  0,'EUR', 905.000000,1, 0.0000,18.0000),
 ('STC','USDC0027','FG-FRAME-ROAD-SL-54','EA', 0,'USD', 442.000000,1, 0.0000,16.0000),
 ('CAL','CALSTD26','CAL-BIKE-ROAD-54',  'EA',  0,'GBP',1450.000000,1, 0.0000,24.0000),
 ('CAL','CALSTD26','CAL-BIKE-GRAVEL-M', 'EA',  0,'GBP',1380.000000,1, 0.0000,24.0000),
 ('CAL','CALSTD26','CAL-BIKE-EBIKE-CITY','EA', 0,'GBP',1980.000000,1, 0.0000,22.0000),
 ('CAL','CALSTD26','SADDLE-COMP-143',   'EA',  0,'GBP',  42.500000,1, 0.0000,30.0000),
 ('CAL','CALSTD26','TYRE-700X32C',      'EA',  0,'GBP',  31.750000,1, 0.0000,26.0000),
 ('CAL','CALSTD26','SVC-ASSEMBLY-HOUR', 'HR',  0,'GBP',  58.000000,1, 0.0000,NULL),
 ('CAL','CALTRD26','CAL-BIKE-ROAD-54',  'EA',  0,'GBP',1450.000000,1, 6.0000,20.0000),
 ('CAL','CALTRD26','CAL-BIKE-GRAVEL-M', 'EA',  5,'GBP',1295.000000,1, 0.0000,20.0000),
 ('CAL','CONTK0001','CAL-BIKE-ROAD-54', 'EA',  0,'GBP',1288.000000,1, 0.0000,15.0000),
 ('CAL','CONTK0001','CAL-BIKE-EBIKE-CITY','EA',0,'GBP',1775.000000,1, 0.0000,15.0000);

MERGE so.PriceListLine AS t
USING (SELECT * FROM @l) AS s
ON t.CompanyCode = s.CompanyCode AND t.PriceListCode = s.PriceListCode AND t.ItemCode = s.ItemCode
   AND t.QtyBreak = s.QtyBreak AND t.CurrencyCode = s.CurrencyCode AND t.ValidFrom = @from
WHEN MATCHED THEN UPDATE SET UomCode = s.UomCode, UnitPrice = s.UnitPrice, PricePerQty = s.PricePerQty,
        DiscountPct = s.DiscountPct, MinimumMarginPct = s.MinimumMarginPct, ValidTo = @to,
        ModifiedBy = 'SEED', ModifiedAt = SYSDATETIME()
WHEN NOT MATCHED THEN
    INSERT (CompanyCode, PriceListCode, ItemCode, UomCode, PricePerQty, QtyBreak, CurrencyCode, UnitPrice,
            DiscountPct, MinimumMarginPct, ValidFrom, ValidTo, CreatedBy)
    VALUES (s.CompanyCode, s.PriceListCode, s.ItemCode, s.UomCode, s.PricePerQty, s.QtyBreak, s.CurrencyCode,
            s.UnitPrice, s.DiscountPct, s.MinimumMarginPct, @from, @to, 'SEED');
PRINT 'Seeded so.PriceListLine';
GO
/*  Retrospective volume rebates.  A rebate is variable consideration: it is accrued against
    revenue as the customer trades, not when it is settled, so the accrual is a reduction of
    turnover and a liability, never a cost (D-12).  REBATE computes and posts the accruals;
    these rows are only the agreements.  REQ: SO-027 */
MERGE so.RebateAgreement AS t
USING (VALUES
 ('STC','REB-2026-001','C0001','Pennine Cycles annual turnover rebate, 2.5% over 50,000',NULL,NULL,
  'TURNOVER',50000.000000,2.5000,NULL,'2026-04-01','2027-03-31','GBP','ACTIVE','SALESMGR'),
 ('STC','REB-2026-002','C0003','Kestrel Velo frame volume rebate, 4.00 per frame over 100',NULL,'FRAME',
  'QUANTITY',100.000000,NULL,4.000000,'2026-04-01','2027-03-31','GBP','ACTIVE','SALESMGR'),
 ('STC','REB-2026-003','C0007','Caxton OEM tiered contract rebate',NULL,NULL,
  'TIERED',75000.000000,3.0000,NULL,'2026-04-01','2027-03-31','GBP','ACTIVE','SALESMGR'),
 ('STC','REB-2026-004','C0018','Belfast Velo growth rebate, 1.5%, closed for settlement',NULL,NULL,
  'TURNOVER',20000.000000,1.5000,NULL,'2026-04-01','2026-09-30','GBP','CLOSED','SALESMGR'),
 ('CAL','REB-2026-101','K0001','Clyde Cycle Co complete bicycle rebate, 2% over 40,000',NULL,'BICYCLE',
  'TURNOVER',40000.000000,2.0000,NULL,'2026-04-01','2027-03-31','GBP','ACTIVE','SALESMGR')
) AS s (CompanyCode, RebateRef, CustomerCode, Description, ItemCode, ProductGroup, Basis, ThresholdValue,
        RebatePct, RebatePerUnit, PeriodStart, PeriodEnd, CurrencyCode, Status, ApprovedBy)
ON t.CompanyCode = s.CompanyCode AND t.RebateRef = s.RebateRef
WHEN MATCHED THEN UPDATE SET CustomerCode = s.CustomerCode, Description = s.Description, ItemCode = s.ItemCode,
        ProductGroup = s.ProductGroup, Basis = s.Basis, ThresholdValue = s.ThresholdValue, RebatePct = s.RebatePct,
        RebatePerUnit = s.RebatePerUnit, PeriodStart = s.PeriodStart, PeriodEnd = s.PeriodEnd,
        CurrencyCode = s.CurrencyCode, Status = s.Status, ApprovedBy = s.ApprovedBy,
        ModifiedBy = 'SEED', ModifiedAt = SYSDATETIME()
WHEN NOT MATCHED THEN
    INSERT (CompanyCode, RebateRef, CustomerCode, Description, ItemCode, ProductGroup, Basis, ThresholdValue,
            RebatePct, RebatePerUnit, PeriodStart, PeriodEnd, CurrencyCode, Status, ApprovedBy, CreatedBy)
    VALUES (s.CompanyCode, s.RebateRef, s.CustomerCode, s.Description, s.ItemCode, s.ProductGroup, s.Basis,
            s.ThresholdValue, s.RebatePct, s.RebatePerUnit, s.PeriodStart, s.PeriodEnd, s.CurrencyCode,
            s.Status, s.ApprovedBy, 'SEED');
PRINT 'Seeded so.RebateAgreement';
GO
