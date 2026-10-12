/*  Sales demonstration transactions: quotations in every quote state and sales orders in every
    combination of commercial, release, fulfilment and billing state the specification defines,
    for both companies.  The data stops at "picked": despatches, COGS postings and invoices are
    deliberately NOT seeded here, because they are accounting events and must be produced by
    running the sales programs (DESPATCH, SOINVRUN) so that the stock movements, the journals and
    the sales ledger agree.  Order SOS900008 and SOC900004 are left picked and billing-eligible
    exactly so a demonstration can confirm a despatch and then run the invoice run against them.
    Their items are lot / serial controlled, so the pick list a despatch issues from (naming the
    bin and the lot or serial) is not seeded here: SODEMO (2700-STAGE-PICKED) reserves and picks
    them through SORESV and PICKGEN (GUI-005).
    Document numbers use the 900000 band so they can never collide with a number the live series
    issues.  Re-runnable: every insert is guarded, and the header totals are recomputed from the
    lines each time.  REQ: SO-001, SO-002, SO-003, SO-007, SO-008, SO-011, SO-015, SO-021,
    REQ: SO-024, SO-025, SO-026, SO-029 */
SET NOCOUNT ON;
GO
DECLARE @today date = CAST(SYSDATETIME() AS date);

/* ---------------------------------------------------------------- quotations (SO-001, SO-024) */
DECLARE @q table (CompanyCode varchar(10), QuoteNumber varchar(30), Version smallint, CustomerCode varchar(20) NULL,
                  ProspectName varchar(120) NULL, DayOffset int, ValidDays int, CurrencyCode char(3), FxRate decimal(18,8),
                  TermsCode varchar(12) NULL, Salesperson varchar(20), Status varchar(12), ConvertedOrderNumber varchar(30) NULL,
                  Assumptions varchar(400) NULL);
INSERT @q VALUES
 ('STC','SQS900001',1,'C0002',NULL,  -12,30,'GBP',1,'EOM30','SALESREP','ISSUED',  NULL,'Prices hold for 30 days; carriage charged at cost.'),
 ('STC','SQS900002',1,'C0005',NULL,  -24,21,'GBP',1,'D2N30','SALESREP','REVISED', NULL,'Superseded by version 2 after the customer changed the quantity.'),
 ('STC','SQS900002',2,'C0005',NULL,  -18,30,'GBP',1,'D2N30','SALESREP','ISSUED',  NULL,'Revision 2: quantity raised to the 25 break.'),
 ('STC','SQS900003',1,'C0001',NULL,  -20,30,'GBP',1,'N30',  'SALESREP','CONVERTED','SOS900002','Converted in full to a sales order.'),
 ('STC','SQS900004',1,'C0013',NULL,  -40,30,'GBP',1,'N30',  'SALESREP','DECLINED',NULL,'Declined: the customer placed the business elsewhere on price.'),
 ('STC','SQS900005',1,'C0010',NULL,  -95,30,'GBP',1,'DOM31','SALESREP','EXPIRED', NULL,'Validity passed without acceptance; re-quote required.'),
 ('STC','SQS900006',1,NULL,'Harrogate Cycle Works (prospect)',-3,30,'GBP',1,NULL,'SALESREP','DRAFT',NULL,'Prospect enquiry, no credit account yet.'),
 ('STC','SQS900007',1,'C0007',NULL,  -30,45,'GBP',1,'N60',  'SALESMGR','PARTCONV','SOS900009','Blanket agreement: part converted, the balance remains open.'),
 ('STC','SQS900008',1,'C0021',NULL,  -10,30,'EUR',1.17000000,'N30','SALESREP','ISSUED',NULL,'Euro quotation, delivered at place, export evidence required.'),
 ('CAL','SQC900001',1,'K0002',NULL,  -14,30,'GBP',1,'N30',  'SALESREP','ISSUED',  NULL,'Assembly lead time four weeks from acceptance.'),
 ('CAL','SQC900002',1,'K0001',NULL,  -22,30,'GBP',1,'N30',  'SALESMGR','ACCEPTED',NULL,'Accepted by e-mail; order to follow against the supply agreement.');

INSERT so.Quote (CompanyCode, QuoteNumber, Version, CustomerCode, ProspectName, QuoteDate, ValidUntil, CurrencyCode,
                 FxRate, TermsCode, SalespersonCode, Status, AcceptedAt, AcceptanceEvidence, ConvertedOrderNumber,
                 Assumptions, DeliveryEstimate, CreatedBy)
SELECT q.CompanyCode, q.QuoteNumber, q.Version, q.CustomerCode, q.ProspectName,
       DATEADD(day, q.DayOffset, @today), DATEADD(day, q.DayOffset + q.ValidDays, @today), q.CurrencyCode,
       q.FxRate, q.TermsCode, q.Salesperson, q.Status,
       CASE WHEN q.Status IN ('ACCEPTED','CONVERTED','PARTCONV') THEN DATEADD(day, q.DayOffset + 5, CAST(@today AS datetime2(3))) END,
       CASE WHEN q.Status IN ('ACCEPTED','CONVERTED','PARTCONV') THEN 'Customer e-mail of acceptance filed against the quotation' END,
       q.ConvertedOrderNumber, q.Assumptions, '10 working days from acceptance', 'SEED'
  FROM @q q
 WHERE NOT EXISTS (SELECT 1 FROM so.Quote x WHERE x.CompanyCode = q.CompanyCode
                     AND x.QuoteNumber = q.QuoteNumber AND x.Version = q.Version);

DECLARE @ql table (CompanyCode varchar(10), QuoteNumber varchar(30), Version smallint, LineNum int, ItemCode varchar(40),
                   Description varchar(200), Quantity decimal(19,6), UomCode varchar(10), UnitPrice decimal(19,6),
                   DiscountPct1 decimal(9,4), VatCode varchar(4), VatRate decimal(9,4), UnitCost decimal(19,6));
INSERT @ql VALUES
 ('STC','SQS900001',1,1,'FG-FRAME-ROAD-SL-54','Frame, road Superlight, 54 cm',   10,'EA',342.000000, 5.0000,'S',20,236.00),
 ('STC','SQS900001',1,2,'SADDLE-COMP-143','Saddle, composite rail, 143 mm',      10,'EA', 42.500000, 5.0000,'S',20, 27.40),
 ('STC','SQS900002',1,1,'FG-FRAME-GRAVEL-M','Frame, gravel Trailhead, medium',   10,'EA',298.000000, 0.0000,'S',20,208.00),
 ('STC','SQS900002',2,1,'FG-FRAME-GRAVEL-M','Frame, gravel Trailhead, medium',   25,'EA',279.500000, 0.0000,'S',20,208.00),
 ('STC','SQS900003',1,1,'FG-FRAME-ROAD-SL-54','Frame, road Superlight, 54 cm',   25,'EA',342.000000,10.0000,'S',20,236.00),
 ('STC','SQS900004',1,1,'FG-BIKE-COMMUTE-M','Complete bicycle, commuter, medium', 6,'EA',780.000000, 0.0000,'S',20,548.00),
 ('STC','SQS900005',1,1,'FG-WHEELSET-700C-DISC','Wheelset, 700C disc',            8,'SET',415.000000,0.0000,'S',20,296.00),
 ('STC','SQS900006',1,1,'FG-BIKE-COMMUTE-M','Complete bicycle, commuter, medium', 2,'EA',780.000000, 0.0000,'S',20,548.00),
 ('STC','SQS900007',1,1,'FG-FRAME-ROAD-SL-54','Frame, road Superlight, 54 cm',  200,'EA',284.000000, 0.0000,'S',20,236.00),
 ('STC','SQS900008',1,1,'FG-FRAME-ROAD-SL-54','Frame, road Superlight, 54 cm',   12,'EA',398.000000, 0.0000,'EXP',0,236.00),
 ('CAL','SQC900001',1,1,'CAL-BIKE-GRAVEL-M','Complete bicycle, gravel, medium',   5,'EA',1295.000000,0.0000,'S',20,968.00),
 ('CAL','SQC900002',1,1,'CAL-BIKE-ROAD-54','Complete bicycle, road, 54 cm',       4,'EA',1288.000000,0.0000,'S',20,962.00);

INSERT so.QuoteLine (QuoteId, LineNum, ItemCode, Description, Quantity, UomCode, UnitPrice, PricePerQty,
                     DiscountPct1, DiscountPct2, NetAmount, VatCode, VatAmount, EstimatedCostGBP, CostBasis,
                     MarginPct, LeadTimeDays)
SELECT h.QuoteId, l.LineNum, l.ItemCode, l.Description, l.Quantity, l.UomCode, l.UnitPrice, 1,
       l.DiscountPct1, 0,
       CAST(l.Quantity * l.UnitPrice * (1 - l.DiscountPct1 / 100.0) AS decimal(18,2)),
       l.VatCode,
       CAST(CAST(l.Quantity * l.UnitPrice * (1 - l.DiscountPct1 / 100.0) AS decimal(18,2)) * l.VatRate / 100.0 AS decimal(18,2)),
       CAST(l.Quantity * l.UnitCost AS decimal(18,2)), 'STANDARD',
       CAST(100.0 * (l.UnitPrice * (1 - l.DiscountPct1 / 100.0) - l.UnitCost)
            / NULLIF(l.UnitPrice * (1 - l.DiscountPct1 / 100.0), 0) AS decimal(9,4)), 10
  FROM @ql l
  JOIN so.Quote h ON h.CompanyCode = l.CompanyCode AND h.QuoteNumber = l.QuoteNumber AND h.Version = l.Version
 WHERE NOT EXISTS (SELECT 1 FROM so.QuoteLine x WHERE x.QuoteId = h.QuoteId AND x.LineNum = l.LineNum);

UPDATE h SET NetAmount = t.Net, VatAmount = t.Vat, GrossAmount = t.Net + t.Vat,
             CostBasisGBP = t.Cost,
             MarginPct = CAST(100.0 * (t.Net - t.Cost) / NULLIF(t.Net, 0) AS decimal(9,4))
  FROM so.Quote h
 CROSS APPLY (SELECT Net = SUM(l.NetAmount), Vat = SUM(l.VatAmount), Cost = SUM(l.EstimatedCostGBP)
                FROM so.QuoteLine l WHERE l.QuoteId = h.QuoteId) t
 WHERE h.CreatedBy = 'SEED' AND t.Net IS NOT NULL;
PRINT 'Seeded so.Quote / so.QuoteLine (demonstration)';
GO
/* --------------------------------------------------------- sales orders in every state (SO-002) */
DECLARE @today date = CAST(SYSDATETIME() AS date);
DECLARE @o table (CompanyCode varchar(10), SiteCode varchar(10), OrderNumber varchar(30), OrderType varchar(12),
                  BlanketOrderNumber varchar(30) NULL, CustomerCode varchar(20), CustomerOrderRef varchar(40),
                  DayOffset int, QuoteNumber varchar(30) NULL, CurrencyCode char(3), FxRate decimal(18,8),
                  TermsCode varchar(12), Salesperson varchar(20), Incoterms varchar(3), NamedPlace varchar(60),
                  CommercialStatus varchar(12), ReleaseStatus varchar(14), FulfilmentStatus varchar(14),
                  BillingStatus varchar(14), HoldReason varchar(200) NULL, DepositReq decimal(18,2) NULL,
                  DepositRec decimal(18,2), VatLocation varchar(3), VatBasis varchar(40));
INSERT @o VALUES
 ('STC','RDG','SOS900001','STANDARD',NULL,'C0002','PO-SEV-5512', -2,NULL,'GBP',1,'EOM30','SALESREP','DAP','Severnside, Bristol',
  'DRAFT','AWAITAPPROVAL','UNALLOCATED','NOTELIGIBLE',NULL,NULL,0,'GB','UK standard rated'),
 ('STC','RDG','SOS900002','STANDARD',NULL,'C0001','PEN-2026-118',-15,'SQS900003','GBP',1,'N30','SALESREP','DAP','Pennine Cycles, Leeds',
  'ACCEPTED','RELEASED','RESERVED','NOTELIGIBLE',NULL,NULL,0,'GB','UK standard rated'),
 ('STC','RDG','SOS900003','STANDARD',NULL,'C0008','WHL-9921',    -9,NULL,'GBP',1,'N30','SALESREP','DAP','Wheelhouse, Derby',
  'ACCEPTED','CREDITHOLD','UNALLOCATED','NOTELIGIBLE','Account on hold: balance over the credit limit and 42 days overdue',NULL,0,'GB','UK standard rated'),
 ('STC','RDG','SOS900004','STANDARD',NULL,'C0011','PRE-7781',    -6,NULL,'GBP',1,'PREPAY','SALESREP','EXW','Reading',
  'ACCEPTED','COMMERCIALHOLD','UNALLOCATED','NOTELIGIBLE','Prepayment terms: deposit of 1,500.00 not yet received',1500.00,0,'GB','UK standard rated'),
 ('STC','BHM','SOS900005','STANDARD',NULL,'C0005','WCR-0042',    -8,NULL,'GBP',1,'D2N30','SALESREP','DAP','Wheelcraft, Birmingham',
  'ACCEPTED','SUPPLYHOLD','PARTRESERVED','NOTELIGIBLE','Back-to-back line awaiting supplier confirmation',NULL,0,'GB','UK standard rated'),
 ('STC','RDG','SOS900006','STANDARD',NULL,'C0003','KES-2026-77', -21,NULL,'GBP',1,'N45','SALESREP','DAP','Kestrel Velo, Exeter',
  'AMENDED','RELEASED','PARTPICKED','NOTELIGIBLE',NULL,NULL,0,'GB','UK standard rated'),
 ('STC','RDG','SOS900007','STANDARD',NULL,'C0013','PDF-3310',   -28,NULL,'GBP',1,'N30','SALESREP','DAP','Pedal Forward, Cardiff',
  'CANCELLED','COMMERCIALHOLD','UNALLOCATED','NOTELIGIBLE','Cancelled at the customer''s request before any despatch',NULL,0,'GB','UK standard rated'),
 ('STC','RDG','SOS900008','STANDARD',NULL,'C0010','COT-5589',    -5,NULL,'GBP',1,'DOM31','SALESREP','DAP','Cotswold Gravel, Stroud',
  'ACCEPTED','RELEASED','PICKED','ELIGIBLE',NULL,NULL,0,'GB','UK standard rated'),
 ('STC','RDG','SOS900009','BLANKET',NULL,'C0007','CAX-BLANKET-26',-30,'SQS900007','GBP',1,'N60','SALESMGR','DAP','Caxton Cycles, Coventry',
  'ACCEPTED','RELEASED','UNALLOCATED','NOTELIGIBLE',NULL,NULL,0,'GB','UK standard rated'),
 ('STC','RDG','SOS900010','CALLOFF','SOS900009','C0007','CAX-CALL-04',-4,NULL,'GBP',1,'N60','SALESMGR','DAP','Caxton Cycles, Coventry',
  'ACCEPTED','RELEASED','RESERVED','NOTELIGIBLE',NULL,NULL,0,'GB','UK standard rated'),
 ('STC','RDG','SOS900011','STANDARD',NULL,'C0021','VA-26-0091',  -7,'SQS900008','EUR',1.17000000,'N30','SALESREP','DAP','Nantes, France',
  'ACCEPTED','RELEASED','RESERVED','NOTELIGIBLE',NULL,NULL,0,'EU','Export zero rated'),
 ('STC','RDG','SOS900012','DROPSHIP',NULL,'C0020','OEM-DS-221',  -3,NULL,'GBP',1,'N30','SALESMGR','DAP','Customer nominated site, Hull',
  'ACCEPTED','RELEASED','UNALLOCATED','NOTELIGIBLE',NULL,NULL,0,'GB','UK standard rated'),
 ('CAL','GLA','SOC900001','STANDARD',NULL,'K0002','CLY-77',      -2,'SQC900001','GBP',1,'N30','SALESREP','DAP','Glasgow',
  'DRAFT','AWAITAPPROVAL','UNALLOCATED','NOTELIGIBLE',NULL,NULL,0,'GB','UK standard rated'),
 ('CAL','GLA','SOC900002','STANDARD',NULL,'K0001','CCC-2026-14',-16,'SQC900002','GBP',1,'N30','SALESMGR','DAP','Clydebank',
  'ACCEPTED','RELEASED','RESERVED','NOTELIGIBLE',NULL,NULL,0,'GB','UK standard rated'),
 ('CAL','LIV','SOC900003','STANDARD',NULL,'K0004','LIV-0098',    -11,NULL,'GBP',1,'N14','SALESREP','DAP','Livingston',
  'ACCEPTED','CREDITHOLD','UNALLOCATED','NOTELIGIBLE','New account: first order over the provisional credit limit',NULL,0,'GB','UK standard rated'),
 ('CAL','GLA','SOC900004','STANDARD',NULL,'K0003','EDN-4412',    -6,NULL,'GBP',1,'EOM30','SALESREP','DAP','Edinburgh',
  'ACCEPTED','RELEASED','PICKED','ELIGIBLE',NULL,NULL,0,'GB','UK standard rated'),
 ('CAL','GLA','SOC900005','STANDARD',NULL,'K0005','ABD-1120',   -19,NULL,'GBP',1,'N30','SALESREP','DAP','Aberdeen',
  'AMENDED','RELEASED','PARTRESERVED','NOTELIGIBLE',NULL,NULL,0,'GB','UK standard rated');

INSERT so.SalesOrder (CompanyCode, SiteCode, OrderNumber, OrderType, BlanketOrderNumber, CustomerCode, CustomerOrderRef,
                      OrderDate, QuoteNumber, CurrencyCode, FxRate, FxRateDate, TermsCode, TermsSnapshot, SalespersonCode,
                      IncotermsRule, IncotermsNamedPlace, DeliveryAddressSnapshot, TermsAndConditionsVersion,
                      RetentionOfTitle, VatTreatmentBasis, VatLocation, CommercialStatus, ReleaseStatus, FulfilmentStatus,
                      BillingStatus, HoldReason, DepositRequiredGBP, DepositReceivedGBP, AcknowledgementIssuedAt, CreatedBy)
SELECT o.CompanyCode, o.SiteCode, o.OrderNumber, o.OrderType, o.BlanketOrderNumber, o.CustomerCode, o.CustomerOrderRef,
       DATEADD(day, o.DayOffset, @today), o.QuoteNumber, o.CurrencyCode, o.FxRate, DATEADD(day, o.DayOffset, @today),
       o.TermsCode, 'Terms snapshotted at acceptance: ' + o.TermsCode, o.Salesperson, o.Incoterms, o.NamedPlace,
       o.NamedPlace, 'STERP-TCS-2026-04', 1, o.VatBasis, o.VatLocation, o.CommercialStatus, o.ReleaseStatus,
       o.FulfilmentStatus, o.BillingStatus, o.HoldReason, o.DepositReq, o.DepositRec,
       CASE WHEN o.CommercialStatus IN ('ACCEPTED','AMENDED') AND o.ReleaseStatus = 'RELEASED'
            THEN DATEADD(day, o.DayOffset + 1, CAST(@today AS datetime2(3))) END, 'SEED'
  FROM @o o
 WHERE NOT EXISTS (SELECT 1 FROM so.SalesOrder x WHERE x.CompanyCode = o.CompanyCode AND x.OrderNumber = o.OrderNumber);

/* The held Livingston fixture invoices its own customer; acceptance still validates the full header. */
UPDATE so.SalesOrder
   SET InvoiceCustomerCode = CustomerCode
 WHERE CompanyCode = 'CAL' AND OrderNumber = 'SOC900003'
   AND CreatedBy = 'SEED' AND InvoiceCustomerCode IS NULL;

DECLARE @ol table (CompanyCode varchar(10), OrderNumber varchar(30), LineNum int, LineType varchar(10), ItemCode varchar(40),
                   Description varchar(200), Qty decimal(19,6), Cancelled decimal(19,6), Despatched decimal(19,6),
                   UomCode varchar(10), UnitPrice decimal(19,6), Disc1 decimal(9,4), PriceSource varchar(20),
                   PriceList varchar(20), VatCode varchar(4), VatRate decimal(9,4), UnitCost decimal(19,6),
                   SupplyType varchar(12), LinkedPo varchar(30) NULL, LineStatus varchar(10), DueOffset int, Slip int);
INSERT @ol VALUES
 ('STC','SOS900001',1,'STOCK','FG-FRAME-ROAD-SL-54','Frame, road Superlight, 54 cm', 6,0,0,'EA',342.000000, 5.0000,'GROUP','TRADE26','S',20,236.00,'STOCK',NULL,'OPEN', 12,0),
 ('STC','SOS900001',2,'CHARGE','SVC-CARRIAGE-UK','Carriage within the UK',           1,0,0,'EA',  9.950000, 0.0000,'STANDARD','STD26','S',20,  7.20,'STOCK',NULL,'OPEN', 12,0),
 ('STC','SOS900002',1,'STOCK','FG-FRAME-ROAD-SL-54','Frame, road Superlight, 54 cm',25,0,0,'EA',342.000000,10.0000,'CUSTOMER','CUSTC0001','S',20,236.00,'STOCK',NULL,'OPEN', 9,0),
 ('STC','SOS900002',2,'STOCK','SADDLE-COMP-143','Saddle, composite rail, 143 mm',   25,0,0,'EA', 42.500000, 0.0000,'STANDARD','STD26','S',20, 27.40,'STOCK',NULL,'OPEN', 9,0),
 ('STC','SOS900003',1,'STOCK','FG-BIKE-COMMUTE-M','Complete bicycle, commuter',      4,0,0,'EA',780.000000, 5.0000,'GROUP','TRADE26','S',20,548.00,'STOCK',NULL,'OPEN', 14,0),
 ('STC','SOS900004',1,'STOCK','FG-WHEELSET-700C-DISC','Wheelset, 700C disc',         6,0,0,'SET',415.000000,0.0000,'STANDARD','STD26','S',20,296.00,'STOCK',NULL,'OPEN', 18,0),
 ('STC','SOS900005',1,'STOCK','FG-FRAME-GRAVEL-M','Frame, gravel Trailhead, medium',10,0,0,'EA',298.000000, 5.0000,'GROUP','TRADE26','S',20,208.00,'STOCK',NULL,'OPEN', 10,0),
 ('STC','SOS900005',2,'STOCK','SUSP-FORK-120MM','Suspension fork 120 mm',           10,0,0,'EA',245.000000, 0.0000,'MANUAL',NULL,'S',20,176.00,'BACKTOBACK','POS900031','OPEN', 25,0),
 ('STC','SOS900006',1,'STOCK','FG-FRAME-ROAD-SL-54','Frame, road Superlight, 54 cm',30,5,0,'EA',318.000000, 0.0000,'GROUP','DIST26','S',20,236.00,'STOCK',NULL,'OPEN', -2,7),
 ('STC','SOS900006',2,'STOCK','CASSETTE-11-34-11S','Cassette 11-34, eleven speed',  30,0,0,'EA', 48.500000, 0.0000,'STANDARD','STD26','S',20, 33.10,'STOCK',NULL,'OPEN', -2,7),
 ('STC','SOS900007',1,'STOCK','FG-BIKE-COMMUTE-M','Complete bicycle, commuter',      3,3,0,'EA',780.000000, 5.0000,'GROUP','TRADE26','S',20,548.00,'STOCK',NULL,'CANCELLED', 6,0),
 ('STC','SOS900008',1,'STOCK','FG-FRAME-GRAVEL-M','Frame, gravel Trailhead, medium', 8,0,0,'EA',298.000000, 5.0000,'GROUP','TRADE26','S',20,208.00,'STOCK',NULL,'OPEN', 2,0),
 ('STC','SOS900008',2,'STOCK','TYRE-700X32C','Tyre 700 x 32C, folding',             16,0,0,'EA', 31.750000, 5.0000,'GROUP','TRADE26','S',20, 21.40,'STOCK',NULL,'OPEN', 2,0),
 ('STC','SOS900009',1,'STOCK','FG-FRAME-ROAD-SL-54','Frame, road Superlight, 54 cm, blanket quantity',200,0,0,'EA',284.000000,0.0000,'GROUP','OEM26','S',20,236.00,'STOCK',NULL,'OPEN',120,0),
 ('STC','SOS900010',1,'STOCK','FG-FRAME-ROAD-SL-54','Frame, road Superlight, 54 cm, call-off 4',50,0,0,'EA',284.000000,0.0000,'GROUP','OEM26','S',20,236.00,'STOCK',NULL,'OPEN', 11,0),
 ('STC','SOS900011',1,'STOCK','FG-FRAME-ROAD-SL-54','Frame, road Superlight, 54 cm',12,0,0,'EA',398.000000, 0.0000,'CUSTOMER','EURC0021','EXP',0,236.00,'STOCK',NULL,'OPEN', 16,0),
 ('STC','SOS900011',2,'CHARGE','SVC-CARRIAGE-EXPORT','Export carriage, DAP Nantes',  1,0,0,'EA', 56.000000, 0.0000,'MANUAL',NULL,'EXP',0, 42.00,'STOCK',NULL,'OPEN', 16,0),
 ('STC','SOS900012',1,'NONSTOCK','NS-CATALOGUE-BOLT-KIT','Catalogue bolt kit, drop shipped',40,0,0,'EA',18.500000,0.0000,'MANUAL',NULL,'S',20,12.80,'DROPSHIP','POS900047','OPEN', 13,0),
 ('CAL','SOC900001',1,'STOCK','CAL-BIKE-GRAVEL-M','Complete bicycle, gravel, medium', 5,0,0,'EA',1295.000000,0.0000,'GROUP','CALTRD26','S',20,968.00,'STOCK',NULL,'OPEN', 20,0),
 ('CAL','SOC900002',1,'STOCK','CAL-BIKE-ROAD-54','Complete bicycle, road, 54 cm',     4,0,0,'EA',1288.000000,0.0000,'CONTRACT','CONTK0001','S',20,962.00,'STOCK',NULL,'OPEN', 12,0),
 ('CAL','SOC900003',1,'STOCK','CAL-BIKE-EBIKE-CITY','Complete electric city bicycle', 3,0,0,'EA',1980.000000,0.0000,'STANDARD','CALSTD26','S',20,1486.00,'STOCK',NULL,'OPEN', 24,0),
 ('CAL','SOC900004',1,'STOCK','CAL-BIKE-GRAVEL-M','Complete bicycle, gravel, medium', 3,0,0,'EA',1380.000000,6.0000,'GROUP','CALTRD26','S',20,968.00,'STOCK',NULL,'OPEN', 3,0),
 ('CAL','SOC900005',1,'STOCK','CAL-BIKE-ROAD-54','Complete bicycle, road, 54 cm',     6,1,0,'EA',1450.000000,6.0000,'GROUP','CALTRD26','S',20,962.00,'STOCK',NULL,'OPEN', -1,5),
 ('CAL','SOC900005',2,'SERVICE','SVC-ASSEMBLY-HOUR','Service, assembly labour',      12,0,0,'HR',  58.000000,0.0000,'STANDARD','CALSTD26','S',20, 38.00,'STOCK',NULL,'OPEN', -1,5);

INSERT so.SalesOrderLine (SalesOrderId, CompanyCode, OrderNumber, LineNum, LineRef, ItemCode, Description, LineType,
                          AcceptedQty, CancelledQty, DespatchedQty, BilledQty, ReturnedQty, UomCode, ConversionFactor,
                          StockQty, UnitPrice, PricePerQty, PriceSource, PriceListCode, DiscountPct1, DiscountPct2,
                          NetAmount, VatCode, VatRatePct, VatAmount, CostSnapshotGBP, CostBasis, MarginPct,
                          SupplyType, LinkedPoNumber, Status)
SELECT h.SalesOrderId, l.CompanyCode, l.OrderNumber, l.LineNum, CONVERT(varchar(10), l.LineNum * 10), l.ItemCode,
       l.Description, l.LineType, l.Qty, l.Cancelled, l.Despatched, 0, 0, l.UomCode, 1, l.Qty - l.Cancelled,
       l.UnitPrice, 1, l.PriceSource, l.PriceList, l.Disc1, 0,
       CAST((l.Qty - l.Cancelled) * l.UnitPrice * (1 - l.Disc1 / 100.0) AS decimal(18,2)), l.VatCode, l.VatRate,
       CAST(CAST((l.Qty - l.Cancelled) * l.UnitPrice * (1 - l.Disc1 / 100.0) AS decimal(18,2)) * l.VatRate / 100.0 AS decimal(18,2)),
       l.UnitCost, 'STANDARD',
       CAST(100.0 * (l.UnitPrice * (1 - l.Disc1 / 100.0) / NULLIF(h.FxRate, 0) - l.UnitCost)
            / NULLIF(l.UnitPrice * (1 - l.Disc1 / 100.0) / NULLIF(h.FxRate, 0), 0) AS decimal(9,4)),
       l.SupplyType, l.LinkedPo, l.LineStatus
  FROM @ol l
  JOIN so.SalesOrder h ON h.CompanyCode = l.CompanyCode AND h.OrderNumber = l.OrderNumber
 WHERE NOT EXISTS (SELECT 1 FROM so.SalesOrderLine x WHERE x.SalesOrderId = h.SalesOrderId AND x.LineNum = l.LineNum);

/*  One schedule per line.  SOS900006 and SOC900005 carry a re-promised schedule, so the original
    promise is still visible beside the current one and the slippage is measurable (SO-008). */
INSERT so.DeliverySchedule (SalesOrderLineId, CompanyCode, OrderNumber, LineNum, ScheduleNo, Quantity, DespatchedQty,
                            SiteCode, RequestedDate, OriginalPromiseDate, CurrentPromiseDate, ChangeReason, ChangeCount,
                            FulfilmentMethod, Priority, Status)
SELECT sl.SalesOrderLineId, l.CompanyCode, l.OrderNumber, l.LineNum, 1, l.Qty - l.Cancelled, l.Despatched,
       h.SiteCode, DATEADD(day, l.DueOffset, @today), DATEADD(day, l.DueOffset, @today),
       DATEADD(day, l.DueOffset + l.Slip, @today),
       CASE WHEN l.Slip > 0 THEN 'Re-promised after a supply shortage; the original promise is retained' END,
       CASE WHEN l.Slip > 0 THEN 1 ELSE 0 END, 'WAREHOUSE', 5,
       CASE WHEN l.LineStatus = 'CANCELLED' THEN 'CANCELLED' ELSE 'OPEN' END
  FROM @ol l
  JOIN so.SalesOrder h ON h.CompanyCode = l.CompanyCode AND h.OrderNumber = l.OrderNumber
  JOIN so.SalesOrderLine sl ON sl.SalesOrderId = h.SalesOrderId AND sl.LineNum = l.LineNum
 WHERE NOT EXISTS (SELECT 1 FROM so.DeliverySchedule x WHERE x.SalesOrderLineId = sl.SalesOrderLineId AND x.ScheduleNo = 1);

/*  Header money is always the sum of the lines; recomputing it keeps the seed re-runnable. */
UPDATE h SET NetAmount = t.Net, VatAmount = t.Vat, GrossAmount = t.Net + t.Vat,
             GrossAmountGBP = CAST((t.Net + t.Vat) / NULLIF(h.FxRate, 0) AS decimal(18,2))
  FROM so.SalesOrder h
 CROSS APPLY (SELECT Net = SUM(l.NetAmount), Vat = SUM(l.VatAmount)
                FROM so.SalesOrderLine l WHERE l.SalesOrderId = h.SalesOrderId) t
 WHERE h.CreatedBy = 'SEED' AND t.Net IS NOT NULL;
PRINT 'Seeded so.SalesOrder / so.SalesOrderLine / so.DeliverySchedule (demonstration)';
GO
/* ------------------------------------------------- the sales demonstration account (SO-007, D-06)
    Every seeded account that carries a picked order is deliberately in difficulty - over its limit,
    overdue, or newly opened - because the credit controls are part of what the module demonstrates.
    That leaves nothing to despatch, so each company also gets ONE account in good standing: no open
    items at all, a reviewed limit, no hold.  SODEMO finds it by Classification = 'SODEMO' and still
    puts it through CREDCHK before it quotes against it.  The two picked orders are re-pointed at it,
    so the order-to-cash demonstration runs end to end while C0010/K0003 stay properly held.
    REQ: SO-002, SO-007, SO-011, AR-001, AR-007 */
SET NOCOUNT ON;
GO
DECLARE @today date = CAST(SYSDATETIME() AS date);
DECLARE @dc table (CompanyCode varchar(10), CustomerCode varchar(20), LegalName varchar(120), TradingName varchar(120),
                   VatRegNo varchar(15), TermsCode varchar(12), Line1 varchar(60), Town varchar(40), County varchar(40),
                   Postcode varchar(12), Contact varchar(80), Email varchar(128), Phone varchar(30),
                   CreditLimit decimal(18,2), OverdueThreshold decimal(18,2));
INSERT @dc VALUES
 ('STC','C0090','Thameside Demonstration Cycles Limited','Thameside Cycles','GB914782551','N30',
  'Unit 7, Kennet Trade Park','Reading','Berkshire','RG1 8QD','Hannah Fielding','accounts@thamesidecycles.example','0118 496 2210',
  150000.00, 25000.00),
 ('CAL','K0090','Clyde Demonstration Cycles Limited','Clyde Cycles','GB914782552','N30',
  'Block 3, Riverside Works','Glasgow','Lanarkshire','G51 2QT','Douglas Fraser','accounts@clydecycles.example','0141 496 3307',
  120000.00, 20000.00);

INSERT ar.Customer (CompanyCode, CustomerCode, LegalName, TradingName, LegalForm, VatLocation, VatRegNo, VatValidatedAt,
                    IsVatBusiness, CountryCode, CurrencyCode, AllowedCurrencies, TermsCode, ArControlAccount,
                    CollectionMethod, StatementFrequency, SalespersonCode, AccountOwner, Classification,
                    RequiresPurchaseOrderRef, SanctionsStatus, SanctionsScreenedAt, Status, FirstTradedDate, CreatedBy)
SELECT d.CompanyCode, d.CustomerCode, d.LegalName, d.TradingName, 'LTD', 'GB', d.VatRegNo, DATEADD(day, -60, @today),
       1, 'GB', 'GBP', 'GBP', d.TermsCode,
       (SELECT TOP (1) c.ArControlAccount FROM ar.Customer c
         WHERE c.CompanyCode = d.CompanyCode AND c.ArControlAccount IS NOT NULL ORDER BY c.CustomerCode),
       'BACS', 'MONTHLY', 'SALESREP', 'SALESMGR', 'SODEMO', 0, 'CLEAR', DATEADD(day, -30, CAST(@today AS datetime2(3))),
       'ACTIVE', DATEADD(day, -200, @today), 'SEED'
  FROM @dc d
 WHERE NOT EXISTS (SELECT 1 FROM ar.Customer x WHERE x.CompanyCode = d.CompanyCode AND x.CustomerCode = d.CustomerCode);

INSERT ar.CustomerAddress (CompanyCode, CustomerCode, AddressRole, AddressRef, Name, Line1, Town, County, Postcode,
                           CountryCode, ContactName, ContactEmail, ContactPhone, ContactPurpose, IsDefault, ValidFrom,
                           CreatedBy)
SELECT d.CompanyCode, d.CustomerCode, r.AddressRole, 'MAIN', d.LegalName, d.Line1, d.Town, d.County, d.Postcode,
       'GB', d.Contact, d.Email, d.Phone, r.Purpose, 1, DATEADD(day, -200, @today), 'SEED'
  FROM @dc d
 CROSS JOIN (VALUES ('REGISTERED','Registered office'), ('BILLING','Invoices and statements'),
                    ('DELIVERY','Goods inwards, 08:00 to 16:00')) r (AddressRole, Purpose)
 WHERE NOT EXISTS (SELECT 1 FROM ar.CustomerAddress x WHERE x.CompanyCode = d.CompanyCode
                     AND x.CustomerCode = d.CustomerCode AND x.AddressRole = r.AddressRole AND x.AddressRef = 'MAIN');

INSERT ar.CustomerCredit (CompanyCode, CustomerCode, CreditLimitGBP, OverdueThresholdGBP, OverdueDaysTrigger, RiskClass,
                          CreditAgency, CreditScore, CreditScoreDate, ReviewDate, ControllerUser, ApprovedBy, ApprovedAt,
                          HoldFlag, RequiresPrepayment, ProFormaOnly, CreatedBy)
SELECT d.CompanyCode, d.CustomerCode, d.CreditLimit, d.OverdueThreshold, 30, 'LOW', 'Experian', 88,
       DATEADD(day, -45, @today), DATEADD(day, 320, @today), 'CREDITCTL', 'CREDITCTL',
       DATEADD(day, -45, CAST(@today AS datetime2(3))), 0, 0, 0, 'SEED'
  FROM @dc d
 WHERE NOT EXISTS (SELECT 1 FROM ar.CustomerCredit x WHERE x.CompanyCode = d.CompanyCode
                     AND x.CustomerCode = d.CustomerCode);

/*  Re-point the two picked orders at the demonstration account.  Guarded on the current value, so
    the script is re-runnable and so an order a demonstration has already despatched is untouched. */
UPDATE o
   SET o.CustomerCode = d.CustomerCode,
       o.InvoiceCustomerCode = d.CustomerCode,
       o.DeliveryAddressRef = 'MAIN',
       o.DeliveryAddressSnapshot = d.LegalName + ', ' + d.Line1 + ', ' + d.Town + ', ' + d.Postcode,
       o.BillToAddressSnapshot = d.LegalName + ', ' + d.Line1 + ', ' + d.Town + ', ' + d.Postcode,
       o.TermsCode = d.TermsCode,
       o.TermsSnapshot = 'Terms snapshotted at acceptance: ' + d.TermsCode,
       o.ModifiedBy = 'SEED', o.ModifiedAt = SYSDATETIME()
  FROM so.SalesOrder o
  JOIN @dc d ON d.CompanyCode = o.CompanyCode
 WHERE o.CreatedBy = 'SEED'
   AND o.OrderNumber IN ('SOS900008','SOC900004')
   AND o.CustomerCode <> d.CustomerCode;
PRINT 'Seeded the sales demonstration account (C0090 / K0090) and re-pointed the picked orders';
GO
/*  Demonstration orders built by SODEMO before the quote conversion completed the header (CR2601)
    can never be accepted - no invoiced party, no delivery address, no terms, no VAT basis - and a
    later run would keep adopting them.  They carry no despatch, pick or invoice, so they are
    removed here and the driver raises a complete one.  Guarded, so nothing a demonstration has
    actually worked on is touched.  REQ: SO-001, SO-002 */
SET NOCOUNT ON;
GO
DECLARE @stale table (SalesOrderId bigint PRIMARY KEY);
INSERT @stale (SalesOrderId)
SELECT o.SalesOrderId FROM so.SalesOrder o
 WHERE o.CustomerOrderRef LIKE 'SODEMO-%'
   AND o.CommercialStatus = 'DRAFT'
   AND (o.InvoiceCustomerCode IS NULL OR o.DeliveryAddressSnapshot IS NULL
        OR o.TermsAndConditionsVersion IS NULL OR o.VatTreatmentBasis IS NULL)
   AND NOT EXISTS (SELECT 1 FROM so.Despatch d WHERE d.CompanyCode = o.CompanyCode AND d.OrderNumber = o.OrderNumber)
   AND NOT EXISTS (SELECT 1 FROM so.PickList p WHERE p.CompanyCode = o.CompanyCode AND p.OrderNumber = o.OrderNumber);

DELETE s FROM so.DeliverySchedule s
  JOIN so.SalesOrderLine l ON l.SalesOrderLineId = s.SalesOrderLineId
 WHERE l.SalesOrderId IN (SELECT SalesOrderId FROM @stale);
DELETE l FROM so.SalesOrderLine l WHERE l.SalesOrderId IN (SELECT SalesOrderId FROM @stale);
UPDATE q SET q.Status = 'ACCEPTED', q.ConvertedOrderNumber = NULL
  FROM so.Quote q JOIN so.SalesOrder o ON o.QuoteNumber = q.QuoteNumber AND o.CompanyCode = q.CompanyCode
 WHERE o.SalesOrderId IN (SELECT SalesOrderId FROM @stale) AND q.Status = 'CONVERTED';
DELETE o FROM so.SalesOrder o WHERE o.SalesOrderId IN (SELECT SalesOrderId FROM @stale);
PRINT 'Removed incomplete SODEMO orders left by an earlier run';
GO
/*  ... and the quotations those abandoned runs left behind.  SODEMO stamps its own quotations with
    a delivery estimate nothing else uses, so they can be recognised without guessing; one that
    never became an order is removed, which also keeps the SOQUOTE number series and the quotation
    register in step.  REQ: SO-001 */
SET NOCOUNT ON;
GO
DECLARE @sq table (QuoteId bigint PRIMARY KEY);
INSERT @sq (QuoteId)
SELECT q.QuoteId FROM so.Quote q
 WHERE q.DeliveryEstimate = 'Demonstration quotation built by SODEMO'
   AND (q.ConvertedOrderNumber IS NULL
        OR NOT EXISTS (SELECT 1 FROM so.SalesOrder o WHERE o.CompanyCode = q.CompanyCode
                         AND o.OrderNumber = q.ConvertedOrderNumber));
DELETE l FROM so.QuoteLine l WHERE l.QuoteId IN (SELECT QuoteId FROM @sq);
DELETE q FROM so.Quote q WHERE q.QuoteId IN (SELECT QuoteId FROM @sq);
PRINT 'Removed abandoned SODEMO quotations';
GO
/*  The quotation number series.  SOQUOTE asks NUMGEN for series SOQUOTE and nothing ever defined
    it, so every run after the first collided with the quotation the run before had numbered.  It
    is defined here beside the demonstration data it serves, modelled on the sales order series of
    the same company, and wound past anything already issued so the register stays unique.
    REQ: SO-001, COM-012 */
SET NOCOUNT ON;
GO
INSERT core.NumberSeries (CompanyCode, SeriesCode, DocumentType, Description, Prefix, NumberLength,
                          NextNumber, MinNumber, MaxNumber, IsLegalSequence, IsActive, CreatedBy)
SELECT s.CompanyCode, 'SQUO', 'QUOTE', 'Sales quotations',
       CASE s.CompanyCode WHEN 'CAL' THEN 'SQC' ELSE 'SQS' END,
       s.NumberLength, 1, s.MinNumber, s.MaxNumber, 0, 1, 'SEED'
  FROM core.NumberSeries s
 WHERE s.SeriesCode = 'SORD'
   AND NOT EXISTS (SELECT 1 FROM core.NumberSeries x WHERE x.CompanyCode = s.CompanyCode AND x.SeriesCode = 'SQUO');

UPDATE s SET s.NextNumber = v.NextFree
  FROM core.NumberSeries s
 CROSS APPLY (SELECT NextFree = ISNULL(MAX(x.n), 0) + 1
                FROM (SELECT n = TRY_CONVERT(int, SUBSTRING(q.QuoteNumber, LEN(s.Prefix) + 1, 20))
                        FROM so.Quote q
                       WHERE q.CompanyCode = s.CompanyCode AND q.QuoteNumber LIKE s.Prefix + '%') x
               WHERE x.n < 900000) v
 WHERE s.SeriesCode = 'SQUO' AND s.NextNumber < v.NextFree;
PRINT 'Defined the quotation number series and wound it past the issued numbers';
GO

DELETE FROM core.NumberSeries WHERE SeriesCode = 'SOQUOTE' AND CreatedBy = 'SEED';
GO
/*  An abandoned demonstration order that was already picked cannot be deleted without disturbing
    the stock it holds, but the quotation behind it must not keep the number series blocked: the
    provenance is cleared and the quotation removed, so the next run numbers a fresh one.  The
    order itself stays, visibly incomplete, and SODEMO no longer adopts it.  REQ: SO-001 */
SET NOCOUNT ON;
GO
DECLARE @orphan table (QuoteId bigint PRIMARY KEY, CompanyCode varchar(10), QuoteNumber varchar(30));
INSERT @orphan (QuoteId, CompanyCode, QuoteNumber)
SELECT q.QuoteId, q.CompanyCode, q.QuoteNumber
  FROM so.Quote q
 WHERE q.DeliveryEstimate = 'Demonstration quotation built by SODEMO'
   AND EXISTS (SELECT 1 FROM so.SalesOrder o
                WHERE o.CompanyCode = q.CompanyCode AND o.OrderNumber = q.ConvertedOrderNumber
                  AND o.CommercialStatus = 'DRAFT'
                  AND (o.InvoiceCustomerCode IS NULL OR o.DeliveryAddressSnapshot IS NULL
                       OR o.TermsAndConditionsVersion IS NULL OR o.VatTreatmentBasis IS NULL));
UPDATE o SET o.QuoteNumber = NULL
  FROM so.SalesOrder o JOIN @orphan x ON x.CompanyCode = o.CompanyCode AND x.QuoteNumber = o.QuoteNumber;
DELETE l FROM so.QuoteLine l WHERE l.QuoteId IN (SELECT QuoteId FROM @orphan);
DELETE q FROM so.Quote q WHERE q.QuoteId IN (SELECT QuoteId FROM @orphan);
PRINT 'Released the quotation numbers held by abandoned demonstration orders';
GO
/*  The idempotency keys SODEMO claimed on an earlier run point at the order that run produced.
    Where that order no longer exists, or exists but was raised before the conversion completed the
    header (CR2601) and so can never be accepted, the key is released: the next run converts its
    quotation into a complete order instead of being handed the old one back with SOQ0011.  A key
    whose order is sound is left alone, which is what keeps a repeated run from demanding twice.
    REQ: SO-001, COM-015 */
SET NOCOUNT ON;
GO
DELETE k
  FROM core.IdempotencyKey k
 WHERE k.CorrelationId LIKE 'SODEMO-%'
   AND NOT EXISTS (SELECT 1 FROM so.SalesOrder o
                    WHERE o.OrderNumber = k.ResultRef
                      AND o.InvoiceCustomerCode IS NOT NULL
                      AND o.DeliveryAddressSnapshot IS NOT NULL
                      AND o.TermsAndConditionsVersion IS NOT NULL
                      AND o.VatTreatmentBasis IS NOT NULL);
PRINT 'Released SODEMO idempotency keys that point at an unusable order';
GO
/*  ... and the sales order series with it.  A conversion that fails rolls the allocation back, so
    after a few abandoned runs the series can sit behind orders that were committed.  Winding it
    past what is actually held keeps the next conversion from colliding.  REQ: SO-001, COM-012 */
SET NOCOUNT ON;
GO
UPDATE s SET s.NextNumber = v.NextFree
  FROM core.NumberSeries s
 CROSS APPLY (SELECT NextFree = ISNULL(MAX(x.n), 0) + 1
                FROM (SELECT n = TRY_CONVERT(int, SUBSTRING(o.OrderNumber, LEN(s.Prefix) + 1, 20))
                        FROM so.SalesOrder o
                       WHERE o.CompanyCode = s.CompanyCode AND o.OrderNumber LIKE s.Prefix + '%') x
               WHERE x.n < 900000) v
 WHERE s.SeriesCode = 'SORD' AND s.NextNumber < v.NextFree;
PRINT 'Wound the sales order number series past the orders already held';
GO

/* ------------------------------------------------- a delivery that was made on time (SO-013, XMD-002)
    Every seeded schedule promised in the past is still open, so the customer on time in full KPI
    (bin\x86\KPIOTIF.exe, cohort = original promise date) had nothing to count and always answered
    zero.  A demonstration dataset needs at least one promise that was kept: line 2 of SOS900006 -
    the cassettes - left the warehouse on the day it was originally promised, in full, and was
    billed in an earlier period, so it is complete and outside the open billing queue.
    REQ: SO-013, SO-017 */
SET NOCOUNT ON;
GO
DECLARE @today date = CAST(SYSDATETIME() AS date);
DECLARE @promise date = (SELECT MIN(s.OriginalPromiseDate) FROM so.DeliverySchedule s
                          WHERE s.CompanyCode = 'STC' AND s.OrderNumber = 'SOS900006' AND s.LineNum = 2);
IF @promise IS NOT NULL AND NOT EXISTS (SELECT 1 FROM so.Despatch d WHERE d.CompanyCode = 'STC' AND d.DespatchRef = 'DNS900001')
BEGIN
    INSERT so.Despatch (CompanyCode, SiteCode, DespatchRef, CustomerCode, OrderNumber, DespatchDate, PostingDate,
                        DeliveryAddressSnapshot, IncotermsRule, IncotermsNamedPlace, PackCount, GrossWeightKg,
                        IsExport, DestinationCountry, ExportEvidenceStatus, DangerousGoodsDeclared, Status,
                        CogsPostedGBP, IsCutoffReview, ConfirmedBy, ConfirmedAt, CreatedBy)
    SELECT o.CompanyCode, o.SiteCode, 'DNS900001', o.CustomerCode, o.OrderNumber, @promise, @promise,
           o.DeliveryAddressSnapshot, o.IncotermsRule, o.IncotermsNamedPlace, 2, 18.400,
           0, 'GB', 'NA', 0, 'CONFIRMED', 0, 0, 'SALESREP',
           CAST(@promise AS datetime2(3)), 'SEED'
      FROM so.SalesOrder o
     WHERE o.CompanyCode = 'STC' AND o.OrderNumber = 'SOS900006';

    INSERT so.DespatchLine (DespatchId, LineNum, SalesOrderLineId, OrderNumber, OrderLineNum, ScheduleNo, ItemCode,
                            Description, PickedQty, DespatchedQty, UomCode, ConversionFactor, StockQty,
                            UnitCostGBP, CogsGBP, CostMethod, BilledQty)
    SELECT d.DespatchId, 1, sl.SalesOrderLineId, s.OrderNumber, s.LineNum, s.ScheduleNo, sl.ItemCode,
           sl.Description, s.Quantity, s.Quantity, sl.UomCode, 1, s.Quantity,
           sl.CostSnapshotGBP, s.Quantity * ISNULL(sl.CostSnapshotGBP, 0), 'STD', s.Quantity
      FROM so.Despatch d
      JOIN so.DeliverySchedule s ON s.CompanyCode = d.CompanyCode AND s.OrderNumber = d.OrderNumber AND s.LineNum = 2
      JOIN so.SalesOrderLine sl ON sl.SalesOrderLineId = s.SalesOrderLineId
     WHERE d.CompanyCode = 'STC' AND d.DespatchRef = 'DNS900001';

    UPDATE s SET s.DespatchedQty = s.Quantity, s.Status = 'COMPLETE'
      FROM so.DeliverySchedule s
     WHERE s.CompanyCode = 'STC' AND s.OrderNumber = 'SOS900006' AND s.LineNum = 2;

    UPDATE sl SET sl.DespatchedQty = sl.AcceptedQty - sl.CancelledQty,
                  sl.BilledQty = sl.AcceptedQty - sl.CancelledQty, sl.Status = 'CLOSED'
      FROM so.SalesOrderLine sl
      JOIN so.SalesOrder o ON o.SalesOrderId = sl.SalesOrderId
     WHERE o.CompanyCode = 'STC' AND o.OrderNumber = 'SOS900006' AND sl.LineNum = 2;
END
PRINT 'Seeded the on time in full delivery DNS900001 for SOS900006 line 2';
GO
