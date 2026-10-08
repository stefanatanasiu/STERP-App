/*  Account classes, chart of accounts for both companies, analysis dimensions and the
    account/dimension rules used by fin.usp_PostJournal.  Control accounts are closed to manual
    journals so only their own subledger may move them.
    REQ: FIN-001 to FIN-005, FIN-013, FIN-023, COM-025, D-01 */
SET NOCOUNT ON;
GO
MERGE fin.AccountClass AS t
USING (VALUES
 ('FA','Tangible fixed assets','ASSET','DR','BS',10),('FADEP','Accumulated depreciation','ASSET','CR','BS',15),
 ('STOCK','Stocks','ASSET','DR','BS',20),('DEBT','Debtors','ASSET','DR','BS',30),
 ('CASH','Cash at bank and in hand','ASSET','DR','BS',40),('CRED','Creditors within one year','LIABILITY','CR','BS',50),
 ('CREDLT','Creditors after one year','LIABILITY','CR','BS',55),('TAXBS','Taxation and social security','LIABILITY','CR','BS',60),
 ('EQ','Capital and reserves','EQUITY','CR','BS',70),('TURN','Turnover','REVENUE','CR','PL',80),
 ('COS','Cost of sales','EXPENSE','DR','PL',90),('VARI','Manufacturing variances','EXPENSE','DR','PL',95),
 ('DIST','Distribution costs','EXPENSE','DR','PL',100),('ADMIN','Administrative expenses','EXPENSE','DR','PL',105),
 ('OTHINC','Other operating income','REVENUE','CR','PL',110),('FINC','Interest and similar items','EXPENSE','DR','PL',120),
 ('TAXPL','Tax on profit','EXPENSE','DR','PL',130)
) AS s (ClassCode, Name, ClassType, NormalBalance, Statement, SortOrder)
ON t.ClassCode = s.ClassCode
WHEN MATCHED AND EXISTS (SELECT s.Name, s.ClassType, s.NormalBalance, s.Statement, s.SortOrder
                         EXCEPT SELECT t.Name, t.ClassType, t.NormalBalance, t.Statement, t.SortOrder)
    THEN UPDATE SET Name = s.Name, ClassType = s.ClassType, NormalBalance = s.NormalBalance, Statement = s.Statement, SortOrder = s.SortOrder
WHEN NOT MATCHED THEN INSERT (ClassCode, Name, ClassType, NormalBalance, Statement, SortOrder)
    VALUES (s.ClassCode, s.Name, s.ClassType, s.NormalBalance, s.Statement, s.SortOrder);
GO
DECLARE @a TABLE (AccountCode varchar(20), Name varchar(100), ClassCode varchar(10), ControlType varchar(12),
                  ControlSubledger varchar(10), AllowManual bit, CurrencyCode char(3), IsVatEligible bit,
                  StatutoryLineCode varchar(20), ManagementGroup varchar(20));
INSERT @a VALUES
 ('1000','Freehold land and buildings','FA',NULL,NULL,1,NULL,0,'BS-FA-TAN','FIXEDASSETS'),
 ('1010','Plant and machinery','FA',NULL,NULL,1,NULL,0,'BS-FA-TAN','FIXEDASSETS'),
 ('1020','Fixtures and fittings','FA',NULL,NULL,1,NULL,0,'BS-FA-TAN','FIXEDASSETS'),
 ('1030','Motor vehicles','FA',NULL,NULL,1,NULL,0,'BS-FA-TAN','FIXEDASSETS'),
 ('1050','Depreciation provision, buildings','FADEP',NULL,NULL,1,NULL,0,'BS-FA-TAN','FIXEDASSETS'),
 ('1060','Depreciation provision, plant','FADEP',NULL,NULL,1,NULL,0,'BS-FA-TAN','FIXEDASSETS'),
 ('1070','Depreciation provision, fixtures','FADEP',NULL,NULL,1,NULL,0,'BS-FA-TAN','FIXEDASSETS'),
 ('1080','Depreciation provision, vehicles','FADEP',NULL,NULL,1,NULL,0,'BS-FA-TAN','FIXEDASSETS'),
 ('1100','Stock, raw materials and components','STOCK','INVENTORY','INV',0,NULL,0,'BS-CA-STK','INVENTORY'),
 ('1110','Work in progress','STOCK','WIP','WIP',0,NULL,0,'BS-CA-STK','INVENTORY'),
 ('1120','Stock, finished goods','STOCK','INVENTORY','INV',0,NULL,0,'BS-CA-STK','INVENTORY'),
 ('1130','Stock in transit','STOCK','INVENTORY','INV',0,NULL,0,'BS-CA-STK','INVENTORY'),
 ('1140','Stock held in quarantine','STOCK','INVENTORY','INV',0,NULL,0,'BS-CA-STK','INVENTORY'),
 ('1150','Stock allowance, net realisable value','STOCK',NULL,NULL,1,NULL,0,'BS-CA-STK','INVENTORY'),
 ('1200','Trade debtors control','DEBT','AR','AR',0,NULL,0,'BS-CA-DEB','RECEIVABLES'),
 ('1215','Accrued income, despatched not invoiced','DEBT','ACCRUAL',NULL,1,NULL,0,'BS-CA-DEB','RECEIVABLES'),
 ('1220','Prepayments','DEBT',NULL,NULL,1,NULL,0,'BS-CA-DEB','RECEIVABLES'),
 ('1230','Provision for doubtful debts','DEBT',NULL,NULL,1,NULL,0,'BS-CA-DEB','RECEIVABLES'),
 ('1240','Other debtors','DEBT',NULL,NULL,1,NULL,0,'BS-CA-DEB','RECEIVABLES'),
 ('1250','VAT recoverable, input tax','DEBT','VAT','VAT',0,NULL,0,'BS-CA-DEB','TAX'),
 ('1260','Postponed import VAT, input side','DEBT','VAT','VAT',0,NULL,0,'BS-CA-DEB','TAX'),
 ('1270','Deposits paid to suppliers','DEBT',NULL,NULL,1,NULL,0,'BS-CA-DEB','RECEIVABLES'),
 ('1300','Bank current account, sterling','CASH','BANK','BANK',0,'GBP',0,'BS-CA-CSH','TREASURY'),
 ('1310','Bank current account, euro','CASH','BANK','BANK',0,'EUR',0,'BS-CA-CSH','TREASURY'),
 ('1320','Bank current account, US dollar','CASH','BANK','BANK',0,'USD',0,'BS-CA-CSH','TREASURY'),
 ('1330','Bank deposit account','CASH','DEPOSIT','BANK',0,'GBP',0,'BS-CA-CSH','TREASURY'),
 ('1340','Petty cash','CASH',NULL,NULL,1,'GBP',0,'BS-CA-CSH','TREASURY'),
 ('1350','Card acquirer settlement','CASH','BANK','BANK',0,'GBP',0,'BS-CA-CSH','TREASURY'),
 ('1360','Bank clearing, unpresented items','CASH',NULL,NULL,1,'GBP',0,'BS-CA-CSH','TREASURY'),
 ('1390','Payments in transit','CASH',NULL,NULL,1,'GBP',0,'BS-CA-CSH','TREASURY'),
 ('2000','Trade creditors control','CRED','AP','AP',0,NULL,0,'BS-CR-1YR','PAYABLES'),
 ('2010','Goods received not invoiced','CRED','GRNI','GRNI',0,NULL,0,'BS-CR-1YR','PAYABLES'),
 ('2020','Accruals','CRED','ACCRUAL',NULL,1,NULL,0,'BS-CR-1YR','PAYABLES'),
 ('2030','Other creditors','CRED',NULL,NULL,1,NULL,0,'BS-CR-1YR','PAYABLES'),
 ('2040','VAT payable, output tax','TAXBS','VAT','VAT',0,NULL,0,'BS-CR-1YR','TAX'),
 ('2050','VAT control, return in progress','TAXBS','VAT','VAT',0,NULL,0,'BS-CR-1YR','TAX'),
 ('2060','PAYE and national insurance','TAXBS',NULL,NULL,1,NULL,0,'BS-CR-1YR','TAX'),
 ('2070','Corporation tax payable','TAXBS',NULL,NULL,1,NULL,0,'BS-CR-1YR','TAX'),
 ('2080','Customer deposits and deferred income','CRED','DEPOSIT',NULL,1,NULL,0,'BS-CR-1YR','RECEIVABLES'),
 ('2090','Rebate and retrospective discount accrual','CRED','ACCRUAL',NULL,1,NULL,0,'BS-CR-1YR','PAYABLES'),
 ('2100','Customer credit balances','CRED',NULL,NULL,1,NULL,0,'BS-CR-1YR','RECEIVABLES'),
 ('2110','Payroll control','CRED',NULL,NULL,1,NULL,0,'BS-CR-1YR','PAYROLL'),
 ('2200','Bank loans due within one year','CRED',NULL,NULL,1,'GBP',0,'BS-CR-1YR','TREASURY'),
 ('2300','Bank loans due after one year','CREDLT',NULL,NULL,1,'GBP',0,'BS-CR-GT1','TREASURY'),
 ('3000','Called up share capital','EQ',NULL,NULL,1,NULL,0,'BS-CAP-SHR','EQUITY'),
 ('3100','Share premium account','EQ',NULL,NULL,1,NULL,0,'BS-CAP-SHR','EQUITY'),
 ('3200','Profit and loss reserve','EQ','RETAINED',NULL,0,NULL,0,'BS-CAP-RES','EQUITY'),
 ('3300','Profit or loss for the financial year','EQ',NULL,NULL,0,NULL,0,'BS-CAP-RES','EQUITY'),
 ('3400','Opening balance reserve, cut over differences','EQ',NULL,NULL,1,NULL,0,'BS-CAP-RES','EQUITY'),
 ('4000','Sales, components, United Kingdom','TURN',NULL,NULL,0,NULL,1,'PL-TURN','SALES'),
 ('4010','Sales, assemblies, United Kingdom','TURN',NULL,NULL,0,NULL,1,'PL-TURN','SALES'),
 ('4020','Sales, kits, United Kingdom','TURN',NULL,NULL,0,NULL,1,'PL-TURN','SALES'),
 ('4030','Sales, services','TURN',NULL,NULL,0,NULL,1,'PL-TURN','SALES'),
 ('4040','Sales, export to the European Union','TURN',NULL,NULL,0,NULL,1,'PL-TURN','SALES'),
 ('4050','Sales, export, rest of the world','TURN',NULL,NULL,0,NULL,1,'PL-TURN','SALES'),
 ('4060','Sales, Northern Ireland','TURN',NULL,NULL,0,NULL,1,'PL-TURN','SALES'),
 ('4100','Sales returns and credits','TURN',NULL,NULL,0,NULL,1,'PL-TURN','SALES'),
 ('4110','Settlement discounts allowed','TURN',NULL,NULL,1,NULL,1,'PL-TURN','SALES'),
 ('4120','Rebates and retrospective discounts','TURN',NULL,NULL,1,NULL,1,'PL-TURN','SALES'),
 ('4200','Carriage recharged to customers','TURN',NULL,NULL,1,NULL,1,'PL-TURN','SALES'),
 ('4900','Other operating income','OTHINC',NULL,NULL,1,NULL,1,'PL-OTHINC','OTHER'),
 ('4950','Scrap and by-product income','OTHINC',NULL,NULL,1,NULL,1,'PL-OTHINC','OTHER'),
 ('5000','Materials consumed at standard','COS',NULL,NULL,0,NULL,0,'PL-COS','PRODUCTION'),
 ('5010','Cost of goods sold, components','COS',NULL,NULL,0,NULL,0,'PL-COS','PRODUCTION'),
 ('5020','Cost of goods sold, assemblies','COS',NULL,NULL,0,NULL,0,'PL-COS','PRODUCTION'),
 ('5030','Cost of services delivered','COS',NULL,NULL,1,NULL,0,'PL-COS','PRODUCTION'),
 ('5100','Purchase price variance','VARI',NULL,NULL,0,NULL,0,'PL-COS','VARIANCE'),
 ('5110','Material usage variance','VARI',NULL,NULL,0,NULL,0,'PL-COS','VARIANCE'),
 ('5120','Labour rate variance','VARI',NULL,NULL,0,NULL,0,'PL-COS','VARIANCE'),
 ('5130','Labour efficiency variance','VARI',NULL,NULL,0,NULL,0,'PL-COS','VARIANCE'),
 ('5140','Overhead absorption variance','VARI',NULL,NULL,0,NULL,0,'PL-COS','VARIANCE'),
 ('5150','Scrap and rework variance','VARI',NULL,NULL,0,NULL,0,'PL-COS','VARIANCE'),
 ('5160','Yield variance','VARI',NULL,NULL,0,NULL,0,'PL-COS','VARIANCE'),
 ('5200','Landed cost absorbed','COS',NULL,NULL,0,NULL,0,'PL-COS','PRODUCTION'),
 ('5210','Carriage inwards','COS',NULL,NULL,1,NULL,1,'PL-COS','PRODUCTION'),
 ('5220','Duty and import charges','COS',NULL,NULL,1,NULL,0,'PL-COS','PRODUCTION'),
 ('5300','Stock adjustments and count differences','COS',NULL,NULL,0,NULL,0,'PL-COS','INVENTORY'),
 ('5310','Stock write down to net realisable value','COS',NULL,NULL,0,NULL,0,'PL-COS','INVENTORY'),
 ('5320','Stock obsolescence provision movement','COS',NULL,NULL,1,NULL,0,'PL-COS','INVENTORY'),
 ('5400','Subcontract costs','COS',NULL,NULL,1,NULL,1,'PL-COS','PRODUCTION'),
 ('5500','Direct labour','COS',NULL,NULL,0,NULL,0,'PL-COS','PRODUCTION'),
 ('5510','Production overhead absorbed','COS',NULL,NULL,0,NULL,0,'PL-COS','PRODUCTION'),
 ('5520','Work in progress movement','COS',NULL,NULL,0,NULL,0,'PL-COS','PRODUCTION'),
 ('6000','Wages and salaries','ADMIN',NULL,NULL,1,NULL,0,'PL-ADMIN','PEOPLE'),
 ('6010','Employer national insurance','ADMIN',NULL,NULL,1,NULL,0,'PL-ADMIN','PEOPLE'),
 ('6020','Pension contributions','ADMIN',NULL,NULL,1,NULL,0,'PL-ADMIN','PEOPLE'),
 ('6100','Rent and business rates','ADMIN',NULL,NULL,1,NULL,1,'PL-ADMIN','PREMISES'),
 ('6110','Light, heat and power','ADMIN',NULL,NULL,1,NULL,1,'PL-ADMIN','PREMISES'),
 ('6120','Insurance','ADMIN',NULL,NULL,1,NULL,0,'PL-ADMIN','PREMISES'),
 ('6130','Repairs and maintenance','ADMIN',NULL,NULL,1,NULL,1,'PL-ADMIN','PREMISES'),
 ('6200','Motor and travel','DIST',NULL,NULL,1,NULL,1,'PL-DIST','SELLING'),
 ('6210','Carriage outwards','DIST',NULL,NULL,1,NULL,1,'PL-DIST','SELLING'),
 ('6220','Packaging and consumables','DIST',NULL,NULL,1,NULL,1,'PL-DIST','SELLING'),
 ('6300','Printing, postage and stationery','ADMIN',NULL,NULL,1,NULL,1,'PL-ADMIN','OFFICE'),
 ('6310','Telephone and data','ADMIN',NULL,NULL,1,NULL,1,'PL-ADMIN','OFFICE'),
 ('6320','Information technology and software','ADMIN',NULL,NULL,1,NULL,1,'PL-ADMIN','OFFICE'),
 ('6400','Professional and legal fees','ADMIN',NULL,NULL,1,NULL,1,'PL-ADMIN','PROFESSIONAL'),
 ('6410','Audit and accountancy','ADMIN',NULL,NULL,1,NULL,1,'PL-ADMIN','PROFESSIONAL'),
 ('6420','Bank charges','ADMIN',NULL,NULL,1,NULL,0,'PL-ADMIN','TREASURY'),
 ('6430','Credit insurance','ADMIN',NULL,NULL,1,NULL,0,'PL-ADMIN','PROFESSIONAL'),
 ('6500','Advertising and marketing','DIST',NULL,NULL,1,NULL,1,'PL-DIST','SELLING'),
 ('6510','Entertaining, not deductible','ADMIN',NULL,NULL,1,NULL,0,'PL-ADMIN','SELLING'),
 ('6600','Depreciation, plant and machinery','ADMIN',NULL,NULL,1,NULL,0,'PL-ADMIN','FIXEDASSETS'),
 ('6610','Depreciation, fixtures and fittings','ADMIN',NULL,NULL,1,NULL,0,'PL-ADMIN','FIXEDASSETS'),
 ('6620','Depreciation, motor vehicles','ADMIN',NULL,NULL,1,NULL,0,'PL-ADMIN','FIXEDASSETS'),
 ('6700','Bad debts written off','ADMIN',NULL,NULL,0,NULL,0,'PL-ADMIN','RECEIVABLES'),
 ('6710','Movement on doubtful debt provision','ADMIN',NULL,NULL,1,NULL,0,'PL-ADMIN','RECEIVABLES'),
 ('6800','Sundry expenses','ADMIN',NULL,NULL,1,NULL,1,'PL-ADMIN','OFFICE'),
 ('6900','Rounding and settlement differences','ADMIN',NULL,NULL,0,NULL,0,'PL-ADMIN','OTHER'),
 ('7000','Interest payable and similar charges','FINC',NULL,NULL,1,NULL,0,'PL-INT','TREASURY'),
 ('7010','Interest receivable','FINC',NULL,NULL,1,NULL,0,'PL-INT','TREASURY'),
 ('7100','Realised exchange differences','FINC',NULL,NULL,0,NULL,0,'PL-INT','TREASURY'),
 ('7110','Unrealised exchange differences on revaluation','FINC',NULL,NULL,0,NULL,0,'PL-INT','TREASURY'),
 ('7200','Late payment interest and compensation','OTHINC',NULL,NULL,1,NULL,0,'PL-OTHINC','RECEIVABLES'),
 ('7300','Profit or loss on disposal of fixed assets','OTHINC',NULL,NULL,1,NULL,0,'PL-OTHINC','FIXEDASSETS'),
 ('8000','Tax on profit for the year','TAXPL',NULL,NULL,1,NULL,0,'PL-TAX','TAX');
INSERT fin.Account (CompanyCode, AccountCode, Name, ClassCode, AccountKind, ControlType, ControlSubledger,
                    AllowManualJournal, CurrencyCode, IsVatEligible, StatutoryLineCode, ManagementGroup, ValidFrom, Status, CreatedBy)
SELECT c.CompanyCode, a.AccountCode, a.Name, a.ClassCode, 'POSTING', a.ControlType, a.ControlSubledger,
       a.AllowManual, a.CurrencyCode, a.IsVatEligible, a.StatutoryLineCode, a.ManagementGroup, '2024-04-01', 'ACTIVE', 'SEED'
  FROM core.Company c CROSS JOIN @a a
 WHERE NOT EXISTS (SELECT 1 FROM fin.Account x WHERE x.CompanyCode = c.CompanyCode AND x.AccountCode = a.AccountCode);
GO
MERGE fin.Dimension AS t
USING (VALUES ('CC','Cost centre','Responsibility reporting and budget control',10,1),
              ('PROJ','Project or job','Project, job and customer programme analysis',20,1)) AS s (DimensionCode, Name, Purpose, SortOrder, IsActive)
ON t.DimensionCode = s.DimensionCode
WHEN MATCHED AND EXISTS (SELECT s.Name, s.Purpose EXCEPT SELECT t.Name, t.Purpose)
    THEN UPDATE SET Name = s.Name, Purpose = s.Purpose, SortOrder = s.SortOrder, IsActive = s.IsActive
WHEN NOT MATCHED THEN INSERT (DimensionCode, Name, Purpose, SortOrder, IsActive) VALUES (s.DimensionCode, s.Name, s.Purpose, s.SortOrder, s.IsActive);
GO
DECLARE @d TABLE (DimensionCode varchar(10), DimValue varchar(20), Name varchar(100), SiteCode varchar(10), OwnerUser varchar(20));
INSERT @d VALUES
 ('CC','ADMIN','Administration and finance',NULL,'FINCTRL'), ('CC','SALES','Sales and marketing',NULL,'SALESMGR'),
 ('CC','PURCH','Purchasing',NULL,'PURMGR'),                  ('CC','ENG','Engineering and design',NULL,'ENGMGR'),
 ('CC','PROD1','Machining and fabrication',NULL,'INVMGR'),   ('CC','PROD2','Assembly and test',NULL,'INVMGR'),
 ('CC','WHSE','Warehouse and distribution',NULL,'INVMGR'),   ('CC','QUAL','Quality and compliance',NULL,'QUALITY'),
 ('CC','MAINT','Maintenance and facilities',NULL,'INVMGR'),  ('CC','IT','Information technology',NULL,'SYSADM'),
 ('PROJ','GEN','General, no specific project',NULL,NULL),    ('PROJ','NPD26','New product development 2026',NULL,'ENGMGR'),
 ('PROJ','TOOLUP','Tooling and capacity uplift',NULL,'ENGMGR'), ('PROJ','EXPORT','Export market development',NULL,'SALESMGR'),
 ('PROJ','LEAN','Lean manufacturing programme',NULL,'INVMGR'), ('PROJ','ERPMIG','ERP migration programme',NULL,'FINCTRL');
INSERT fin.DimensionValue (CompanyCode, DimensionCode, DimValue, Name, OwnerUser, SiteCode, ValidFrom, IsActive, CreatedBy)
SELECT c.CompanyCode, d.DimensionCode, d.DimValue, d.Name, d.OwnerUser, d.SiteCode, '2024-04-01', 1, 'SEED'
  FROM core.Company c CROSS JOIN @d d
 WHERE NOT EXISTS (SELECT 1 FROM fin.DimensionValue x WHERE x.CompanyCode = c.CompanyCode
                     AND x.DimensionCode = d.DimensionCode AND x.DimValue = d.DimValue);
GO
/*  Dimension rules: profit and loss accounts need a cost centre, balance sheet accounts must not
    carry one, and the work in progress and variance accounts also need a project. */
INSERT fin.AccountDimRule (CompanyCode, AccountCode, DimensionCode, Requirement, Description, CreatedBy)
SELECT a.CompanyCode, a.AccountCode, 'CC',
       CASE WHEN cl.Statement = 'PL' THEN 'MANDATORY' ELSE 'FORBIDDEN' END,
       CASE WHEN cl.Statement = 'PL' THEN 'Profit and loss postings carry a cost centre' ELSE 'Balance sheet accounts carry no cost centre' END, 'SEED'
  FROM fin.Account a JOIN fin.AccountClass cl ON cl.ClassCode = a.ClassCode
 WHERE NOT EXISTS (SELECT 1 FROM fin.AccountDimRule r WHERE r.CompanyCode = a.CompanyCode AND r.AccountCode = a.AccountCode AND r.DimensionCode = 'CC');
INSERT fin.AccountDimRule (CompanyCode, AccountCode, DimensionCode, Requirement, Description, CreatedBy)
SELECT a.CompanyCode, a.AccountCode, 'PROJ',
       CASE WHEN a.AccountCode IN ('1110','5520','5500','5510') OR a.ClassCode = 'VARI' THEN 'MANDATORY'
            WHEN cl.Statement = 'PL' THEN 'OPTIONAL' ELSE 'FORBIDDEN' END,
       'Project analysis rule', 'SEED'
  FROM fin.Account a JOIN fin.AccountClass cl ON cl.ClassCode = a.ClassCode
 WHERE NOT EXISTS (SELECT 1 FROM fin.AccountDimRule r WHERE r.CompanyCode = a.CompanyCode AND r.AccountCode = a.AccountCode AND r.DimensionCode = 'PROJ');
GO

/*  FRS 102 / FRS 105 statutory format lines.  Every posting account in the chart above carries a
    StatutoryLineCode; without this catalogue the statutory profit and loss account, the balance
    sheet and the statutory map check have nothing to map on to and every account reads as
    unmapped.  REQ: FIN-023, FIN-030 */
DECLARE @sl TABLE (StatutoryLineCode varchar(20), Statement char(2), Caption varchar(100), SortOrder int, SignConvention smallint);
INSERT @sl VALUES
 ('PL-TURN','PL','Turnover',10,-1),
 ('PL-COS','PL','Cost of sales',20,1),
 ('PL-DIST','PL','Distribution costs',30,1),
 ('PL-ADMIN','PL','Administrative expenses',40,1),
 ('PL-OTHINC','PL','Other operating income',50,-1),
 ('PL-INT','PL','Interest payable and similar charges',60,1),
 ('PL-TAX','PL','Tax on profit',70,1),
 ('BS-FA-TAN','BS','Tangible fixed assets',110,1),
 ('BS-CA-STK','BS','Stocks',120,1),
 ('BS-CA-DEB','BS','Debtors',130,1),
 ('BS-CA-CSH','BS','Cash at bank and in hand',140,1),
 ('BS-CR-1YR','BS','Creditors: amounts falling due within one year',150,-1),
 ('BS-CR-GT1','BS','Creditors: amounts falling due after more than one year',160,-1),
 ('BS-CAP-SHR','BS','Called up share capital',170,-1),
 ('BS-CAP-RES','BS','Profit and loss account reserves',180,-1);
INSERT fin.StatutoryLine (Framework, StatutoryLineCode, Statement, Caption, SortOrder, SignConvention)
SELECT f.Framework, s.StatutoryLineCode, s.Statement, s.Caption, s.SortOrder, s.SignConvention
  FROM @sl s CROSS JOIN (VALUES ('FRS102'), ('FRS105')) AS f (Framework)
 WHERE NOT EXISTS (SELECT 1 FROM fin.StatutoryLine x
                    WHERE x.Framework = f.Framework AND x.StatutoryLineCode = s.StatutoryLineCode);
GO
