/*  Fiscal years and periods (April to March), VAT codes with their MTD nine box mapping and
    effective-dated rates, and the company bank accounts.
    Period status follows the demo baseline: FY2025 hard closed, FY2026 hard to August 2026,
    September 2026 soft closed, October 2026 open, November 2026 open, later periods future.
    REQ: FIN-008 to FIN-012, FIN-016 to FIN-018, FIN-025, AP-011, D-02, D-15, D-16, D-23 */
SET NOCOUNT ON;
GO
DECLARE @fy TABLE (FiscalYear smallint, Status varchar(10));
INSERT @fy VALUES (2025,'CLOSED'), (2026,'OPEN'), (2027,'FUTURE');
INSERT fin.FiscalYear (CompanyCode, FiscalYear, StartDate, EndDate, PeriodCount, Status)
SELECT c.CompanyCode, f.FiscalYear, DATEFROMPARTS(f.FiscalYear, 4, 1), DATEFROMPARTS(f.FiscalYear + 1, 3, 31), 12, f.Status
  FROM core.Company c CROSS JOIN @fy f
 WHERE NOT EXISTS (SELECT 1 FROM fin.FiscalYear x WHERE x.CompanyCode = c.CompanyCode AND x.FiscalYear = f.FiscalYear);
WITH n AS (SELECT TOP (12) ROW_NUMBER() OVER (ORDER BY (SELECT 1)) AS PeriodNo FROM sys.all_objects)
INSERT fin.Period (CompanyCode, FiscalYear, PeriodNo, StartDate, EndDate, Name, Status, SoftCloseAt, HardCloseAt, ClosedBy)
SELECT c.CompanyCode, f.FiscalYear, n.PeriodNo,
       DATEADD(month, n.PeriodNo - 1, DATEFROMPARTS(f.FiscalYear, 4, 1)),
       EOMONTH(DATEADD(month, n.PeriodNo - 1, DATEFROMPARTS(f.FiscalYear, 4, 1))),
       FORMAT(DATEADD(month, n.PeriodNo - 1, DATEFROMPARTS(f.FiscalYear, 4, 1)), 'MMMM yyyy', 'en-GB'),
       CASE WHEN f.FiscalYear = 2025 THEN 'HARD'
            WHEN f.FiscalYear = 2026 AND n.PeriodNo <= 5 THEN 'HARD'
            WHEN f.FiscalYear = 2026 AND n.PeriodNo = 6 THEN 'SOFT'
            WHEN f.FiscalYear = 2026 AND n.PeriodNo IN (7, 8) THEN 'OPEN'
            ELSE 'FUTURE' END,
       CASE WHEN f.FiscalYear = 2025 OR (f.FiscalYear = 2026 AND n.PeriodNo <= 6)
            THEN DATEADD(day, 3, EOMONTH(DATEADD(month, n.PeriodNo - 1, DATEFROMPARTS(f.FiscalYear, 4, 1)))) END,
       CASE WHEN f.FiscalYear = 2025 OR (f.FiscalYear = 2026 AND n.PeriodNo <= 5)
            THEN DATEADD(day, 10, EOMONTH(DATEADD(month, n.PeriodNo - 1, DATEFROMPARTS(f.FiscalYear, 4, 1)))) END,
       CASE WHEN f.FiscalYear = 2025 OR (f.FiscalYear = 2026 AND n.PeriodNo <= 6) THEN 'FINCTRL' END
  FROM core.Company c CROSS JOIN @fy f CROSS JOIN n
 WHERE NOT EXISTS (SELECT 1 FROM fin.Period p WHERE p.CompanyCode = c.CompanyCode AND p.FiscalYear = f.FiscalYear AND p.PeriodNo = n.PeriodNo);
GO
/*  VAT codes.  Box numbers refer to the nine box MTD return; boxes 2, 8 and 9 are always zero
    because NI to EU goods movements are out of scope (D-23). */
MERGE fin.VatCode AS t
USING (VALUES
 ('S','Standard rated 20 per cent','STANDARD','BOTH','NORMAL',1,0,1,4,6,7,NULL,NULL),
 ('R','Reduced rated 5 per cent','REDUCED','BOTH','NORMAL',1,0,1,4,6,7,NULL,NULL),
 ('Z','Zero rated','ZERO','BOTH','NORMAL',1,0,1,4,6,7,NULL,NULL),
 ('E','Exempt from VAT','EXEMPT','BOTH','NORMAL',0,0,NULL,NULL,6,7,'Exempt supply','Exemption category'),
 ('O','Outside the scope of UK VAT','OUTSIDE','BOTH','NORMAL',0,0,NULL,NULL,NULL,NULL,'Outside the scope of UK VAT',NULL),
 ('RC','Reverse charge services from abroad','STANDARD','PURCH','RCSERVICE',1,1,1,4,6,7,'Reverse charge: customer to account for VAT','Supplier VAT status'),
 ('DRC','Domestic reverse charge, specified goods','STANDARD','BOTH','DRC',1,1,1,4,6,7,'Domestic reverse charge: customer to account to HMRC','Customer VAT registration'),
 ('PIV','Postponed import VAT accounting','STANDARD','PURCH','PVA',1,1,1,4,NULL,7,NULL,'Monthly postponed import VAT statement'),
 ('C79','Import VAT paid at the border','STANDARD','PURCH','C79',1,0,NULL,4,NULL,7,NULL,'Form C79 certificate'),
 ('EXP','Zero rated export of goods','ZERO','SALE','EXPORT',1,0,NULL,NULL,6,NULL,'Zero rated export','Proof of export within three months'),
 ('BD','Bad debt relief claim','STANDARD','BOTH','BADDEBT',1,0,NULL,4,NULL,NULL,NULL,'Debt over six months old and written off')
) AS s (VatCode, Name, Liability, Direction, Mechanism, IsRecoverable, SelfAccounts, OutputVatBox, InputVatBox, OutputNetBox, InputNetBox, InvoiceWording, EvidenceRequired)
ON t.VatCode = s.VatCode
WHEN MATCHED AND EXISTS (SELECT s.Name, s.Liability, s.Direction, s.Mechanism, s.IsRecoverable, s.SelfAccounts, s.OutputVatBox, s.InputVatBox, s.OutputNetBox, s.InputNetBox
                         EXCEPT SELECT t.Name, t.Liability, t.Direction, t.Mechanism, t.IsRecoverable, t.SelfAccounts, t.OutputVatBox, t.InputVatBox, t.OutputNetBox, t.InputNetBox)
    THEN UPDATE SET Name = s.Name, Liability = s.Liability, Direction = s.Direction, Mechanism = s.Mechanism,
                    IsRecoverable = s.IsRecoverable, SelfAccounts = s.SelfAccounts, OutputVatBox = s.OutputVatBox,
                    InputVatBox = s.InputVatBox, OutputNetBox = s.OutputNetBox, InputNetBox = s.InputNetBox,
                    InvoiceWording = s.InvoiceWording, EvidenceRequired = s.EvidenceRequired, ModifiedBy = 'SEED', ModifiedAt = SYSDATETIME()
WHEN NOT MATCHED THEN
    INSERT (VatCode, Name, Liability, Direction, Mechanism, IsRecoverable, SelfAccounts, OutputVatBox, InputVatBox, OutputNetBox, InputNetBox, InvoiceWording, EvidenceRequired, CreatedBy)
    VALUES (s.VatCode, s.Name, s.Liability, s.Direction, s.Mechanism, s.IsRecoverable, s.SelfAccounts, s.OutputVatBox, s.InputVatBox, s.OutputNetBox, s.InputNetBox, s.InvoiceWording, s.EvidenceRequired, 'SEED');
GO
MERGE fin.VatRate AS t
USING (VALUES
 ('S','2011-01-04',20.0000,'VAT Act 1994 s2'),   ('R','2011-01-04',5.0000,'VAT Act 1994 s29A'),
 ('Z','2011-01-04',0.0000,'VAT Act 1994 Sch 8'), ('E','2011-01-04',0.0000,'VAT Act 1994 Sch 9'),
 ('O','2011-01-04',0.0000,'Outside the scope'),  ('RC','2011-01-04',20.0000,'VAT Act 1994 s7A'),
 ('DRC','2021-03-01',20.0000,'SI 2019/892'),     ('PIV','2021-01-01',20.0000,'SI 2020/1495'),
 ('C79','2021-01-01',20.0000,'VAT Act 1994 s24'),('EXP','2011-01-04',0.0000,'VAT Act 1994 s30(6)'),
 ('BD','2011-01-04',20.0000,'VAT Act 1994 s36')
) AS s (VatCode, EffectiveFrom, RatePct, StatutoryRef)
ON t.VatCode = s.VatCode AND t.EffectiveFrom = s.EffectiveFrom
WHEN MATCHED AND t.RatePct <> s.RatePct THEN UPDATE SET RatePct = s.RatePct, StatutoryRef = s.StatutoryRef
WHEN NOT MATCHED THEN INSERT (VatCode, EffectiveFrom, RatePct, ApprovedBy, StatutoryRef)
    VALUES (s.VatCode, s.EffectiveFrom, s.RatePct, 'VATOWNER', s.StatutoryRef);
GO
/*  Company bank accounts.  Sort codes and account numbers are invented test values. */
MERGE fin.BankAccount AS t
USING (VALUES
 ('STC','GBP1','Stellar Components current account','1300','GBP','Thameside Bank plc','Reading','203045','40118622','GB29THMS20304540118622','THMSGB2L','834512',1,1,1),
 ('STC','EUR1','Stellar Components euro account','1310','EUR','Thameside Bank plc','Reading','203045','40118630','GB42THMS20304540118630','THMSGB2L',NULL,0,1,0),
 ('STC','USD1','Stellar Components US dollar account','1320','USD','Thameside Bank plc','Reading','203045','40118649','GB55THMS20304540118649','THMSGB2L',NULL,0,1,0),
 ('STC','DEP1','Stellar Components deposit account','1330','GBP','Thameside Bank plc','Reading','203045','40118657',NULL,'THMSGB2L',NULL,0,0,0),
 ('CAL','GBP1','Caledon Assemblies current account','1300','GBP','Clydebank Mutual plc','Glasgow','832217','19004473','GB61CLYD83221719004473','CLYDGB21',NULL,1,1,1),
 ('CAL','EUR1','Caledon Assemblies euro account','1310','EUR','Clydebank Mutual plc','Glasgow','832217','19004481','GB74CLYD83221719004481','CLYDGB21',NULL,0,1,0)
) AS s (CompanyCode, BankAccountCode, Name, AccountCode, CurrencyCode, BankName, BranchName, SortCode, AccountNumber, Iban, Bic, Sun, Bacs, Chaps, Fps)
ON t.CompanyCode = s.CompanyCode AND t.BankAccountCode = s.BankAccountCode
WHEN MATCHED AND EXISTS (SELECT s.Name, s.AccountCode, s.SortCode, s.AccountNumber EXCEPT SELECT t.Name, t.AccountCode, t.SortCode, t.AccountNumber)
    THEN UPDATE SET Name = s.Name, AccountCode = s.AccountCode, SortCode = s.SortCode, AccountNumber = s.AccountNumber,
                    ModifiedBy = 'SEED', ModifiedAt = SYSDATETIME()
WHEN NOT MATCHED THEN
    INSERT (CompanyCode, BankAccountCode, Name, AccountCode, CurrencyCode, BankName, BranchName, SortCode, AccountNumber,
            Iban, Bic, BacsServiceUserNumber, SupportsBacs, SupportsChaps, SupportsFps, CalendarCode, Status, IsSensitive, CreatedBy)
    VALUES (s.CompanyCode, s.BankAccountCode, s.Name, s.AccountCode, s.CurrencyCode, s.BankName, s.BranchName, s.SortCode, s.AccountNumber,
            s.Iban, s.Bic, s.Sun, s.Bacs, s.Chaps, s.Fps, 'BACS', 'ACTIVE', 1, 'SEED');
GO
