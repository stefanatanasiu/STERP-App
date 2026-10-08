/*  Supplier master: thirty accounts across the United Kingdom, the European Union and the
    United States, with bank details, Confirmation of Payee results, due diligence evidence,
    size flags for payment practices reporting, one pending unverified bank change and one
    potential sanctions match.
    REQ: AP-001 to AP-005, AP-026, AP-027, AP-028, PO-026, COM-029, D-16, D-25, D-29 */
SET NOCOUNT ON;
GO
DECLARE @s TABLE (SupplierCode varchar(20), LegalName varchar(120), TradingName varchar(120), LegalForm varchar(12),
                  CoNo varchar(12), VatLoc varchar(4), VatNo varchar(20), Country char(2), Size varchar(8), Ccy char(3),
                  Terms varchar(10), Method varchar(8), Expense varchar(10), DimCC varchar(20), Status varchar(12),
                  PurchHold bit, PayHold bit, HoldReason varchar(100), Sanctions varchar(12), Spend decimal(18,2));
INSERT @s VALUES
 ('S0001','Avonmouth Alloy Supplies Ltd','Avonmouth Alloys','LTD','03998112','GB','GB411220388','GB','LARGE','GBP','N60','BACS','1100','PURCH','ACTIVE',0,0,NULL,'CLEAR',842000),
 ('S0002','Pennine Tube and Section Ltd',NULL,'LTD','04220118','GB','GB522331499','GB','MEDIUM','GBP','N45','BACS','1100','PURCH','ACTIVE',0,0,NULL,'CLEAR',415000),
 ('S0003','Caerphilly Steel Stockholders Ltd',NULL,'LTD','05331229','GB','GB633442500','GB','MEDIUM','GBP','N30','BACS','1100','PURCH','ACTIVE',0,0,NULL,'CLEAR',288000),
 ('S0004','Titan Light Metals PLC','Titan Light','PLC','02110338','GB','GB744553611','GB','LARGE','GBP','EOM60','BACS','1100','PURCH','ACTIVE',0,0,NULL,'CLEAR',622000),
 ('S0005','Welwyn Precision Bearings Ltd',NULL,'LTD','06442330','GB','GB855664722','GB','SMALL','GBP','N30','BACS','1100','PURCH','ACTIVE',0,0,NULL,'CLEAR',96000),
 ('S0006','Castleford Fastenings Ltd','Castleford Fixings','LTD','07553441','GB','GB966775833','GB','SMALL','GBP','D2N30','BACS','1100','PURCH','ACTIVE',0,0,NULL,'CLEAR',54000),
 ('S0007','Midland Surface Finishing Ltd',NULL,'LTD','08664552','GB','GB177886944','GB','SMALL','GBP','N30','BACS','5400','PROD2','ACTIVE',0,0,NULL,'CLEAR',132000),
 ('S0008','Severn Heat Treatment Ltd',NULL,'LTD','09775663','GB','GB288997055','GB','SMALL','GBP','N30','BACS','5400','PROD2','ACTIVE',0,0,NULL,'CLEAR',78000),
 ('S0009','Thames Valley Chemicals Ltd',NULL,'LTD','10886774','GB','GB399108166','GB','MEDIUM','GBP','N30','BACS','1100','PURCH','ACTIVE',0,0,NULL,'CLEAR',64000),
 ('S0010','Britannia Industrial Gases Ltd',NULL,'LTD','01997885','GB','GB400219277','GB','LARGE','GBP','DOM31','BACS','6110','MAINT','ACTIVE',0,0,NULL,'CLEAR',41000),
 ('S0011','Northgate Packaging Ltd',NULL,'LTD','11008996','GB','GB511320388','GB','MEDIUM','GBP','N45','BACS','1100','PURCH','ACTIVE',0,0,NULL,'CLEAR',118000),
 ('S0012','Clyde Carton and Board Ltd',NULL,'LTD','SC221007','GB','GB622431499','GB','SMALL','GBP','N30','BACS','1100','PURCH','ACTIVE',0,0,NULL,'CLEAR',37000),
 ('S0013','Wessex Logistics Group Ltd','Wessex Logistics','LTD','12119007','GB','GB733542500','GB','LARGE','GBP','N30','BACS','6210','WHSE','ACTIVE',0,0,NULL,'CLEAR',206000),
 ('S0014','Ridgeway Freight Forwarding Ltd',NULL,'LTD','13220118','GB','GB844653611','GB','MEDIUM','GBP','N30','BACS','5210','PURCH','ACTIVE',0,0,NULL,'CLEAR',155000),
 ('S0015','Ashcombe Business Rates Agency Ltd',NULL,'LTD','14331229','GB','GB955764722','GB','SMALL','GBP','N14','FPS','6100','ADMIN','ACTIVE',0,0,NULL,'CLEAR',29000),
 ('S0016','Quadrant Utilities PLC',NULL,'PLC','01442330','GB','GB166875833','GB','LARGE','GBP','DOM15','BACS','6110','ADMIN','ACTIVE',0,0,NULL,'CLEAR',88000),
 ('S0017','Marchmont Insurance Brokers LLP',NULL,'LLP','OC441229','GB','GB277986944','GB','SMALL','GBP','N30','BACS','6120','ADMIN','ACTIVE',0,0,NULL,'CLEAR',34000),
 ('S0018','Pelham Audit LLP',NULL,'LLP','OC552330','GB','GB388097055','GB','MEDIUM','GBP','N30','BACS','6410','ADMIN','ACTIVE',0,0,NULL,'CLEAR',46000),
 ('S0019','Orbital IT Services Ltd',NULL,'LTD','15442330','GB','GB499108166','GB','SMALL','GBP','N30','BACS','6320','IT','ACTIVE',0,0,NULL,'CLEAR',52000),
 ('S0020','Hartwell Tooling Ltd',NULL,'LTD','16553441','GB','GB500219277','GB','SMALL','GBP','N30','BACS','1010','ENG','ACTIVE',0,0,NULL,'CLEAR',71000),
 ('S0021','J P Marsden Engineering',NULL,'SOLETRADER',NULL,'GB','GB611320388','GB','SMALL','GBP','N14','FPS','5400','PROD1','ACTIVE',0,0,NULL,'CLEAR',18000),
 ('S0022','Foyle Components Ltd',NULL,'LTD','NI334410','XI','XI722431499','GB','SMALL','GBP','N30','BACS','1100','PURCH','ACTIVE',0,0,NULL,'CLEAR',63000),
 ('S0023','Rheinmetall Radteile GmbH','Rheinmetall Radteile','OVERSEAS','HRB441002','EU','DE833542500','DE','LARGE','EUR','N45','SEPA','1100','PURCH','ACTIVE',0,0,NULL,'CLEAR',394000),
 ('S0024','Composants Velo Lyonnais SAS',NULL,'OVERSEAS','FR5520114','EU','FR94552011477','FR','MEDIUM','EUR','N60','SEPA','1100','PURCH','ACTIVE',0,0,NULL,'CLEAR',217000),
 ('S0025','Lombardia Ciclo Componenti SRL',NULL,'OVERSEAS','IT6630225','EU','IT06630225118','IT','MEDIUM','EUR','N30','SEPA','1100','PURCH','ACTIVE',0,0,NULL,'CLEAR',164000),
 ('S0026','Ardenne Tooling SARL',NULL,'OVERSEAS','BE7740336','EU','BE0774033611','BE','SMALL','EUR','N30','SEPA','1010','ENG','ONHOLD',1,1,'Possible sanctions list match, purchasing and payment suspended','POTENTIAL',22000),
 ('S0027','Cascadia Electronics Inc','Cascadia Electronics','OVERSEAS','US8850447','ROW',NULL,'US','LARGE','USD','N45','SWIFT','1100','PURCH','ACTIVE',0,0,NULL,'CLEAR',486000),
 ('S0028','Puget Drive Systems LLC',NULL,'OVERSEAS','US9960558','ROW',NULL,'US','MEDIUM','USD','N30','SWIFT','1100','PURCH','ACTIVE',0,0,NULL,'CLEAR',228000),
 ('S0029','Cheriton Calibration Ltd',NULL,'LTD','17664552','GB','GB722431499','GB','SMALL','GBP','N30','BACS','6130','QUAL','ACTIVE',0,0,NULL,'CLEAR',14000),
 ('S0030','Pendleton Trade Prospect Ltd',NULL,'LTD','18775663','GB',NULL,'GB','SMALL','GBP','STAT','BACS','1100','PURCH','PENDING',1,1,'New account, due diligence outstanding','CLEAR',0);
INSERT ap.Supplier (CompanyCode, SupplierCode, LegalName, TradingName, LegalForm, CompaniesHouseNo, CompaniesHouseStatus,
                    CompaniesHouseCheckedAt, VatLocation, VatRegNo, VatValidatedAt, Eori, CountryCode, BusinessSize,
                    CurrencyCode, AllowedCurrencies, TermsCode, ApControlAccount, DefaultExpenseAccount, DimCostCentre,
                    PaymentMethod, MinimumPaymentGBP, BuyerUser, ApOwnerUser, PurchaseHold, PaymentHold, HoldReason,
                    SanctionsStatus, SanctionsScreenedAt, IsCisOrConstructionDrc, EInvoiceEnabled, IsPersonalData,
                    RetentionClass, Status, AnnualSpendGBP, CreatedBy)
SELECT 'STC', s.SupplierCode, s.LegalName, s.TradingName, s.LegalForm, s.CoNo,
       CASE WHEN s.CoNo IS NULL THEN NULL ELSE 'ACTIVE' END,
       CASE WHEN s.CoNo IS NULL THEN NULL ELSE '2026-07-15' END,
       s.VatLoc, s.VatNo, CASE WHEN s.VatNo IS NULL THEN NULL ELSE '2026-04-10' END,
       CASE WHEN s.Country <> 'GB' THEN NULL ELSE 'GB' + REPLACE(ISNULL(s.VatNo, 'GB000000000'), 'GB', '') + '000' END,
       s.Country, s.Size, s.Ccy, CASE WHEN s.Ccy = 'GBP' THEN 'GBP' ELSE 'GBP,' + s.Ccy END, s.Terms, '2000',
       s.Expense, s.DimCC, s.Method, 25.00,
       CASE WHEN s.Expense = '1100' THEN 'BUYER' ELSE 'PURMGR' END, 'APMGR',
       s.PurchHold, s.PayHold, s.HoldReason, s.Sanctions, '2026-09-01T06:00:00', 0,
       CASE WHEN s.Size = 'LARGE' THEN 1 ELSE 0 END,
       CASE WHEN s.LegalForm = 'SOLETRADER' THEN 1 ELSE 0 END,
       CASE WHEN s.LegalForm = 'SOLETRADER' THEN 'CONTACT6' ELSE 'FIN7' END,
       s.Status, s.Spend, 'SEED'
  FROM @s s
 WHERE NOT EXISTS (SELECT 1 FROM ap.Supplier x WHERE x.CompanyCode = 'STC' AND x.SupplierCode = s.SupplierCode);
INSERT ap.Supplier (CompanyCode, SupplierCode, LegalName, LegalForm, CompaniesHouseNo, VatLocation, VatRegNo, CountryCode,
                    BusinessSize, CurrencyCode, AllowedCurrencies, TermsCode, ApControlAccount, DefaultExpenseAccount,
                    DimCostCentre, PaymentMethod, BuyerUser, ApOwnerUser, SanctionsStatus, SanctionsScreenedAt,
                    IsPersonalData, RetentionClass, Status, AnnualSpendGBP, CreatedBy)
SELECT 'CAL', v.Code, v.Name, 'LTD', v.CoNo, 'GB', v.Vat, 'GB', v.Size, 'GBP', 'GBP', v.Terms, '2000', v.Expense,
       v.DimCC, 'BACS', 'BUYER', 'APMGR', 'CLEAR', '2026-09-01T06:00:00', 0, 'FIN7', 'ACTIVE', v.Spend, 'SEED'
  FROM (VALUES
   ('T0001','Stellar Components Ltd','04229011','GB412771903','LARGE','N30','1100','PURCH',940000.00),
   ('T0002','Clyde Carton and Board Ltd','SC221007','GB622431499','SMALL','N30','1100','PURCH',26000.00),
   ('T0003','Lanarkshire Logistics Ltd','SC332118','GB733542500','MEDIUM','N30','6210','WHSE',74000.00),
   ('T0004','Forth Valley Utilities PLC','SC443229','GB844653611','LARGE','DOM15','6110','ADMIN',38000.00),
   ('T0005','Kelvin Workshop Services Ltd','SC554330','GB955764722','SMALL','N14','5400','PROD2',21000.00)
  ) AS v (Code, Name, CoNo, Vat, Size, Terms, Expense, DimCC, Spend)
 WHERE NOT EXISTS (SELECT 1 FROM ap.Supplier x WHERE x.CompanyCode = 'CAL' AND x.SupplierCode = v.Code);
GO
INSERT ap.SupplierAddress (CompanyCode, SupplierCode, AddressRole, AddressRef, Name, Line1, Town, Postcode, CountryCode,
                           ContactName, ContactEmail, ContactPhone, ContactPurpose, IsPersonalData, IsDefault, ValidFrom, CreatedBy)
SELECT s.CompanyCode, s.SupplierCode, r.Role, LEFT(r.Role, 4) + '1', s.LegalName,
       CAST((ABS(CHECKSUM(s.SupplierCode + r.Role)) % 150) + 1 AS varchar(4)) + ' ' +
       CASE ABS(CHECKSUM(s.SupplierCode)) % 6 WHEN 0 THEN 'Dock Road' WHEN 1 THEN 'Forge Way' WHEN 2 THEN 'Canal Street'
            WHEN 3 THEN 'Trident Park' WHEN 4 THEN 'Longbridge Lane' ELSE 'Severn Industrial Estate' END,
       CASE s.CountryCode WHEN 'GB' THEN CASE ABS(CHECKSUM(s.SupplierCode)) % 5 WHEN 0 THEN 'Bristol' WHEN 1 THEN 'Sheffield'
            WHEN 2 THEN 'Cardiff' WHEN 3 THEN 'Glasgow' ELSE 'Coventry' END
            WHEN 'DE' THEN 'Dusseldorf' WHEN 'FR' THEN 'Lyon' WHEN 'IT' THEN 'Brescia' WHEN 'BE' THEN 'Namur' ELSE 'Seattle' END,
       CASE WHEN s.CountryCode = 'GB' THEN 'BS' + CAST((ABS(CHECKSUM(s.SupplierCode)) % 40) + 1 AS varchar(2)) + ' 4TR' END,
       s.CountryCode, 'Sales Office',
       LOWER('sales@' + LOWER(LEFT(REPLACE(s.LegalName, ' ', ''), 12)) + '.example.com'),
       '020' + CAST(70000000 + (ABS(CHECKSUM(s.SupplierCode)) % 9000000) AS varchar(10)),
       'Orders and delivery', s.IsPersonalData, 1, '2021-04-01', 'SEED'
  FROM ap.Supplier s CROSS JOIN (VALUES ('REGISTERED'), ('ORDER'), ('REMIT')) AS r (Role)
 WHERE NOT EXISTS (SELECT 1 FROM ap.SupplierAddress a WHERE a.CompanyCode = s.CompanyCode AND a.SupplierCode = s.SupplierCode AND a.AddressRole = r.Role);
GO
/*  Bank details.  Confirmation of Payee is recorded for every sterling account; the change on
    S0002 is prepared but not yet independently verified or approved, so it cannot be paid. */
INSERT ap.SupplierBank (CompanyCode, SupplierCode, BankRef, BeneficiaryName, CountryCode, CurrencyCode, SortCode,
                        AccountNumber, Iban, Bic, BankName, PaymentMethod, CopResult, CopCheckedAt, CopReference,
                        CopDecision, CallBackVerifiedAt, CallBackVerifiedBy, CallBackContactSource, ValidFrom, Status,
                        PreparedBy, ApprovedBy, ApprovedAt, IsSensitive, CreatedBy)
SELECT s.CompanyCode, s.SupplierCode, 'BANK1', s.LegalName, s.CountryCode, s.CurrencyCode,
       CASE WHEN s.CurrencyCode = 'GBP' THEN RIGHT('00' + CAST(ABS(CHECKSUM(s.SupplierCode)) % 999999 AS varchar(6)), 6) END,
       CASE WHEN s.CurrencyCode = 'GBP' THEN RIGHT('0000' + CAST(ABS(CHECKSUM(s.SupplierCode + 'B')) % 99999999 AS varchar(8)), 8) END,
       CASE WHEN s.CurrencyCode <> 'GBP' THEN 'DE' + CAST(10 + ABS(CHECKSUM(s.SupplierCode)) % 80 AS varchar(2)) + '5001050000' +
            RIGHT('0000000' + CAST(ABS(CHECKSUM(s.SupplierCode)) % 9999999 AS varchar(7)), 7) END,
       CASE WHEN s.CurrencyCode = 'GBP' THEN 'THMSGB2L' WHEN s.CurrencyCode = 'EUR' THEN 'DEUTDEFF' ELSE 'CHASUS33' END,
       CASE WHEN s.CurrencyCode = 'GBP' THEN 'Thameside Bank plc' WHEN s.CurrencyCode = 'EUR' THEN 'Rheinbank AG' ELSE 'Puget Commercial Bank' END,
       s.PaymentMethod,
       CASE WHEN s.CurrencyCode = 'GBP' THEN CASE WHEN s.SupplierCode = 'S0021' THEN 'CLOSEMATCH' ELSE 'MATCH' END END,
       CASE WHEN s.CurrencyCode = 'GBP' THEN CAST('2026-05-14T10:12:00' AS datetime2(3)) END,
       CASE WHEN s.CurrencyCode = 'GBP' THEN 'COP-2026-' + s.SupplierCode END,
       CASE WHEN s.CurrencyCode = 'GBP' THEN CASE WHEN s.SupplierCode = 'S0021' THEN 'ACCEPTED-REVIEWED' ELSE 'ACCEPTED' END END,
       '2026-05-14T11:00:00', 'APMGR', 'Telephone number from the signed supply agreement', '2026-05-15', 'APPROVED',
       'APCLERK', 'TREASURY', '2026-05-15T09:30:00', 1, 'SEED'
  FROM ap.Supplier s
 WHERE s.Status <> 'PENDING'
   AND NOT EXISTS (SELECT 1 FROM ap.SupplierBank b WHERE b.CompanyCode = s.CompanyCode AND b.SupplierCode = s.SupplierCode AND b.BankRef = 'BANK1');
INSERT ap.SupplierBank (CompanyCode, SupplierCode, BankRef, BeneficiaryName, CountryCode, CurrencyCode, SortCode,
                        AccountNumber, Bic, BankName, PaymentMethod, CopResult, CopCheckedAt, CopReference, CopDecision,
                        ValidFrom, Status, PreparedBy, SupersedesBankId, IsSensitive, CreatedBy)
SELECT 'STC', 'S0002', 'BANK2', 'Pennine Tube and Section Ltd', 'GB', 'GBP', '601355', '44120977', 'THMSGB2L',
       'Thameside Bank plc', 'BACS', 'NOMATCH', '2026-09-22T15:40:00', 'COP-2026-S0002-B', 'PENDING-REVIEW',
       '2026-09-22', 'PENDING', 'APCLERK',
       (SELECT TOP (1) SupplierBankId FROM ap.SupplierBank WHERE CompanyCode = 'STC' AND SupplierCode = 'S0002' AND BankRef = 'BANK1'),
       1, 'SEED'
 WHERE EXISTS (SELECT 1 FROM ap.Supplier WHERE CompanyCode = 'STC' AND SupplierCode = 'S0002')
   AND NOT EXISTS (SELECT 1 FROM ap.SupplierBank WHERE CompanyCode = 'STC' AND SupplierCode = 'S0002' AND BankRef = 'BANK2');
GO
/*  Due diligence evidence.  Declarations are required above the spend threshold (D-29) and
    insurance evidence for service and subcontract suppliers. */
INSERT ap.SupplierDueDiligence (CompanyCode, SupplierCode, EvidenceType, Reference, ObtainedDate, ExpiryDate, ReviewDate,
                                Outcome, IsMandatory, Note, RecordedBy)
SELECT s.CompanyCode, s.SupplierCode, e.EvidenceType,
       e.EvidenceType + '-' + s.SupplierCode + '-2026',
       CASE WHEN s.Status = 'PENDING' THEN NULL ELSE CAST('2026-04-20' AS date) END,
       CASE WHEN s.Status = 'PENDING' THEN NULL ELSE CAST('2027-04-19' AS date) END, '2027-03-01',
       CASE WHEN s.Status = 'PENDING' THEN 'PENDING' WHEN s.SanctionsStatus = 'POTENTIAL' AND e.EvidenceType = 'SANCTIONS' THEN 'FAIL' ELSE 'PASS' END,
       1, e.Note, 'APMGR'
  FROM ap.Supplier s
  CROSS APPLY (VALUES
    ('SANCTIONS','Screened against the sanctions list at onboarding and on change'),
    ('MODERNSLAVERY','Modern slavery declaration, required above the spend threshold'),
    ('ANTIBRIBERY','Anti-bribery declaration, required above the spend threshold'),
    ('INSURANCE','Public and product liability insurance certificate')
  ) AS e (EvidenceType, Note)
 WHERE (e.EvidenceType = 'SANCTIONS'
        OR (e.EvidenceType IN ('MODERNSLAVERY','ANTIBRIBERY') AND ISNULL(s.AnnualSpendGBP, 0) > 25000)
        OR (e.EvidenceType = 'INSURANCE' AND s.DefaultExpenseAccount IN ('5400','6130','6320','6410','6120')))
   AND NOT EXISTS (SELECT 1 FROM ap.SupplierDueDiligence d WHERE d.CompanyCode = s.CompanyCode
                     AND d.SupplierCode = s.SupplierCode AND d.EvidenceType = e.EvidenceType);
GO
