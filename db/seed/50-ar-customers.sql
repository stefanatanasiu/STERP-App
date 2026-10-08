/*  Customer master: forty trading accounts covering United Kingdom, Northern Ireland (XI),
    European Union, United States, a sole trader and a charity, with varied terms, limits and
    statuses including on hold, insolvency, pro forma only and Direct Debit payers.
    REQ: AR-001 to AR-008, AR-016, AR-022, AR-027, AR-028, COM-004, D-06, D-23, D-27, D-29 */
SET NOCOUNT ON;
GO
DECLARE @c TABLE (CustomerCode varchar(20), LegalName varchar(120), TradingName varchar(120), LegalForm varchar(12),
                  CoNo varchar(12), VatLoc varchar(4), VatNo varchar(20), Country char(2), Ccy char(3), Terms varchar(10),
                  Method varchar(8), Classification varchar(20), Status varchar(12), Limit decimal(18,2), Risk varchar(8),
                  Hold bit, HoldReason varchar(100), Insolvency varchar(12), Proforma bit, Sanctions varchar(12));
INSERT @c VALUES
 ('C0001','Pennine Cycle Distribution Ltd','Pennine Cycles','LTD','04128877','GB','GB418220144','GB','GBP','N30','BACS','DISTRIBUTOR','ACTIVE',150000,'LOW',0,NULL,NULL,0,'CLEAR'),
 ('C0002','Severnside Bicycle Company Ltd','Severnside Bikes','LTD','05331209','GB','GB522904118','GB','GBP','EOM30','DD','DEALER','ACTIVE',75000,'LOW',0,NULL,NULL,0,'CLEAR'),
 ('C0003','Kestrel Velo Group PLC','Kestrel Velo','PLC','03229014','GB','GB311860922','GB','GBP','N45','BACS','DISTRIBUTOR','ACTIVE',250000,'LOW',0,NULL,NULL,0,'CLEAR'),
 ('C0004','Brackley Cycle Works Ltd',NULL,'LTD','08871223','GB','GB244118306','GB','GBP','N30','BACS','DEALER','ACTIVE',25000,'MEDIUM',0,NULL,NULL,0,'CLEAR'),
 ('C0005','Harlow Wheelcraft Ltd','Wheelcraft','LTD','09110238','GB','GB387221905','GB','GBP','D2N30','BACS','DEALER','ACTIVE',40000,'LOW',0,NULL,NULL,0,'CLEAR'),
 ('C0006','Tyneside Ride Supply Ltd',NULL,'LTD','07442119','GB','GB455901277','GB','GBP','N30','DD','DEALER','ACTIVE',30000,'MEDIUM',0,NULL,NULL,0,'CLEAR'),
 ('C0007','Mercia Frameworks LLP',NULL,'LLP','OC380114','GB','GB601228440','GB','GBP','N60','BACS','OEM','ACTIVE',120000,'LOW',0,NULL,NULL,0,'CLEAR'),
 ('C0008','Solent Cycle Centre Ltd',NULL,'LTD','06552231','GB','GB399110233','GB','GBP','N30','CARD','DEALER','ONHOLD',15000,'HIGH',1,'Invoice more than thirty days past due',NULL,0,'CLEAR'),
 ('C0009','Lakeland Touring Bikes Ltd',NULL,'LTD','10223447','GB','GB712900184','GB','GBP','N30','BACS','DEALER','ACTIVE',20000,'MEDIUM',0,NULL,NULL,0,'CLEAR'),
 ('C0010','Cotswold Gravel Company Ltd','Cotswold Gravel','LTD','11338821','GB','GB822107455','GB','GBP','DOM31','DD','DEALER','ACTIVE',35000,'LOW',0,NULL,NULL,0,'CLEAR'),
 ('C0011','Fenland Cycle Repairs',NULL,'SOLETRADER',NULL,'GB',NULL,'GB','GBP','PREPAY','CARD','WORKSHOP','ACTIVE',2500,'HIGH',0,NULL,NULL,1,'CLEAR'),
 ('C0012','A R Whitlock Cycles',NULL,'SOLETRADER',NULL,'GB','GB199044712','GB','GBP','N14','FPS','WORKSHOP','ACTIVE',5000,'MEDIUM',0,NULL,NULL,0,'CLEAR'),
 ('C0013','Pedal Forward Charity','Pedal Forward','CHARITY','09887712','GB',NULL,'GB','GBP','N30','BACS','CHARITY','ACTIVE',10000,'LOW',0,NULL,NULL,0,'CLEAR'),
 ('C0014','Midshire County Council',NULL,'PUBLICBODY',NULL,'GB','GB888771230','GB','GBP','N30','BACS','PUBLIC','ACTIVE',60000,'LOW',0,NULL,NULL,0,'CLEAR'),
 ('C0015','Clyde Valley Cycles Ltd',NULL,'LTD','SC412008','GB','GB503118277','GB','GBP','N30','DD','DEALER','ACTIVE',45000,'LOW',0,NULL,NULL,0,'CLEAR'),
 ('C0016','Grampian Bike Hub Ltd',NULL,'LTD','SC520117','GB','GB644021183','GB','GBP','N30','BACS','DEALER','ACTIVE',18000,'MEDIUM',0,NULL,NULL,0,'CLEAR'),
 ('C0017','Snowdonia Trail Supplies Ltd',NULL,'LTD','12009887','GB','GB733119204','GB','GBP','N30','BACS','DEALER','ACTIVE',22000,'MEDIUM',0,NULL,NULL,0,'CLEAR'),
 ('C0018','Belfast Velo Supplies Ltd','Belfast Velo','LTD','NI660122','XI','XI811220477','GB','GBP','N30','BACS','DISTRIBUTOR','ACTIVE',55000,'LOW',0,NULL,NULL,0,'CLEAR'),
 ('C0019','Lough Neagh Cycle Co Ltd',NULL,'LTD','NI701338','XI','XI922011568','GB','GBP','EOM30','BACS','DEALER','ACTIVE',28000,'MEDIUM',0,NULL,NULL,0,'CLEAR'),
 ('C0020','Causeway Frame Works Ltd',NULL,'LTD','NI712204','XI','XI133440299','GB','GBP','N30','BACS','OEM','ACTIVE',48000,'LOW',0,NULL,NULL,0,'CLEAR'),
 ('C0021','Velo Atlantique SAS','Velo Atlantique','OVERSEAS','FR8820114','EU','FR40882011477','FR','EUR','N30','CHAPS','DISTRIBUTOR','ACTIVE',90000,'LOW',0,NULL,NULL,0,'CLEAR'),
 ('C0022','Rheinrad Handels GmbH',NULL,'OVERSEAS','HRB220118','EU','DE811904226','DE','EUR','N45','CHAPS','DISTRIBUTOR','ACTIVE',110000,'LOW',0,NULL,NULL,0,'CLEAR'),
 ('C0023','Fietsen Zuiderzee BV',NULL,'OVERSEAS','NL55201188','EU','NL822011447B01','NL','EUR','N30','CHAPS','DEALER','ACTIVE',40000,'MEDIUM',0,NULL,NULL,0,'CLEAR'),
 ('C0024','Ciclo Iberico SL',NULL,'OVERSEAS','ESB7720114','EU','ESB77201148','ES','EUR','N60','CHAPS','DEALER','ACTIVE',35000,'MEDIUM',0,NULL,NULL,0,'CLEAR'),
 ('C0025','Alpi Due Ruote SRL',NULL,'OVERSEAS','IT0992011','EU','IT09920114772','IT','EUR','N30','CHAPS','DEALER','ONHOLD',25000,'HIGH',1,'Credit insurance cover withdrawn',NULL,0,'CLEAR'),
 ('C0026','Nordlys Sykkel AS',NULL,'OVERSEAS','NO991200','ROW','NO991200114MVA','NO','EUR','N30','CHAPS','DEALER','ACTIVE',30000,'MEDIUM',0,NULL,NULL,0,'CLEAR'),
 ('C0027','Cascade Cycle Imports Inc','Cascade Cycle','OVERSEAS','US4410228','ROW',NULL,'US','USD','N45','CHAPS','DISTRIBUTOR','ACTIVE',180000,'LOW',0,NULL,NULL,0,'CLEAR'),
 ('C0028','Great Lakes Bicycle Supply LLC',NULL,'OVERSEAS','US7720114','ROW',NULL,'US','USD','N30','CHAPS','DISTRIBUTOR','ACTIVE',95000,'MEDIUM',0,NULL,NULL,0,'CLEAR'),
 ('C0029','Pacific Rim Cycles Pty Ltd',NULL,'OVERSEAS','AU112003','ROW',NULL,'AU','USD','PROFORMA','CHAPS','DEALER','ACTIVE',0,'HIGH',0,NULL,NULL,1,'CLEAR'),
 ('C0030','Trans Pennine Logistics Ltd',NULL,'LTD','08220114','GB','GB466201889','GB','GBP','N30','BACS','OTHER','ACTIVE',12000,'MEDIUM',0,NULL,NULL,0,'CLEAR'),
 ('C0031','Wessex Cycle Hire Ltd',NULL,'LTD','09440227','GB','GB577310224','GB','GBP','N30','DD','HIRE','ACTIVE',16000,'MEDIUM',0,NULL,NULL,0,'CLEAR'),
 ('C0032','Dalesway Adventure Ltd',NULL,'LTD','10550338','GB','GB688420115','GB','GBP','N30','BACS','HIRE','SUSPENDED',8000,'HIGH',1,'Account under review after dishonoured Direct Debit',NULL,0,'CLEAR'),
 ('C0033','Northgate Retail Group PLC',NULL,'PLC','02118877','GB','GB122033991','GB','GBP','N60','BACS','RETAIL','ACTIVE',300000,'LOW',0,NULL,NULL,0,'CLEAR'),
 ('C0034','Ferrous Fabrications Ltd',NULL,'LTD','11220033','GB','GB799102234','GB','GBP','N30','BACS','OEM','ACTIVE',65000,'LOW',0,NULL,NULL,0,'CLEAR'),
 ('C0035','Orchard Lane Cycles Ltd',NULL,'LTD','07330221','GB','GB233440118','GB','GBP','N30','CHEQUE','DEALER','ACTIVE',9000,'HIGH',0,NULL,NULL,0,'CLEAR'),
 ('C0036','Marlowe Sports Holdings Ltd',NULL,'LTD','06220118','GB','GB344551220','GB','GBP','N30','BACS','RETAIL','ACTIVE',0,'HIGH',1,'Administration appointed, proof of debt lodged','ADMIN',0,'CLEAR'),
 ('C0037','Tamar Cycle Exchange Ltd',NULL,'LTD','05110227','GB','GB455662331','GB','GBP','N30','BACS','DEALER','RETIRED',0,'HIGH',0,NULL,'LIQUIDATION',0,'CLEAR'),
 ('C0038','Harbourlight Trading Ltd',NULL,'LTD','13220114','GB','GB900112345','GB','GBP','PREPAY','CHAPS','DISTRIBUTOR','ONHOLD',0,'HIGH',1,'Possible sanctions list match, awaiting review',NULL,1,'POTENTIAL'),
 ('C0039','Avon Vale Prospect Ltd',NULL,'LTD','14110229','GB',NULL,'GB','GBP','PROFORMA','FPS','DEALER','PROSPECT',0,'HIGH',0,NULL,NULL,1,'CLEAR'),
 ('C0040','Caledon Assemblies Ltd','Caledon Assemblies','LTD','SC330118','GB','GB233471029','GB','GBP','N30','BACS','INTERCO','ACTIVE',200000,'LOW',0,NULL,NULL,0,'CLEAR');
INSERT ar.Customer (CompanyCode, CustomerCode, LegalName, TradingName, LegalForm, CompaniesHouseNo, CompaniesHouseStatus,
                    CompaniesHouseCheckedAt, VatLocation, VatRegNo, VatValidatedAt, IsVatBusiness, DrcEligible, CountryCode,
                    CurrencyCode, AllowedCurrencies, TermsCode, ArControlAccount, CollectionMethod, StatementFrequency,
                    PriceListCode, SalespersonCode, AccountOwner, Classification, RequiresPurchaseOrderRef, EInvoiceEnabled,
                    SanctionsStatus, SanctionsScreenedAt, InsolvencyType, InsolvencyDate, OfficeHolder, IsPersonalData,
                    RetentionClass, Status, StatusReason, FirstTradedDate, CreatedBy)
SELECT 'STC', c.CustomerCode, c.LegalName, c.TradingName, c.LegalForm, c.CoNo,
       CASE WHEN c.CoNo IS NULL THEN NULL WHEN c.Insolvency IS NOT NULL THEN 'INSOLVENT' ELSE 'ACTIVE' END,
       CASE WHEN c.CoNo IS NULL THEN NULL ELSE '2026-07-15' END,
       c.VatLoc, c.VatNo, CASE WHEN c.VatNo IS NULL THEN NULL ELSE '2026-04-10' END,
       CASE WHEN c.VatNo IS NULL THEN 0 ELSE 1 END,
       CASE WHEN c.VatLoc IN ('GB','XI') THEN 1 ELSE 0 END,
       c.Country, c.Ccy, CASE WHEN c.Ccy = 'GBP' THEN 'GBP' ELSE 'GBP,' + c.Ccy END, c.Terms, '1200', c.Method,
       CASE WHEN c.Status IN ('PROSPECT','RETIRED') THEN 'NONE' ELSE 'MONTHLY' END,
       CASE WHEN c.Classification = 'DISTRIBUTOR' THEN 'DIST26' WHEN c.Classification = 'OEM' THEN 'OEM26' ELSE 'TRADE26' END,
       CASE WHEN c.Country = 'GB' THEN 'SALESREP' ELSE 'SALESMGR' END, 'SALESMGR', c.Classification,
       CASE WHEN c.Classification IN ('PUBLIC','RETAIL','OEM') THEN 1 ELSE 0 END,
       CASE WHEN c.Classification IN ('DISTRIBUTOR','PUBLIC') THEN 1 ELSE 0 END,
       c.Sanctions, '2026-09-01T06:00:00', c.Insolvency,
       CASE WHEN c.Insolvency IS NOT NULL THEN '2026-06-30' END,
       CASE WHEN c.Insolvency IS NOT NULL THEN 'Redgrave Insolvency LLP' END,
       CASE WHEN c.LegalForm = 'SOLETRADER' THEN 1 ELSE 0 END,
       CASE WHEN c.LegalForm = 'SOLETRADER' THEN 'CONTACT6' ELSE 'FIN7' END,
       c.Status, c.HoldReason, '2021-09-01', 'SEED'
  FROM @c c
 WHERE NOT EXISTS (SELECT 1 FROM ar.Customer x WHERE x.CompanyCode = 'STC' AND x.CustomerCode = c.CustomerCode);
INSERT ar.CustomerCredit (CompanyCode, CustomerCode, CreditLimitGBP, OverdueThresholdGBP, OverdueDaysTrigger, RiskClass,
                          CreditAgency, CreditScore, CreditScoreDate, CreditInsuranceCoverGBP, CreditInsurer, ReviewDate,
                          ControllerUser, ApprovedBy, ApprovedAt, HoldFlag, HoldReason, HoldSetBy, HoldSetAt,
                          RequiresPrepayment, ProFormaOnly, CreatedBy)
SELECT 'STC', c.CustomerCode, c.Limit, CASE WHEN c.Limit = 0 THEN 0 ELSE ROUND(c.Limit * 0.1, 2) END, 30, c.Risk,
       'Thornbury Credit Reference', CASE c.Risk WHEN 'LOW' THEN 82 WHEN 'MEDIUM' THEN 61 ELSE 34 END, '2026-06-30',
       CASE WHEN c.Risk = 'LOW' THEN ROUND(c.Limit * 0.8, 2) END,
       CASE WHEN c.Risk = 'LOW' THEN 'Northbridge Trade Credit' END,
       '2027-06-30', 'CREDMGR', CASE WHEN c.Limit > 0 THEN 'CREDMGR' END,
       CASE WHEN c.Limit > 0 THEN CAST('2026-06-30T11:00:00' AS datetime2(3)) END,
       c.Hold, c.HoldReason, CASE WHEN c.Hold = 1 THEN 'CREDMGR' END,
       CASE WHEN c.Hold = 1 THEN CAST('2026-09-18T14:25:00' AS datetime2(3)) END,
       CASE WHEN c.Terms = 'PREPAY' THEN 1 ELSE 0 END, c.Proforma, 'SEED'
  FROM @c c
 WHERE NOT EXISTS (SELECT 1 FROM ar.CustomerCredit x WHERE x.CompanyCode = 'STC' AND x.CustomerCode = c.CustomerCode);
GO
/*  Caledon Assemblies sells to a small United Kingdom dealer base of its own. */
INSERT ar.Customer (CompanyCode, CustomerCode, LegalName, LegalForm, CompaniesHouseNo, VatLocation, VatRegNo, IsVatBusiness,
                    DrcEligible, CountryCode, CurrencyCode, AllowedCurrencies, TermsCode, ArControlAccount, CollectionMethod,
                    StatementFrequency, Classification, SanctionsStatus, SanctionsScreenedAt, IsPersonalData, RetentionClass,
                    Status, FirstTradedDate, CreatedBy)
SELECT 'CAL', v.Code, v.Name, 'LTD', v.CoNo, 'GB', v.Vat, 1, 1, 'GB', 'GBP', 'GBP', v.Terms, '1200', 'BACS', 'MONTHLY',
       v.Class, 'CLEAR', '2026-09-01T06:00:00', 0, 'FIN7', 'ACTIVE', '2023-04-01', 'SEED'
  FROM (VALUES
   ('K0001','Firth of Forth Cycles Ltd','SC440118','GB712003441','N30','DEALER'),
   ('K0002','Highland Trail Bikes Ltd','SC551229','GB823114552','N30','DEALER'),
   ('K0003','Lothian Commuter Supply Ltd','SC662330','GB934225663','EOM30','DISTRIBUTOR'),
   ('K0004','Borders Cycle Hire Ltd','SC773441','GB145336774','N14','HIRE'),
   ('K0005','Stellar Components Ltd','04229011','GB412771903','N30','INTERCO')
  ) AS v (Code, Name, CoNo, Vat, Terms, Class)
 WHERE NOT EXISTS (SELECT 1 FROM ar.Customer x WHERE x.CompanyCode = 'CAL' AND x.CustomerCode = v.Code);
INSERT ar.CustomerCredit (CompanyCode, CustomerCode, CreditLimitGBP, OverdueThresholdGBP, OverdueDaysTrigger, RiskClass,
                          ReviewDate, ControllerUser, ApprovedBy, ApprovedAt, HoldFlag, RequiresPrepayment, ProFormaOnly, CreatedBy)
SELECT 'CAL', c.CustomerCode, 50000, 5000, 30, 'LOW', '2027-06-30', 'CREDMGR', 'CREDMGR', '2026-06-30T11:00:00', 0, 0, 0, 'SEED'
  FROM ar.Customer c
 WHERE c.CompanyCode = 'CAL'
   AND NOT EXISTS (SELECT 1 FROM ar.CustomerCredit x WHERE x.CompanyCode = 'CAL' AND x.CustomerCode = c.CustomerCode);
GO
/*  Registered and delivery addresses.  Sole trader addresses are personal data. */
INSERT ar.CustomerAddress (CompanyCode, CustomerCode, AddressRole, AddressRef, Name, Line1, Town, County, Postcode,
                           CountryCode, ContactName, ContactEmail, ContactPhone, ContactPurpose, IsPersonalData, IsDefault, ValidFrom, CreatedBy)
SELECT c.CompanyCode, c.CustomerCode, r.Role, LEFT(r.Role, 4) + '1', c.LegalName,
       CAST((ABS(CHECKSUM(c.CustomerCode + r.Role)) % 180) + 1 AS varchar(4)) + ' ' +
       CASE ABS(CHECKSUM(c.CustomerCode)) % 8 WHEN 0 THEN 'Station Road' WHEN 1 THEN 'Mill Lane' WHEN 2 THEN 'Chapel Street'
            WHEN 3 THEN 'Enterprise Way' WHEN 4 THEN 'Foundry Road' WHEN 5 THEN 'Kingsway' WHEN 6 THEN 'Riverside Drive' ELSE 'Albion Street' END,
       CASE c.CountryCode WHEN 'GB' THEN CASE ABS(CHECKSUM(c.CustomerCode)) % 6 WHEN 0 THEN 'Reading' WHEN 1 THEN 'Leeds'
            WHEN 2 THEN 'Bristol' WHEN 3 THEN 'Glasgow' WHEN 4 THEN 'Belfast' ELSE 'Birmingham' END
            WHEN 'FR' THEN 'Nantes' WHEN 'DE' THEN 'Koln' WHEN 'NL' THEN 'Utrecht' WHEN 'ES' THEN 'Zaragoza'
            WHEN 'IT' THEN 'Bergamo' WHEN 'NO' THEN 'Trondheim' WHEN 'US' THEN 'Portland' ELSE 'Melbourne' END,
       NULL,
       CASE WHEN c.CountryCode = 'GB' THEN 'RG' + CAST((ABS(CHECKSUM(c.CustomerCode)) % 30) + 1 AS varchar(2)) + ' ' +
            CAST((ABS(CHECKSUM(r.Role)) % 9) + 1 AS varchar(1)) + 'QP' ELSE NULL END,
       c.CountryCode,
       CASE r.Role WHEN 'BILLING' THEN 'Accounts Payable' ELSE 'Goods Inwards' END,
       LOWER(CASE r.Role WHEN 'BILLING' THEN 'accounts@' ELSE 'goodsin@' END + LOWER(REPLACE(LEFT(c.CustomerCode, 8), ' ', '')) + '.example.co.uk'),
       '01' + CAST(100000000 + (ABS(CHECKSUM(c.CustomerCode)) % 800000000) AS varchar(10)),
       CASE r.Role WHEN 'BILLING' THEN 'Invoicing and statements' ELSE 'Delivery arrangements' END,
       c.IsPersonalData, 1, '2021-09-01', 'SEED'
  FROM ar.Customer c CROSS JOIN (VALUES ('REGISTERED'), ('BILLING'), ('DELIVERY')) AS r (Role)
 WHERE NOT EXISTS (SELECT 1 FROM ar.CustomerAddress a WHERE a.CompanyCode = c.CompanyCode AND a.CustomerCode = c.CustomerCode AND a.AddressRole = r.Role);
GO
/*  Direct Debit mandates for the customers collected by Direct Debit (D-16, D-20). */
INSERT ar.DirectDebitMandate (CompanyCode, CustomerCode, MandateReference, ServiceUserNumber, PayerName, SortCode,
                              AccountNumber, SignedDate, LodgedDate, FirstCollectionDate, AdvanceNoticeDays,
                              AdvanceNoticeAgreedInWriting, Status, CancelledDate, CancelReason, GuaranteeIssuedAt, CreatedBy)
SELECT c.CompanyCode, c.CustomerCode, 'STC-DD-' + c.CustomerCode, '834512', c.LegalName,
       RIGHT('00' + CAST((ABS(CHECKSUM(c.CustomerCode)) % 999999) AS varchar(6)), 6),
       RIGHT('0000' + CAST((ABS(CHECKSUM(c.CustomerCode + 'A')) % 99999999) AS varchar(8)), 8),
       '2025-05-12', '2025-05-16', '2025-06-02', 3, 1,
       CASE WHEN c.CustomerCode = 'C0032' THEN 'CANCELLED' ELSE 'ACTIVE' END,
       CASE WHEN c.CustomerCode = 'C0032' THEN CAST('2026-08-20' AS date) END,
       CASE WHEN c.CustomerCode = 'C0032' THEN 'Collection returned unpaid by the paying bank' END,
       '2025-05-16T09:00:00', 'SEED'
  FROM ar.Customer c
 WHERE c.CollectionMethod = 'DD'
   AND NOT EXISTS (SELECT 1 FROM ar.DirectDebitMandate m WHERE m.CompanyCode = c.CompanyCode AND m.CustomerCode = c.CustomerCode);
GO
