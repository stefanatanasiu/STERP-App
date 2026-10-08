/*  Reference seed 00 - legal companies, their effective-dated statutory profile and their sites.
    STC Stellar Components Ltd (Reading, Birmingham) and CAL Caledon Assemblies Ltd (Glasgow,
    Livingston - Scottish bank-holiday calendar, company registered in England and Wales).
    REQ: COM-001, COM-020, COM-025, D-01 */
SET NOCOUNT ON;
GO
MERGE core.Company AS t
USING (VALUES
 ('STC','Stellar Components Ltd','Stellar Components','LTD','07412936','England and Wales','GB412771903','XI412771903','GB412771903000','XI412771903000','FRS102','03-31','QUARTER-03','INVOICE','GBP','ENG',
  'Unit 7 Kennet Island Business Park','Pinnacle Way','Reading','RG2 0RP','United Kingdom',1,1,1,1,1),
 ('CAL','Caledon Assemblies Ltd','Caledon Assemblies','LTD','09233471','England and Wales','GB233471029',NULL,'GB233471029000',NULL,'FRS102','03-31','QUARTER-03','INVOICE','GBP','ENG',
  'Oakfield House','112 Bridge Street','Manchester','M3 3QX','United Kingdom',0,0,1,0,1)
) AS s (CompanyCode,Name,TradingName,LegalForm,RegisteredNumber,Jurisdiction,VatRegNo,XiVatRegNo,Eori,XiEori,AccountingFramework,AccountingRefDate,VatStagger,VatScheme,BaseCurrency,CalendarCode,
        RegOfficeLine1,RegOfficeLine2,RegOfficeTown,RegOfficePostcode,RegOfficeCountry,PaymentPracticesInScope,PackagingInScope,EInvoicingEnabled,DomesticReverseChargeEnabled,IsActive)
ON t.CompanyCode = s.CompanyCode
WHEN MATCHED THEN UPDATE SET t.Name=s.Name, t.TradingName=s.TradingName, t.LegalForm=s.LegalForm, t.RegisteredNumber=s.RegisteredNumber, t.Jurisdiction=s.Jurisdiction,
  t.VatRegNo=s.VatRegNo, t.XiVatRegNo=s.XiVatRegNo, t.Eori=s.Eori, t.XiEori=s.XiEori, t.AccountingFramework=s.AccountingFramework, t.AccountingRefDate=s.AccountingRefDate,
  t.VatStagger=s.VatStagger, t.VatScheme=s.VatScheme, t.BaseCurrency=s.BaseCurrency, t.CalendarCode=s.CalendarCode, t.RegOfficeLine1=s.RegOfficeLine1, t.RegOfficeLine2=s.RegOfficeLine2,
  t.RegOfficeTown=s.RegOfficeTown, t.RegOfficePostcode=s.RegOfficePostcode, t.RegOfficeCountry=s.RegOfficeCountry, t.PaymentPracticesInScope=s.PaymentPracticesInScope,
  t.PackagingInScope=s.PackagingInScope, t.EInvoicingEnabled=s.EInvoicingEnabled, t.DomesticReverseChargeEnabled=s.DomesticReverseChargeEnabled, t.IsActive=s.IsActive, t.ModifiedBy='SEED', t.ModifiedAt=SYSDATETIME()
WHEN NOT MATCHED THEN INSERT (CompanyCode,Name,TradingName,LegalForm,RegisteredNumber,Jurisdiction,VatRegNo,XiVatRegNo,Eori,XiEori,AccountingFramework,AccountingRefDate,VatStagger,VatScheme,
  BaseCurrency,CalendarCode,RegOfficeLine1,RegOfficeLine2,RegOfficeTown,RegOfficePostcode,RegOfficeCountry,PaymentPracticesInScope,PackagingInScope,EInvoicingEnabled,DomesticReverseChargeEnabled,IsActive,CreatedBy)
  VALUES (s.CompanyCode,s.Name,s.TradingName,s.LegalForm,s.RegisteredNumber,s.Jurisdiction,s.VatRegNo,s.XiVatRegNo,s.Eori,s.XiEori,s.AccountingFramework,s.AccountingRefDate,s.VatStagger,s.VatScheme,
    s.BaseCurrency,s.CalendarCode,s.RegOfficeLine1,s.RegOfficeLine2,s.RegOfficeTown,s.RegOfficePostcode,s.RegOfficeCountry,s.PaymentPracticesInScope,s.PackagingInScope,s.EInvoicingEnabled,
    s.DomesticReverseChargeEnabled,s.IsActive,'SEED');
GO
MERGE core.CompanyProfile AS t
USING (VALUES
 ('STC','2019-04-01','GB412771903','XI412771903','GB412771903000','FRS102','03-31','QUARTER-03','INVOICE','2019-04-01','Unit 7 Kennet Island Business Park','Reading','RG2 0RP','Initial registration at incorporation'),
 ('CAL','2021-04-01','GB233471029',NULL,'GB233471029000','FRS102','03-31','QUARTER-03','INVOICE','2021-04-01','Oakfield House','Manchester','M3 3QX','Initial registration at incorporation')
) AS s (CompanyCode,EffectiveFrom,VatRegNo,XiVatRegNo,Eori,AccountingFramework,AccountingRefDate,VatStagger,VatScheme,VatRegisteredFrom,RegOfficeLine1,RegOfficeTown,RegOfficePostcode,ChangeReason)
ON t.CompanyCode = s.CompanyCode AND t.EffectiveFrom = s.EffectiveFrom
WHEN NOT MATCHED THEN INSERT (CompanyCode,EffectiveFrom,VatRegNo,XiVatRegNo,Eori,AccountingFramework,AccountingRefDate,VatStagger,VatScheme,VatRegisteredFrom,RegOfficeLine1,RegOfficeTown,RegOfficePostcode,ChangeReason,CreatedBy)
  VALUES (s.CompanyCode,s.EffectiveFrom,s.VatRegNo,s.XiVatRegNo,s.Eori,s.AccountingFramework,s.AccountingRefDate,s.VatStagger,s.VatScheme,s.VatRegisteredFrom,s.RegOfficeLine1,s.RegOfficeTown,s.RegOfficePostcode,s.ChangeReason,'SEED');
GO
MERGE core.Site AS t
USING (VALUES
 ('STC','RDG','Reading Works','ENG','ENG','Unit 7 Kennet Island Business Park','Pinnacle Way','Reading','RG2 0RP',1),
 ('STC','BHM','Birmingham Distribution Centre','ENG','ENG','Fort Dunlop Industrial Estate','Bromford Lane','Birmingham','B24 9FD',0),
 ('CAL','GLA','Glasgow Assembly Plant','SCT','SCT','Unit 14 Clydeway Industrial Estate','Plantation Quay','Glasgow','G51 1EA',1),
 ('CAL','LIV','Livingston Finishing Unit','SCT','SCT','Deans Industrial Estate','Alderstone Road','Livingston','EH54 8AU',0)
) AS s (CompanyCode,SiteCode,Name,Nation,CalendarCode,AddressLine1,AddressLine2,Town,Postcode,IsManufacturing)
ON t.CompanyCode = s.CompanyCode AND t.SiteCode = s.SiteCode
WHEN MATCHED THEN UPDATE SET t.Name=s.Name, t.Nation=s.Nation, t.CalendarCode=s.CalendarCode, t.AddressLine1=s.AddressLine1, t.AddressLine2=s.AddressLine2,
  t.Town=s.Town, t.Postcode=s.Postcode, t.IsManufacturing=s.IsManufacturing, t.ModifiedBy='SEED', t.ModifiedAt=SYSDATETIME()
WHEN NOT MATCHED THEN INSERT (CompanyCode,SiteCode,Name,Nation,CalendarCode,AddressLine1,AddressLine2,Town,Postcode,IsManufacturing,CreatedBy)
  VALUES (s.CompanyCode,s.SiteCode,s.Name,s.Nation,s.CalendarCode,s.AddressLine1,s.AddressLine2,s.Town,s.Postcode,s.IsManufacturing,'SEED');
GO
PRINT 'Seed 00: companies and sites';
GO
