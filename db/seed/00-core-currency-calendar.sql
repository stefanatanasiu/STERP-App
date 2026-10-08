/*  Reference seed 00 - currencies, exchange rates and the UK business-day calendars.
    Rates are quoted as foreign-currency units per one pound sterling (D-14).  Calendars ENG
    (England and Wales, the default), SCT, NIR and BACS (weekends plus England and Wales bank
    holidays) carry real UK bank holidays for 2026 to 2028.
    REQ: COM-010, COM-026, FIN-019, AP-026, AR-025, D-14 */
SET NOCOUNT ON;
GO
MERGE core.Currency AS t
USING (VALUES ('GBP','Pound sterling',N'£',2,1),('EUR','Euro',N'€',2,1),('USD','US dollar','$',2,1)) AS s (CurrencyCode,Name,Symbol,MinorUnits,IsActive)
ON t.CurrencyCode = s.CurrencyCode
WHEN MATCHED THEN UPDATE SET t.Name=s.Name, t.Symbol=s.Symbol, t.MinorUnits=s.MinorUnits, t.IsActive=s.IsActive
WHEN NOT MATCHED THEN INSERT (CurrencyCode,Name,Symbol,MinorUnits,IsActive,CreatedBy) VALUES (s.CurrencyCode,s.Name,s.Symbol,s.MinorUnits,s.IsActive,'SEED');
GO
MERGE core.Calendar AS t
USING (VALUES
 ('ENG','England and Wales bank holidays','ENG','BANK'),
 ('SCT','Scotland bank holidays','SCT','BANK'),
 ('NIR','Northern Ireland bank holidays','NIR','BANK'),
 ('BACS','Bacs processing calendar','ENG','BACS')
) AS s (CalendarCode,Name,Nation,CalendarType)
ON t.CalendarCode = s.CalendarCode
WHEN MATCHED THEN UPDATE SET t.Name=s.Name, t.Nation=s.Nation, t.CalendarType=s.CalendarType
WHEN NOT MATCHED THEN INSERT (CalendarCode,Name,Nation,CalendarType,CreatedBy) VALUES (s.CalendarCode,s.Name,s.Nation,s.CalendarType,'SEED');
GO
/*  England and Wales 2026-2028 (substitute days where a holiday falls at a weekend). */
DECLARE @h TABLE (CalendarCode varchar(10), HolidayDate date, Name varchar(60), IsSub bit);
INSERT @h VALUES
 ('ENG','2026-01-01','New Year''s Day',0),('ENG','2026-04-03','Good Friday',0),('ENG','2026-04-06','Easter Monday',0),
 ('ENG','2026-05-04','Early May bank holiday',0),('ENG','2026-05-25','Spring bank holiday',0),('ENG','2026-08-31','Summer bank holiday',0),
 ('ENG','2026-12-25','Christmas Day',0),('ENG','2026-12-28','Boxing Day (substitute day)',1),
 ('ENG','2027-01-01','New Year''s Day',0),('ENG','2027-03-26','Good Friday',0),('ENG','2027-03-29','Easter Monday',0),
 ('ENG','2027-05-03','Early May bank holiday',0),('ENG','2027-05-31','Spring bank holiday',0),('ENG','2027-08-30','Summer bank holiday',0),
 ('ENG','2027-12-27','Christmas Day (substitute day)',1),('ENG','2027-12-28','Boxing Day (substitute day)',1),
 ('ENG','2028-01-03','New Year''s Day (substitute day)',1),('ENG','2028-04-14','Good Friday',0),('ENG','2028-04-17','Easter Monday',0),
 ('ENG','2028-05-01','Early May bank holiday',0),('ENG','2028-05-29','Spring bank holiday',0),('ENG','2028-08-28','Summer bank holiday',0),
 ('ENG','2028-12-25','Christmas Day',0),('ENG','2028-12-26','Boxing Day',0),
 ('SCT','2026-01-01','New Year''s Day',0),('SCT','2026-01-02','2 January',0),('SCT','2026-04-03','Good Friday',0),
 ('SCT','2026-05-04','Early May bank holiday',0),('SCT','2026-05-25','Spring bank holiday',0),('SCT','2026-08-03','Summer bank holiday',0),
 ('SCT','2026-11-30','St Andrew''s Day',0),('SCT','2026-12-25','Christmas Day',0),('SCT','2026-12-28','Boxing Day (substitute day)',1),
 ('SCT','2027-01-01','New Year''s Day',0),('SCT','2027-01-04','2 January (substitute day)',1),('SCT','2027-03-26','Good Friday',0),
 ('SCT','2027-05-03','Early May bank holiday',0),('SCT','2027-05-31','Spring bank holiday',0),('SCT','2027-08-02','Summer bank holiday',0),
 ('SCT','2027-11-30','St Andrew''s Day',0),('SCT','2027-12-27','Christmas Day (substitute day)',1),('SCT','2027-12-28','Boxing Day (substitute day)',1),
 ('SCT','2028-01-03','New Year''s Day (substitute day)',1),('SCT','2028-01-04','2 January (substitute day)',1),('SCT','2028-04-14','Good Friday',0),
 ('SCT','2028-05-01','Early May bank holiday',0),('SCT','2028-05-29','Spring bank holiday',0),('SCT','2028-08-07','Summer bank holiday',0),
 ('SCT','2028-11-30','St Andrew''s Day',0),('SCT','2028-12-25','Christmas Day',0),('SCT','2028-12-26','Boxing Day',0),
 ('NIR','2026-01-01','New Year''s Day',0),('NIR','2026-03-17','St Patrick''s Day',0),('NIR','2026-04-03','Good Friday',0),('NIR','2026-04-06','Easter Monday',0),
 ('NIR','2026-05-04','Early May bank holiday',0),('NIR','2026-05-25','Spring bank holiday',0),('NIR','2026-07-13','Battle of the Boyne (substitute day)',1),
 ('NIR','2026-08-31','Summer bank holiday',0),('NIR','2026-12-25','Christmas Day',0),('NIR','2026-12-28','Boxing Day (substitute day)',1),
 ('NIR','2027-01-01','New Year''s Day',0),('NIR','2027-03-17','St Patrick''s Day',0),('NIR','2027-03-26','Good Friday',0),('NIR','2027-03-29','Easter Monday',0),
 ('NIR','2027-05-03','Early May bank holiday',0),('NIR','2027-05-31','Spring bank holiday',0),('NIR','2027-07-12','Battle of the Boyne',0),
 ('NIR','2027-08-30','Summer bank holiday',0),('NIR','2027-12-27','Christmas Day (substitute day)',1),('NIR','2027-12-28','Boxing Day (substitute day)',1),
 ('NIR','2028-01-03','New Year''s Day (substitute day)',1),('NIR','2028-03-17','St Patrick''s Day',0),('NIR','2028-04-14','Good Friday',0),('NIR','2028-04-17','Easter Monday',0),
 ('NIR','2028-05-01','Early May bank holiday',0),('NIR','2028-05-29','Spring bank holiday',0),('NIR','2028-07-12','Battle of the Boyne',0),
 ('NIR','2028-08-28','Summer bank holiday',0),('NIR','2028-12-25','Christmas Day',0),('NIR','2028-12-26','Boxing Day',0);
INSERT @h (CalendarCode, HolidayDate, Name, IsSub) SELECT 'BACS', HolidayDate, Name, IsSub FROM @h WHERE CalendarCode = 'ENG';
MERGE core.Holiday AS t USING @h AS s ON t.CalendarCode = s.CalendarCode AND t.HolidayDate = s.HolidayDate
WHEN MATCHED THEN UPDATE SET t.Name = s.Name, t.IsSubstituteDay = s.IsSub
WHEN NOT MATCHED THEN INSERT (CalendarCode,HolidayDate,Name,IsSubstituteDay,CreatedBy) VALUES (s.CalendarCode,s.HolidayDate,s.Name,s.IsSub,'SEED');
GO
/*  Expand every calendar day from 2025 to 2029 so business-day arithmetic is a single seek. */
;WITH n AS (SELECT TOP (1827) ROW_NUMBER() OVER (ORDER BY (SELECT 1)) - 1 AS i FROM sys.all_objects a CROSS JOIN sys.all_objects b),
 d AS (SELECT DATEADD(day, i, CAST('2025-01-01' AS date)) AS CalDate FROM n),
 x AS (SELECT c.CalendarCode, d.CalDate, h.Name AS HolidayName,
              CASE WHEN ((DATEPART(dw, d.CalDate) + @@DATEFIRST) % 7) IN (0,1) THEN 'WEEKEND' WHEN h.HolidayDate IS NOT NULL THEN 'HOLIDAY' ELSE 'WORKING' END AS DayType
         FROM core.Calendar c CROSS JOIN d LEFT JOIN core.Holiday h ON h.CalendarCode = c.CalendarCode AND h.HolidayDate = d.CalDate)
MERGE core.CalendarDay AS t
USING (SELECT CalendarCode, CalDate, DayType, HolidayName, CASE WHEN DayType = 'WORKING' THEN 1 ELSE 0 END AS IsBusinessDay FROM x) AS s
ON t.CalendarCode = s.CalendarCode AND t.CalDate = s.CalDate
WHEN MATCHED THEN UPDATE SET t.DayType = s.DayType, t.HolidayName = s.HolidayName, t.IsBusinessDay = s.IsBusinessDay
WHEN NOT MATCHED THEN INSERT (CalendarCode,CalDate,IsBusinessDay,DayType,HolidayName) VALUES (s.CalendarCode,s.CalDate,s.IsBusinessDay,s.DayType,s.HolidayName);
GO
/*  Daily Bank of England spot rates for 2026 and 2027 (published on working days only) and the
    HMRC monthly rates used for customs values.  Deterministic waves so demos reproduce exactly. */
;WITH n AS (SELECT TOP (731) ROW_NUMBER() OVER (ORDER BY (SELECT 1)) - 1 AS i FROM sys.all_objects a CROSS JOIN sys.all_objects b),
 d AS (SELECT DATEADD(day, i, CAST('2026-01-01' AS date)) AS RateDate, i FROM n),
 r AS (SELECT RateDate, 'EUR' AS CurrencyCode, CAST(1.17500 + 0.0300 * SIN(i / 9.0) + 0.0150 * SIN(i / 61.0) AS decimal(18,8)) AS UnitsPerGBP FROM d
       UNION ALL
       SELECT RateDate, 'USD', CAST(1.26500 + 0.0400 * SIN(i / 7.0) + 0.0250 * SIN(i / 53.0) AS decimal(18,8)) FROM d)
MERGE core.FxRate AS t
USING (SELECT CurrencyCode, RateDate, 'SPOT' AS RateType, ROUND(UnitsPerGBP, 6) AS UnitsPerGBP, 'Bank of England daily spot' AS Source FROM r
        WHERE ((DATEPART(dw, RateDate) + @@DATEFIRST) % 7) NOT IN (0,1)) AS s
ON t.CurrencyCode = s.CurrencyCode AND t.RateDate = s.RateDate AND t.RateType = s.RateType
WHEN MATCHED THEN UPDATE SET t.UnitsPerGBP = s.UnitsPerGBP, t.Source = s.Source
WHEN NOT MATCHED THEN INSERT (CurrencyCode,RateDate,RateType,UnitsPerGBP,Source,EnteredQuotation,CreatedBy)
  VALUES (s.CurrencyCode,s.RateDate,s.RateType,s.UnitsPerGBP,s.Source,'Units per GBP','SEED');
GO
;WITH m AS (SELECT TOP (24) ROW_NUMBER() OVER (ORDER BY (SELECT 1)) - 1 AS i FROM sys.all_objects),
 d AS (SELECT DATEADD(month, i, CAST('2026-01-01' AS date)) AS RateDate, i FROM m)
MERGE core.FxRate AS t
USING (SELECT 'EUR' AS CurrencyCode, RateDate, 'HMRC' AS RateType, ROUND(CAST(1.1700 + 0.0250 * SIN(i / 3.0) AS decimal(18,8)), 4) AS UnitsPerGBP, 'HMRC monthly rate' AS Source FROM d
        UNION ALL
       SELECT 'USD', RateDate, 'HMRC', ROUND(CAST(1.2600 + 0.0350 * SIN(i / 4.0) AS decimal(18,8)), 4), 'HMRC monthly rate' FROM d) AS s
ON t.CurrencyCode = s.CurrencyCode AND t.RateDate = s.RateDate AND t.RateType = s.RateType
WHEN MATCHED THEN UPDATE SET t.UnitsPerGBP = s.UnitsPerGBP, t.Source = s.Source
WHEN NOT MATCHED THEN INSERT (CurrencyCode,RateDate,RateType,UnitsPerGBP,Source,EnteredQuotation,CreatedBy)
  VALUES (s.CurrencyCode,s.RateDate,s.RateType,s.UnitsPerGBP,s.Source,'Units per GBP','SEED');
GO
PRINT 'Seed 00: currencies, calendars and exchange rates';
GO
