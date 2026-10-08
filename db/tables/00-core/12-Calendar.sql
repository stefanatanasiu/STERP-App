/*  core.Calendar, core.Holiday, core.CalendarDay - England & Wales, Scotland, Northern Ireland
    bank holidays and the Bacs processing calendar.  REQ: COM-026, AP-026, AR-025 */
SET NOCOUNT ON;
IF OBJECT_ID(N'core.Calendar') IS NULL CREATE TABLE core.Calendar (
  CalendarCode varchar(10) NOT NULL CONSTRAINT PK_Calendar PRIMARY KEY, Name varchar(60) NOT NULL, Nation varchar(3) NULL,
  CalendarType varchar(12) NOT NULL CONSTRAINT DF_Cal_Type DEFAULT 'BANK' CONSTRAINT CK_Cal_Type CHECK (CalendarType IN ('BANK','BACS','SITE')), WorksSaturday bit NOT NULL CONSTRAINT DF_Cal_Sat DEFAULT 0,
  WorksSunday bit NOT NULL CONSTRAINT DF_Cal_Sun DEFAULT 0, IsActive bit NOT NULL CONSTRAINT DF_Cal_Act DEFAULT 1, CreatedBy varchar(20) NULL, CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Cal_CA DEFAULT SYSDATETIME(), ModifiedBy varchar(20) NULL,
  ModifiedAt datetime2(3) NULL, RowVer rowversion);
GO
IF OBJECT_ID(N'core.Holiday') IS NULL CREATE TABLE core.Holiday (
  CalendarCode varchar(10) NOT NULL REFERENCES core.Calendar(CalendarCode), HolidayDate date NOT NULL, Name varchar(60) NOT NULL, IsSubstituteDay bit NOT NULL CONSTRAINT DF_Hol_Sub DEFAULT 0, CreatedBy varchar(20) NULL,
  CreatedAt datetime2(3) NOT NULL CONSTRAINT DF_Hol_CA DEFAULT SYSDATETIME(), RowVer rowversion, CONSTRAINT PK_Holiday PRIMARY KEY (CalendarCode, HolidayDate));
GO
/*  Expanded day table so business-day arithmetic is a single seek (seeded 2025-2029). */
IF OBJECT_ID(N'core.CalendarDay') IS NULL CREATE TABLE core.CalendarDay (
  CalendarCode varchar(10) NOT NULL REFERENCES core.Calendar(CalendarCode), CalDate date NOT NULL, IsBusinessDay bit NOT NULL, DayType varchar(10) NOT NULL CONSTRAINT CK_CalDay_Type CHECK (DayType IN ('WORKING','WEEKEND','HOLIDAY')),
  HolidayName varchar(60) NULL, BusinessDayNo int NULL, CONSTRAINT PK_CalendarDay PRIMARY KEY (CalendarCode, CalDate));
GO
