/*  Document number series, payment terms, retention classes, the statutory obligations register,
    a fictional sanctions list for demonstration, and the top level menu bar.
    REQ: COM-005, COM-009, COM-012, COM-021, COM-027, COM-029, D-15, D-17, D-25, D-29 */
SET NOCOUNT ON;
GO
/*  One continuous sequence per company and document type (D-15).  Legal sequences
    (sales invoices and credit notes) are flagged so voids are retained, never reused. */
DECLARE @ns TABLE (SeriesCode varchar(20), DocumentType varchar(20), Description varchar(100), Prefix varchar(10), NumberLength tinyint, IsLegal bit);
INSERT @ns VALUES
 ('SINV','SALESINVOICE','Sales invoice','SI',8,1),      ('SCRN','SALESCREDIT','Sales credit note','SC',8,1),
 ('SPRO','PROFORMA','Pro forma invoice','PF',8,0),       ('SQUO','QUOTE','Sales quotation','SQ',8,0),
 ('SORD','SALESORDER','Sales order','SO',8,0),           ('SDES','DESPATCH','Despatch note','DN',8,0),
 ('SPCK','PICKLIST','Pick list','PK',8,0),               ('SRMA','RETURNAUTH','Return authorisation','RA',8,0),
 ('PREQ','REQUISITION','Purchase requisition','PR',8,0), ('PORD','PURCHASEORDER','Purchase order','PO',8,0),
 ('PGRN','GOODSRECEIPT','Goods received note','GR',8,0), ('PSRN','SUPPLIERRETURN','Supplier return note','SR',8,0),
 ('PINV','PURCHINVOICE','Purchase invoice register','PI',8,0), ('PCRN','PURCHCREDIT','Purchase credit register','PC',8,0),
 ('PIMP','IMPORTENTRY','Import entry','IM',8,0),         ('JNL','JOURNAL','General ledger journal','JN',8,1),
 ('JNLR','JOURNALREV','Journal reversal','JR',8,1),      ('ARRC','RECEIPT','Customer receipt','RC',8,0),
 ('ARDD','DDCOLLECTION','Direct Debit collection','DD',8,0), ('ARWO','WRITEOFF','Receivable write off','WO',8,0),
 ('ARLP','LATEPAYCLAIM','Late payment claim','LP',8,0),  ('APPY','PAYMENT','Supplier payment','PY',8,0),
 ('APPP','PAYPROPOSAL','Payment proposal','PP',8,0),     ('KEXE','EXECUTIONKIT','Execution kit','KT',8,0),
 ('KTRL','TRIALKIT','Trial kit','TK',8,0),               ('KDIS','DISASSEMBLY','Disassembly order','DS',8,0),
 ('CJOB','JOB','Job','JB',8,0),                          ('CLND','LANDEDCOST','Landed cost document','LC',8,0),
 ('CSET','COSTSET','Standard cost set','CS',6,0),        ('BECO','BOMCHANGE','Engineering change order','EC',8,0),
 ('ICNT','STOCKCOUNT','Stock count','CN',8,0),           ('IADJ','STOCKADJUST','Stock adjustment','AJ',8,0),
 ('IXFR','STOCKTRANSFER','Stock transfer','TR',8,0),     ('VRET','VATRETURN','VAT return','VR',6,1),
 ('XEXC','EXCEPTION','Exception reference','EX',8,0);
INSERT core.NumberSeries (CompanyCode, SeriesCode, DocumentType, Description, Prefix, NumberLength, NextNumber, MinNumber, MaxNumber, IsLegalSequence, CreatedBy)
SELECT c.CompanyCode, n.SeriesCode, n.DocumentType, n.Description, n.Prefix + LEFT(c.CompanyCode, 1), n.NumberLength, 1, 1, 99999999, n.IsLegal, 'SEED'
  FROM core.Company c CROSS JOIN @ns n
 WHERE NOT EXISTS (SELECT 1 FROM core.NumberSeries s WHERE s.CompanyCode = c.CompanyCode AND s.SeriesCode = n.SeriesCode);
GO
/*  Payment terms.  The statutory default applies where nothing else is agreed (D-25). */
MERGE core.PaymentTerms AS t
USING (VALUES
 ('N30','30 days net','INVOICE',30,NULL,NULL,NULL,0,30),
 ('N14','14 days net','INVOICE',14,NULL,NULL,NULL,0,14),
 ('N45','45 days net','INVOICE',45,NULL,NULL,NULL,0,45),
 ('N60','60 days net','INVOICE',60,NULL,NULL,NULL,0,60),
 ('EOM30','30 days end of month','EOM',30,NULL,NULL,NULL,0,45),
 ('EOM60','60 days end of month','EOM',60,NULL,NULL,NULL,0,75),
 ('DOM31','Day 31 of the following month','DAYOFNEXTMONTH',0,31,NULL,NULL,0,45),
 ('DOM15','Day 15 of the following month','DAYOFNEXTMONTH',0,15,NULL,NULL,0,30),
 ('D2N30','2 per cent 10 days, net 30','INVOICE',30,NULL,10,2.0000,0,30),
 ('D1N45','1 per cent 14 days, net 45','INVOICE',45,NULL,14,1.0000,0,45),
 ('STAT','Statutory default, 30 days','INVOICE',30,NULL,NULL,NULL,1,30),
 ('PREPAY','Payment with order','INVOICE',0,NULL,NULL,NULL,0,0),
 ('PROFORMA','Pro forma, cleared funds before despatch','INVOICE',0,NULL,NULL,NULL,0,0)
) AS s (TermsCode, Name, BasisType, Days, DayOfMonth, DiscountDays, DiscountPct, IsStatutoryDefault, EffectiveDays)
ON t.TermsCode = s.TermsCode
WHEN MATCHED AND EXISTS (SELECT s.Name, s.BasisType, s.Days, s.DayOfMonth, s.DiscountDays, s.DiscountPct, s.IsStatutoryDefault
                         EXCEPT SELECT t.Name, t.BasisType, t.Days, t.DayOfMonth, t.DiscountDays, t.DiscountPct, t.IsStatutoryDefault)
    THEN UPDATE SET Name = s.Name, BasisType = s.BasisType, Days = s.Days, DayOfMonth = s.DayOfMonth,
                    DiscountDays = s.DiscountDays, DiscountPct = s.DiscountPct, IsStatutoryDefault = s.IsStatutoryDefault,
                    EffectiveDays = s.EffectiveDays, ModifiedBy = 'SEED', ModifiedAt = SYSDATETIME()
WHEN NOT MATCHED THEN
    INSERT (TermsCode, Name, BasisType, Days, DayOfMonth, DiscountDays, DiscountPct, IsStatutoryDefault, EffectiveDays, CreatedBy)
    VALUES (s.TermsCode, s.Name, s.BasisType, s.Days, s.DayOfMonth, s.DiscountDays, s.DiscountPct, s.IsStatutoryDefault, s.EffectiveDays, 'SEED');
GO
/*  Retention classes (D-17). */
MERGE core.RetentionClass AS t
USING (VALUES
 ('FIN7','Financial and accounting records',7,'FINYEAREND',0,'REVIEW','D-17'),
 ('VAT7','VAT, customs and import records',7,'FINYEAREND',0,'REVIEW','D-17'),
 ('TRACE11','Lot and serial traceability, safety relevant',11,'LASTMOVEMENT',0,'REVIEW','D-17'),
 ('CONTACT6','Contact personal data',6,'LASTTRANSACTION',1,'ANONYMISE','D-17'),
 ('BANK7','Bank and payee details',7,'LASTTRANSACTION',1,'DELETE','D-17'),
 ('QUOTE2','Unconverted quotations and enquiries',2,'DOCUMENTDATE',1,'DELETE','D-17'),
 ('AUDIT7','Audit trail and approval evidence',7,'EVENTDATE',1,'REVIEW','D-17'),
 ('LOG1','Technical logs and spool files',1,'EVENTDATE',0,'DELETE','D-17')
) AS s (RetentionClass, Name, RetentionYears, Basis, ContainsPersonalData, DisposalAction, DecisionRef)
ON t.RetentionClass = s.RetentionClass
WHEN MATCHED AND EXISTS (SELECT s.Name, s.RetentionYears, s.Basis, s.ContainsPersonalData, s.DisposalAction
                         EXCEPT SELECT t.Name, t.RetentionYears, t.Basis, t.ContainsPersonalData, t.DisposalAction)
    THEN UPDATE SET Name = s.Name, RetentionYears = s.RetentionYears, Basis = s.Basis,
                    ContainsPersonalData = s.ContainsPersonalData, DisposalAction = s.DisposalAction
WHEN NOT MATCHED THEN INSERT (RetentionClass, Name, RetentionYears, Basis, ContainsPersonalData, DisposalAction, DecisionRef)
    VALUES (s.RetentionClass, s.Name, s.RetentionYears, s.Basis, s.ContainsPersonalData, s.DisposalAction, s.DecisionRef);
GO
/*  Statutory obligations register for the current and next filing cycles (COM-012, COM-025). */
DECLARE @ob TABLE (ObligationCode varchar(20), PeriodKey varchar(20), PeriodStart date, PeriodEnd date, DueDate date, OwnerRole varchar(20), Notes varchar(200));
INSERT @ob VALUES
 ('VATRETURN','2026Q2','2026-04-01','2026-06-30','2026-08-07','VATOWNER','Quarterly MTD VAT return, stagger 1'),
 ('VATRETURN','2026Q3','2026-07-01','2026-09-30','2026-11-07','VATOWNER','Quarterly MTD VAT return, stagger 1'),
 ('VATRETURN','2026Q4','2026-10-01','2026-12-31','2027-02-07','VATOWNER','Quarterly MTD VAT return, stagger 1'),
 ('VATRETURN','2027Q1','2027-01-01','2027-03-31','2027-05-07','VATOWNER','Quarterly MTD VAT return, stagger 1'),
 ('ANNUALACCTS','FY2026','2026-04-01','2027-03-31','2027-12-31','FINCTRL','Statutory accounts filed at Companies House'),
 ('CONFSTMT','2026','2026-01-01','2026-12-31','2027-01-14','FINCTRL','Confirmation statement'),
 ('CORPTAX','FY2026','2026-04-01','2027-03-31','2028-01-01','FINCTRL','Corporation tax return CT600'),
 ('CORPTAXPAY','FY2026','2026-04-01','2027-03-31','2028-01-01','FINCTRL','Corporation tax payment'),
 ('PAYPRACTICES','2026H1','2026-04-01','2026-09-30','2026-10-30','FINCTRL','Payment practices and performance report'),
 ('PAYPRACTICES','2026H2','2026-10-01','2027-03-31','2027-04-30','FINCTRL','Payment practices and performance report'),
 ('PACKAGINGDATA','2026','2026-01-01','2026-12-31','2027-04-01','FINCTRL','Extended producer responsibility packaging data');
INSERT core.StatutoryObligation (CompanyCode, ObligationCode, PeriodKey, PeriodStart, PeriodEnd, DueDate, OwnerRole, Status, Notes)
SELECT c.CompanyCode, o.ObligationCode, o.PeriodKey, o.PeriodStart, o.PeriodEnd, o.DueDate, o.OwnerRole,
       CASE WHEN o.DueDate < CAST(SYSDATETIME() AS date) THEN 'COMPLETE' ELSE 'OPEN' END, o.Notes
  FROM core.Company c CROSS JOIN @ob o
 WHERE (o.ObligationCode <> 'PAYPRACTICES' OR c.PaymentPracticesInScope = 1)
   AND NOT EXISTS (SELECT 1 FROM core.StatutoryObligation x WHERE x.CompanyCode = c.CompanyCode
                     AND x.ObligationCode = o.ObligationCode AND x.PeriodKey = o.PeriodKey);
GO
/*  Fictional sanctions list used for demonstration only - it is not the OFSI list and must not
    be relied on.  Names are invented; one is a deliberate near match to a seeded supplier. */
DECLARE @sl TABLE (EntryRef varchar(20), EntityType varchar(12), Name varchar(120), Aliases varchar(200), Country varchar(40), Regime varchar(40));
INSERT @sl VALUES
 ('DEMO-0001','ENTITY','VOSTOK PRECISION METALS OOO','VOSTOK PRECISION;VPM METALS','Narnia','Demonstration regime A'),
 ('DEMO-0002','ENTITY','ARDENNE TOOLING SARL','ARDENNE TOOL;ATS SARL','Ruritania','Demonstration regime A'),
 ('DEMO-0003','ENTITY','HARBOURLIGHT TRADING FZE','HARBOURLIGHT FZE','Zubrowka','Demonstration regime B'),
 ('DEMO-0004','ENTITY','KESTREL MARITIME HOLDINGS LTD','KESTREL MARITIME','Freedonia','Demonstration regime B'),
 ('DEMO-0005','ENTITY','NORTHSTAR ALLOY WORKS JSC','NORTHSTAR ALLOY','Sylvania','Demonstration regime A'),
 ('DEMO-0006','INDIVIDUAL','GREGOR ALEKSANDR VOSTOKOV',NULL,'Narnia','Demonstration regime A'),
 ('DEMO-0007','INDIVIDUAL','MARTA ILIESCU-DRAGAN',NULL,'Ruritania','Demonstration regime A'),
 ('DEMO-0008','VESSEL','MV SILVER ALBATROSS','IMO9999001','Freedonia','Demonstration regime B'),
 ('DEMO-0009','ENTITY','TELLURIC CIRCUITS PLC','TELLURIC CIRCUIT','Latveria','Demonstration regime C'),
 ('DEMO-0010','ENTITY','BLACKMOOR CHEMICAL CO','BLACKMOOR CHEM','Genovia','Demonstration regime C');
INSERT core.SanctionsList (ListCode, EntryRef, EntityType, Name, NameNormalised, Aliases, Country, Regime, ListedOn, LastUpdated, IsActive)
SELECT 'OFSI-DEMO', s.EntryRef, s.EntityType, s.Name,
       REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(UPPER(LTRIM(RTRIM(s.Name))), '.', ''), ',', ''), '''', ''), ' LIMITED', ' LTD'), '  ', ' '),
       s.Aliases, s.Country, s.Regime, '2025-11-03', '2026-09-01', 1
  FROM @sl s
 WHERE NOT EXISTS (SELECT 1 FROM core.SanctionsList x WHERE x.ListCode = 'OFSI-DEMO' AND x.EntryRef = s.EntryRef);
GO
/*  Top level menu bar (COM-024).  Sub-menus and forms are registered by each module. */
INSERT core.MenuItem (MenuCode, ParentMenuCode, Caption, PermissionCode, SortOrder, IsActive, CreatedBy)
SELECT v.MenuCode, NULL, v.Caption, NULL, v.SortOrder, 1, 'SEED'
  FROM (VALUES ('FILE','&File',10), ('SYSTEM','&System',20), ('FIN','Fi&nance',30), ('AR','&Debtors',40),
               ('AP','&Creditors',50), ('SO','Sa&les',60), ('PO','&Purchasing',70), ('ITM','&Items',80),
               ('BOM','&BOM',90), ('KIT','&Kits',100), ('CST','Cos&ting',110), ('RPT','&Reports',120),
               ('WINDOW','&Window',130), ('HELP','&Help',140)) AS v (MenuCode, Caption, SortOrder)
 WHERE NOT EXISTS (SELECT 1 FROM core.MenuItem m WHERE m.MenuCode = v.MenuCode);
GO
