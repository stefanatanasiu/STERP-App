-- =====================================================================
-- 97-xmd-reports.sql - cross module controls and reporting (XMD/RPT)
-- Registers the complete RPT-001..RPT-019 report catalogue of
-- specification 11 section 8 in core.ReportDef, the xmd module's own
-- reports, the published shared measure definitions (XMD-010, D-19),
-- the tuning parameters of the control suite and the RPT menu rows.
-- Re-runnable: every statement is a guarded MERGE.
-- REQ: RPT-001, RPT-002, RPT-003, RPT-004, RPT-005, RPT-006, RPT-007
-- REQ: RPT-008, RPT-009, RPT-010, RPT-011, RPT-012, RPT-013, RPT-014
-- REQ: RPT-015, RPT-016, RPT-017, RPT-018, RPT-019
-- REQ: XMD-005, XMD-006, XMD-010, XMD-013, XMD-007, COM-022, COM-025
-- =====================================================================
SET NOCOUNT ON;
GO

-- ---------------------------------------------------------------- reports
IF OBJECT_ID(N'core.ReportDef') IS NOT NULL
BEGIN
    DECLARE @r TABLE (
        ReportCode varchar(20), Module varchar(6), Title varchar(100),
        ProgramName varchar(8), ParamSpec varchar(100),
        PermissionCode varchar(40), RequirementRefs varchar(200),
        Description varchar(300));

    INSERT @r VALUES
    -- the required catalogue of specification 11 section 8.  where a
    -- module already owns the program the catalogue entry points at it,
    -- so that two reports of the same measure cannot drift apart.
    ('RPT-001','AR','Aged receivables and statements','AGEDDEBT','CO,ASAT,CUST',
     'AR.ENQUIRY','RPT-001, AR-013, AR-017, AR-021',
     'Aged debt with buckets, disputes and unapplied credits; statements are produced by STATEMNT. Reconciles to AR carrying values (RECONCTL control type AR).'),
    ('RPT-002','AP','Aged payables and payment control','APAGED','CO,ASAT,SUPP',
     'AP.ENQUIRY.VIEW','RPT-002, AP-020, AP-022',
     'Aged creditors, blocked amounts, reserved payments and cash requirement. Reconciles to the AP control account.'),
    ('RPT-003','AR','Credit and collection exposure','DUNNING','CO,ASAT,STAGE,OWNER',
     'AR.DUNNING.RUN','RPT-003, AR-007, AR-008, AR-016, D-19',
     'Limit, open debt, eligible credits, unshipped and shipped-unbilled exposure, overrides and the next collection action.'),
    ('RPT-004','xmd','Cash forecast','CASHFCST','CO,BANK,FROM,TO,SCENARIO,USER,MODE',
     'FIN.BANK.RECONCILE','RPT-004, FIN-017, FIN-018',
     'Opening available bank balance and dated customer, supplier, VAT and tax flows. Contractual, expected and confirmed flows are kept apart; settled amounts are never counted twice with forecasts.'),
    ('RPT-005','fin','Trial balance and account activity','TBRPT','CO,YEAR,PERFROM,PERTO,ZEROS',
     'FIN.ENQUIRY.VIEW','RPT-005, FIN-021, FIN-023, FIN-030',
     'Opening, movement and closing by account and dimension; total debits equal credits or the run ends with return code 8.'),
    ('RPT-006','fin','Financial statements and budget comparison','PLRPT','CO,YEAR,PERFROM,PERTO,COMPARE',
     'FIN.ENQUIRY.VIEW','RPT-006, FIN-021, FIN-022, FIN-019',
     'Profit and loss, balance sheet (BSRPT) and budget variance (BUDGRPT) in management and statutory layout, with the mapping version and unmapped accounts disclosed.'),
    ('RPT-007','xmd','VAT detail and return review','VATAUDIT','CO,VATPERIOD,MODE,USER',
     'FIN.VAT.SUBMIT','RPT-007, FIN-016, FIN-028',
     'Supporting records for each VAT return box, special mechanisms, adjustments, late entries and finalisation status; reconciles to the VAT control account through RECONCTL.'),
    ('RPT-010','itm','Item stock and traceability','STKSTAT','CO,SITE,ITEM,ASAT,MODE',
     'ITM.STOCK.VIEW','RPT-010, ITM-011, ITM-012, ITM-013, ITM-016',
     'Physical, eligible, reserved, free and expected quantity by owner, location, lot and serial; genealogy through LOTTRACE.'),
    ('RPT-012','BOM','BOM explosion, cost and where used','BOMRP010','CO,ITEM,REV,QTY,ASAT,LEVELS,MODE,USER',
     'BOM.VIEW','RPT-012, BOM-006, BOM-021, BOM-023',
     'Complete calculation context, paths, revisions, rounding and leaf totals; where used through BOMRP030.'),
    ('RPT-015','xmd','Close and control account reconciliation','RECONCTL','CO,ASAT,MODE,USER,TOL',
     'FIN.ENQUIRY.VIEW','RPT-015, XMD-005, XMD-010, XMD-012, FIN-021',
     'Subsidiary ledgers against the general ledger at one shared cutoff for AR, AP, inventory, WIP, GRNI, VAT, bank and accruals. Every unresolved difference stays explicit and owned.'),
    ('RPT-016','xmd','Audit, approvals and data quality exceptions','EXCPAGE','CO,ASAT,MODE,USER,OWNER,DAYS',
     'FIN.ENQUIRY.VIEW','RPT-016, XMD-006, XMD-009, COM-022',
     'Exception ageing and ownership from first detection, escalation, pending approvals, authority references and restricted audit rows (values withheld).'),
    ('RPT-017','xmd','VAT return and MTD audit trail','VATAUDIT','CO,VATPERIOD,MODE,USER',
     'FIN.VAT.SUBMIT','RPT-017, FIN-016, FIN-027, FIN-028',
     'Boxes 1 to 9 with contributing transactions, adjustments and error corrections, postponed import VAT, submission evidence and the HMRC receipt reference.'),
    -- the xmd module''s own reports
    ('XMD.OBLIGREG','xmd','Statutory obligations register','OBLIGREG','CO,ASAT,MODE,USER,DAYS',
     'FIN.ENQUIRY.VIEW','XMD-013, XMD-009, COM-022',
     'Derived due dates for VAT returns and payments, payment practices, annual accounts, confirmation statement, corporation tax and packaging data, with owner, evidence and escalation.'),
    ('XMD.KPIOTIF','xmd','On time in full - customer and supplier','KPIOTIF','CO,FROM,TO,USER,SITE,DETAIL',
     'FIN.ENQUIRY.VIEW','XMD-010, XMD-001, XMD-002, RPT-008, RPT-009, D-19',
     'Customer OTIF on the original promised date and full quantity using the despatch date; supplier OTIF on the original confirmed date with a five working day early tolerance. A zero cohort is reported as undefined.'),
    ('XMD.CHAINRPT','xmd','Business chain trace','CHAINRPT','CO,TYPE,REF,USER',
     'FIN.ENQUIRY.VIEW','XMD-001, XMD-002, XMD-003, XMD-004',
     'Order to cash, procure to pay and assembly chain for one document, with the operational steps, the financial effects and the separate return, credit and reversal consequences.'),
    ('XMD.CONVLOAD','xmd','Legacy conversion load','CONVLOAD','CO,FILE,CUTOVER,MODE,USER',
     'FIN.JOURNAL.POST','XMD-007, XMD-008, COM-018, COM-023, FIN-024, AR-021, AP-024, ITM-024',
     'Fixed width legacy extract loaded at a dated cut over with per record accept, reject, duplicate and already-loaded outcomes. PREVIEW changes nothing; COMMIT is restartable without duplicates.');

    MERGE core.ReportDef AS t
    USING @r AS s ON t.ReportCode = s.ReportCode
    WHEN MATCHED THEN UPDATE SET
        Module = s.Module, Title = s.Title, ProgramName = s.ProgramName,
        ParamSpec = s.ParamSpec, PermissionCode = s.PermissionCode,
        RequirementRefs = s.RequirementRefs, Description = s.Description,
        IsActive = 1, ModifiedBy = 'STERP', ModifiedAt = SYSDATETIME()
    WHEN NOT MATCHED BY TARGET THEN INSERT
        (ReportCode, Module, Title, ProgramName, ParamSpec,
         PermissionCode, RequirementRefs, Description, IsActive, CreatedBy)
        VALUES (s.ReportCode, s.Module, s.Title, s.ProgramName, s.ParamSpec,
         s.PermissionCode, s.RequirementRefs, s.Description, 1, 'STERP');
    PRINT 'Seeded core.ReportDef (xmd / RPT catalogue)';
END
GO

-- RPT-008, RPT-009, RPT-011, RPT-013, RPT-014, RPT-018 and RPT-019 are
-- registered by the owning module seeds.  Only the requirement tag is
-- added here where the module used its own report code, so that the
-- catalogue can be proved complete from core.ReportDef alone.
IF OBJECT_ID(N'core.ReportDef') IS NOT NULL
BEGIN
    DECLARE @m TABLE (ReportCode varchar(20), Tag varchar(10));
    INSERT @m VALUES ('POPIPE','RPT-009'), ('CSTRP010','RPT-011'),
                     ('CSTRP020','RPT-014'), ('KITRP010','RPT-013'),
                     ('APPPR','RPT-018'), ('SOANAL','RPT-008');
    UPDATE t SET RequirementRefs =
        CASE WHEN t.RequirementRefs LIKE '%' + m.Tag + '%'
             THEN t.RequirementRefs
             ELSE LEFT(ISNULL(t.RequirementRefs,'') + ', ' + m.Tag, 200) END
    FROM core.ReportDef t JOIN @m m ON m.ReportCode = t.ReportCode;
    PRINT 'Tagged owning-module reports with their RPT ids';
END
GO

-- ---------------------------------------------------------------- measures
IF OBJECT_ID(N'rpt.MetricDefinition') IS NOT NULL
BEGIN
    DECLARE @d TABLE (
        MetricCode varchar(30), Name varchar(80), Definition varchar(600),
        NumeratorBasis varchar(200), DenominatorBasis varchar(200),
        CurrencyBasis varchar(40), CutoffBasis varchar(60),
        ZeroDenominatorRule varchar(100), DecisionRef varchar(10),
        OwnerRole varchar(20));

    INSERT @d VALUES
    ('OTIF.CUSTOMER','On time in full - customer',
     'Eligible completed schedule obligations satisfying the published quantity and date rule, divided by eligible obligations due in the window. Measured against the ORIGINAL promised date and the full scheduled quantity, using the despatch date.',
     'Schedules despatched in full on or before so.DeliverySchedule.OriginalPromiseDate',
     'Schedules whose OriginalPromiseDate falls in the reporting window and are not cancelled',
     'Quantity basis, no currency','Despatch date, confirmed despatches only',
     'Undefined - never zero and never one hundred per cent','D-19','SALESMGR'),
    ('OTIF.SUPPLIER','On time in full - supplier',
     'Eligible purchase schedules received in full by the ORIGINAL confirmed date, divided by eligible schedules due. A delivery up to five working days early still counts as on time; earlier than that is reported as early, not as on time.',
     'Schedules received in full between OriginalConfirmedDate less five working days and OriginalConfirmedDate',
     'Schedules whose OriginalConfirmedDate falls in the reporting window and are not cancelled',
     'Quantity basis, no currency','Physical receipt date, confirmed receipts only',
     'Undefined - never zero and never one hundred per cent','D-19','PURMGR'),
    ('RECON.DIFFERENCE','Control account difference',
     'Subsidiary ledger carrying value at the cutoff less the general ledger control balance at the same cutoff, in sterling, for one control family. Positive means the subsidiary ledger is higher than the ledger.',
     'Sum of subsidiary carrying values at the cutoff',
     'Not a ratio - an absolute difference',
     'Sterling at the posted rate','Shared event cutoff, POSTED journals only',
     'Not applicable','XMD-005','FINCTRL'),
    ('EXCEPTION.AGE','Exception age',
     'Whole days from the moment an exception was first detected to the report date. Reassigning an exception does not reset its age or erase the previous owner.',
     'Days since core.ExceptionItem.RaisedAt','Not a ratio',
     'Not applicable','Report as-at date',
     'Not applicable','XMD-006','FINCTRL'),
    ('GRNI.AGE','GRNI age',
     'Days from the actual ownership-recognition receipt date to the report date for the uncleared accrual amount.',
     'Days since po.Receipt.PhysicalDate for the uncleared amount','Not a ratio',
     'Sterling base currency','Report as-at date, confirmed receipts only',
     'Not applicable','XMD-002','PURMGR');

    MERGE rpt.MetricDefinition AS t
    USING @d AS s ON t.MetricCode = s.MetricCode
    WHEN MATCHED THEN UPDATE SET
        Name = s.Name, Definition = s.Definition,
        NumeratorBasis = s.NumeratorBasis, DenominatorBasis = s.DenominatorBasis,
        CurrencyBasis = s.CurrencyBasis, CutoffBasis = s.CutoffBasis,
        ZeroDenominatorRule = s.ZeroDenominatorRule,
        DecisionRef = s.DecisionRef, OwnerRole = s.OwnerRole, IsActive = 1
    WHEN NOT MATCHED BY TARGET THEN INSERT
        (MetricCode, Name, Definition, NumeratorBasis, DenominatorBasis,
         CurrencyBasis, CutoffBasis, ZeroDenominatorRule, DecisionRef,
         OwnerRole, IsActive)
        VALUES (s.MetricCode, s.Name, s.Definition, s.NumeratorBasis,
         s.DenominatorBasis, s.CurrencyBasis, s.CutoffBasis,
         s.ZeroDenominatorRule, s.DecisionRef, s.OwnerRole, 1);
    PRINT 'Seeded rpt.MetricDefinition (shared measures, XMD-010)';
END
GO

-- ---------------------------------------------------------------- parameters
IF OBJECT_ID(N'core.Parameter') IS NOT NULL
BEGIN
    DECLARE @p TABLE (ParamCode varchar(40), CompanyCode varchar(10),
        EffectiveFrom date, Value varchar(200), DataType varchar(12),
        UnitOfValue varchar(20), DecisionRef varchar(10),
        Description varchar(200), OwnerRole varchar(20));
    INSERT @p VALUES
    ('XMD.RECON.TOLERANCE','*','2026-04-01','0.01','MONEY','GBP','XMD-005',
     'Control account difference treated as agreed; anything larger is an owned exception.','FINCTRL'),
    ('XMD.EXCEPTION.ESCALATE.DAYS','*','2026-04-01','30','INT','days','XMD-006',
     'Age at which an open exception is escalated to the financial controller.','FINCTRL'),
    ('XMD.OBLIG.ESCALATE.DAYS','*','2026-04-01','30','INT','days','XMD-013',
     'Days before the due date at which a statutory obligation without evidence is escalated.','FINCTRL'),
    ('XMD.OTIF.EARLY.DAYS','*','2026-04-01','5','INT','days','D-19',
     'Working days a supplier delivery may be early and still count as on time.','PURMGR'),
    ('XMD.CONV.CUTOVER','CAL','2026-04-01','20260331','DATE','date','XMD-007',
     'Dated cut over of the Caledon legacy conversion load.','FINCTRL');

    MERGE core.Parameter AS t
    USING @p AS s ON t.ParamCode = s.ParamCode
                 AND t.CompanyCode = s.CompanyCode
                 AND t.EffectiveFrom = s.EffectiveFrom
    WHEN MATCHED THEN UPDATE SET Value = s.Value, DataType = s.DataType,
        UnitOfValue = s.UnitOfValue, DecisionRef = s.DecisionRef,
        Description = s.Description, OwnerRole = s.OwnerRole
    WHEN NOT MATCHED BY TARGET THEN INSERT
        (ParamCode, CompanyCode, EffectiveFrom, Value, DataType,
         UnitOfValue, DecisionRef, Description, OwnerRole)
        VALUES (s.ParamCode, s.CompanyCode, s.EffectiveFrom, s.Value,
         s.DataType, s.UnitOfValue, s.DecisionRef, s.Description, s.OwnerRole);
    PRINT 'Seeded core.Parameter (xmd control suite)';
END
GO

-- ---------------------------------------------------------------- menu
IF OBJECT_ID(N'core.MenuItem') IS NOT NULL
BEGIN
    DECLARE @mi TABLE (MenuCode varchar(40), ParentMenuCode varchar(40),
        Caption varchar(60), FormClass varchar(60), FormArgument varchar(60),
        PermissionCode varchar(40), SortOrder int);
    INSERT @mi VALUES
    ('RPT.CONTROLS','RPT','&Controls and reconciliation',NULL,NULL,NULL,10),
    ('RPT.CONTROLS.RECON','RPT.CONTROLS','Control account &reconciliation','frmReportLauncher','RPT-015','FIN.ENQUIRY.VIEW',10),
    ('RPT.CONTROLS.EXCP','RPT.CONTROLS','&Exception ageing and escalation','frmReportLauncher','RPT-016','FIN.ENQUIRY.VIEW',20),
    ('RPT.CONTROLS.OBLIG','RPT.CONTROLS','Statutory &obligations register','frmReportLauncher','XMD.OBLIGREG','FIN.ENQUIRY.VIEW',30),
    ('RPT.CONTROLS.KPI','RPT.CONTROLS','On time in full (&KPI)','frmReportLauncher','XMD.KPIOTIF','FIN.ENQUIRY.VIEW',40),
    ('RPT.CONTROLS.CHAIN','RPT.CONTROLS','Business chain &trace','frmChainTrace',NULL,'FIN.ENQUIRY.VIEW',50),
    ('RPT.CONTROLS.CHAINRPT','RPT.CONTROLS','Chain trace &report','frmReportLauncher','XMD.CHAINRPT','FIN.ENQUIRY.VIEW',55),
    ('RPT.CONTROLS.CONV','RPT.CONTROLS','Legacy conversion &load','frmReportLauncher','XMD.CONVLOAD','FIN.JOURNAL.POST',60);

    MERGE core.MenuItem AS t
    USING @mi AS s ON t.MenuCode = s.MenuCode
    WHEN MATCHED THEN UPDATE SET ParentMenuCode = s.ParentMenuCode,
        Caption = s.Caption, FormClass = s.FormClass,
        FormArgument = s.FormArgument, PermissionCode = s.PermissionCode,
        SortOrder = s.SortOrder, IsActive = 1
    WHEN NOT MATCHED BY TARGET THEN INSERT
        (MenuCode, ParentMenuCode, Caption, FormClass, FormArgument,
         PermissionCode, SortOrder, IsActive)
        VALUES (s.MenuCode, s.ParentMenuCode, s.Caption, s.FormClass,
         s.FormArgument, s.PermissionCode, s.SortOrder, 1);
    PRINT 'Seeded core.MenuItem (RPT controls)';
END
GO
