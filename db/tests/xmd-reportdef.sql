/*  Report catalogue tests.  The cross-module brief claims a catalogue of nineteen reports,
    RPT-001 to RPT-019, and the client menu offers them, so core.ReportDef has to be able to answer
    for every one of them.  A report id counts as registered when an active row in core.ReportDef
    either carries it as its report code or names it in RequirementRefs, because several of the
    nineteen are owned by another module and were registered there under that module's own code
    (POPIPE, CSTRP010, CSTRP020, KITRP010, APPPR, SOANAL).  Any id that is registered nowhere is
    listed by name in the output so the gap is visible rather than implied.
    REQ: RPT-001, RPT-002, RPT-003, RPT-004, RPT-005, RPT-006, RPT-007, RPT-008, RPT-009, RPT-010
    REQ: RPT-011, RPT-012, RPT-013, RPT-014, RPT-015, RPT-016, RPT-017, RPT-018, RPT-019
    REQ: XMD-009, XMD-012, COM-022
    AC: every report the catalogue claims can be found, named and run from core.ReportDef */
SET NOCOUNT ON;
GO
PRINT '# xmd-reportdef';
DECLARE @n int, @gaps int, @own varchar(400), @tag varchar(400), @gap varchar(400);
DECLARE @ids TABLE (ReportId char(7) NOT NULL PRIMARY KEY, RegisteredAs varchar(20) NULL, Via varchar(16) NULL);

INSERT @ids (ReportId)
SELECT 'RPT-' + RIGHT('00' + CAST(v.n AS varchar(3)), 3)
  FROM (VALUES (1),(2),(3),(4),(5),(6),(7),(8),(9),(10),(11),(12),(13),(14),(15),(16),(17),(18),(19)) AS v (n);

UPDATE i
   SET RegisteredAs = x.ReportCode, Via = x.Via
  FROM @ids i
 CROSS APPLY (SELECT TOP (1) r.ReportCode,
                     CASE WHEN r.ReportCode = i.ReportId THEN 'ReportCode' ELSE 'RequirementRefs' END AS Via
                FROM core.ReportDef r
               WHERE r.IsActive = 1
                 AND (r.ReportCode = i.ReportId OR ISNULL(r.RequirementRefs, '') LIKE '%' + i.ReportId + '%')
               ORDER BY CASE WHEN r.ReportCode = i.ReportId THEN 0 ELSE 1 END, r.ReportCode) AS x;

SELECT @own = STRING_AGG(ReportId, ' ') WITHIN GROUP (ORDER BY ReportId) FROM @ids WHERE Via = 'ReportCode';
SELECT @tag = STRING_AGG(ReportId + '=' + RegisteredAs, ' ') WITHIN GROUP (ORDER BY ReportId) FROM @ids WHERE Via = 'RequirementRefs';
SELECT @gap = STRING_AGG(ReportId, ' ') WITHIN GROUP (ORDER BY ReportId) FROM @ids WHERE RegisteredAs IS NULL;
SELECT @gaps = COUNT(*) FROM @ids WHERE RegisteredAs IS NULL;
PRINT '# registered under their own report code: ' + ISNULL(@own, '(none)');
PRINT '# registered by the owning module and tagged: ' + ISNULL(@tag, '(none)');
PRINT '# not registered anywhere in core.ReportDef: ' + ISNULL(@gap, '(none)');

/*  1 - the catalogue is complete: every claimed id can be found. */
SELECT @n = COUNT(*) FROM @ids WHERE RegisteredAs IS NOT NULL;
IF @gaps = 0 AND @n = 19
    PRINT 'ok 1 - all nineteen catalogue reports RPT-001 to RPT-019 are registered in core.ReportDef -- REQ: XMD-009, COM-022';
ELSE PRINT 'not ok 1 - ' + CAST(@gaps AS varchar(4)) + ' catalogue report id(s) are registered nowhere: ' + ISNULL(@gap, '(none)');

/*  2 - a catalogue row describes itself, so a reader never has to guess which requirement it answers. */
SELECT @n = COUNT(*) FROM core.ReportDef r
 WHERE r.ReportCode LIKE 'RPT-[0-9][0-9][0-9]' AND ISNULL(r.RequirementRefs, '') NOT LIKE '%' + r.ReportCode + '%';
IF @n = 0 PRINT 'ok 2 - every RPT-nnn definition names its own requirement id in RequirementRefs -- REQ: XMD-012, COM-022';
ELSE PRINT 'not ok 2 - ' + CAST(@n AS varchar(4)) + ' RPT-nnn definitions do not name themselves';

/*  3 - a registered report can actually be run: it names a title and a batch program. */
SELECT @n = COUNT(*) FROM @ids i JOIN core.ReportDef r ON r.ReportCode = i.RegisteredAs
 WHERE ISNULL(r.ProgramName, '') = '' OR ISNULL(r.Title, '') = '' OR ISNULL(r.Module, '') = '';
IF @n = 0 PRINT 'ok 3 - every catalogue report names a module, a title and the batch program that produces it -- REQ: XMD-009';
ELSE PRINT 'not ok 3 - ' + CAST(@n AS varchar(4)) + ' catalogue reports have no module, title or program';

/*  4 - a report that is permission controlled points at a permission that exists. */
SELECT @n = COUNT(*) FROM core.ReportDef r
  LEFT JOIN core.Permission p ON p.PermissionCode = r.PermissionCode
 WHERE ISNULL(r.PermissionCode, '') <> '' AND p.PermissionCode IS NULL;
IF @n = 0 PRINT 'ok 4 - every permission a report names exists in core.Permission -- REQ: COM-002, COM-022';
ELSE PRINT 'not ok 4 - ' + CAST(@n AS varchar(4)) + ' reports name a permission that does not exist';

/*  5 - the two control reports this module owns are the ones the rpt views mirror. */
SELECT @n = COUNT(*) FROM core.ReportDef r
 WHERE (r.ReportCode = 'RPT-015' AND r.ProgramName = 'RECONCTL' AND r.IsActive = 1)
    OR (r.ReportCode = 'RPT-016' AND r.ProgramName = 'EXCPAGE'  AND r.IsActive = 1);
IF @n = 2 PRINT 'ok 5 - RPT-015 runs RECONCTL and RPT-016 runs EXCPAGE, the programs the rpt views mirror -- REQ: RPT-015, RPT-016, XMD-005, XMD-006';
ELSE PRINT 'not ok 5 - only ' + CAST(@n AS varchar(4)) + ' of the two control reports name their batch program';

/*  6 - the cross-module reports this module adds on top of the numbered catalogue are registered. */
SELECT @n = COUNT(*) FROM core.ReportDef r
 WHERE r.IsActive = 1 AND r.ReportCode IN ('XMD.OBLIGREG', 'XMD.KPIOTIF', 'XMD.CHAINRPT', 'XMD.CONVLOAD');
IF @n = 4 PRINT 'ok 6 - the obligations register, OTIF, chain trace and conversion reports are registered -- REQ: XMD-001, XMD-010, XMD-013';
ELSE PRINT 'not ok 6 - only ' + CAST(@n AS varchar(4)) + ' of the four xmd reports are registered and active';

/*  7 - a parameter specification is published for the reports that take parameters, so the client
    can build the run dialogue without a hard coded list. */
SELECT @n = COUNT(*) FROM @ids i JOIN core.ReportDef r ON r.ReportCode = i.RegisteredAs
 WHERE ISNULL(r.ParamSpec, '') NOT LIKE '%CO%';
IF @n = 0 PRINT 'ok 7 - every catalogue report takes the company parameter its run dialogue needs -- REQ: XMD-009, COM-001';
ELSE PRINT 'not ok 7 - ' + CAST(@n AS varchar(4)) + ' catalogue reports publish no company parameter';
GO
