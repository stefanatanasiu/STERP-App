SET NOCOUNT ON;
PRINT '# bom-header-freeze';
GO
DECLARE @n int = 0, @field varchar(40), @error nvarchar(4000), @sql nvarchar(500);
DECLARE fields CURSOR LOCAL FAST_FORWARD FOR
SELECT Field FROM (VALUES ('Description'), ('YieldFactor'), ('EffectiveFrom')) v(Field);
OPEN fields;
FETCH NEXT FROM fields INTO @field;
WHILE @@FETCH_STATUS = 0
BEGIN
    SET @n += 1;
    SET @error = '';
    BEGIN TRY
        BEGIN TRANSACTION;
        INSERT bom.BomHeader (CompanyCode, BomRef, BomRevision, ParentItemCode, Description,
            EffectiveFrom, Status, AuthorUser)
        VALUES ('STC', 'ZZBOM-GUI-FREEZE', 'A', 'FG-BIKE-COMMUTE-M', 'QA released header',
            '20260101', 'RELEASED', 'ENGINEER');
        SET @sql = N'UPDATE bom.BomHeader SET ' + QUOTENAME(@field) + N' = ' +
            CASE @field WHEN 'Description' THEN '''changed''' WHEN 'YieldFactor' THEN '0.9' ELSE '''20260102''' END +
            N' WHERE BomRef = ''ZZBOM-GUI-FREEZE'';';
        EXEC sys.sp_executesql @sql;
    END TRY
    BEGIN CATCH
        SET @error = ERROR_MESSAGE();
    END CATCH;
    IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION;
    IF @error LIKE '%BOM0413%'
        PRINT 'ok ' + CAST(@n AS varchar(10)) + ' - released header refuses ' + @field + ' changes';
    ELSE PRINT 'not ok ' + CAST(@n AS varchar(10)) + ' - released header ' + @field + ': ' + @error;
    FETCH NEXT FROM fields INTO @field;
END
CLOSE fields;
DEALLOCATE fields;
GO
BEGIN TRANSACTION;
INSERT bom.BomHeader (CompanyCode, BomRef, BomRevision, ParentItemCode, Description,
    EffectiveFrom, Status, AuthorUser)
VALUES ('STC', 'ZZBOM-GUI-FREEZE', 'A', 'FG-BIKE-COMMUTE-M', 'QA released header',
    '20260101', 'RELEASED', 'ENGINEER'),
       ('STC', 'ZZBOM-GUI-FREEZE', 'B', 'FG-BIKE-COMMUTE-M', 'QA next revision',
    '20260101', 'DRAFT', 'ENGINEER');
DECLARE @next int = (SELECT BomHeaderId FROM bom.BomHeader WHERE BomRef = 'ZZBOM-GUI-FREEZE' AND BomRevision = 'B');
UPDATE bom.BomHeader SET ModifiedBy = 'ENGMGR', ModifiedAt = SYSDATETIME()
WHERE BomRef = 'ZZBOM-GUI-FREEZE' AND BomRevision = 'A';
PRINT 'ok 4 - released header allows audit stamps without content changes';
UPDATE bom.BomHeader SET Status = 'SUPERSEDED', SupersededByBomHeaderId = @next, EffectiveTo = '20261012'
WHERE BomRef = 'ZZBOM-GUI-FREEZE' AND BomRevision = 'A';
PRINT 'ok 5 - lifecycle supersession still closes the old revision effectivity';
UPDATE bom.BomHeader SET Description = 'QA editable draft'
WHERE BomRef = 'ZZBOM-GUI-FREEZE' AND BomRevision = 'B';
PRINT 'ok 6 - draft headers remain editable';
ROLLBACK TRANSACTION;
GO
