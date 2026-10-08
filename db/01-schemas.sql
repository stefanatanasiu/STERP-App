/*  STERP - Stefan's ERP - 01-schemas.sql
    Creates every STERP schema.  Re-runnable.
    core = shared control, fin = nominal ledger, ar/ap = sales & purchase ledgers,
    so/po = order management, inv = items & stock (module itm), cst = costing,
    bom = bills of material, kit = kits, rpt = reporting views, stg = import staging. */
SET NOCOUNT ON;
DECLARE @s sysname, @c nvarchar(200);
DECLARE cs CURSOR LOCAL FAST_FORWARD FOR
    SELECT v FROM (VALUES ('core'),('fin'),('ar'),('ap'),('so'),('po'),('inv'),('cst'),('bom'),('kit'),('rpt'),('stg')) AS t(v);
OPEN cs; FETCH NEXT FROM cs INTO @s;
WHILE @@FETCH_STATUS = 0
BEGIN
    IF SCHEMA_ID(@s) IS NULL BEGIN SET @c = N'CREATE SCHEMA ' + QUOTENAME(@s); EXEC (@c); END
    FETCH NEXT FROM cs INTO @s;
END
CLOSE cs; DEALLOCATE cs;
GO
