/*  core.fn_Param - effective value of a company policy parameter, company row first, then '*'.
    REQ: COM-025 */
SET NOCOUNT ON;
GO
CREATE OR ALTER FUNCTION core.fn_Param (@ParamCode varchar(40), @CompanyCode varchar(10), @AsOf date)
RETURNS varchar(200)
AS
BEGIN
    DECLARE @v varchar(200);
    SET @AsOf = ISNULL(@AsOf, CAST(SYSDATETIME() AS date));
    SELECT TOP (1) @v = p.Value FROM core.Parameter p
     WHERE p.ParamCode = @ParamCode AND p.CompanyCode IN (ISNULL(@CompanyCode, '*'), '*') AND p.EffectiveFrom <= @AsOf
     ORDER BY CASE WHEN p.CompanyCode = @CompanyCode THEN 0 ELSE 1 END, p.EffectiveFrom DESC;
    RETURN @v;
END
GO
