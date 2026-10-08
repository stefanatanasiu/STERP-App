/*  core.usp_GetParameter - effective policy value with its decision reference.  A missing
    parameter is an error: a zero or absent policy value is never assumed.  REQ: COM-025, D-07..D-30 */
SET NOCOUNT ON;
GO
CREATE OR ALTER PROCEDURE core.usp_GetParameter @ParamCode varchar(40), @CompanyCode varchar(10) = '*', @AsOf date = NULL
AS
BEGIN
    SET NOCOUNT ON; SET XACT_ABORT ON;
    SET @AsOf = ISNULL(@AsOf, CAST(SYSDATETIME() AS date));
    IF core.fn_Param(@ParamCode, @CompanyCode, @AsOf) IS NULL
        THROW 50003, 'STERP-E-0003: No effective value for parameter requested. Maintain it in reference data before using this function.', 1;
    SELECT TOP (1) p.ParamCode, p.CompanyCode, p.Value, p.DataType, p.UnitOfValue, p.EffectiveFrom, p.DecisionRef, p.Description, p.OwnerRole
      FROM core.Parameter p
     WHERE p.ParamCode = @ParamCode AND p.CompanyCode IN (ISNULL(@CompanyCode, '*'), '*') AND p.EffectiveFrom <= @AsOf
     ORDER BY CASE WHEN p.CompanyCode = @CompanyCode THEN 0 ELSE 1 END, p.EffectiveFrom DESC;
END
GO
