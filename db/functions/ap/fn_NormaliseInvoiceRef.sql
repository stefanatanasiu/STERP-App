SET NOCOUNT ON;
GO
CREATE OR ALTER FUNCTION ap.fn_NormaliseInvoiceRef(@Reference varchar(40))
RETURNS varchar(40)
AS
BEGIN
    -- STRUTIL DOCREF with its default KEEP policy (2000-NORMALISE-REF).
    DECLARE @Upper varchar(40) = UPPER(@Reference), @Result varchar(40) = '',
            @Index int = 1, @Character char(1);
    WHILE @Index <= LEN(@Upper)
    BEGIN
        SET @Character = SUBSTRING(@Upper, @Index, 1);
        IF @Character COLLATE Latin1_General_100_BIN2 BETWEEN 'A' AND 'Z'
            OR @Character COLLATE Latin1_General_100_BIN2 BETWEEN '0' AND '9'
            SET @Result += @Character;
        SET @Index += 1;
    END
    RETURN @Result;
END
GO
