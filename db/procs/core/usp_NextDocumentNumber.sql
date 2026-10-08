/*  core.usp_NextDocumentNumber / core.usp_VoidDocumentNumber - one gap-free continuous sequence
    per company and document type.  Two simultaneous confirmations cannot take the same number,
    and a voided number stays traceable and is never reissued.  REQ: COM-005, D-15 */
SET NOCOUNT ON;
GO
CREATE OR ALTER PROCEDURE core.usp_NextDocumentNumber @CompanyCode varchar(10), @SeriesCode varchar(20), @UserCode varchar(20) = NULL, @CorrelationId varchar(64) = NULL
AS
BEGIN
    SET NOCOUNT ON; SET XACT_ABORT ON;
    DECLARE @seq bigint, @prefix varchar(6), @len tinyint, @max bigint, @num varchar(30), @own bit = 0;
    IF @@TRANCOUNT = 0 BEGIN BEGIN TRANSACTION; SET @own = 1; END
    SELECT @seq = NextNumber, @prefix = Prefix, @len = NumberLength, @max = MaxNumber
      FROM core.NumberSeries WITH (UPDLOCK, HOLDLOCK)
     WHERE CompanyCode = @CompanyCode AND SeriesCode = @SeriesCode AND IsActive = 1;
    IF @seq IS NULL
    BEGIN
        IF @own = 1 ROLLBACK TRANSACTION;
        THROW 50004, 'STERP-E-0004: No active number series for this company and document type.', 1;
    END
    IF @seq > @max
    BEGIN
        IF @own = 1 ROLLBACK TRANSACTION;
        THROW 50004, 'STERP-E-0004: The number series is exhausted. Extend its upper limit before confirming more documents.', 1;
    END
    SET @num = @prefix + RIGHT(REPLICATE('0', @len) + CAST(@seq AS varchar(20)), @len);
    UPDATE core.NumberSeries SET NextNumber = @seq + 1, ModifiedBy = @UserCode, ModifiedAt = SYSDATETIME()
     WHERE CompanyCode = @CompanyCode AND SeriesCode = @SeriesCode;
    INSERT core.DocumentNumber (CompanyCode, SeriesCode, DocumentNumber, SequenceNo, Status, IssuedBy, CorrelationId)
    VALUES (@CompanyCode, @SeriesCode, @num, @seq, 'ISSUED', @UserCode, @CorrelationId);
    IF @own = 1 COMMIT TRANSACTION;
    SELECT @CompanyCode AS CompanyCode, @SeriesCode AS SeriesCode, @num AS DocumentNumber, @seq AS SequenceNo;
END
GO
CREATE OR ALTER PROCEDURE core.usp_VoidDocumentNumber @CompanyCode varchar(10), @SeriesCode varchar(20), @DocumentNumber varchar(30), @UserCode varchar(20), @Reason varchar(200)
AS
BEGIN
    SET NOCOUNT ON; SET XACT_ABORT ON;
    IF @Reason IS NULL OR LEN(LTRIM(@Reason)) = 0 THROW 50005, 'STERP-E-0005: A reason is required before a confirmed document reference can be voided.', 1;
    IF NOT EXISTS (SELECT 1 FROM core.DocumentNumber WHERE CompanyCode = @CompanyCode AND SeriesCode = @SeriesCode AND DocumentNumber = @DocumentNumber)
        THROW 50005, 'STERP-E-0005: That document reference was never issued in this series.', 1;
    IF EXISTS (SELECT 1 FROM core.DocumentNumber WHERE CompanyCode = @CompanyCode AND SeriesCode = @SeriesCode AND DocumentNumber = @DocumentNumber AND Status = 'VOID')
        THROW 50005, 'STERP-E-0005: That document reference is already void. Voided references are never reissued.', 1;
    UPDATE core.DocumentNumber SET Status = 'VOID', VoidedAt = SYSDATETIME(), VoidedBy = @UserCode, VoidReason = @Reason
     WHERE CompanyCode = @CompanyCode AND SeriesCode = @SeriesCode AND DocumentNumber = @DocumentNumber;
    INSERT core.AuditLog (UserCode, CompanyCode, EntityType, EntityKey, Action, FieldName, OldValue, NewValue, Reason, Source)
    VALUES (@UserCode, @CompanyCode, 'core.DocumentNumber', @SeriesCode + '/' + @DocumentNumber, 'VOID', 'Status', 'ISSUED', 'VOID', @Reason, 'SQL');
    SELECT @CompanyCode AS CompanyCode, @SeriesCode AS SeriesCode, @DocumentNumber AS DocumentNumber, 'VOID' AS Status, @Reason AS VoidReason;
END
GO
