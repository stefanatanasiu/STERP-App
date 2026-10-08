/*  core.usp_IdempotentBegin / core.usp_IdempotentEnd / core.usp_JobRunStart / core.usp_JobRunEnd.
    Repeating a confirmed instruction must not create a second effect; an uncertain outcome is
    reported as uncertain, never as a safe-to-repeat failure.  REQ: COM-014, COM-015, 3.6 */
SET NOCOUNT ON;
GO
CREATE OR ALTER PROCEDURE core.usp_IdempotentBegin @CorrelationId varchar(64), @Operation varchar(60), @CompanyCode varchar(10) = NULL, @UserCode varchar(20) = NULL
AS
BEGIN
    SET NOCOUNT ON; SET XACT_ABORT ON;
    IF @CorrelationId IS NULL OR LEN(LTRIM(@CorrelationId)) = 0 THROW 50016, 'STERP-E-0016: A correlation id is required so that a repeated instruction can be recognised.', 1;
    DECLARE @st varchar(12), @ref varchar(60), @msg varchar(400), @outcome varchar(12);
    SELECT @st = Status, @ref = ResultRef, @msg = ResultMessage FROM core.IdempotencyKey WITH (UPDLOCK, HOLDLOCK) WHERE CorrelationId = @CorrelationId;
    IF @st IS NULL
    BEGIN
        INSERT core.IdempotencyKey (CorrelationId, Operation, CompanyCode, UserCode, Status) VALUES (@CorrelationId, @Operation, @CompanyCode, @UserCode, 'INPROGRESS');
        SET @outcome = 'NEW';
    END
    ELSE IF @st = 'COMPLETED' SET @outcome = 'DUPLICATE';
    ELSE IF @st = 'FAILED'
    BEGIN
        UPDATE core.IdempotencyKey SET Status = 'INPROGRESS', StartedAt = SYSDATETIME(), EndedAt = NULL, ResultRef = NULL, ResultMessage = NULL WHERE CorrelationId = @CorrelationId;
        SET @outcome = 'NEW';
    END
    ELSE SET @outcome = 'INPROGRESS';
    SELECT @CorrelationId AS CorrelationId, @Operation AS Operation, @outcome AS Outcome, @ref AS ResultRef, @msg AS ResultMessage;
END
GO
CREATE OR ALTER PROCEDURE core.usp_IdempotentEnd @CorrelationId varchar(64), @Status varchar(12), @ResultRef varchar(60) = NULL, @ResultMessage varchar(400) = NULL
AS
BEGIN
    SET NOCOUNT ON; SET XACT_ABORT ON;
    IF @Status NOT IN ('COMPLETED', 'FAILED') THROW 50016, 'STERP-E-0016: An instruction must end as COMPLETED or FAILED.', 1;
    UPDATE core.IdempotencyKey SET Status = @Status, ResultRef = @ResultRef, ResultMessage = @ResultMessage, EndedAt = SYSDATETIME() WHERE CorrelationId = @CorrelationId;
    IF @@ROWCOUNT = 0 THROW 50016, 'STERP-E-0016: That correlation id was never started.', 1;
    SELECT CorrelationId, Operation, Status, ResultRef, ResultMessage, StartedAt, EndedAt FROM core.IdempotencyKey WHERE CorrelationId = @CorrelationId;
END
GO
CREATE OR ALTER PROCEDURE core.usp_JobRunStart @JobName varchar(40), @CompanyCode varchar(10) = NULL, @SubmittedBy varchar(20) = NULL, @SiteCode varchar(10) = NULL, @Arguments varchar(300) = NULL
AS
BEGIN
    SET NOCOUNT ON; SET XACT_ABORT ON;
    INSERT core.JobRun (JobName, CompanyCode, SiteCode, Arguments, SubmittedBy, Status) VALUES (@JobName, @CompanyCode, @SiteCode, @Arguments, @SubmittedBy, 'RUNNING');
    SELECT CAST(SCOPE_IDENTITY() AS bigint) AS JobRunId, @JobName AS JobName, 'RUNNING' AS Status, SYSDATETIME() AS StartedAt;
END
GO
CREATE OR ALTER PROCEDURE core.usp_JobRunEnd @JobRunId bigint, @Status varchar(12), @ReturnCode int = 0, @RowsProcessed int = NULL, @LogFile varchar(260) = NULL,
    @SpoolFile varchar(260) = NULL, @Message varchar(400) = NULL
AS
BEGIN
    SET NOCOUNT ON; SET XACT_ABORT ON;
    UPDATE core.JobRun SET Status = @Status, ReturnCode = @ReturnCode, RowsProcessed = @RowsProcessed, LogFile = @LogFile, SpoolFile = @SpoolFile, Message = @Message, EndedAt = SYSDATETIME()
     WHERE JobRunId = @JobRunId;
    IF @@ROWCOUNT = 0 THROW 50016, 'STERP-E-0016: That job run does not exist.', 1;
    SELECT JobRunId, JobName, CompanyCode, Status, ReturnCode, RowsProcessed, StartedAt, EndedAt, SpoolFile, Message FROM core.JobRun WHERE JobRunId = @JobRunId;
END
GO
