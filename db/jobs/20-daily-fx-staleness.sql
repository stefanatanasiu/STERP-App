/*  SQL Server Agent job: daily exchange rate staleness check.  Raises an exception when no
    spot rate has been published for a live currency within the permitted age (FX.MAX.AGE.DAYS),
    so that no posting silently uses an out of date rate.  REQ: FIN-019, COM-010, D-14 */
SET NOCOUNT ON;
GO
IF OBJECT_ID(N'msdb.dbo.sp_add_job') IS NULL
BEGIN
    PRINT 'SQL Server Agent is not available on this instance; job STERP Daily FX Staleness Check skipped.';
    RETURN;
END
GO
DECLARE @job nvarchar(128) = N'STERP Daily FX Staleness Check';
IF EXISTS (SELECT 1 FROM msdb.dbo.sysjobs WHERE name = @job) EXEC msdb.dbo.sp_delete_job @job_name = @job, @delete_unused_schedule = 1;
EXEC msdb.dbo.sp_add_job @job_name = @job, @enabled = 1, @description = N'Checks that a spot rate exists for each live currency (D-14).',
     @category_name = N'[Uncategorized (Local)]', @owner_login_name = NULL;
EXEC msdb.dbo.sp_add_jobstep @job_name = @job, @step_name = N'Check exchange rate staleness', @subsystem = N'TSQL',
     @database_name = N'STERP', @retry_attempts = 1, @retry_interval = 5,
     @command = N'DECLARE @r bigint;
DECLARE @t TABLE (JobRunId bigint, JobName varchar(40), Status varchar(12), StartedAt datetime2(3));
INSERT @t EXEC core.usp_JobRunStart @JobName = ''FXSTALE'', @CompanyCode = ''*'', @SubmittedBy = ''SYSADM'';
SELECT TOP (1) @r = JobRunId FROM @t;
BEGIN TRY
    DECLARE @age int = TRY_CAST(core.fn_Param(''FX.MAX.AGE.DAYS'', ''*'', CAST(SYSDATETIME() AS date)) AS int);
    EXEC core.usp_CheckFxStaleness @MaxAgeDays = @age, @RateType = ''SPOT'';
    EXEC core.usp_JobRunEnd @JobRunId = @r, @Status = ''COMPLETED'', @ReturnCode = 0;
END TRY
BEGIN CATCH
    EXEC core.usp_JobRunEnd @JobRunId = @r, @Status = ''FAILED'', @ReturnCode = 16, @Message = ''Exchange rate staleness check failed'';
    THROW;
END CATCH';
EXEC msdb.dbo.sp_add_jobschedule @job_name = @job, @name = N'STERP daily 07:15',
     @freq_type = 4, @freq_interval = 1, @active_start_time = 71500;
EXEC msdb.dbo.sp_add_jobserver @job_name = @job, @server_name = N'(LOCAL)';
PRINT 'Job created: ' + @job;
GO
