/*  SQL Server Agent job: nightly exception ageing.  Re-raises the priority of open exceptions,
    escalates overdue ones to the owning role and writes a job run record.  Runs against msdb;
    re-runnable (the job is dropped and recreated).  REQ: XMD-007, XMD-008, COM-017 */
SET NOCOUNT ON;
GO
IF OBJECT_ID(N'msdb.dbo.sp_add_job') IS NULL
BEGIN
    PRINT 'SQL Server Agent is not available on this instance; job STERP Nightly Exception Ageing skipped.';
    RETURN;
END
GO
DECLARE @job nvarchar(128) = N'STERP Nightly Exception Ageing';
IF EXISTS (SELECT 1 FROM msdb.dbo.sysjobs WHERE name = @job) EXEC msdb.dbo.sp_delete_job @job_name = @job, @delete_unused_schedule = 1;
EXEC msdb.dbo.sp_add_job @job_name = @job, @enabled = 1, @description = N'Ages open STERP exceptions and escalates overdue items (XMD-007).',
     @category_name = N'[Uncategorized (Local)]', @owner_login_name = NULL;
EXEC msdb.dbo.sp_add_jobstep @job_name = @job, @step_name = N'Age exceptions', @subsystem = N'TSQL',
     @database_name = N'STERP', @retry_attempts = 2, @retry_interval = 5,
     @command = N'DECLARE @r bigint;
DECLARE @t TABLE (JobRunId bigint, JobName varchar(40), Status varchar(12), StartedAt datetime2(3));
INSERT @t EXEC core.usp_JobRunStart @JobName = ''EXCAGEING'', @CompanyCode = ''*'', @SubmittedBy = ''SYSADM'';
SELECT TOP (1) @r = JobRunId FROM @t;
BEGIN TRY
    EXEC core.usp_AgeExceptions;
    EXEC core.usp_JobRunEnd @JobRunId = @r, @Status = ''COMPLETED'', @ReturnCode = 0;
END TRY
BEGIN CATCH
    EXEC core.usp_JobRunEnd @JobRunId = @r, @Status = ''FAILED'', @ReturnCode = 16, @Message = ''Exception ageing failed'';
    THROW;
END CATCH';
EXEC msdb.dbo.sp_add_jobschedule @job_name = @job, @name = N'STERP nightly 02:10',
     @freq_type = 4, @freq_interval = 1, @active_start_time = 21000;
EXEC msdb.dbo.sp_add_jobserver @job_name = @job, @server_name = N'(LOCAL)';
PRINT 'Job created: ' + @job;
GO
