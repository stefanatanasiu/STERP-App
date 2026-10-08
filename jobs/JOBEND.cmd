@echo off
rem ================================================================
rem //JOBEND  PROC - CLOSE THE STREAM DOWN
rem   CALLED AS:  call "%~dp0JOBEND.cmd" <COND CODE>
rem   Writes the //JOB END card, closes core.JobRun and returns the
rem   highest step COND code to the submitter (operator console,
rem   Windows scheduler or another stream).
rem   0/4 = the night shift carries on, 8 and above = call the duty
rem   officer, see docs\OPERATOR-GUIDE.md.
rem   REQ: XMD-005, COM-018
rem   2026-10-06 STERP  original
rem ================================================================
set "ENDCC=%~1"
if not defined ENDCC set "ENDCC=0"
set "JOBSTAT=OK"
if %ENDCC% GEQ 4 set "JOBSTAT=WARNING"
if %ENDCC% GEQ 8 set "JOBSTAT=FAILED"
if %ENDCC% GEQ 12 set "JOBSTAT=ABENDED"

echo.>> "%SYSOUT%"
echo //*  JOB %JOBNAME% ENDED - MAXIMUM COND CODE %ENDCC% (%JOBSTAT%)>> "%SYSOUT%"
echo //*  %DATE% %TIME%>> "%SYSOUT%"
echo //>> "%SYSOUT%"
echo STERP-I-0009 %JOBNAME% ended - MAXCC %ENDCC% (%JOBSTAT%) - %SYSOUT%

if defined JOBRUNID sqlcmd -S "%STERP_SQLSERVER%" -E -C -b -I -d "%STERP_DATABASE%" -W -h -1 -Q "SET NOCOUNT ON; EXEC core.usp_JobRunEnd @JobRunId=%JOBRUNID%, @Status='%JOBSTAT%', @ReturnCode=%ENDCC%, @LogFile='%SYSOUT%', @Message='Job stream %JOBNAME% ended with COND code %ENDCC%';" >nul 2>&1
exit /b %ENDCC%
