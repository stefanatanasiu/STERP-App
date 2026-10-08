@echo off
rem ================================================================
rem //STERPSET PROC - RUN TIME SETTINGS FOR EVERY STERP JOB STREAM
rem   This is the batch equivalent of build\env.ps1: it sets the
rem   STERP run time environment, parses the SYSIN style KEY=VALUE
rem   parameters, opens the SYSOUT log and writes the //JOB card.
rem   CALLED AS:  call "%~dp0SETENV.cmd" <JOBNAME> [KEY=VALUE ...]
rem   REQ: XMD-005, COM-018
rem   2026-10-06 STERP  original
rem ================================================================
set "JOBNAME=%~1"
set "JOBDIR=%~dp0"
shift

if not defined STERP_HOME set "STERP_HOME=%JOBDIR%.."
for %%I in ("%STERP_HOME%") do set "STERP_HOME=%%~fI"
set "STERP_RUN=%STERP_HOME%\run"
if not defined STERP_SQLSERVER set "STERP_SQLSERVER=."
if not defined STERP_DATABASE set "STERP_DATABASE=STERP"
set "STERP_ODBC=Driver={ODBC Driver 18 for SQL Server};Server=%STERP_SQLSERVER%;Database=%STERP_DATABASE%;Trusted_Connection=yes;TrustServerCertificate=yes;MARS_Connection=yes;APP=STERP-BATCH"
set "PATH=%STERP_HOME%\bin\x86;%PATH%"
rem Unattended batch: never wait for a keypress at STOP RUN after screen I/O
set "COB_EXIT_WAIT=0"
for %%D in (logs spool bacs mtd einvoice extracts work) do (
  if not exist "%STERP_RUN%\%%D" mkdir "%STERP_RUN%\%%D" >nul 2>&1
)

rem ---- //SYSIN DD *   defaults, overridden by KEY=VALUE arguments
set "P_CO=STC"
set "P_USER=OPS"
set "P_MODE=PREVIEW"
set "P_SITE="
set "P_ASAT="
set "P_PERIOD="
set "P_YEAR="
set "P_RUN="
set "P_STAGE="
set "P_VATPER="
set "P_DAYS=0"
set "P_RESTART="
set "P_SUN="
set "P_STYLE="
rem ---- cmd splits an unquoted KEY=VALUE argument into TWO tokens,
rem ---- so a pair arrives either as one token (quoted) or as two
:PARMLOOP
if "%~1"=="" goto PARMEND
set "PTOK=%~1"
shift
set "PVAL=%~1"
set "PEMB="
for /f "tokens=1,2 delims==" %%K in ("%PTOK%") do set "PEMB=%%L"
if defined PEMB (
  for /f "tokens=1,* delims==" %%K in ("%PTOK%") do set "P_%%K=%%L"
) else (
  for /f %%K in ("%PTOK%") do set "P_%%K=%PVAL%"
  shift
)
goto PARMLOOP
:PARMEND

for /f %%T in ('powershell -NoProfile -Command "Get-Date -Format yyyyMMddHHmmss"') do set "JOBSTAMP=%%T"
if not defined P_ASAT set "P_ASAT=%JOBSTAMP:~0,8%"
set /a JM=1%JOBSTAMP:~4,2% - 100
set /a JY=%JOBSTAMP:~0,4%
if %JM% GEQ 4 (set /a JP=%JM%-3) else (set /a JP=%JM%+9)
if %JM% LSS 4 set /a JY=%JY%-1
if not defined P_PERIOD set "P_PERIOD=%JP%"
if not defined P_YEAR set "P_YEAR=%JY%"
rem ---- CONVENTIONS 3.6 also uses the packed YYYYPP period form
set "P_PP=0%P_PERIOD%"
set "P_PP=%P_PP:~-2%"
set "P_YYYYPP=%P_YEAR%%P_PP%"

rem ---- P_PEREND is the last day of the accounting period being run.
rem ---- A period end stream reconciles AT THE PERIOD END, not at the
rem ---- clock date, so every step of MONTHEND reads the same cutoff
rem ---- as ARRECON PERIOD=%P_YYYYPP%.  Falls back to the as-at date
rem ---- when the calendar has no such period.  REQ: COM-009, XMD-005
set "P_PEREND="
for /f "usebackq tokens=*" %%I in (`sqlcmd -S "%STERP_SQLSERVER%" -E -C -b -h -1 -W -d "%STERP_DATABASE%" -Q "SET NOCOUNT ON; SELECT CONVERT(char(8), EndDate, 112) FROM fin.Period WHERE CompanyCode='%P_CO%' AND FiscalYear=%P_YEAR% AND PeriodNo=%P_PERIOD%;" 2^>nul`) do set "P_PEREND=%%I"
echo %P_PEREND%| findstr /r "^[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]$" >nul || set "P_PEREND=%P_ASAT%"

set "SYSOUT=%STERP_RUN%\logs\%JOBNAME%-%JOBSTAMP%.log"
set "LASTCC=0"
set "MAXCC=0"
set "JOBRUNID="

echo //%JOBNAME% JOB (STERP),'%JOBNAME% SERVICE',CLASS=A,MSGCLASS=X> "%SYSOUT%"
echo //*  SUBMITTED %DATE% %TIME% BY %P_USER% ON %COMPUTERNAME%>> "%SYSOUT%"
echo //*  COMPANY=%P_CO% SITE=%P_SITE% MODE=%P_MODE% ASAT=%P_ASAT%>> "%SYSOUT%"
echo //*  FISCAL YEAR=%P_YEAR% PERIOD=%P_PERIOD% RUN=%P_RUN%>> "%SYSOUT%"
echo //*  STERP_HOME=%STERP_HOME%>> "%SYSOUT%"
echo //*  SYSOUT=%SYSOUT%>> "%SYSOUT%"
echo //JOBLIB  DD DSN=STERP.BIN.X86,DISP=SHR>> "%SYSOUT%"
echo.>> "%SYSOUT%"
echo STERP-I-0001 %JOBNAME% started - SYSOUT %SYSOUT%

rem ---- register the stream itself in core.JobRun (client job monitor)
for /f "usebackq tokens=*" %%I in (`sqlcmd -S "%STERP_SQLSERVER%" -E -C -b -h -1 -W -d "%STERP_DATABASE%" -Q "SET NOCOUNT ON; DECLARE @r TABLE(Id bigint, Nm varchar(40), St varchar(12), Sa datetime2(3)); INSERT @r EXEC core.usp_JobRunStart @JobName='%JOBNAME%', @CompanyCode='%P_CO%', @SubmittedBy='%P_USER%', @Arguments='MODE=%P_MODE% ASAT=%P_ASAT% YEAR=%P_YEAR% PERIOD=%P_PERIOD%'; SELECT CAST(Id AS varchar(20)) FROM @r;" 2^>nul`) do set "JOBRUNID=%%I"
if defined JOBRUNID echo //*  core.JobRun id %JOBRUNID%>> "%SYSOUT%"
exit /b 0
