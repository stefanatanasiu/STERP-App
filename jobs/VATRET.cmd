@echo off
rem ================================================================
rem //VATRET  JOB (STERP),'VAT RETURN',CLASS=B,MSGCLASS=X
rem //*  PREPARES THE VAT RETURN FOR ONE VAT PERIOD AND WRITES THE
rem //*  MAKING TAX DIGITAL SUBMISSION BODY TO run\mtd.  NOTHING IS
rem //*  TRANSMITTED - THE PAYLOAD IS FILED AS EVIDENCE AND THE
rem //*  RETURN IS FINALISED ONLINE BY THE FINANCIAL CONTROLLER.
rem //*  SUBMIT:  jobs\VATRET.cmd CO=STC VATPER=26A3 USER=GLCLERK
rem //*           (VATPER omitted = the oldest period still open)
rem //*  RESTART: FROM THE TOP - BOTH STEPS ARE READ ONLY IN
rem //*           MODE=PREVIEW AND THE JSON FILE IS STAMPED
rem //*  REQ: XMD-005, FIN-025, FIN-026, COM-018
rem //*  2026-10-06 STERP  ORIGINAL
rem ================================================================
setlocal
call "%~dp0SETENV.cmd" VATRET %*
if defined P_RESTART goto %P_RESTART%

rem ---------------------------------------------------------------
:STEP005
rem //*  WHICH VAT PERIOD ARE WE FILING?
if not defined P_VATPER for /f "usebackq tokens=*" %%V in (`sqlcmd -S "%STERP_SQLSERVER%" -E -C -b -h -1 -W -d "%STERP_DATABASE%" -Q "SET NOCOUNT ON; SELECT TOP 1 VatPeriodKey FROM fin.VatPeriod WHERE CompanyCode='%P_CO%' AND Status IN ('OPEN','PREPARING') ORDER BY EndDate;"`) do set "P_VATPER=%%V"
echo //*  VAT PERIOD %P_VATPER%>> "%SYSOUT%"
call "%~dp0RUNSTEP.cmd" STEP005 @SQL "SELECT VatPeriodKey, StartDate, EndDate, FilingDueDate, Status FROM fin.VatPeriod WHERE CompanyCode='%P_CO%' AND VatPeriodKey='%P_VATPER%';"
if %LASTCC% GEQ 8 goto ABEND

rem ---------------------------------------------------------------
:STEP010
rem //*  THE NINE BOX RETURN AND ITS AUDIT TRAIL (FIN-024)
call "%~dp0RUNSTEP.cmd" STEP010 VATRET "CO=%P_CO% VATPER=%P_VATPER% MODE=%P_MODE% USER=%P_USER%"
if %LASTCC% GEQ 8 goto ABEND

rem ---------------------------------------------------------------
:STEP020
rem //*  MTD SUBMISSION BODY (FIN-025, FIN-026)
call "%~dp0RUNSTEP.cmd" STEP020 MTDJSON "CO=%P_CO% VATPER=%P_VATPER% USER=%P_USER%"
if %LASTCC% GEQ 8 goto ABEND
goto EOJ

:ABEND
echo //*  STREAM TERMINATED BY COND CODE %LASTCC%>> "%SYSOUT%"
call "%~dp0JOBEND.cmd" %MAXCC%
endlocal & exit /b %MAXCC%

:EOJ
call "%~dp0JOBEND.cmd" %MAXCC%
endlocal & exit /b %MAXCC%
