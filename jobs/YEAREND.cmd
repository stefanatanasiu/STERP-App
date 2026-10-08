@echo off
rem ================================================================
rem //YEAREND JOB (STERP),'YEAR END CLOSE',CLASS=D,MSGCLASS=X
rem //*  THE ANNUAL CLOSE.  PRINTS THE FINAL STATUTORY SET, CLOSES
rem //*  THE PROFIT AND LOSS ACCOUNTS TO RETAINED EARNINGS AND
rem //*  OPENS THE NEW YEAR.  RUN IT ONLY AFTER THE LAST MONTHEND
rem //*  OF THE YEAR HAS COMPLETED WITH COND CODE 4 OR LESS AND THE
rem //*  AUDITOR HAS SIGNED THE TRIAL BALANCE OFF.
rem //*  SUBMIT:  jobs\YEAREND.cmd CO=STC YEAR=2026 MODE=PREVIEW
rem //*           MODE=COMMIT performs the close (FIN-027)
rem //*  RESTART: RESTART=STEP040 after a reporting failure; the
rem //*           close itself refuses to run twice for one year
rem //*  REQ: XMD-005, FIN-027, RPT-001, COM-009, COM-018
rem //*  2026-10-06 STERP  ORIGINAL
rem ================================================================
setlocal
call "%~dp0SETENV.cmd" YEAREND %*
if defined P_RESTART goto %P_RESTART%

rem ---------------------------------------------------------------
:STEP005
rem //*  EVERY PERIOD OF THE YEAR MUST BE SOFT OR HARD CLOSED
call "%~dp0RUNSTEP.cmd" STEP005 @SQL "SELECT Status, COUNT(*) AS Periods FROM fin.Period WHERE CompanyCode='%P_CO%' AND FiscalYear=%P_YEAR% GROUP BY Status;"
if %LASTCC% GEQ 8 goto ABEND

rem ---------------------------------------------------------------
:STEP010
rem //*  FINAL TRIAL BALANCE, ALL TWELVE PERIODS (FIN-021, RPT-001)
call "%~dp0RUNSTEP.cmd" STEP010 TBRPT "CO=%P_CO% YEAR=%P_YEAR% PERFROM=1 PERTO=12 ZEROS=Y USER=%P_USER%"
if %LASTCC% GEQ 8 goto ABEND

rem ---------------------------------------------------------------
:STEP020
rem //*  FINAL PROFIT AND LOSS (FIN-022)
call "%~dp0RUNSTEP.cmd" STEP020 PLRPT "CO=%P_CO% YEAR=%P_YEAR% PERFROM=1 PERTO=12 COMPARE=PRIOR USER=%P_USER%"
if %LASTCC% GEQ 8 goto ABEND

rem ---------------------------------------------------------------
:STEP030
rem //*  FINAL BALANCE SHEET (FIN-023)
call "%~dp0RUNSTEP.cmd" STEP030 BSRPT "CO=%P_CO% YEAR=%P_YEAR% PERIOD=12 COMPARE=PRIOR USER=%P_USER%"
if %LASTCC% GEQ 8 goto ABEND

rem ---------------------------------------------------------------
:STEP040
rem //*  CLOSE THE YEAR AND CARRY THE RESULT FORWARD (FIN-027)
call "%~dp0RUNSTEP.cmd" STEP040 YEAREND "CO=%P_CO% YEAR=%P_YEAR% MODE=%P_MODE% USER=%P_USER%"
if %LASTCC% GEQ 8 goto ABEND

rem ---------------------------------------------------------------
:STEP050
rem //*  PROVE THE NEW YEAR IS OPEN FOR POSTING
call "%~dp0RUNSTEP.cmd" STEP050 @SQL "SELECT FiscalYear, COUNT(*) AS Periods, SUM(CASE WHEN Status='OPEN' THEN 1 ELSE 0 END) AS OpenPeriods FROM fin.Period WHERE CompanyCode='%P_CO%' AND FiscalYear>=%P_YEAR% GROUP BY FiscalYear;"
if %LASTCC% GEQ 8 goto ABEND
goto EOJ

:ABEND
echo //*  STREAM TERMINATED BY COND CODE %LASTCC%>> "%SYSOUT%"
echo //*  THE YEAR IS NOT CLOSED - CORRECT AND RESTART THE STEP>> "%SYSOUT%"
call "%~dp0JOBEND.cmd" %MAXCC%
endlocal & exit /b 8

:EOJ
call "%~dp0JOBEND.cmd" %MAXCC%
endlocal & exit /b 0
