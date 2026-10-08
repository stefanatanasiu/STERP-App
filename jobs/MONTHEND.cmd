@echo off
rem ================================================================
rem //MONTHEND JOB (STERP),'PERIOD END',CLASS=B,MSGCLASS=X
rem //*  THE PERIOD END STREAM, RUN AFTER THE LAST NIGHTLY OF THE
rem //*  PERIOD AND BEFORE THE PERIOD IS SOFT CLOSED.  REVALUES
rem //*  FOREIGN CURRENCY BALANCES, RELEASES ACCRUALS, VALUES THE
rem //*  INVENTORY, RECONCILES THE CONTROL ACCOUNTS AND PRINTS THE
rem //*  TRIAL BALANCE, PROFIT AND LOSS AND BALANCE SHEET.
rem //*  SUBMIT:  jobs\MONTHEND.cmd CO=STC YEAR=2026 PERIOD=7
rem //*           MODE=PREVIEW (dry run) or MODE=COMMIT (posting)
rem //*  RESTART: FROM THE FAILING STEP - THE REPORT STEPS CHANGE
rem //*           NOTHING, THE POSTING STEPS ARE IDEMPOTENT
rem //*  REQ: XMD-005, XMD-007, COM-009, COM-018, RPT-015, RPT-016
rem //*  2026-10-06 STERP  ORIGINAL
rem ================================================================
setlocal
call "%~dp0SETENV.cmd" MONTHEND %*
if defined P_RESTART goto %P_RESTART%

rem ---------------------------------------------------------------
:STEP005
rem //*  THE PERIOD MUST STILL BE OPEN OR SOFT CLOSED (COM-009)
call "%~dp0RUNSTEP.cmd" STEP005 @SQL "SELECT CompanyCode, FiscalYear, PeriodNo, Status FROM fin.Period WHERE CompanyCode='%P_CO%' AND FiscalYear=%P_YEAR% AND PeriodNo=%P_PERIOD%;"
if %LASTCC% GEQ 8 goto ABEND

rem ---------------------------------------------------------------
:STEP010
rem //*  FOREIGN CURRENCY REVALUATION (FIN-018, FIN-019)
call "%~dp0RUNSTEP.cmd" STEP010 REVALRUN "CO=%P_CO% ASAT=%P_ASAT% MODE=%P_MODE% USER=%P_USER%"
if %LASTCC% GEQ 8 goto ABEND

rem ---------------------------------------------------------------
:STEP020
rem //*  ACCRUAL AND PREPAYMENT RELEASES (FIN-013)
rem //*  RECURJNL is the batch driver; it releases every schedule due
rem //*  on or before the period end through the online ACCRREL
rem //*  service, which owns the posting and the occurrence book
rem //*  keeping (FIN-007, FIN-008, AC-011).
call "%~dp0RUNSTEP.cmd" STEP020 RECURJNL "CO=%P_CO% YEAR=%P_YEAR% PERIOD=%P_PERIOD% ASAT=%P_PEREND% MODE=%P_MODE% USER=%P_USER%"
if %LASTCC% GEQ 8 goto ABEND

rem ---------------------------------------------------------------
:STEP030
rem //*  INVENTORY VALUATION AND GL RECONCILIATION (CST-022)
call "%~dp0RUNSTEP.cmd" STEP030 CSTINVAL "CO=%P_CO% ASAT=%P_ASAT% DETAIL=SUMMARY MODE=%P_MODE% USER=%P_USER%"
if %LASTCC% GEQ 8 goto ABEND

rem ---------------------------------------------------------------
:STEP040
rem //*  DEBTORS CONTROL RECONCILIATION (AR-026, XMD-002)
rem //*  ARRECON takes the packed YYYYPP period, not YEAR + PERIOD
call "%~dp0RUNSTEP.cmd" STEP040 ARRECON "CO=%P_CO% PERIOD=%P_YYYYPP% USER=%P_USER%"
if %LASTCC% GEQ 8 goto ABEND

rem ---------------------------------------------------------------
:STEP050
rem //*  CROSS LEDGER CONTROL RECONCILIATION (XMD-002, XMD-003)
rem //*  RECONCTL is the batch proof; CTLRECON is the online enquiry
rem //*  over the same figures.  The cutoff is the period end, so it
rem //*  agrees with STEP040 ARRECON PERIOD=%P_YYYYPP%.
call "%~dp0RUNSTEP.cmd" STEP050 RECONCTL "CO=%P_CO% ASAT=%P_PEREND% MODE=%P_MODE% USER=%P_USER%"
if %LASTCC% GEQ 8 goto ABEND

rem ---------------------------------------------------------------
:STEP060
rem //*  TRIAL BALANCE (FIN-021, RPT-001)
call "%~dp0RUNSTEP.cmd" STEP060 TBRPT "CO=%P_CO% YEAR=%P_YEAR% PERFROM=%P_PERIOD% PERTO=%P_PERIOD% ZEROS=N USER=%P_USER%"
if %LASTCC% GEQ 8 goto ABEND

rem ---------------------------------------------------------------
:STEP070
rem //*  PROFIT AND LOSS (FIN-022, RPT-001)
call "%~dp0RUNSTEP.cmd" STEP070 PLRPT "CO=%P_CO% YEAR=%P_YEAR% PERFROM=1 PERTO=%P_PERIOD% COMPARE=BUDGET USER=%P_USER%"
if %LASTCC% GEQ 8 goto ABEND

rem ---------------------------------------------------------------
:STEP080
rem //*  BALANCE SHEET (FIN-023, RPT-001)
call "%~dp0RUNSTEP.cmd" STEP080 BSRPT "CO=%P_CO% YEAR=%P_YEAR% PERIOD=%P_PERIOD% COMPARE=PRIOR USER=%P_USER%"
if %LASTCC% GEQ 8 goto ABEND
goto EOJ

:ABEND
echo //*  STREAM TERMINATED BY COND CODE %LASTCC%>> "%SYSOUT%"
echo //*  CORRECT THE FAULT AND RESTART AT THE FAILING STEP>> "%SYSOUT%"
call "%~dp0JOBEND.cmd" %MAXCC%
endlocal & exit /b 8

:EOJ
call "%~dp0JOBEND.cmd" %MAXCC%
endlocal & exit /b 0
