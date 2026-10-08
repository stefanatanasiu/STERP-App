@echo off
rem ================================================================
rem //DDCOLL  JOB (STERP),'DIRECT DEBIT COLLECTION',CLASS=B
rem //*  BUILDS THE AR DIRECT DEBIT COLLECTION SCHEDULE AND, IN
rem //*  MODE=COMMIT, THE BACS COLLECTION FILE IN run\bacs.
rem //*  ADVANCE NOTICE RULES AND THE DIRECT DEBIT GUARANTEE ARE
rem //*  ENFORCED BY DDCOLL ITSELF - DO NOT BYPASS THEM.
rem //*  SUBMIT:  jobs\DDCOLL.cmd CO=STC SUN=123456 MODE=PREVIEW
rem //*  RESTART: FROM THE TOP - A COLLECTION ALREADY SUBMITTED FOR
rem //*           THE SAME DUE DATE IS NOT COLLECTED TWICE
rem //*  REQ: XMD-005, AR-019, AR-020, COM-018
rem //*  2026-10-06 STERP  ORIGINAL
rem ================================================================
setlocal
call "%~dp0SETENV.cmd" DDCOLL %*
if defined P_RESTART goto %P_RESTART%

rem ---------------------------------------------------------------
:STEP010
rem //*  COLLECTION SCHEDULE AND BACS FILE (AR-019)
set "DDPARM=CO=%P_CO% ASAT=%P_ASAT% MODE=%P_MODE% USER=%P_USER%"
if defined P_SUN set "DDPARM=%DDPARM% SUN=%P_SUN%"
call "%~dp0RUNSTEP.cmd" STEP010 DDCOLL "%DDPARM%"
if %LASTCC% GEQ 8 goto ABEND

rem ---------------------------------------------------------------
:STEP020
rem //*  WHAT THE STREAM HAS PRODUCED - EVIDENCE FOR THE MORNING
call "%~dp0RUNSTEP.cmd" STEP020 @SQL "SELECT Status, COUNT(*) AS Collections, SUM(AmountGBP) AS GBP FROM ar.DirectDebitCollection WHERE CompanyCode='%P_CO%' GROUP BY Status;"
if %LASTCC% GEQ 8 goto ABEND
goto EOJ

:ABEND
echo //*  STREAM TERMINATED BY COND CODE %LASTCC%>> "%SYSOUT%"
call "%~dp0JOBEND.cmd" %MAXCC%
endlocal & exit /b 8

:EOJ
call "%~dp0JOBEND.cmd" %MAXCC%
endlocal & exit /b 0
