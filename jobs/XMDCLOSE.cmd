@echo off
rem ================================================================
rem //XMDCLOSE JOB (STERP),'CONTROL CLOSE PACK',CLASS=A
rem //*  THE CROSS MODULE CLOSE PACK: THE CONTROL ACCOUNT
rem //*  RECONCILIATION, THE STATUTORY OBLIGATIONS REGISTER AND THE
rem //*  EXCEPTION AGEING, ALL AT ONE SHARED CUTOFF SO THAT THE
rem //*  THREE LISTINGS DESCRIBE THE SAME LEDGER (XMD-005, XMD-006,
rem //*  XMD-013).  EVERY STEP IS READ ONLY IN MODE=PREVIEW.
rem //*  SUBMIT:  jobs\XMDCLOSE.cmd CO=STC ASAT=20261031 MODE=PREVIEW
rem //*  RESTART: RESTART=STEPnnn - ALL THREE STEPS ARE REPORTS AND
rem //*           MAY BE REPEATED
rem //*  REQ: XMD-005, XMD-006, XMD-013, RPT-015, RPT-016, COM-018
rem //*  2026-10-06 STERP  ORIGINAL
rem ================================================================
setlocal
call "%~dp0SETENV.cmd" XMDCLOSE %*
if defined P_RESTART goto %P_RESTART%

rem ---------------------------------------------------------------
:STEP010
rem //*  SUBSIDIARY LEDGERS AGAINST THE GENERAL LEDGER (RPT-015)
set "RCPARM=CO=%P_CO% ASAT=%P_ASAT% MODE=%P_MODE% USER=%P_USER%"
if defined P_TOL set "RCPARM=%RCPARM% TOL=%P_TOL%"
call "%~dp0RUNSTEP.cmd" STEP010 RECONCTL "%RCPARM%"
if %LASTCC% GEQ 8 goto ABEND

rem ---------------------------------------------------------------
:STEP020
rem //*  STATUTORY OBLIGATIONS AND THEIR DERIVED DUE DATES (XMD-013)
set "OBPARM=CO=%P_CO% ASAT=%P_ASAT% MODE=%P_MODE% USER=%P_USER%"
if defined P_DAYS set "OBPARM=%OBPARM% DAYS=%P_DAYS%"
call "%~dp0RUNSTEP.cmd" STEP020 OBLIGREG "%OBPARM%"
if %LASTCC% GEQ 8 goto ABEND

rem ---------------------------------------------------------------
:STEP030
rem //*  EXCEPTION AGEING, OWNERSHIP AND ESCALATION (RPT-016)
set "EXPARM=CO=%P_CO% ASAT=%P_ASAT% MODE=%P_MODE% USER=%P_USER%"
if defined P_OWNER set "EXPARM=%EXPARM% OWNER=%P_OWNER%"
if defined P_DAYS set "EXPARM=%EXPARM% DAYS=%P_DAYS%"
call "%~dp0RUNSTEP.cmd" STEP030 EXCPAGE "%EXPARM%"
if %LASTCC% GEQ 8 goto ABEND
goto EOJ

:ABEND
echo //*  STREAM TERMINATED BY COND CODE %LASTCC%>> "%SYSOUT%"
call "%~dp0JOBEND.cmd" %MAXCC%
endlocal & exit /b %MAXCC%

:EOJ
call "%~dp0JOBEND.cmd" %MAXCC%
endlocal & exit /b %MAXCC%
