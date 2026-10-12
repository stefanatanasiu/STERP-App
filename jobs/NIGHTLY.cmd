@echo off
rem ================================================================
rem //NIGHTLY JOB (STERP),'NIGHTLY SERVICE',CLASS=A,MSGCLASS=X
rem //*  THE EVERY-NIGHT STREAM.  AGES THE EXCEPTION QUEUE, POSTS
rem //*  THE RECURRING JOURNALS THAT FALL DUE, PRINTS THE CREDIT
rem //*  CONTROL AND AGED REPORTS AND RUNS THE AP SIX MONTH VAT
rem //*  CHECK.  SAFE TO RESTART FROM THE TOP: EVERY POSTING STEP
rem //*  IS DRIVEN BY MODE= AND IS IDEMPOTENT PER DOCUMENT.
rem //*  SUBMIT:  jobs\NIGHTLY.cmd CO=STC USER=OPS MODE=PREVIEW
rem //*  COND:    0/4 CONTINUE, 8 OR MORE STOP THE STREAM
rem //*  REQ: XMD-005, XMD-007, COM-017, COM-018, RPT-015
rem //*  2026-10-06 STERP  ORIGINAL
rem ================================================================
setlocal
call "%~dp0SETENV.cmd" NIGHTLY %*
if defined P_RESTART goto %P_RESTART%

rem ---------------------------------------------------------------
:STEP010
rem //*  AGE THE EXCEPTION WORK QUEUE AND ESCALATE (COM-017)
call "%~dp0RUNSTEP.cmd" STEP010 @SQL "EXEC core.usp_AgeExceptions;"
if %LASTCC% GEQ 8 goto ABEND

rem ---------------------------------------------------------------
:STEP020
rem //*  RECURRING JOURNALS DUE TODAY (FIN-012)
call "%~dp0RUNSTEP.cmd" STEP020 RECURJNL "CO=%P_CO% YEAR=%P_YEAR% PERIOD=%P_PERIOD% MODE=%P_MODE% ASAT=%P_ASAT% USER=%P_USER%"
if %LASTCC% GEQ 8 goto ABEND

rem ---------------------------------------------------------------
:STEP030
rem //*  AGED DEBT / CREDIT CONTROL (AR-010, RPT-002)
call "%~dp0RUNSTEP.cmd" STEP030 AGEDDEBT "CO=%P_CO% ASAT=%P_ASAT% USER=%P_USER%"
if %LASTCC% GEQ 8 goto ABEND

rem ---------------------------------------------------------------
:STEP040
rem //*  AGED CREDITORS AND CASH REQUIREMENT (AP-010, RPT-003)
call "%~dp0RUNSTEP.cmd" STEP040 APAGED "CO=%P_CO% ASAT=%P_ASAT% USER=%P_USER%"
if %LASTCC% GEQ 8 goto ABEND

rem ---------------------------------------------------------------
:STEP050
rem //*  SIX MONTH UNPAID INPUT VAT - REPAY / RECLAIM (AP-025)
call "%~dp0RUNSTEP.cmd" STEP050 APVAT6M "CO=%P_CO% ASAT=%P_ASAT% MODE=%P_MODE% USER=%P_USER%"
if %LASTCC% GEQ 8 goto ABEND

rem ---------------------------------------------------------------
:STEP060
rem //*  FX RATE STALENESS WARNING FOR THE MORNING SHIFT (COM-010)
call "%~dp0RUNSTEP.cmd" STEP060 @SQL "EXEC core.usp_CheckFxStaleness;"
if %LASTCC% GEQ 8 goto ABEND
goto EOJ

:ABEND
echo //*  STREAM TERMINATED BY COND CODE %LASTCC%>> "%SYSOUT%"
call "%~dp0JOBEND.cmd" %MAXCC%
endlocal & exit /b %MAXCC%

:EOJ
call "%~dp0JOBEND.cmd" %MAXCC%
endlocal & exit /b %MAXCC%
