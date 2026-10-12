@echo off
rem ================================================================
rem //STMTS   JOB (STERP),'CUSTOMER STATEMENTS',CLASS=C,MSGCLASS=X
rem //*  MONTH END CUSTOMER STATEMENTS FOLLOWED BY THE COLLECTION
rem //*  REMINDER (DUNNING) RUN.  RUN AFTER THE LAST AR POSTING OF
rem //*  THE MONTH AND AFTER NIGHTLY, SO THE AGEING IS CURRENT.
rem //*  SUBMIT:  jobs\STMTS.cmd CO=STC ASAT=20261031 STAGE=ALL
rem //*  RESTART: FROM THE TOP - STATEMENTS ARE REPRINTS, DUNNING
rem //*           ONLY ADVANCES A CUSTOMER'S STAGE ONCE PER CYCLE
rem //*  REQ: XMD-005, AR-012, AR-013, RPT-004, COM-018
rem //*  2026-10-06 STERP  ORIGINAL
rem ================================================================
setlocal
call "%~dp0SETENV.cmd" STMTS %*
if defined P_RESTART goto %P_RESTART%
if not defined P_STYLE set "P_STYLE=OPENITEM"
if not defined P_STAGE set "P_STAGE=ALL"

rem ---------------------------------------------------------------
:STEP010
rem //*  CUSTOMER STATEMENTS (AR-012, RPT-004)
call "%~dp0RUNSTEP.cmd" STEP010 STATEMNT "CO=%P_CO% ASAT=%P_ASAT% STYLE=%P_STYLE% USER=%P_USER%"
if %LASTCC% GEQ 8 goto ABEND

rem ---------------------------------------------------------------
:STEP020
rem //*  COLLECTION REMINDERS AND LETTERS (AR-013)
call "%~dp0RUNSTEP.cmd" STEP020 DUNNING "CO=%P_CO% ASAT=%P_ASAT% STAGE=%P_STAGE% MODE=%P_MODE% USER=%P_USER%"
if %LASTCC% GEQ 8 goto ABEND

rem ---------------------------------------------------------------
:STEP030
rem //*  WHAT THE CREDIT CONTROLLER MUST PICK UP IN THE MORNING
call "%~dp0RUNSTEP.cmd" STEP030 @SQL "SELECT Severity, Category, COUNT(*) AS Items FROM core.ExceptionItem WHERE CompanyCode='%P_CO%' AND Module='AR' AND Status='OPEN' GROUP BY Severity, Category;"
if %LASTCC% GEQ 8 goto ABEND
goto EOJ

:ABEND
echo //*  STREAM TERMINATED BY COND CODE %LASTCC%>> "%SYSOUT%"
call "%~dp0JOBEND.cmd" %MAXCC%
endlocal & exit /b %MAXCC%

:EOJ
call "%~dp0JOBEND.cmd" %MAXCC%
endlocal & exit /b %MAXCC%
