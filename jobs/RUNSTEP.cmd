@echo off
rem ================================================================
rem //RUNSTEP PROC - EXECUTE ONE JOB STEP AND SET THE COND CODE
rem   CALLED AS:
rem     call "%~dp0RUNSTEP.cmd" STEP010 RECURJNL "CO=STC MODE=PREVIEW"
rem     call "%~dp0RUNSTEP.cmd" STEP010 @SQL     "EXEC core.usp_Age..."
rem   Sets LASTCC (this step) and MAXCC (highest so far).
rem   A step whose program is not installed is BYPASSED with COND
rem   CODE 0004 - the stream carries on, exactly like a JCL step with
rem   COND=(8,LE) - so a stream stays runnable while modules are
rem   still being delivered.
rem   REQ: XMD-005, COM-018
rem   2026-10-06 STERP  original
rem ================================================================
set "STEPNAME=%~1"
set "STEPPGM=%~2"
set "STEPPARM=%~3"
set "LASTCC=0"
if not defined MAXCC set "MAXCC=0"
if not defined SYSOUT set "SYSOUT=%STERP_RUN%\logs\STERPJOB.log"

echo.>> "%SYSOUT%"
echo //%STEPNAME%  EXEC PGM=%STEPPGM%,PARM='%STEPPARM%'>> "%SYSOUT%"
echo //SYSPRINT DD SYSOUT=*>> "%SYSOUT%"
echo STERP-I-0002 %STEPNAME% %STEPPGM% %STEPPARM%

if /i "%STEPPGM%"=="@SQL" goto SQLSTEP

if not exist "%STERP_HOME%\bin\x86\%STEPPGM%.exe" (
  echo //*  STEP BYPASSED - %STEPPGM%.EXE IS NOT INSTALLED>> "%SYSOUT%"
  echo STERP-W-0004 %STEPNAME% bypassed - %STEPPGM%.EXE not installed
  set "LASTCC=4"
  goto SETMAX
)
pushd "%STERP_HOME%"
"%STERP_HOME%\bin\x86\%STEPPGM%.exe" %STEPPARM% >> "%SYSOUT%" 2>&1
set "LASTCC=%ERRORLEVEL%"
popd
goto SETMAX

:SQLSTEP
sqlcmd -S "%STERP_SQLSERVER%" -E -C -b -I -d "%STERP_DATABASE%" -W -Q "SET NOCOUNT ON; %STEPPARM%" >> "%SYSOUT%" 2>&1
set "LASTCC=%ERRORLEVEL%"
if not "%LASTCC%"=="0" set "LASTCC=8"

:SETMAX
if %LASTCC% GTR %MAXCC% set "MAXCC=%LASTCC%"
echo //*  %STEPNAME% ENDED - COND CODE %LASTCC%  (MAXCC %MAXCC%)>> "%SYSOUT%"
if %LASTCC% GEQ 8 echo STERP-E-0008 %STEPNAME% failed - COND CODE %LASTCC%
exit /b 0
