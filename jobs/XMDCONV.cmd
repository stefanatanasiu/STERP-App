@echo off
rem ================================================================
rem //XMDCONV  JOB (STERP),'LEGACY CONVERSION LOAD',CLASS=C
rem //*  LOADS THE FOUR CALEDON CUT OVER EXTRACTS IN data\legacy
rem //*  THROUGH CONVLOAD.  EVERY FILE IS READ TWICE: ONCE IN
rem //*  PREVIEW (STEP01n), WHICH CHANGES NOTHING AND PRINTS THE
rem //*  ACCEPT / REJECT / DUPLICATE / ALREADY LOADED COUNTS, AND
rem //*  THEN IN COMMIT (STEP02n) UNLESS MODE=PREVIEW WAS GIVEN.
rem //*  THE COMMIT PASS IS RESTARTABLE: A RECORD ALREADY LOADED IS
rem //*  REPORTED AS LOADED AND IS NOT POSTED TWICE (XMD-007).
rem //*  SUBMIT:  jobs\XMDCONV.cmd CO=CAL MODE=COMMIT USER=CONV
rem //*  RESTART: RESTART=STEPnnn, OR SIMPLY RESUBMIT
rem //*  REQ: XMD-007, XMD-008, COM-018, COM-023
rem //*  2026-10-06 STERP  ORIGINAL
rem ================================================================
setlocal
call "%~dp0SETENV.cmd" XMDCONV %*
if not defined P_CUTOVER set "P_CUTOVER=20260331"
set "CVTAIL=CUTOVER=%P_CUTOVER% USER=%P_USER%"
if defined P_RESTART goto %P_RESTART%

rem ---------------------------------------------------------------
:STEP010
rem //*  TRIAL BALANCE, OPEN SALES, OPEN PURCHASE, STOCK - PREVIEW
call "%~dp0RUNSTEP.cmd" STEP010 CONVLOAD "CO=%P_CO% FILE=data\legacy\CALGLTB.DAT MODE=PREVIEW %CVTAIL%"
if %LASTCC% GEQ 8 goto ABEND

:STEP011
call "%~dp0RUNSTEP.cmd" STEP011 CONVLOAD "CO=%P_CO% FILE=data\legacy\CALAROPN.DAT MODE=PREVIEW %CVTAIL%"
if %LASTCC% GEQ 8 goto ABEND

:STEP012
call "%~dp0RUNSTEP.cmd" STEP012 CONVLOAD "CO=%P_CO% FILE=data\legacy\CALAPOPN.DAT MODE=PREVIEW %CVTAIL%"
if %LASTCC% GEQ 8 goto ABEND

:STEP013
call "%~dp0RUNSTEP.cmd" STEP013 CONVLOAD "CO=%P_CO% FILE=data\legacy\CALSTOCK.DAT MODE=PREVIEW %CVTAIL%"
if %LASTCC% GEQ 8 goto ABEND
if /i not "%P_MODE%"=="COMMIT" (
    echo //*  MODE=PREVIEW - THE COMMIT PASS IS BYPASSED>> "%SYSOUT%"
    goto EOJ
)

rem ---------------------------------------------------------------
:STEP020
rem //*  THE SAME FOUR FILES, COMMITTED IN LEDGER ORDER
call "%~dp0RUNSTEP.cmd" STEP020 CONVLOAD "CO=%P_CO% FILE=data\legacy\CALGLTB.DAT MODE=COMMIT %CVTAIL%"
if %LASTCC% GEQ 8 goto ABEND

:STEP021
call "%~dp0RUNSTEP.cmd" STEP021 CONVLOAD "CO=%P_CO% FILE=data\legacy\CALAROPN.DAT MODE=COMMIT %CVTAIL%"
if %LASTCC% GEQ 8 goto ABEND

:STEP022
call "%~dp0RUNSTEP.cmd" STEP022 CONVLOAD "CO=%P_CO% FILE=data\legacy\CALAPOPN.DAT MODE=COMMIT %CVTAIL%"
if %LASTCC% GEQ 8 goto ABEND

:STEP023
call "%~dp0RUNSTEP.cmd" STEP023 CONVLOAD "CO=%P_CO% FILE=data\legacy\CALSTOCK.DAT MODE=COMMIT %CVTAIL%"
if %LASTCC% GEQ 8 goto ABEND

rem ---------------------------------------------------------------
:STEP030
rem //*  WHAT THE CUT OVER PRODUCED - EVIDENCE FOR THE CONVERSION LOG
call "%~dp0RUNSTEP.cmd" STEP030 @SQL "SELECT SourceFile, Outcome, COUNT(*) AS Records FROM stg.ImportRow WHERE CompanyCode='%P_CO%' GROUP BY SourceFile, Outcome ORDER BY SourceFile, Outcome;"
if %LASTCC% GEQ 8 goto ABEND
goto EOJ

:ABEND
echo //*  STREAM TERMINATED BY COND CODE %LASTCC%>> "%SYSOUT%"
call "%~dp0JOBEND.cmd" %MAXCC%
endlocal & exit /b %MAXCC%

:EOJ
call "%~dp0JOBEND.cmd" %MAXCC%
endlocal & exit /b %MAXCC%
