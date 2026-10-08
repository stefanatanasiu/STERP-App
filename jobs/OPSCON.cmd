@echo off
rem ================================================================
rem //OPSCON   PROC  'OPERATOR CONSOLE'
rem //*  STARTS THE SCREEN SECTION OPERATOR CONSOLE WITH THE TWO
rem //*  RUN TIME SWITCHES GnuCOBOL 3.2 NEEDS BEFORE IT WILL PASS
rem //*  THE FUNCTION KEYS (F3 EXIT, F7 / F8 PAGING) AND ESCAPE TO
rem //*  ACCEPT.  WITHOUT THEM ONLY THE TYPED EQUIVALENTS X, - AND
rem //*  + WORK (docs\OPERATOR-GUIDE.md section 1).
rem //*  SUBMIT:  jobs\OPSCON.cmd            (interactive console)
rem //*           jobs\OPSCON.cmd SMOKE      (self test, no screen)
rem //*  REQ: XMD-005, COM-017, COM-018
rem //*  2026-10-06 STERP  ORIGINAL
rem ================================================================
setlocal
set "STERP_HOME=%~dp0.."
for %%I in ("%STERP_HOME%") do set "STERP_HOME=%%~fI"
if not defined STERP_RUN set "STERP_RUN=%STERP_HOME%\run"
if not defined STERP_ODBC set "STERP_ODBC=Driver={ODBC Driver 18 for SQL Server};Server=.;Database=STERP;Trusted_Connection=Yes;TrustServerCertificate=Yes;"
set "COB_SCREEN_EXCEPTIONS=Y"
set "COB_SCREEN_ESC=Y"
set "PATH=%STERP_HOME%\bin\x86;%PATH%"
if not exist "%STERP_HOME%\bin\x86\STERPOPS.exe" (
    echo STERP-E-0016 bin\x86\STERPOPS.exe is not installed - run build\build-cobol.ps1 -Module tui
    endlocal & exit /b 16
)
pushd "%STERP_HOME%"
"%STERP_HOME%\bin\x86\STERPOPS.exe" %*
set "OPSRC=%ERRORLEVEL%"
popd
endlocal & exit /b %OPSRC%
