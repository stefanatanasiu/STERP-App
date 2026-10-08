@echo off
rem STERP - Stefan's ERP - wrapper for build-cobol.ps1 (all arguments passed through)
powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0build-cobol.ps1" %*
exit /b %ERRORLEVEL%
