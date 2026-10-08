@echo off
rem STERP - Stefan's ERP - wrapper for loc-report.ps1 (all arguments passed through)
powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0loc-report.ps1" %*
exit /b %ERRORLEVEL%
