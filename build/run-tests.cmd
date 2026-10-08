@echo off
rem STERP - Stefan's ERP - wrapper for run-tests.ps1 (all arguments passed through)
powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0run-tests.ps1" %*
exit /b %ERRORLEVEL%
