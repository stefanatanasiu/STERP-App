@echo off
rem STERP - Stefan's ERP - wrapper for demo-data.ps1 (all arguments passed through)
powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0demo-data.ps1" %*
exit /b %ERRORLEVEL%
