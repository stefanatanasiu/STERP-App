@echo off
rem STERP - Stefan's ERP - wrapper for ac-coverage.ps1 (all arguments passed through)
powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0ac-coverage.ps1" %*
exit /b %ERRORLEVEL%
