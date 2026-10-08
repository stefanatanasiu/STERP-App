@echo off
rem STERP - Stefan's ERP - wrapper for build-all.ps1 (all arguments passed through)
powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0build-all.ps1" %*
exit /b %ERRORLEVEL%
