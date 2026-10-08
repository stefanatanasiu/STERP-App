@echo off
rem STERP - Stefan's ERP - wrapper for build-net.ps1 (all arguments passed through)
powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0build-net.ps1" %*
exit /b %ERRORLEVEL%
