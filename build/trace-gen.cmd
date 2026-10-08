@echo off
rem STERP - Stefan's ERP - wrapper for trace-gen.ps1 (all arguments passed through)
powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0trace-gen.ps1" %*
exit /b %ERRORLEVEL%
