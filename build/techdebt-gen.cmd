@echo off
rem STERP - Stefan's ERP - wrapper for techdebt-gen.ps1 (all arguments passed through)
powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0techdebt-gen.ps1" %*
exit /b %ERRORLEVEL%
