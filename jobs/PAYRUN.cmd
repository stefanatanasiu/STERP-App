@echo off
rem ================================================================
rem //PAYRUN  JOB (STERP),'SUPPLIER PAYMENT RUN',CLASS=B,MSGCLASS=X
rem //*  PAYS AN APPROVED PAYMENT PROPOSAL.  THE PROPOSAL ITSELF IS
rem //*  BUILT AND RESERVED ONLINE (APPAYPRP, CREDITORS - PAYMENT
rem //*  PROPOSAL AND RUN); THIS STREAM REFUSES TO PAY ANYTHING
rem //*  THAT HAS NOT BEEN APPROVED - SEGREGATION OF DUTIES D-07.
rem //*  SUBMIT:  jobs\PAYRUN.cmd CO=STC RUN=PP-26-0007 USER=APMGR
rem //*           (RUN= omitted = the most recent APPROVED/RELEASED
rem //*            proposal; if none, the most recent of any status,
rem //*            which the approval gate then refuses)
rem //*  RESTART: RESTART=STEP030 reprints/rebuilds the Bacs file;
rem //*           APBACS is idempotent on the proposal reference
rem //*  REQ: XMD-005, AP-015, AP-016, COM-002, COM-018
rem //*  2026-10-06 STERP  ORIGINAL
rem ================================================================
setlocal
call "%~dp0SETENV.cmd" PAYRUN %*
if defined P_RESTART goto %P_RESTART%

rem ---------------------------------------------------------------
:STEP010
rem //*  LOCATE THE PROPOSAL AND SHOW IT ON THE JOB LOG
if not defined P_RUN for /f "usebackq tokens=*" %%R in (`sqlcmd -S "%STERP_SQLSERVER%" -E -C -b -h -1 -W -d "%STERP_DATABASE%" -Q "SET NOCOUNT ON; SELECT TOP 1 ProposalRef FROM ap.PaymentProposal WHERE CompanyCode='%P_CO%' ORDER BY CASE WHEN Status IN ('APPROVED','RELEASED') THEN 0 ELSE 1 END, ProposalId DESC;"`) do set "P_RUN=%%R"
call "%~dp0RUNSTEP.cmd" STEP010 @SQL "SELECT ProposalRef, Status, PaymentMethod, PayDate, CashPrincipalGBP FROM ap.PaymentProposal WHERE CompanyCode='%P_CO%' AND ProposalRef='%P_RUN%';"
if %LASTCC% GEQ 8 goto ABEND

rem ---------------------------------------------------------------
:STEP020
rem //*  APPROVAL GATE - COND CODE 8 IF THE PROPOSAL IS NOT APPROVED
set "PAYOK="
for /f "usebackq tokens=*" %%S in (`sqlcmd -S "%STERP_SQLSERVER%" -E -C -b -h -1 -W -d "%STERP_DATABASE%" -Q "SET NOCOUNT ON; SELECT TOP 1 Status FROM ap.PaymentProposal WHERE CompanyCode='%P_CO%' AND ProposalRef='%P_RUN%';"`) do set "PAYOK=%%S"
echo //STEP020  EXEC PGM=APPAYAPR,PARM='%P_RUN% STATUS=%PAYOK%'>> "%SYSOUT%"
echo STERP-I-0002 STEP020 approval check %P_RUN% status %PAYOK%
if /i "%PAYOK%"=="APPROVED" goto STEP030
if /i "%PAYOK%"=="RELEASED" goto STEP030
if /i "%PAYOK%"=="PAID" goto PAIDALREADY
echo //*  PROPOSAL %P_RUN% IS '%PAYOK%' - NOT APPROVED - COND CODE 0008>> "%SYSOUT%"
echo STERP-E-0008 STEP020 proposal %P_RUN% is not approved (%PAYOK%)
set "MAXCC=8"
goto ABEND

:PAIDALREADY
echo //*  PROPOSAL %P_RUN% HAS ALREADY BEEN PAID - NOTHING TO DO>> "%SYSOUT%"
set "MAXCC=4"
goto EOJ

rem ---------------------------------------------------------------
:STEP030
rem //*  BACS STANDARD 18 PAYMENT FILE (AP-015)
call "%~dp0RUNSTEP.cmd" STEP030 APBACS "CO=%P_CO% RUN=%P_RUN% MODE=%P_MODE% USER=%P_USER%"
if %LASTCC% GEQ 8 goto ABEND

rem ---------------------------------------------------------------
:STEP040
rem //*  REMITTANCE ADVICES (AP-016)
call "%~dp0RUNSTEP.cmd" STEP040 APREMIT "CO=%P_CO% RUN=%P_RUN% USER=%P_USER%"
if %LASTCC% GEQ 8 goto ABEND
goto EOJ

:ABEND
echo //*  STREAM TERMINATED - NO PAYMENT FILE HAS BEEN RELEASED>> "%SYSOUT%"
call "%~dp0JOBEND.cmd" %MAXCC%
endlocal & exit /b %MAXCC%

:EOJ
call "%~dp0JOBEND.cmd" %MAXCC%
endlocal & exit /b %MAXCC%
