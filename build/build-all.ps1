<#
 STERP full build: database, COBOL engine/batch/tests, .NET client, then (optionally) tests.
   build-all.ps1                 incremental build of everything
   build-all.ps1 -Full           drop/recreate DB, clean COBOL objects, rebuild .NET
   build-all.ps1 -Test           also run all tests
#>
param([switch]$Full, [switch]$Test)
$ErrorActionPreference = 'Continue'
$ps = @('-NoProfile', '-ExecutionPolicy', 'Bypass', '-File')
$rc = 0
$dbArgs = @(); if ($Full) { $dbArgs += '-Full' }
& powershell @ps (Join-Path $PSScriptRoot 'build-db.ps1') @dbArgs; if ($LASTEXITCODE) { $rc = 1 }
$cbArgs = @(); if ($Full) { $cbArgs += '-Clean' }
& powershell @ps (Join-Path $PSScriptRoot 'build-cobol.ps1') @cbArgs; if ($LASTEXITCODE) { $rc = 1 }
$nArgs = @(); if ($Full) { $nArgs += '-Rebuild' }
& powershell @ps (Join-Path $PSScriptRoot 'build-net.ps1') @nArgs; if ($LASTEXITCODE) { $rc = 1 }
if ($Test) { & powershell @ps (Join-Path $PSScriptRoot 'run-tests.ps1'); if ($LASTEXITCODE) { $rc = 1 } }
& powershell @ps (Join-Path $PSScriptRoot 'loc-report.ps1') -NoWrite | Select-Object -Last 1
if ($rc) { Write-Host "BUILD-ALL FAILED" -ForegroundColor Red } else { Write-Host "BUILD-ALL OK" -ForegroundColor Green }
exit $rc
