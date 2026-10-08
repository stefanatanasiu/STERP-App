<#
 STERP test runner - runs every test layer and prints a summary.
   run-tests.ps1                 COBOL unit drivers + SQL tests + .NET integration/acceptance harness
   run-tests.ps1 -Layer cobol    only one layer (cobol | sql | net)
   run-tests.ps1 -Filter BOM     only tests whose name matches (COBOL exe name / SQL file / harness filter)
 Exit code = number of failing tests (0 = all green).
#>
param([ValidateSet('all','cobol','sql','net')][string]$Layer = 'all', [string]$Filter)
. "$PSScriptRoot\env.ps1"
$ErrorActionPreference = 'Continue'
Set-Location $STERP_ROOT
$summary = @()
$totalFail = 0

if ($Layer -in 'all','cobol') {
    $p = 0; $f = 0
    Push-Location $STERP_TBIN
    foreach ($exe in Get-ChildItem $STERP_TBIN -Filter 'T*.exe' | Sort-Object Name) {
        if ($Filter -and $exe.BaseName -notmatch $Filter) { continue }
        $out = & $exe.FullName 2>&1 | ForEach-Object { "$_" }
        $ok = @($out | Where-Object { $_ -match '^ok ' }).Count
        $nok = @($out | Where-Object { $_ -match '^not ok ' }).Count
        if ($LASTEXITCODE -ne 0 -and $nok -eq 0) { $nok = 1; $out += "not ok - $($exe.BaseName) exited with $LASTEXITCODE" }
        $p += $ok; $f += $nok
        $flag = if ($nok) { 'FAIL' } else { 'ok  ' }
        Write-Host ("{0} COBOL {1,-12} {2,4} passed {3,3} failed" -f $flag, $exe.BaseName, $ok, $nok)
        if ($nok) { $out | Where-Object { $_ -match '^not ok|^#   ' } | ForEach-Object { Write-Host "      $_" -ForegroundColor Red } }
    }
    Pop-Location
    $summary += "COBOL unit tests : $p passed, $f failed"; $totalFail += $f
}

if ($Layer -in 'all','sql') {
    $args2 = @('-NoProfile','-ExecutionPolicy','Bypass','-File',(Join-Path $PSScriptRoot 'build-db.ps1'),'-NoDeploy','-Tests','-Quiet')
    if ($Filter) { $args2 += @('-Only', $Filter) }
    $out = & powershell @args2 2>&1 | ForEach-Object { "$_" }
    $out | Where-Object { $_ -match '^not ok|ERROR' } | ForEach-Object { Write-Host "      $_" -ForegroundColor Red }
    $line = $out | Where-Object { $_ -match '^# SQL tests passed (\d+) failed (\d+)' } | Select-Object -Last 1
    if ($line -match 'passed (\d+) failed (\d+)') { $summary += "SQL tests        : $($Matches[1]) passed, $($Matches[2]) failed"; $totalFail += [int]$Matches[2] }
    else { $summary += "SQL tests        : runner error"; $totalFail++ ; $out | Select-Object -Last 20 | Write-Host }
}

if ($Layer -in 'all','net') {
    $h = Join-Path $STERP_BIN 'STERP.Tests.exe'
    if (Test-Path $h) {
        Push-Location $STERP_BIN
        $a = @(); if ($Filter) { $a += "/filter:$Filter" }
        $out = & $h @a 2>&1 | ForEach-Object { "$_" }
        Pop-Location
        $out | Where-Object { $_ -match '^(ok|not ok|#)' } | ForEach-Object { if ($_ -match '^not ok') { Write-Host $_ -ForegroundColor Red } else { Write-Host $_ } }
        $ok = @($out | Where-Object { $_ -match '^ok ' }).Count
        $nok = @($out | Where-Object { $_ -match '^not ok ' }).Count
        if ($LASTEXITCODE -ne 0 -and $nok -eq 0) { $nok = 1; $out | Select-Object -Last 20 | Write-Host }
        $summary += ".NET harness     : $ok passed, $nok failed"; $totalFail += $nok
    } else { $summary += ".NET harness     : not built (bin\x86\STERP.Tests.exe missing)" }
}

Write-Host "`n================ STERP TEST SUMMARY ================"
$summary | ForEach-Object { Write-Host $_ }
if ($totalFail) { Write-Host "RESULT: $totalFail FAILURE(S)" -ForegroundColor Red } else { Write-Host "RESULT: ALL GREEN" -ForegroundColor Green }
exit $totalFail
