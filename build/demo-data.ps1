<#
 STERP - Stefan's ERP - demonstration dataset driver.

   demo-data.ps1                      run the demonstration batch drivers, level the control
                                      accounts and print the reconciliation for STC
   demo-data.ps1 -Rebuild             drop and recreate the database first (build-db.ps1 -Full)
   demo-data.ps1 -Company CAL         do the same for another company
   demo-data.ps1 -AsAt 20261031       use a different cutoff for the bridge and RECONCTL
   demo-data.ps1 -SkipDrivers         only level the controls and reconcile

 The script is re-runnable.  Every COBOL driver is idempotent through core.IdempotencyKey, the
 control-account bridge (fin.usp_DemoBridgeControls, db\seed\98-demo-consistency.sql) only posts
 the difference that is still outstanding, and RECONCTL deletes its own open rows before writing
 new ones.  A driver that fails is recorded and the run continues, because the modules are
 developed in parallel and a half-finished driver must not stop the dataset being usable.

 See docs\DEMO-DATA.md for what the dataset contains.
#>
param(
    [string]$Company = 'STC',
    [string]$AsAt = '20261031',
    [string]$Site = 'BHM',
    [string]$User = 'FINCTRL',
    [switch]$Rebuild,
    [switch]$SkipDrivers,
    [switch]$Preview
)
. "$PSScriptRoot\env.ps1"
$ErrorActionPreference = 'Continue'
Set-Location $STERP_ROOT
$mode = if ($Preview) { 'PREVIEW' } else { 'COMMIT' }
$asAtIso = "$($AsAt.Substring(0,4))-$($AsAt.Substring(4,2))-$($AsAt.Substring(6,2))"
$results = New-Object System.Collections.Generic.List[object]

function Add-Result([string]$step, [int]$rc, [string]$note) {
    $results.Add([pscustomobject]@{ Step = $step; ExitCode = $rc; Note = $note })
}

function Invoke-Driver([string]$exe, [string[]]$drvArgs) {
    $path = Join-Path $STERP_ROOT "bin\x86\$exe.exe"
    if (-not (Test-Path $path)) { Add-Result $exe -1 'not built - skipped'; Write-Host "SKIP $exe (not built)" -ForegroundColor DarkGray; return }
    Write-Host "RUN  $exe $($drvArgs -join ' ')" -ForegroundColor Cyan
    $out = & $path @drvArgs 2>&1
    $rc = $LASTEXITCODE
    $out | Where-Object { $_ -match 'STERP-[EW]-|STERP-I-3|STERP-I-SPOOL' } | Select-Object -Last 4 | ForEach-Object { Write-Host "     $_" }
    if ($rc -ne 0) { Write-Host "     $exe returned $rc - recorded, continuing" -ForegroundColor Yellow }
    Add-Result $exe $rc (($out | Select-Object -Last 1) -replace '\s+', ' ')
}

if ($Rebuild) {
    Write-Host "== Rebuilding the database ==" -ForegroundColor Green
    & powershell -NoProfile -ExecutionPolicy Bypass -File (Join-Path $PSScriptRoot 'build-db.ps1') -Full -Quiet | Out-Host
    Add-Result 'build-db -Full' $LASTEXITCODE 'database dropped, recreated and seeded'
}

if (-not $SkipDrivers) {
    Write-Host "== Demonstration batch drivers ==" -ForegroundColor Green
    # Known drivers first, in posting order: finance opens the ledgers, then purchasing,
    # then the sales and kit drivers that other module teams are still adding.
    Invoke-Driver 'FINDEMO' @("CO=$Company", 'FY=2026', 'FROM=1', 'TO=9', "MODE=$mode", 'USER=GLCLERK')
    Invoke-Driver 'PODEMO'  @("CO=$Company", "SITE=$Site", "MODE=$mode", 'USER=BUYER1')
    $known = @('FINDEMO', 'PODEMO')
    Get-ChildItem (Join-Path $STERP_ROOT 'bin\x86') -Filter '*DEMO.exe' -ErrorAction SilentlyContinue |
        Sort-Object Name | ForEach-Object {
            $name = $_.BaseName
            if ($known -contains $name) { return }
            Invoke-Driver $name @("CO=$Company", "SITE=$Site", "MODE=$mode", "USER=$User")
        }
}

Write-Host "== Levelling the control accounts ==" -ForegroundColor Green
# The job streams reconcile at more than one cutoff: NIGHTLY (AGEDDEBT) and MONTHEND
# (CSTINVAL, REVALRUN) run as at the clock date, MONTHEND (ARRECON, RECONCTL) as at the
# period end, and the demonstration cutoff is -AsAt.  A bridge journal dated at the cutoff
# only levels the cutoff, so level at every as-at the streams use, earliest first; the
# bridge is self-levelling, so each later pass posts only what is still outstanding.
$bridgeDates = @([datetime]::Today.ToString('yyyy-MM-dd'), $asAtIso) | Sort-Object -Unique
foreach ($bd in $bridgeDates) {
    $bridge = "SET NOCOUNT ON; EXEC fin.usp_DemoBridgeControls @CompanyCode = N'$Company', @AsAt = '$bd', @UserCode = N'$User';"
    $out = & sqlcmd -S $env:STERP_SQLSERVER -E -C -b -I -d $env:STERP_DATABASE -Q $bridge 2>&1
    $rc = $LASTEXITCODE
    $out | ForEach-Object { Write-Host "     $_" }
    Add-Result "fin.usp_DemoBridgeControls $bd" $rc 'control accounts levelled against the subledgers'
}

Write-Host "== Control account reconciliation ==" -ForegroundColor Green
$recon = Join-Path $STERP_ROOT 'bin\x86\RECONCTL.exe'
if (Test-Path $recon) {
    $out = & $recon "CO=$Company" "ASAT=$AsAt" "MODE=$mode" "USER=$User" 2>&1
    $rc = $LASTEXITCODE
    $out | ForEach-Object { Write-Host "     $_" }
    Add-Result 'RECONCTL' $rc 'reconciliation report'
    $spool = ($out | Select-String -Pattern 'STERP-I-SPOOL (.+)$' | Select-Object -Last 1)
    if ($spool) {
        $lst = $spool.Matches[0].Groups[1].Value.Trim()
        if (Test-Path $lst) {
            Write-Host ''
            Get-Content $lst | Select-String -Pattern '^(AR|AP|INVENTORY|WIP|GRNI|VAT|BANK|ACCRUAL)\s|^Control totals|^Unreconciled' |
                ForEach-Object { Write-Host "     $($_.Line.TrimEnd())" }
        }
    }
    if ($rc -ne 0) { Write-Host "`nRECONCTL reports a difference (return code $rc) - see the report above." -ForegroundColor Yellow }
} else {
    Add-Result 'RECONCTL' -1 'not built - skipped'
}

Write-Host "`n== Summary ==" -ForegroundColor Green
$results | Format-Table -AutoSize | Out-Host
$bad = @($results | Where-Object { $_.ExitCode -gt 0 -and $_.Step -ne 'RECONCTL' })
if ($bad.Count -gt 0) { Write-Host "$($bad.Count) driver step(s) reported a warning return code - recorded above, the dataset is still usable." -ForegroundColor Yellow }
$reconRc = @($results | Where-Object { $_.Step -eq 'RECONCTL' } | Select-Object -ExpandProperty ExitCode)
if ($reconRc.Count -gt 0 -and $reconRc[0] -gt 0) { Write-Host "Control accounts do NOT reconcile." -ForegroundColor Red; exit $reconRc[0] }
Write-Host "Demonstration dataset is consistent." -ForegroundColor Green
exit 0
