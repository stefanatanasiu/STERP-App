# ui-smoke.ps1 - unattended screen sweep of the STERP client.
#
#   Runs bin\x86\STERP.exe /qa-screens <OutDir>.  The client signs on as SYSADM, opens
#   every core.MenuItem FormClass in turn, writes one PNG per screen and a tab separated
#   inspection report (qa-report.tsv) covering the COM-024 classic-look rules.
#
#   STERP.exe holds bin\x86\STERPCOB.dll open, so the script ALWAYS stops the process by
#   PID - on success, on failure and on Ctrl+C.
#
#   Examples
#     powershell -NoProfile -ExecutionPolicy Bypass -File build\ui-smoke.ps1
#     powershell -NoProfile -ExecutionPolicy Bypass -File build\ui-smoke.ps1 -OutDir run\work\screens -TimeoutSeconds 600
param(
    [string]$OutDir = 'run\work\screens',
    [string]$Company = '',
    [int]$TimeoutSeconds = 600,
    [switch]$NoBuild
)

$ErrorActionPreference = 'Stop'
$root = Split-Path -Parent $PSScriptRoot
$exe = Join-Path $root 'bin\x86\STERP.exe'
if (-not [System.IO.Path]::IsPathRooted($OutDir)) { $OutDir = Join-Path $root $OutDir }

if (-not $NoBuild) {
    Write-Host "Building the client..."
    & powershell -NoProfile -ExecutionPolicy Bypass -File (Join-Path $root 'build\build-net.ps1') |
        Select-String -Pattern 'error|Build succeeded|Build FAILED' | Select-Object -Last 8
    if ($LASTEXITCODE -ne 0) { throw "build-net.ps1 failed with exit code $LASTEXITCODE" }
}
if (-not (Test-Path $exe)) { throw "client not built: $exe" }

New-Item -ItemType Directory -Force -Path $OutDir | Out-Null
Get-ChildItem $OutDir -Filter *.png -ErrorAction SilentlyContinue | Remove-Item -Force
$tsv = Join-Path $OutDir 'qa-report.tsv'
Remove-Item $tsv -Force -ErrorAction SilentlyContinue

Write-Host "Sweeping screens into $OutDir ..."
$qaArgs = @('/qa-screens', "`"$OutDir`"")
if ($Company) { $qaArgs += $Company }
$proc = Start-Process -FilePath $exe -ArgumentList $qaArgs `
                      -WorkingDirectory (Split-Path -Parent $exe) -PassThru
$pid_ = $proc.Id
Write-Host "  STERP.exe PID $pid_"
try {
    if (-not $proc.WaitForExit($TimeoutSeconds * 1000)) {
        Write-Warning "sweep did not finish inside $TimeoutSeconds s - stopping PID $pid_"
    }
} finally {
    # STERPCOB.dll must be released for every other build in the tree
    $live = Get-Process -Id $pid_ -ErrorAction SilentlyContinue
    if ($live) { Stop-Process -Id $pid_ -Force; Start-Sleep -Milliseconds 500 }
    Write-Host "  STERP.exe PID $pid_ stopped"
}

if (-not (Test-Path $tsv)) { throw "no qa-report.tsv was produced - see run\logs for the client log" }

$rows = Import-Csv $tsv -Delimiter "`t"
$shots = @(Get-ChildItem $OutDir -Filter *.png)
$fail = @($rows | Where-Object Kind -eq 'FAIL')
$defect = @($rows | Where-Object Kind -eq 'DEFECT')

Write-Host ""
Write-Host ("screens captured : {0}" -f $shots.Count)
Write-Host ("forms opened     : {0}" -f @($rows | Where-Object Kind -eq 'OK').Count)
Write-Host ("forms failed     : {0}" -f $fail.Count)
Write-Host ("defects reported : {0}" -f $defect.Count)
if ($fail.Count) {
    Write-Host ""
    Write-Host "FAILURES"
    $fail | ForEach-Object { "  {0} {1}: {2}" -f $_.FormClass, $_.Argument, $_.Detail }
}
if ($defect.Count) {
    Write-Host ""
    Write-Host "DEFECTS BY KIND"
    $defect | Group-Object { ($_.Detail -split ':')[0] } | Sort-Object Count -Descending |
        ForEach-Object { "  {0,-10} {1,4}" -f $_.Name, $_.Count }
}
Write-Host ""
Write-Host "report: $tsv"
exit $(if ($fail.Count) { 1 } else { 0 })
