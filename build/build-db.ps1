<#
 STERP database deploy (SQL Server, Windows authentication, sqlcmd -C).
   build-db.ps1                deploy everything idempotently (create DB if missing)
   build-db.ps1 -Full          DROP and recreate the STERP database, then deploy everything
   build-db.ps1 -Only ar       deploy only script files whose relative path matches 'ar' (regex)
   build-db.ps1 -Tests         run db\tests\*.sql after deploying (use -NoDeploy to only test)
 Deployment order (each folder sorted by relative path):
   00-create-database.sql (master) -> 01-schemas.sql -> tables -> functions -> views
   -> procs -> triggers -> seed -> jobs (msdb)
 Every script must be re-runnable: tables guarded by IF OBJECT_ID(...) IS NULL,
 code objects use CREATE OR ALTER, seeds use MERGE / IF NOT EXISTS.
#>
param([switch]$Full, [string]$Only, [switch]$Tests, [switch]$NoDeploy, [switch]$NoSeed, [switch]$Quiet)
. "$PSScriptRoot\env.ps1"
$ErrorActionPreference = 'Continue'
Set-Location $STERP_ROOT
$db = Join-Path $STERP_ROOT 'db'
$failed = New-Object System.Collections.Generic.List[string]

function Run-File([System.IO.FileInfo]$f, [switch]$Master) {
    $rel = $f.FullName.Substring($db.Length + 1)
    if ($Only -and $rel -notmatch $Only) { return }
    if (-not $Quiet) { Write-Host "SQL $rel" }
    if ($Master) { $r = Invoke-Sql $f.FullName -NoDb } else { $r = Invoke-Sql $f.FullName }
    if ($r.ExitCode -ne 0) {
        $failed.Add("$rel`n$($r.Output)")
        Write-Host "  FAIL $rel" -ForegroundColor Red
        Write-Host $r.Output
    } elseif ($r.Output -and -not $Quiet -and $r.Output -match '(?i)warning|PRINT:') { Write-Host $r.Output -ForegroundColor DarkGray }
}

Invoke-WithBuildLock 'DB' {
    if (-not $NoDeploy) {
        if ($Full) {
            Write-Host "Dropping database $($env:STERP_DATABASE)" -ForegroundColor Yellow
            & sqlcmd -S $env:STERP_SQLSERVER -E -C -b -Q "IF DB_ID(N'$($env:STERP_DATABASE)') IS NOT NULL BEGIN ALTER DATABASE [$($env:STERP_DATABASE)] SET SINGLE_USER WITH ROLLBACK IMMEDIATE; DROP DATABASE [$($env:STERP_DATABASE)]; END" | Out-Host
        }
        Run-File (Get-Item (Join-Path $db '00-create-database.sql')) -Master
        if (Test-Path (Join-Path $db '01-schemas.sql')) { Run-File (Get-Item (Join-Path $db '01-schemas.sql')) }
        $folders = @('tables', 'functions', 'views', 'procs', 'triggers')
        if (-not $NoSeed) { $folders += 'seed' }
        foreach ($folder in $folders) {
            $p = Join-Path $db $folder
            if (-not (Test-Path $p)) { continue }
            Get-ChildItem $p -Recurse -Filter '*.sql' | Sort-Object { $_.FullName.Substring($p.Length).ToLower() } | ForEach-Object { Run-File $_ }
        }
        $jp = Join-Path $db 'jobs'
        if (Test-Path $jp) { Get-ChildItem $jp -Filter '*.sql' | Sort-Object Name | ForEach-Object { Run-File $_ -Master } }
    }
    if ($Tests) {
        $tp = Join-Path $db 'tests'
        $pass = 0; $fail = 0
        Get-ChildItem $tp -Recurse -Filter '*.sql' -ErrorAction SilentlyContinue | Sort-Object FullName | ForEach-Object {
            $rel = $_.FullName.Substring($db.Length + 1)
            if ($Only -and $rel -notmatch $Only) { return }
            $r = Invoke-Sql $_.FullName
            $lines = $r.Output -split "`n" | Where-Object { $_ -match '^(ok|not ok|#)' }
            $lines | ForEach-Object { Write-Host $_ }
            $pass += @($lines | Where-Object { $_ -match '^ok' }).Count
            $fail += @($lines | Where-Object { $_ -match '^not ok' }).Count
            if ($r.ExitCode -ne 0) { $fail++; $failed.Add("TEST $rel`n$($r.Output)"); Write-Host "  ERROR in $rel`n$($r.Output)" -ForegroundColor Red }
        }
        Write-Host "# SQL tests passed $pass failed $fail"
        if ($fail -gt 0 -and -not ($failed | Where-Object { $_ -like 'TEST*' })) { $failed.Add("SQL test failures: $fail") }
    }
}

if ($failed.Count) { Write-Host "`n==== $($failed.Count) DB ERROR(S) ====" -ForegroundColor Red; exit 1 }
Write-Host "DB deploy OK" -ForegroundColor Green
exit 0
