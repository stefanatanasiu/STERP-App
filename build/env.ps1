# STERP - Stefan's ERP build environment - dot-source from every build script:  . "$PSScriptRoot\env.ps1"
# Sets GnuCOBOL variables (equivalent of GC32-BDB-SP1\set_env.cmd) and common paths.
$ErrorActionPreference = 'Stop'

$global:STERP_ROOT = (Resolve-Path (Join-Path $PSScriptRoot '..')).Path
if ($env:STERP_GNUCOBOL) { $gc = $env:STERP_GNUCOBOL } else { $gc = Join-Path $STERP_ROOT '..\GC32-BDB-SP1' }
$global:STERP_GC = (Resolve-Path $gc).Path.TrimEnd('\') + '\'

$env:COB_MAIN_DIR   = $STERP_GC
$env:COB_CONFIG_DIR = "${STERP_GC}config"
$env:COB_COPY_DIR   = "${STERP_GC}copy"
$env:COB_CFLAGS     = "-I`"${STERP_GC}include`""
$env:COB_LDFLAGS    = "-L`"${STERP_GC}lib`""
$env:COB_LIBRARY_PATH = "${STERP_GC}extras"
$env:COB_EXIT_WAIT    = '0'   # unattended batch/tests: no keypress wait at STOP RUN after screen I/O
if (-not ($env:PATH -like "*$($STERP_GC)bin*")) { $env:PATH = "${STERP_GC}bin;$env:PATH" }

$global:STERP_BIN   = Join-Path $STERP_ROOT 'bin\x86'
$global:STERP_TBIN  = Join-Path $STERP_ROOT 'bin\tests'
$global:STERP_OBJ   = Join-Path $STERP_ROOT 'obj'
$global:STERP_RUN   = Join-Path $STERP_ROOT 'run'
foreach ($d in $STERP_BIN, $STERP_TBIN, $STERP_OBJ) { if (-not (Test-Path $d)) { New-Item -ItemType Directory -Force $d | Out-Null } }

# SQL Server connection (local default instance, Windows authentication)
if (-not $env:STERP_SQLSERVER) { $env:STERP_SQLSERVER = '.' }
if (-not $env:STERP_DATABASE)  { $env:STERP_DATABASE  = 'STERP' }
$env:STERP_ODBC = "Driver={ODBC Driver 18 for SQL Server};Server=$($env:STERP_SQLSERVER);Database=$($env:STERP_DATABASE);Trusted_Connection=yes;TrustServerCertificate=yes;MARS_Connection=yes;APP=STERP-COBOL"
$env:STERP_RUN = $STERP_RUN

# GnuCOBOL runtime DLLs that must ship next to STERP.exe / batch EXEs
$global:STERP_COB_RUNTIME = @('libcob-4.dll','libgcc_s_dw2-1.dll','libdb-18.1.dll','libgmp-10.dll','libintl-8.dll','libiconv-2.dll','pdcurses.dll')

# Named mutex so parallel builds (several developers / agents) do not collide
function Invoke-WithBuildLock([string]$Name, [scriptblock]$Body, [int]$TimeoutSec = 900) {
    $m = New-Object System.Threading.Mutex($false, "Global\STERP_BUILD_$Name")
    $got = $false
    try {
        try { $got = $m.WaitOne([TimeSpan]::FromSeconds($TimeoutSec)) } catch [System.Threading.AbandonedMutexException] { $got = $true }
        if (-not $got) { throw "Timed out waiting for build lock $Name" }
        & $Body
    } finally {
        if ($got) { $m.ReleaseMutex() }
        $m.Dispose()
    }
}

function Invoke-Sql([string]$File, [string]$Database = $env:STERP_DATABASE, [switch]$NoDb) {
    $sqlArgs = @('-S', $env:STERP_SQLSERVER, '-E', '-C', '-b', '-I', '-f', '65001', '-i', $File)
    if (-not $NoDb) { $sqlArgs += @('-d', $Database) }
    # transient login failures under machine load ("delay in opening server connection",
    # login timeout) are retried; genuine T-SQL errors are returned straight away
    for ($try = 1; $try -le 5; $try++) {
        $out = & sqlcmd @sqlArgs 2>&1
        $code = $LASTEXITCODE
        $txt = ($out -join "`n")
        if ($code -eq 0 -or $txt -notmatch '(?i)login process|opening server connection|Login timeout|unable to establish connection|TCP Provider') { break }
        Start-Sleep -Seconds (3 * $try)
    }
    return [pscustomobject]@{ ExitCode = $code; Output = $txt }
}

function Invoke-SqlQuery([string]$Query, [string]$Database = $env:STERP_DATABASE) {
    $out = & sqlcmd -S $env:STERP_SQLSERVER -E -C -b -I -d $Database -W -s '|' -Q $Query 2>&1
    return [pscustomobject]@{ ExitCode = $LASTEXITCODE; Output = ($out -join "`n") }
}
