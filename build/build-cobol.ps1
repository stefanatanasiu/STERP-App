<#
 STERP COBOL build.
   build-cobol.ps1                  build everything (C shim, all online modules, DLL, batch, tests)
   build-cobol.ps1 -Module ar,fin   compile only those modules' online/batch/tests (plus C shim)
   build-cobol.ps1 -Module ar -NoDll   skip relinking STERPCOB.DLL (useful while the client is running)
   build-cobol.ps1 -Clean           remove obj\cobol before building
 Output:
   obj\cobol\online\<mod>\*.o, obj\cobol\lib\libst_<mod>.a
   bin\x86\STERPCOB.dll (online engine + COMMAREA bridge), bin\x86\<BATCH>.exe
   bin\tests\<TEST>.exe
 Exit code 0 = success, 1 = one or more compile/link errors (listed at the end).
#>
param([string[]]$Module, [switch]$NoDll, [switch]$NoBatch, [switch]$NoTests, [switch]$Clean, [switch]$Quiet)
. "$PSScriptRoot\env.ps1"
$ErrorActionPreference = 'Continue'
Set-Location $STERP_ROOT

$dialect = @{}
Get-Content 'cobol\DIALECTS.cfg' | Where-Object { $_ -match '^\s*([a-z]+)\s*=\s*(\S+)' } | ForEach-Object {
    if ($_ -match '^\s*([a-z]+)\s*=\s*(\S+)') { $dialect[$Matches[1]] = $Matches[2] } }
$allMods = @('common','fin','ar','ap','so','po','cst','itm','kit','bom','xmd')
if ($Module) { $mods = @($Module | ForEach-Object { $_.ToLower() }) } else { $mods = $allMods }
if ($Clean) { Remove-Item -Recurse -Force 'obj\cobol' -ErrorAction SilentlyContinue }
$errors = New-Object System.Collections.Generic.List[string]

function Std([string]$mod) { if ($dialect.ContainsKey($mod)) { $dialect[$mod] } else { 'default' } }
function Rel([string]$p) { $p.Substring($STERP_ROOT.Length + 1).Replace('\', '/') }
function Run-Cobc([string[]]$a, [string]$what) {
    $out = & cmd /c "cobc $(($a | ForEach-Object { if ($_ -match '\s') { '"' + $_ + '"' } else { $_ } }) -join ' ') 2>&1"
    $ok = ($LASTEXITCODE -eq 0)
    $txt = ($out | Out-String).Trim()
    if (-not $ok) { $errors.Add("$what`n$txt") ; Write-Host "  FAIL $what" -ForegroundColor Red; if (-not $Quiet) { Write-Host $txt } }
    elseif ($txt -and -not $Quiet -and $txt -match 'warning') { Write-Host $txt -ForegroundColor DarkYellow }
    return $ok
}
$common = @('-fstatic-call', '-I', 'cobol/copy', '-fno-trunc', '-Wno-others')

# ---------------------------------------------------------------- C shim
New-Item -ItemType Directory -Force 'obj\c' | Out-Null
if (-not (Test-Path 'obj\c\sterpsql.o') -or (Get-Item 'c\sterpsql.c').LastWriteTime -gt (Get-Item 'obj\c\sterpsql.o').LastWriteTime) {
    Write-Host "C   sterpsql.c"
    $null = Run-Cobc @('-c', '-o', 'obj/c/sterpsql.o', 'c/sterpsql.c') 'c/sterpsql.c'
}

# ---------------------------------------------------------------- online modules
foreach ($mod in $mods) {
    $src = Join-Path $STERP_ROOT "cobol\online\$mod"
    if (-not (Test-Path $src)) { continue }
    $od = Join-Path $STERP_OBJ "cobol\online\$mod"
    New-Item -ItemType Directory -Force $od | Out-Null
    $std = Std $mod
    $files = Get-ChildItem $src -Filter '*.cob'
    foreach ($f in $files) {
        $o = Join-Path $od ($f.BaseName + '.o')
        if ((Test-Path $o) -and (Get-Item $o).LastWriteTime -ge $f.LastWriteTime) { continue }
        Write-Host "COB [$mod/$std] $($f.Name)"
        $null = Run-Cobc ($common + @("-std=$std", '-c', '-o', (Rel $o), (Rel $f.FullName))) "online/$mod/$($f.Name)"
    }
    # remove objects whose source has gone
    Get-ChildItem $od -Filter '*.o' | Where-Object { -not (Test-Path (Join-Path $src ($_.BaseName + '.cob'))) } | Remove-Item -Force
    New-Item -ItemType Directory -Force 'obj\cobol\lib' | Out-Null
    $lib = "obj/cobol/lib/libst_$mod.a"
    $objs = @(Get-ChildItem $od -Filter '*.o' | ForEach-Object { Rel $_.FullName })
    if ($objs) {
        # build into a private temp archive, then swap it in atomically so concurrent
        # builds of other modules never link against a missing/half-written archive
        $tmpLib = "obj/cobol/lib/libst_$mod.$PID.tmp"
        Remove-Item $tmpLib -ErrorAction SilentlyContinue
        & ar rcs $tmpLib @objs 2>&1 | Out-Null
        Invoke-WithBuildLock "LIB_$mod" { Move-Item $tmpLib $lib -Force }
    }
}

# archives of all modules (whatever has been built so far) for static linking of EXEs
function Lib-Group {
    $libs = @(Get-ChildItem 'obj\cobol\lib' -Filter 'libst_*.a' -ErrorAction SilentlyContinue | ForEach-Object { Rel $_.FullName })
    $g = '-Wl,--start-group,' + ((@($libs) + @('obj/c/sterpsql.o')) -join ',') + ',--end-group'
    return @('-Q', $g, '-lodbc32')
}

# Sources that define user-defined FUNCTION-IDs. libcob always resolves these at
# run time (cob_resolve_func), even with -fstatic-call, so nothing in the archive
# refers to them and the linker never extracts the member. Their objects are
# therefore put on the link line explicitly.
function Func-Objs {
    $o = @()
    foreach ($m in $allMods) {
        $src = Join-Path $STERP_ROOT "cobol\online\$m"
        if (-not (Test-Path $src)) { continue }
        foreach ($f in Get-ChildItem $src -Filter '*.cob') {
            if ((Get-Content $f.FullName -Raw) -match '(?im)^\s*FUNCTION-ID\s*\.') {
                $p = Join-Path $STERP_OBJ "cobol\online\$m\$($f.BaseName).o"
                if (Test-Path $p) { $o += (Rel $p) }
            }
        }
    }
    return $o
}

# ---------------------------------------------------------------- program directory + DLL
function Build-Dll {
    $entries = @()
    foreach ($m in $allMods) {
        $src = Join-Path $STERP_ROOT "cobol\online\$m"
        if (-not (Test-Path $src)) { continue }
        foreach ($f in Get-ChildItem $src -Filter '*.cob') {
            $t = Get-Content $f.FullName -Raw
            if ($t -match '(?is)PROCEDURE\s+DIVISION\s+USING\s+DFHCOMMAREA' -and $t -match '(?im)PROGRAM-ID\.\s*"?([A-Z0-9]+)"?') {
                if (Test-Path (Join-Path $STERP_OBJ "cobol\online\$m\$($f.BaseName).o")) { $entries += $Matches[1].ToUpper() }
            }
        }
    }
    $entries = $entries | Sort-Object -Unique
    $h = New-Object System.Text.StringBuilder
    [void]$h.AppendLine('/* GENERATED by build\build-cobol.ps1 - DO NOT EDIT */')
    foreach ($e in $entries) { [void]$h.AppendLine("extern int $e(unsigned char *);") }
    [void]$h.AppendLine('static struct { const char *name; cobprog fn; } PROGDIR[] = {')
    foreach ($e in $entries) { [void]$h.AppendLine("    { `"$e`", (cobprog)$e },") }
    [void]$h.AppendLine('    { NULL, NULL } };')
    New-Item -ItemType Directory -Force 'obj\gen' | Out-Null
    [IO.File]::WriteAllText((Join-Path $STERP_OBJ 'gen\sterpdir.h'), $h.ToString())
    $allObjs = Get-ChildItem 'obj\cobol\online' -Recurse -Filter '*.o' | ForEach-Object { Rel $_.FullName }
    Write-Host "DLL STERPCOB.dll ($($entries.Count) LINKable programs, $($allObjs.Count) objects)"
    Invoke-WithBuildLock 'DLL' {
        $tmp = 'obj/STERPCOB.tmp.dll'
        $ok = Run-Cobc (@('-b', '-fstatic-call', '-A', '-Iobj/gen', '-o', $tmp, 'c/sterpbrg.c') + $allObjs + @('obj/c/sterpsql.o', '-lodbc32')) 'link STERPCOB.dll'
        if ($ok) {
            try { Copy-Item $tmp (Join-Path $STERP_BIN 'STERPCOB.dll') -Force }
            catch { $errors.Add("STERPCOB.dll is locked (client running?) - new DLL left in obj\STERPCOB.tmp.dll") }
        }
    }
}
if (-not $NoDll) { Build-Dll }

# ---------------------------------------------------------------- runtime DLLs
foreach ($d in $STERP_COB_RUNTIME) {
    $t = Join-Path $STERP_BIN $d
    if (-not (Test-Path $t)) { Copy-Item (Join-Path "${STERP_GC}bin" $d) $t }
    $t2 = Join-Path $STERP_TBIN $d
    if (-not (Test-Path $t2)) { Copy-Item (Join-Path "${STERP_GC}bin" $d) $t2 }
}

# ---------------------------------------------------------------- batch EXEs and test drivers
function Build-Exes([string]$kind, [string]$outDir) {
    foreach ($mod in $mods + @('tui')) {
        $src = if ($kind -eq 'tui') { $null } else { Join-Path $STERP_ROOT "cobol\$kind\$mod" }
        if ($mod -eq 'tui') { if ($kind -ne 'batch') { continue }; $src = Join-Path $STERP_ROOT 'cobol\tui' }
        if (-not (Test-Path $src)) { continue }
        $std = Std $mod
        foreach ($f in Get-ChildItem $src -Filter '*.cob') {
            $exe = Join-Path $outDir ($f.BaseName.ToUpper() + '.exe')
            if ((Test-Path $exe) -and (Get-Item $exe).LastWriteTime -ge $f.LastWriteTime -and -not $Clean) {
                $newest = Get-ChildItem 'obj\cobol\lib' -Filter '*.a' -ErrorAction SilentlyContinue | Sort-Object LastWriteTime -Descending | Select-Object -First 1
                if (-not $newest -or $newest.LastWriteTime -le (Get-Item $exe).LastWriteTime) { continue }
            }
            Write-Host "EXE [$kind/$mod] $($f.Name)"
            $null = Run-Cobc ($common + @("-std=$std", '-x', '-o', (Rel $exe), (Rel $f.FullName)) + (Func-Objs) + (Lib-Group)) "$kind/$mod/$($f.Name)"
        }
    }
}
if (-not $NoBatch) { Build-Exes 'batch' $STERP_BIN }
if (-not $NoTests) { Build-Exes 'tests' $STERP_TBIN }

# copy copybooks next to the client - CobolInterop parses COMMAREA layouts at run time
New-Item -ItemType Directory -Force (Join-Path $STERP_BIN 'copy') | Out-Null
Copy-Item 'cobol\copy\*.cpy' (Join-Path $STERP_BIN 'copy') -Force -ErrorAction SilentlyContinue

if ($errors.Count) {
    Write-Host "`n==== $($errors.Count) COBOL BUILD ERROR(S) ====" -ForegroundColor Red
    $errors | ForEach-Object { Write-Host "---- $_" }
    exit 1
}
Write-Host "COBOL build OK" -ForegroundColor Green
exit 0
