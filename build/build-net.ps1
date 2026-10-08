<#
 STERP .NET build (legacy non-SDK projects, .NET Framework 4.8, x86) using MSBuild from the dotnet SDK.
   build-net.ps1               Release|x86 build of dotnet\STERP.sln into bin\x86
   build-net.ps1 -Debug        Debug configuration
#>
param([switch]$Debug, [switch]$Rebuild)
. "$PSScriptRoot\env.ps1"
$ErrorActionPreference = 'Continue'
Set-Location $STERP_ROOT
$cfg = if ($Debug) { 'Debug' } else { 'Release' }
$target = if ($Rebuild) { 'Rebuild' } else { 'Build' }
$code = 0
Invoke-WithBuildLock 'NET' {
    $out = & dotnet msbuild 'dotnet\STERP.sln' /nologo /v:m /m:1 "/t:$target" "/p:Configuration=$cfg" '/p:Platform=x86' 2>&1 | ForEach-Object { "$_" }
    $script:code = $LASTEXITCODE
    $out | Where-Object { $_ -match 'error|warning BC|warning CS|->' } | Select-Object -First 200 | ForEach-Object {
        if ($_ -match ' error ') { Write-Host $_ -ForegroundColor Red } else { Write-Host $_ } }
}
if ($code -ne 0) { Write-Host ".NET build FAILED" -ForegroundColor Red; exit 1 }
# app config copy for the runtime folder
Write-Host ".NET build OK" -ForegroundColor Green
exit 0
