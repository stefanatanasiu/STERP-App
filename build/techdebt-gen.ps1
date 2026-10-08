<#
 STERP technical-debt register generator.
 Aggregates the hand-written per-module notes in docs\tech-debt\*.md into
 docs\TECH-DEBT-REGISTER.md: a summary table (module, item count, categories) followed by
 every module's items.  An item is either a row of a tech-debt table (column headings are
 mapped onto Where / Pattern / Why / Modernisation, whatever the module called them) or a
 prose section that carries no table.
   techdebt-gen.ps1            write docs\TECH-DEBT-REGISTER.md
   techdebt-gen.ps1 -NoWrite   print the summary only
   techdebt-gen.ps1 -Strict    exit 1 if a module in cobol\DIALECTS.cfg has no tech-debt file
#>
param([switch]$NoWrite, [switch]$Strict)
. "$PSScriptRoot\env.ps1"
Set-Location $STERP_ROOT

$CATS = @('COBOL', 'SQL', 'Seed data', '.NET', 'C bridge', 'Build/jobs', 'Process/data')

function Get-Category([string]$where, [string]$rest) {
    foreach ($t in @($where, "$where $rest")) {
        if ($t -match '(?i)db\\seed|\\seed\\|seed script|demo data|demonstration data') { return 'Seed data' }
        if ($t -match '(?i)dotnet\\|\.vb\b|\.cs\b|WinForms|frm[A-Z]|CobolGateway|SqlHelper') { return '.NET' }
        if ($t -match '(?i)\bc\\sterp|sterpbrg|sterpsql|\.c\b|\bbridge\b') { return 'C bridge' }
        if ($t -match '(?i)build\\|jobs\\|\.ps1\b|\.cmd\b|\bJCL\b|build script') { return 'Build/jobs' }
        if ($t -match '(?i)cobol\\|\.cob\b|\.cpy\b|COBOL|COMMAREA|WORKING-STORAGE|Report Writer|copybook') { return 'COBOL' }
        if ($t -match '(?i)db\\|\.sql\b|usp_|\btrigger\b|\bview\b|(core|fin|ar|ap|so|po|inv|cst|bom|kit|rpt|stg)\.[A-Za-z]') { return 'SQL' }
    }
    return 'Process/data'
}

# column heading -> canonical slot
function Get-Slot([string]$head) {
    $h = $head.Trim().ToLower()
    if ($h -eq '#' -or $h -eq 'no' -or $h -eq '') { return '' }
    if ($h -match 'modernis') { return 'Hint' }
    if ($h -match 'why|story|reason') { return 'Why' }
    if ($h -match 'file|where|object|program') { return 'Where' }
    if ($h -match 'pattern|what it is|what') { return 'Pattern' }
    return 'Why'
}

function Split-Row([string]$line) {
    $t = $line.Trim().TrimStart('|').TrimEnd('|')
    return @($t -split '(?<!\\)\|' | ForEach-Object { $_.Trim() })
}

function Clip([string]$s, [int]$max) {
    $s = ($s -replace '\s+', ' ').Trim()
    if ($s.Length -gt $max) { return $s.Substring(0, $max - 3).TrimEnd() + '...' }
    return $s
}

# A prose section without a table: every top-level bullet is one item, otherwise the
# section itself is the item.  Bullets are written "<where> - <what>. Why ... Modernisation: ..."
function Get-ProseItems([string]$head, [string[]]$body) {
    $bullets = @(); $cur = $null
    foreach ($l in $body) {
        if ($l -match '^[*-]\s+\S') { if ($cur) { $bullets += , $cur }; $cur = @($l) }
        elseif ($l.Trim() -eq '') { if ($cur) { $bullets += , $cur; $cur = $null } }
        elseif ($cur) { $cur += $l }
    }
    if ($cur) { $bullets += , $cur }

    $out = @()
    foreach ($b in $bullets) {
        $t = (($b -join ' ') -replace '^\s*[*-]\s+', '')
        $t = ($t -replace '\s+', ' ').Trim()
        $hint = ''
        if ($t -match '(?i)^(.*?)[*_]*Modernis(?:ation|e)[^*_:]{0,24}[*_]*:\s*(.+)$') { $t = $Matches[1]; $hint = $Matches[2] }
        $why = $t; $pattern = ''
        if ($t -match '(?i)^(.*?)[*_]*Why it is (?:debt|there)[^*_:]{0,12}[*_]*:\s*(.+)$') { $pattern = $Matches[1]; $why = $Matches[2] }
        $where = ''
        if ($t -match '`([^`]+)`') { $where = '`' + $Matches[1] + '`' }
        elseif ($t -match '\*\*([^*]+)\*\*') { $where = $Matches[1] }
        else { $where = Clip $t 70 }
        $out += [pscustomobject]@{ Where = "$where ($head)"; Pattern = (Clip $pattern 500); Why = (Clip $why 500)
                                   Hint = (Clip $hint 400); Category = (Get-Category $where $t); Prose = $false }
    }
    if ($out.Count -eq 0) {
        $txt = Clip (($body | Where-Object { $_.Trim() -ne '' }) -join ' ') 500
        if ($txt) { $out += [pscustomobject]@{ Where = $head; Pattern = ''; Why = $txt; Hint = ''
                                               Category = (Get-Category $head $txt); Prose = $false } }
    }
    return $out
}

function Read-Module([IO.FileInfo]$file) {
    $lines = Get-Content $file.FullName -Encoding UTF8
    $title = ($lines | Where-Object { $_ -match '^#\s+' } | Select-Object -First 1)
    if ($title) { $title = ($title -replace '^#\s+', '').Trim() } else { $title = $file.BaseName }
    $items = @()
    $slots = @(); $prose = @(); $proseHead = ''; $sectionHadTable = $false

    for ($i = 0; $i -lt $lines.Count; $i++) {
        $l = $lines[$i]
        if ($l -match '^(#{2,})\s+(.*)$') {
            if ($proseHead -and -not $sectionHadTable) { $items += Get-ProseItems $proseHead $prose }
            $proseHead = $Matches[2].Trim(); $prose = @(); $sectionHadTable = $false; $slots = @()
            continue
        }
        if ($l -match '^\s*\|') {
            $cells = Split-Row $l
            if ($l -match '^\s*\|[\s:-]*\|') { continue }                      # separator
            if ($slots.Count -eq 0) { $slots = @($cells | ForEach-Object { Get-Slot $_ }); continue }   # heading
            $sectionHadTable = $true
            $v = @{ Where = ''; Pattern = ''; Why = ''; Hint = '' }
            for ($c = 0; $c -lt $cells.Count; $c++) {
                $s = if ($c -lt $slots.Count) { $slots[$c] } else { 'Why' }
                if (-not $s) { continue }
                if ($v[$s]) { $v[$s] += ' ' + $cells[$c] } else { $v[$s] = $cells[$c] }
            }
            if (($v.Where + $v.Pattern + $v.Why + $v.Hint).Trim() -eq '') { continue }
            $items += [pscustomobject]@{ Where = $v.Where; Pattern = $v.Pattern; Why = $v.Why; Hint = $v.Hint
                                         Category = (Get-Category $v.Where "$($v.Pattern) $($v.Why)"); Prose = $false }
            continue
        }
        if ($slots.Count -gt 0 -and $l.Trim() -eq '') { $slots = @() }          # table finished
        if ($proseHead) { $prose += $l }
    }
    if ($proseHead -and -not $sectionHadTable) { $items += Get-ProseItems $proseHead $prose }
    return [pscustomobject]@{ Code = $file.BaseName; Title = $title; Items = $items
                              Rel = 'docs/tech-debt/' + $file.Name }
}

$mods = @()
foreach ($f in Get-ChildItem (Join-Path $STERP_ROOT 'docs\tech-debt') -Filter '*.md' | Sort-Object Name) {
    $mods += Read-Module $f
}
$total = ($mods | ForEach-Object { $_.Items.Count } | Measure-Object -Sum).Sum

function Cat-Summary($items) {
    $parts = @()
    foreach ($c in $CATS) {
        $n = @($items | Where-Object { $_.Category -eq $c }).Count
        if ($n) { $parts += "$c $n" }
    }
    return ($parts -join ', ')
}
function Esc([string]$s) { return ($s -replace '\|', '\|').Trim() }

$md = New-Object System.Text.StringBuilder
[void]$md.AppendLine('# STERP - Stefan''s ERP: technical debt register')
[void]$md.AppendLine('')
[void]$md.AppendLine("Generated by ``build\techdebt-gen.ps1`` on $(Get-Date -Format 'dd/MM/yyyy HH:mm') from ``docs\tech-debt\*.md``. Do not edit by hand - edit the module file and re-run the generator.")
[void]$md.AppendLine('')
[void]$md.AppendLine('STERP is a modernisation demonstration asset, so most of what follows is **deliberate**: each')
[void]$md.AppendLine('item is a working, tested piece of the system that also shows a real legacy pattern, together')
[void]$md.AppendLine('with the shape of the modernisation it invites. Items are classified by the artefact they live')
[void]$md.AppendLine('in, not by severity.')
[void]$md.AppendLine('')
[void]$md.AppendLine("**$total items across $($mods.Count) module files.**")
[void]$md.AppendLine('')
[void]$md.AppendLine('## Summary')
[void]$md.AppendLine('')
[void]$md.AppendLine('| Module | Source | Items | Categories |')
[void]$md.AppendLine('|---|---|---:|---|')
foreach ($m in $mods) {
    $anchor = '#' + ($m.Code.ToLower())
    [void]$md.AppendLine("| [``$($m.Code)``]($anchor) | [``$($m.Rel)``]($($m.Rel -replace '^docs/', '')) | $($m.Items.Count) | $(Cat-Summary $m.Items) |")
}
[void]$md.AppendLine("| **Total** | | **$total** | $(Cat-Summary ($mods | ForEach-Object { $_.Items })) |")
[void]$md.AppendLine('')
[void]$md.AppendLine('## Items by module')

foreach ($m in $mods) {
    [void]$md.AppendLine('')
    [void]$md.AppendLine("### $($m.Code)")
    [void]$md.AppendLine('')
    [void]$md.AppendLine("$($m.Title) - source [``$($m.Rel)``]($($m.Rel -replace '^docs/', '')), $($m.Items.Count) items.")
    [void]$md.AppendLine('')
    $rows = @($m.Items)
    if ($rows.Count) {
        [void]$md.AppendLine('| # | Category | Where | Pattern | Why it is debt | Modernisation hint |')
        [void]$md.AppendLine('|---:|---|---|---|---|---|')
        $n = 0
        foreach ($it in $rows) {
            $n++
            [void]$md.AppendLine("| $n | $($it.Category) | $(Esc $it.Where) | $(Esc $it.Pattern) | $(Esc $it.Why) | $(Esc $it.Hint) |")
        }
    }
}

$text = $md.ToString()
if (-not $NoWrite) { [IO.File]::WriteAllText((Join-Path $STERP_ROOT 'docs\TECH-DEBT-REGISTER.md'), $text) }

$mods | ForEach-Object { [pscustomobject]@{ Module = $_.Code; Items = $_.Items.Count; Categories = (Cat-Summary $_.Items) } } |
    Format-Table -AutoSize | Out-Host
Write-Host "Technical debt items: $total in $($mods.Count) module files"

# every COBOL module should keep a tech-debt file (plus net.md and common.md, which have no dialect)
$declared = @(Get-Content (Join-Path $STERP_ROOT 'cobol\DIALECTS.cfg') |
    Where-Object { $_ -match '^([a-z]+)=' } | ForEach-Object { ($_ -split '=')[0] })
$missing = @($declared | Where-Object { $_ -notin $mods.Code })
if ($missing) { Write-Host "No tech-debt file for: $($missing -join ', ')" -ForegroundColor Yellow }
if ($Strict -and $missing) { exit 1 }
exit 0
