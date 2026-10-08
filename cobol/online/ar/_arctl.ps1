# Renames the bare STCAHDR references in an AR source to the AR-CTL
# working-storage control fields.  Qualified references (CA-xxx OF ...)
# are left alone.  Used once per new AR program while it is drafted.
param([Parameter(Mandatory=$true)][string[]]$Path)

$map = @{
  'CA-RETURN-CODE'   = 'AR-RC'
  'CA-REASON-CODE'   = 'AR-REASON'
  'CA-MESSAGE'       = 'AR-MSG'
  'CA-MSG-COUNT'     = 'AR-MSG-CNT'
  'CA-FUNCTION'      = 'AR-FUNC'
  'CA-COMPANY'       = 'AR-CO'
  'CA-SITE'          = 'AR-SITE'
  'CA-USER'          = 'AR-USERID'
  'CA-BUSINESS-DATE' = 'AR-BUSDATE'
  'CA-CORRELATION-ID'= 'AR-CORREL'
  'CA-REQ-REF'       = 'AR-REQ-REF'
  'CA-FAILED'        = 'AR-RC-BAD'
  'CA-SEVERE'        = 'AR-RC-SEVERE'
  'CA-OK'            = 'AR-RC-OK'
  'CA-WARNING'       = 'AR-RC-WARN'
  'CA-HOLD'          = 'AR-RC-HOLD'
  'CA-ERROR'         = 'AR-RC-ERROR'
  'CA-NOT-FOUND'     = 'AR-RC-NOTFOUND'
}
foreach ($p in $Path) {
  $t = Get-Content -Raw -LiteralPath $p
  foreach ($k in ($map.Keys | Sort-Object -Property Length -Descending)) {
    $t = [regex]::Replace($t, "\b$k\b(?!\s+OF\s)", $map[$k])
  }
  Set-Content -LiteralPath $p -Value $t -NoNewline
  Write-Output "rewrote $p"
}
