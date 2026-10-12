Imports STERP.Business
Imports STERP.Business.Bom
Imports STERP.CobolInterop
Imports STERP.Common

''' <summary>
''' Explosion enquiry: one calculation, four readings.  Indented shows the tree as the engineer
''' reads it, single level shows only what the parent itself calls for, leaf shows the consolidated
''' material to buy or make, costed shows the value roll-up and netted shows what is still to make
''' once eligible stock and inbound supply have been consumed once.
''' The arithmetic is entirely BOMEXPL / BOMLEAF / BOMCOST / BOMNET; this screen only asks.
''' REQ: BOM-012, BOM-014, BOM-015, BOM-017, BOM-019, BOM-020, BOM-021, BOM-025, BOM-032
''' </summary>
Public Class frmBomExplosion
    Inherits ListForm

    Private ReadOnly _svc As New BomService()
    Private _txtItem As TextBox
    Private _txtRev As TextBox
    Private _txtQty As TextBox
    Private _txtAsAt As TextBox
    Private _cboView As ComboBox
    Private _txtCostSet As TextBox

    Public Sub New()
        Me.Text = "Bill of Material Explosion"
        Me.ClientSize = New Size(980, 500)
    End Sub

    Protected Overrides Sub BuildFilters()
        _txtItem = FilterEdit("&Parent item:", 170)
        _txtRev = FilterEdit("Re&v:", 40)
        _txtQty = FilterEdit("&Quantity:", 80)
        _txtAsAt = FilterEdit("&As at:", 80)
        _cboView = FilterList("&View:", 110, "INDENTED", "SINGLE", "LEAF", "COSTED", "NETTED")
        _txtCostSet = FilterEdit("Cost &set:", 90)
        _txtQty.Text = "1"
        _txtAsAt.Text = Fmt.Date(SterpSession.Current.BusinessDate)
        _txtCostSet.Text = "BOMDEMO"
        If FormArgument <> "" AndAlso FormArgument <> "COMPARE" Then _txtItem.Text = FormArgument
        If FormArgument = "COMPARE" Then
            Me.Text = "Bill Revision Comparison"
            _cboView.SelectedIndex = 0
        End If
        AddHandler _cboView.SelectedIndexChanged, Sub(s As Object, e As EventArgs) LoadData()
    End Sub

    Protected Overrides Sub BuildTools()
        AddSeparator()
        AddTool("Explode (F10)", "ENGINE", AddressOf ExplodeClicked)
        AddTool("Where used", "FIND", AddressOf WhereUsedClicked)
        AddTool("Compare revisions", "RUN", AddressOf CompareClicked)
        AddSeparator()
    End Sub

    Protected Overrides Sub LoadData()
        If Item() = "" Then
            Populate(Nothing)
            SetStatus("Enter a parent item, a quantity and a date, then press F10 to explode.")
            Return
        End If
        ExplodeClicked(Nothing, EventArgs.Empty)
    End Sub

    Protected Overrides Sub OnConfirm()
        ExplodeClicked(Nothing, EventArgs.Empty)
    End Sub

    ''' <summary>The engine owns the calculation; the view only decides which reading is shown.</summary>
    Private Sub ExplodeClicked(ByVal sender As Object, ByVal e As EventArgs)
        Populate(Nothing)
        If Item() = "" Then
            SetStatus("Enter a parent item code first.", "!  WARNING")
            Return
        End If
        Dim quantity As Decimal
        If Not Decimal.TryParse(_txtQty.Text.Trim(), Globalization.NumberStyles.Number, Fmt.UK, quantity) OrElse
           quantity <= 0D Then
            SetStatus("Enter a positive numeric quantity before exploding the bill.", "!  WARNING")
            Return
        End If
        Dim asAt As Date
        If Not TryAsAt(asAt) Then Return
        Guard(Sub()
                  Dim rows As DataTable = Nothing
                  Dim view As String = Convert.ToString(_cboView.SelectedItem)
                  Dim r As CobolResult
                  Select Case view
                      Case "NETTED"
                          r = _svc.NettedPlan(Item(), quantity, asAt, rows)
                      Case "COSTED"
                          r = _svc.CostedExplosion(Item(), Rev(), quantity, asAt, _txtCostSet.Text.Trim().ToUpperInvariant(), rows)
                      Case Else
                          r = _svc.Explode(Item(), Rev(), quantity, asAt, view, 0, rows)
                  End Select
                  If Not ShowResult(r) Then Return
                  If rows IsNot Nothing Then Populate(rows)
                  Report(view)
              End Sub)
    End Sub

    Private Sub Report(ByVal view As String)
        Dim cue As String = If(_svc.Completeness = "COMPLETE", "OK", "!  WARNING")
        Select Case view
            Case "NETTED"
                SetStatus("Netted plan " & _svc.Completeness & " | maximum buildable " &
                          Fmt.Qty(_svc.MaxBuildable, "") & " | " & _svc.Shortages & " shortage(s).", cue)
            Case "COSTED"
                SetStatus("Costed roll-up " & _svc.Completeness & " | total " & Fmt.Money(_svc.TotalCost, "GBP") &
                          " | unit " & Fmt.Money(_svc.UnitCost, "GBP") & " | material " &
                          Fmt.Money(_svc.MaterialCost, "GBP") & " | conversion " &
                          Fmt.Money(_svc.ConversionCost, "GBP") & " | recovery " &
                          Fmt.Money(_svc.RecoveryCredit, "GBP") &
                          If(_svc.MissingRates > 0, " | " & _svc.MissingRates & " missing rate(s)", ""),
                          If(_svc.MissingRates > 0, "!  WARNING", cue))
            Case Else
                SetStatus(view & " explosion " & _svc.Completeness & " | " & _svc.Occurrences &
                          " occurrence(s), depth " & _svc.DepthReached &
                          If(_svc.StopSummary.Trim() <> "", " | " & _svc.StopSummary.Trim(), ""), cue)
        End Select
    End Sub

    Private Sub WhereUsedClicked(ByVal sender As Object, ByVal e As EventArgs)
        Dim code As String = Fmt.Str(GridHelper.Selected(Grid, "Item"))
        If code = "" Then code = Item()
        Populate(Nothing)
        If code = "" Then
            SetStatus("Select a component row, or enter a parent item.", "!  WARNING")
            Return
        End If
        Dim asAt As Date
        If Not TryAsAt(asAt) Then Return
        Guard(Sub()
                  Populate(_svc.WhereUsed(SterpSession.Current.CompanyCode, code, asAt, 25, True))
                  SetStatus("Where used for " & code & " as at " & Fmt.Date(asAt) & ".", "OK")
              End Sub)
    End Sub

    ''' <summary>Two revisions of the same bill: added, removed and changed, with the cost impact.</summary>
    Private Sub CompareClicked(ByVal sender As Object, ByVal e As EventArgs)
        Populate(Nothing)
        If Item() = "" Then
            SetStatus("Enter the bill reference in the parent item field to compare revisions.", "!  WARNING")
            Return
        End If
        Dim asAt As Date
        If Not TryAsAt(asAt) Then Return
        Guard(Sub()
                  Dim revs As DataTable = _svc.Revisions("", Item(), asAt)
                  Dim bomRef As String = Item()
                  If revs IsNot Nothing AndAlso revs.Rows.Count > 0 Then bomRef = Fmt.Str(revs.Rows(0)("BomRef"))
                  Dim fromRev As String = ""
                  Dim toRev As String = Rev()
                  If revs IsNot Nothing AndAlso revs.Rows.Count > 1 Then
                      If toRev = "" Then toRev = Fmt.Str(revs.Rows(0)("BomRevision"))
                      fromRev = Fmt.Str(revs.Rows(1)("BomRevision"))
                  End If
                  If fromRev = "" OrElse toRev = "" Then
                      SetStatus("Bill " & bomRef & " has only one revision - nothing to compare.", "!  WARNING")
                      Return
                  End If
                  Populate(_svc.CompareRevisions(SterpSession.Current.CompanyCode, bomRef, fromRev, toRev,
                                                 _txtCostSet.Text.Trim().ToUpperInvariant()))
                  SetStatus("Bill " & bomRef & ": revision " & fromRev & " compared with " & toRev & ".", "OK")
              End Sub)
    End Sub

    Private Function Item() As String
        Return _txtItem.Text.Trim().ToUpperInvariant()
    End Function

    Private Function Rev() As String
        Return _txtRev.Text.Trim().ToUpperInvariant()
    End Function

    Private Function TryAsAt(ByRef value As Date) As Boolean
        Dim parsed As Date? = Fmt.ParseDate(_txtAsAt.Text)
        If Not parsed.HasValue Then
            SetStatus("Enter a valid as-at date in dd/MM/yyyy form.", "!  WARNING")
            Return False
        End If
        value = parsed.Value
        Return True
    End Function

End Class
