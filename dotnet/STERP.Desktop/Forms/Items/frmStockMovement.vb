Imports STERP.Business
Imports STERP.CobolInterop
Imports STERP.Common

''' <summary>
''' Stock movement entry and posting: receipts, issues, scrap, adjustments, quality holds and
''' transfers (optionally through an in-transit location).  The rules, the costing and the
''' negative-stock refusal belong to STKMOVE and to inv.usp_PostStockMovement.
''' REQ: ITM-015, ITM-016, ITM-017, COM-015
''' </summary>
Public Class frmStockMovement
    Inherits BaseForm

    Private Shared ReadOnly _fields As String()() = New String()() {
        New String() {"ITEM", "&Item:", "200"}, New String() {"REV", "&Revision:", "50"},
        New String() {"SITE", "&Site:", "60"}, New String() {"WHS", "&Warehouse:", "80"},
        New String() {"LOC", "&Location:", "90"}, New String() {"LOT", "L&ot:", "120"},
        New String() {"SERIAL", "Se&rial:", "120"}, New String() {"QTY", "&Quantity:", "90"},
        New String() {"UOM", "&Unit:", "60"}, New String() {"COST", "Unit &cost:", "90"},
        New String() {"DATE", "&Date:", "90"}, New String() {"TOSITE", "To si&te:", "60"},
        New String() {"TOWHS", "To ware&house:", "80"}, New String() {"TOLOC", "To locatio&n:", "90"},
        New String() {"REASONCODE", "Reason cod&e:", "90"}, New String() {"REASON", "Reaso&n:", "260"},
        New String() {"DOCREF", "Document re&f:", "140"}}

    Private ReadOnly _svc As New StockService()
    Private ReadOnly _edit As New Dictionary(Of String, TextBox)
    Private _cboType As ComboBox
    Private _chkTransit As CheckBox
    Private _grid As DataGridView

    Public Sub New()
        Me.Text = "Stock Movement Entry"
        Me.ClientSize = New Size(960, 470)
        UiStyle.Caption(Me, "&Movement:", 8, 8, 72)
        _cboType = UiStyle.Picker(Me, 82, 6, 120)
        _cboType.Items.AddRange(New String() {"RECEIPT", "ISSUE", "RETURN", "ADJUST", "SCRAP",
                                              "COMPLETE", "TRANSFER", "HOLD", "RELEASE"})
        _cboType.SelectedIndex = 0
        _chkTransit = UiStyle.Tick(Me, "Move via an in-&transit location", 212, 6, 200)
        Dim x As Integer = 8, y As Integer = 34
        For Each f As String() In _fields
            UiStyle.Caption(Me, f(1), x, y, 86)
            _edit(f(0)) = UiStyle.Edit(Me, x + 88, y, CInt(f(2)))
            y += 25
            If y > 230 Then
                x += 310
                y = 34
            End If
        Next
        _grid = New DataGridView()
        _grid.SetBounds(8, 262, 940, 180)
        _grid.Anchor = AnchorStyles.Top Or AnchorStyles.Left Or AnchorStyles.Right Or AnchorStyles.Bottom
        GridHelper.Style(_grid)
        Me.Controls.Add(_grid)
    End Sub

    Protected Overrides Sub OnLoad(ByVal e As EventArgs)
        MyBase.OnLoad(e)
        _edit("SITE").Text = SterpSession.Current.SiteCode
        _edit("WHS").Text = "MAIN"
        _edit("DATE").Text = Fmt.Date(SterpSession.Current.BusinessDate)
        If FormArgument <> "" Then _edit("ITEM").Text = FormArgument
        LoadData()
        SetStatus("Complete the movement and press F10 to post it.  Quantities are entered positive; the type gives the direction.")
    End Sub

    Protected Overrides Sub OnRefreshKey()
        LoadData()
    End Sub

    Protected Overrides Sub OnLookup()
        Dim code As String = frmLookup.Pick(Me, "core.usp_UiLookup", "ITM.ITEM", "Item")
        If code <> "" Then
            _edit("ITEM").Text = code
            LoadData()
        End If
    End Sub

    ''' <summary>F10 posts through STKMOVE with a correlation id, so a repeat is not a double posting.</summary>
    Protected Overrides Sub OnConfirm()
        Dim func As String = Convert.ToString(_cboType.SelectedItem)
        If _edit("ITEM").Text.Trim() = "" OrElse _edit("QTY").Text.Trim() = "" Then
            SetStatus("An item code and a quantity are required.", "!  WARNING")
            Return
        End If
        If Not Confirm("Post this " & LCase(func) & " of " & _edit("QTY").Text & " " &
                       _edit("ITEM").Text.Trim() & "?") Then Return
        Guard(Sub()
                  Dim v As New Dictionary(Of String, String)
                  For Each f As String() In _fields
                      v(f(0)) = _edit(f(0)).Text.Trim().ToUpperInvariant()
                  Next
                  v("REASON") = _edit("REASON").Text.Trim()
                  v("TRANSIT") = If(_chkTransit.Checked, "Y", "N")
                  v("DOCTYPE") = "STOCKMOVE"
                  Dim r As CobolResult = _svc.Post(func, v)
                  If Not ShowResult(r) Then Return
                  SetStatus("Movement " & Fmt.Str(_svc.MovementId) & " posted; balance now " &
                            Fmt.Qty(_svc.QtyAfter, _edit("UOM").Text.Trim()) & ".", "OK")
                  _edit("QTY").Text = ""
                  LoadData()
              End Sub)
    End Sub

    Private Sub LoadData()
        Guard(Sub()
                  GridHelper.Bind(_grid, _svc.Movements(_edit("ITEM").Text.Trim().ToUpperInvariant(),
                                                        _edit("SITE").Text.Trim().ToUpperInvariant(), 90))
              End Sub)
    End Sub

End Class
