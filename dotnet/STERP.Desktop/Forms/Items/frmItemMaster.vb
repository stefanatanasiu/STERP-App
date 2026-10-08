Imports STERP.Business
Imports STERP.CobolInterop
Imports STERP.Common

''' <summary>
''' Item and part master: identity and revisions, classification, units of measure, supply
''' rules, partner references, traceability and shelf life, VAT, customs and packaging.
''' Each tab is a read-only grid; changes go through the audited maintenance screens and the
''' COBOL services.  REQ: ITM-001, ITM-002, ITM-005, ITM-009, ITM-010, ITM-011, ITM-025
''' </summary>
Public Class frmItemMaster
    Inherits BaseForm

    Private Shared ReadOnly _tabs As String()() = New String()() {
        New String() {"&Revisions", "REVISION"}, New String() {"&Units", "UOM"},
        New String() {"&Supply rules", "SUPPLY"}, New String() {"&Partner refs", "PARTNER"},
        New String() {"&Lots / shelf life", "TRACE"}, New String() {"S&ubstitutes", "SUBST"},
        New String() {"&VAT", "VAT"}, New String() {"&Customs", "CUSTOMS"}, New String() {"Pac&kaging", "PACKAGING"}}

    Private ReadOnly _svc As New ItemService()
    Private _txtItem As TextBox
    Private _lblWho As Label
    Private _tabs2 As TabControl
    Private _grids As New Dictionary(Of String, DataGridView)

    Public Sub New()
        Me.Text = "Item and Part Maintenance"
        Me.ClientSize = New Size(820, 460)
        UiStyle.Caption(Me, "&Item code:", 8, 10, 64)
        _txtItem = UiStyle.Edit(Me, 76, 8, 240, 40)
        AddHandler UiStyle.Push(Me, "F&ind (F2)", 322, 7, 76).Click, AddressOf FindClicked
        _lblWho = UiStyle.Caption(Me, "No item selected.", 8, 32, 790)
        _tabs2 = New TabControl()
        _tabs2.SetBounds(8, 54, 800, 350)
        _tabs2.Anchor = AnchorStyles.Top Or AnchorStyles.Left Or AnchorStyles.Right Or AnchorStyles.Bottom
        For Each t As String() In _tabs
            Dim page As New TabPage(t(0))
            Dim g As New DataGridView() With {.Dock = DockStyle.Fill, .Tag = t(1)}
            GridHelper.Style(g)
            page.Controls.Add(g)
            _tabs2.TabPages.Add(page)
            _grids(t(1)) = g
        Next
        Me.Controls.Add(_tabs2)
        AddHandler _tabs2.SelectedIndexChanged, AddressOf TabChanged
    End Sub

    Protected Overrides Sub OnLoad(ByVal e As EventArgs)
        MyBase.OnLoad(e)
        If FormArgument <> "" Then _txtItem.Text = FormArgument
        If _txtItem.Text <> "" Then LoadData()
        SetStatus("Enter an item code and press F5, or press F2 to search by code, description or partner part number.")
    End Sub

    Protected Overrides Sub OnRefreshKey()
        LoadData()
    End Sub

    Protected Overrides Sub OnLookup()
        FindClicked(Nothing, EventArgs.Empty)
    End Sub

    ''' <summary>F10 runs the activation review, which lists what is still missing (ITM-010).</summary>
    Protected Overrides Sub OnConfirm()
        If _txtItem.Text = "" Then Return
        Guard(Sub()
                  Dim r As CobolResult = _svc.Review(_txtItem.Text.Trim().ToUpperInvariant(), "VALIDATE")
                  If _svc.Faults IsNot Nothing Then
                      _tabs2.SelectedIndex = 0
                      GridHelper.Bind(_grids("REVISION"), _svc.Faults)
                  End If
                  If ShowResult(r) Then SetStatus("Activation review complete - the item may be released.", "OK")
              End Sub)
    End Sub

    Private Sub FindClicked(ByVal sender As Object, ByVal e As EventArgs)
        Dim code As String = frmLookup.Pick(Me, "core.usp_UiLookup", "ITM.ITEM", "Item")
        If code <> "" Then
            _txtItem.Text = code
            LoadData()
        End If
    End Sub

    Private Sub TabChanged(ByVal sender As Object, ByVal e As EventArgs)
        LoadTab()
    End Sub

    Private Sub LoadData()
        Guard(Sub()
                  Dim item As String = _txtItem.Text.Trim().ToUpperInvariant()
                  _txtItem.Text = item
                  Dim h As DataRow = _svc.Header(item)
                  If h Is Nothing Then
                      _lblWho.Text = "No item selected."
                      SetStatus("Item " & item & " does not exist in " & SterpSession.Current.CompanyCode & ".", "*** ERROR")
                      Return
                  End If
                  _lblWho.Text = Fmt.Str(h("Description")) & "   |   " & Fmt.Str(h("ItemClass")) & " / " &
                                 Fmt.Str(h("Category")) & "   |   stock unit " & Fmt.Str(h("StockUomCode")) &
                                 "   |   revision " & Fmt.Str(h("CurrentRevision")) &
                                 "   |   traceability " & Fmt.Str(h("TraceabilityType")) &
                                 "   |   cost method " & Fmt.Str(h("CostMethod"))
                  SetStatus("Lifecycle status " & Fmt.Str(h("LifecycleStatus")) & ", steward " &
                            Fmt.Str(h("StewardUser")) & ".", If(Fmt.Str(h("LifecycleStatus")) = "RELEASED", "OK", "!  WARNING"))
                  LoadTab()
              End Sub)
    End Sub

    Private Sub LoadTab()
        If _txtItem.Text = "" Then Return
        Dim kind As String = Convert.ToString(_tabs(_tabs2.SelectedIndex)(1))
        Guard(Sub() GridHelper.Bind(_grids(kind), _svc.Part(kind, _txtItem.Text.Trim())))
    End Sub

End Class
