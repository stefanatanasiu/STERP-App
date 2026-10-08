Imports STERP.Business
Imports STERP.Business.Bom
Imports STERP.CobolInterop
Imports STERP.Common

''' <summary>
''' Bill maintenance: the header with its declared boundary (nominal basis, yield, production
''' quantum), the component occurrences, the by-product recoveries, the other revisions of the
''' same bill and the validation findings.
''' A released revision is never edited here - the engineer validates a draft (BOMVALID) and then
''' releases it through bom.usp_ReleaseBom, which insists on a validation that still matches the
''' content and on a reviewer who is neither the author nor the releaser.
''' REQ: BOM-001, BOM-002, BOM-004, BOM-012, BOM-026, BOM-028, BOM-029, BOM-030
''' </summary>
Public Class frmBomEditor
    Inherits BaseForm

    Private Shared ReadOnly _tabs As String()() = New String()() {
        New String() {"&Components", "LINES"}, New String() {"&By-products", "BYPRODUCT"},
        New String() {"&Revisions", "REVISIONS"}, New String() {"&Validation", "FINDINGS"}}

    Private ReadOnly _svc As New BomService()
    Private _txtBom As TextBox
    Private _txtRev As TextBox
    Private _lblWho As Label
    Private _lblBoundary As Label
    Private _pages As TabControl
    Private _grids As New Dictionary(Of String, DataGridView)
    Private _findings As DataTable
    Private _status As String = ""

    Public Sub New()
        Me.Text = "Bill of Material Maintenance"
        Me.ClientSize = New Size(900, 520)
        UiStyle.Caption(Me, "&Bill reference:", 8, 10, 82)
        _txtBom = UiStyle.Edit(Me, 94, 8, 200, 30)
        UiStyle.Caption(Me, "Re&vision:", 302, 10, 52)
        _txtRev = UiStyle.Edit(Me, 358, 8, 50, 4)
        AddHandler UiStyle.Push(Me, "F&ind (F2)", 414, 7, 76).Click, AddressOf FindClicked
        AddHandler UiStyle.Push(Me, "&Load (F5)", 496, 7, 76).Click, AddressOf LoadClicked
        AddHandler UiStyle.Push(Me, "V&alidate (F10)", 578, 7, 94).Click, AddressOf ValidateClicked
        AddHandler UiStyle.Push(Me, "&Release...", 678, 7, 80).Click, AddressOf ReleaseClicked
        AddHandler UiStyle.Push(Me, "&Explode", 764, 7, 70).Click, AddressOf ExplodeClicked
        _lblWho = UiStyle.Caption(Me, "No bill loaded.", 8, 34, 870)
        _lblBoundary = UiStyle.Caption(Me, "", 8, 52, 870)
        _pages = New TabControl()
        _pages.SetBounds(8, 74, 880, 410)
        _pages.Anchor = AnchorStyles.Top Or AnchorStyles.Left Or AnchorStyles.Right Or AnchorStyles.Bottom
        For Each t As String() In _tabs
            Dim page As New TabPage(t(0))
            Dim g As New DataGridView() With {.Dock = DockStyle.Fill, .Tag = t(1)}
            GridHelper.Style(g)
            page.Controls.Add(g)
            _pages.TabPages.Add(page)
            _grids(t(1)) = g
        Next
        Me.Controls.Add(_pages)
        AddHandler _pages.SelectedIndexChanged, AddressOf TabChanged
    End Sub

    Protected Overrides Sub OnLoad(ByVal e As EventArgs)
        MyBase.OnLoad(e)
        If FormArgument <> "" Then _txtBom.Text = FormArgument
        If _txtBom.Text <> "" Then LoadBill()
        SetStatus("Enter a bill reference (or a parent item) and press F5. F10 validates, " &
                  "release is refused unless the validation still matches the content.")
    End Sub

    Protected Overrides Sub OnRefreshKey()
        LoadBill()
    End Sub

    Protected Overrides Sub OnLookup()
        FindClicked(Nothing, EventArgs.Empty)
    End Sub

    Protected Overrides Sub OnConfirm()
        ValidateClicked(Nothing, EventArgs.Empty)
    End Sub

    Private Sub FindClicked(ByVal sender As Object, ByVal e As EventArgs)
        Dim code As String = frmLookup.Pick(Me, "core.usp_UiLookup", "ITM.ITEM", "Parent item")
        If code <> "" Then
            _txtBom.Text = code
            LoadBill()
        End If
    End Sub

    Private Sub LoadClicked(ByVal sender As Object, ByVal e As EventArgs)
        LoadBill()
    End Sub

    ''' <summary>A blank revision means "the one effective today", exactly as the engine selects it.</summary>
    Private Sub LoadBill()
        If Bom() = "" Then Return
        Guard(Sub()
                  _findings = Nothing
                  Dim h As DataRow = _svc.BillHeader(Bom(), Rev(), Date.Today)
                  If h Is Nothing Then
                      Dim revs As DataTable = _svc.Revisions("", Bom(), Date.Today)
                      If revs IsNot Nothing AndAlso revs.Rows.Count > 0 Then
                          _txtBom.Text = Fmt.Str(revs.Rows(0)("BomRef"))
                          h = _svc.BillHeader(Bom(), Rev(), Date.Today)
                      End If
                  End If
                  If h Is Nothing Then
                      _lblWho.Text = "No bill loaded."
                      _lblBoundary.Text = ""
                      _status = ""
                      SetStatus("No bill " & Bom() & " in " & SterpSession.Current.CompanyCode & ".", "*** ERROR")
                      Return
                  End If
                  _txtBom.Text = Fmt.Str(h("BomRef"))
                  _txtRev.Text = Fmt.Str(h("BomRevision"))
                  _status = Fmt.Str(h("Status"))
                  _lblWho.Text = Fmt.Str(h("BomRef")) & " rev " & Fmt.Str(h("BomRevision")) & "   |   " &
                                 Fmt.Str(h("ParentItemCode")) & " " & Fmt.Str(h("ParentDescription")) &
                                 "   |   " & Fmt.Str(h("Purpose")) &
                                 If(Fmt.Str(h("VariantCode")) <> "", " / " & Fmt.Str(h("VariantCode")), "") &
                                 "   |   " & Fmt.Str(h("ComponentCount")) & " component(s), " &
                                 Fmt.Str(h("ByProductCount")) & " by-product(s)"
                  _lblBoundary.Text = "Nominal basis " & Fmt.Qty(h("NominalBasis"), Fmt.Str(h("StockUom"))) &
                                      "   |   yield " & Fmt.Rate(h("YieldFactor")) &
                                      "   |   production quantum " & Fmt.Qty(h("ProductionQuantum"), "") &
                                      "   |   effective " & Fmt.Date(h("EffectiveFrom")) & " to " &
                                      If(Fmt.Str(h("EffectiveTo")) = "", "open", Fmt.Date(h("EffectiveTo"))) &
                                      "   |   author " & Fmt.Str(h("AuthorUser")) &
                                      ", reviewer " & Fmt.Str(h("ReviewerUser"))
                  SetStatus("Status " & _status &
                            If(Fmt.Bool(h("IsFrozen")), " - frozen, a change needs a new revision (BOM-004).",
                                                        " - draft, validate before release."),
                            If(_status = "RELEASED", "OK", "!  WARNING"))
                  LoadTab()
              End Sub)
    End Sub

    Private Sub TabChanged(ByVal sender As Object, ByVal e As EventArgs)
        LoadTab()
    End Sub

    Private Sub LoadTab()
        If Bom() = "" Then Return
        Dim kind As String = _tabs(_pages.SelectedIndex)(1)
        If kind = "FINDINGS" Then
            GridHelper.Bind(_grids(kind), _findings)
            Return
        End If
        Guard(Sub()
                  Dim data As DataTable
                  If kind = "REVISIONS" Then
                      data = _svc.Revisions(Bom(), "", Date.Today)
                  ElseIf kind = "BYPRODUCT" Then
                      data = _svc.ByProducts(Bom(), Rev(), Date.Today)
                  Else
                      data = _svc.BillLines(Bom(), Rev(), Date.Today)
                  End If
                  GridHelper.Bind(_grids(kind), data)
                  GridHelper.FillWidth(_grids(kind))
              End Sub)
    End Sub

    ''' <summary>BOMVALID CHECK: the same rules the release gate applies, with nothing recorded.</summary>
    Private Sub ValidateClicked(ByVal sender As Object, ByVal e As EventArgs)
        If Bom() = "" Then
            SetStatus("Load a bill first.", "!  WARNING")
            Return
        End If
        Guard(Sub()
                  Dim r As CobolResult = _svc.Validate(Bom(), Rev(), Date.Today, False, _findings)
                  _pages.SelectedIndex = 3
                  GridHelper.Bind(_grids("FINDINGS"), _findings)
                  GridHelper.FillWidth(_grids("FINDINGS"))
                  If Not ShowResult(r) Then Return
                  Dim cue As String = If(_svc.ReleaseAllowed, "OK", "!  WARNING")
                  SetStatus("Validation " & _svc.ValidationOutcome & " - " & _svc.ValidationErrors &
                            " error(s), " & _svc.ValidationWarnings & " warning(s)" &
                            If(_svc.CyclePath.Trim() <> "", " | cycle: " & _svc.CyclePath.Trim(), "") &
                            If(_svc.ReleaseAllowed, " | this revision may be released.",
                                                    " | release is blocked until the errors are cleared."), cue)
              End Sub)
    End Sub

    ''' <summary>The gate is the procedure, not the screen: it refuses anything it is not happy with.</summary>
    Private Sub ReleaseClicked(ByVal sender As Object, ByVal e As EventArgs)
        If Bom() = "" OrElse Rev() = "" Then
            SetStatus("Load the bill and the revision to release first.", "!  WARNING")
            Return
        End If
        If _status = "RELEASED" Then
            SetStatus("Revision " & Rev() & " is already released - supersede it with a new revision.", "!  WARNING")
            Return
        End If
        Dim reviewer As String = Microsoft.VisualBasic.Interaction.InputBox(
            "Reviewer who approved revision " & Rev() & " of " & Bom() & "." & vbCrLf &
            "The reviewer may be neither the author nor you.", "Release bill of material", "")
        If reviewer.Trim() = "" Then Return
        Dim reason As String = Microsoft.VisualBasic.Interaction.InputBox(
            "Reason for the release (recorded on the revision).", "Release bill of material", "Engineering release")
        If Not Confirm("Release " & Bom() & " revision " & Rev() & " with effect from today?") Then Return
        Guard(Sub()
                  Dim res As DataTable = _svc.Release(SterpSession.Current.CompanyCode, Bom(), Rev(),
                                                      SterpSession.Current.UserCode, reviewer.Trim().ToUpperInvariant(),
                                                      Date.Today, "", reason.Trim())
                  If res IsNot Nothing AndAlso res.Rows.Count > 0 Then
                      GridHelper.Bind(_grids("FINDINGS"), res)
                      _pages.SelectedIndex = 3
                  End If
                  LoadBill()
                  SetStatus("Release of " & Bom() & " revision " & Rev() & " accepted - the revision is now frozen.", "OK")
              End Sub)
    End Sub

    Private Sub ExplodeClicked(ByVal sender As Object, ByVal e As EventArgs)
        Dim parent As String = ""
        Dim h As DataRow = _svc.BillHeader(Bom(), Rev(), Date.Today)
        If h IsNot Nothing Then parent = Fmt.Str(h("ParentItemCode"))
        If parent = "" Then
            SetStatus("Load a bill first.", "!  WARNING")
            Return
        End If
        Dim f As New frmBomExplosion()
        f.FormArgument = parent
        f.Show(Me)
    End Sub

    Private Function Bom() As String
        Return _txtBom.Text.Trim().ToUpperInvariant()
    End Function

    Private Function Rev() As String
        Return _txtRev.Text.Trim().ToUpperInvariant()
    End Function

End Class
