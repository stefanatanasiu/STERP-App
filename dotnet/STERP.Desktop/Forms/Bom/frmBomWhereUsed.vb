Imports STERP.Business
Imports STERP.Business.Bom
Imports STERP.Common

''' <summary>
''' Where used: every parent that calls for a component, directly or through any number of
''' intermediate levels, with the quantity per parent and the top level assemblies the change
''' would reach.  The traversal is bom.usp_BomWhereUsed; the screen only chooses the depth and
''' whether unreleased drafts are included.
''' REQ: BOM-023, BOM-028, BOM-031
''' </summary>
Public Class frmBomWhereUsed
    Inherits ListForm

    Private ReadOnly _svc As New BomService()
    Private _txtItem As TextBox
    Private _txtAsAt As TextBox
    Private _cboLevels As ComboBox
    Private _chkDraft As ComboBox

    Public Sub New()
        Me.Text = "Where Used Enquiry"
        Me.ClientSize = New Size(900, 460)
    End Sub

    Protected Overrides Sub BuildFilters()
        _txtItem = FilterEdit("&Component:", 170)
        _txtAsAt = FilterEdit("&As at:", 80)
        _cboLevels = FilterList("&Levels:", 90, "ALL", "1", "2", "3", "5", "10")
        _chkDraft = FilterList("&Include:", 150, "RELEASED AND DRAFT", "RELEASED ONLY")
        _txtAsAt.Text = Date.Today.ToString("dd/MM/yyyy")
        If FormArgument <> "" Then _txtItem.Text = FormArgument
        AddHandler _cboLevels.SelectedIndexChanged, Sub(s As Object, e As EventArgs) LoadData()
        AddHandler _chkDraft.SelectedIndexChanged, Sub(s As Object, e As EventArgs) LoadData()
    End Sub

    Protected Overrides Sub BuildTools()
        AddSeparator()
        AddTool("Where used (F10)", "FIND", AddressOf FindClicked)
        AddTool("Explode parent", "ENGINE", AddressOf ExplodeClicked)
        AddSeparator()
    End Sub

    Protected Overrides Sub LoadData()
        If Item() = "" Then
            SetStatus("Enter the component code and press F10 to see every bill that uses it.")
            Return
        End If
        FindClicked(Nothing, EventArgs.Empty)
    End Sub

    Protected Overrides Sub OnConfirm()
        FindClicked(Nothing, EventArgs.Empty)
    End Sub

    Private Sub FindClicked(ByVal sender As Object, ByVal e As EventArgs)
        If Item() = "" Then
            SetStatus("Enter a component item code first.", "!  WARNING")
            Return
        End If
        Guard(Sub()
                  Dim rows As DataTable = _svc.WhereUsed(SterpSession.Current.CompanyCode, Item(), AsAt(),
                                                         Levels(), IncludeDraft())
                  Populate(rows)
                  Dim direct As Integer = 0
                  Dim deepest As Integer = 0
                  If rows IsNot Nothing Then
                      For Each r As DataRow In rows.Rows
                          Dim lvl As Integer = Fmt.Int(ServiceBase.Col(r, "Level", 0))
                          If lvl > deepest Then deepest = lvl
                          If Fmt.Bool(ServiceBase.Col(r, "IsDirect", False)) Then direct += 1
                      Next
                  End If
                  SetStatus(Item() & " is used by " & If(rows Is Nothing, 0, rows.Rows.Count) &
                            " parent bill(s) - " & direct & " direct, " & deepest & " level(s) up.",
                            If(rows IsNot Nothing AndAlso rows.Rows.Count > 0, "OK", "!  WARNING"))
              End Sub)
    End Sub

    ''' <summary>Open the explosion enquiry on the parent the engineer selected.</summary>
    Private Sub ExplodeClicked(ByVal sender As Object, ByVal e As EventArgs)
        Dim parent As String = Fmt.Str(GridHelper.Selected(Grid, "ParentItemCode"))
        If parent = "" Then parent = Fmt.Str(GridHelper.Selected(Grid, "TopLevelItemCode"))
        If parent = "" Then
            SetStatus("Select a parent row first.", "!  WARNING")
            Return
        End If
        Dim f As New frmBomExplosion()
        f.FormArgument = parent
        f.Show(Me)
    End Sub

    Private Function Item() As String
        Return _txtItem.Text.Trim().ToUpperInvariant()
    End Function

    Private Function Levels() As Integer
        Dim s As String = Convert.ToString(_cboLevels.SelectedItem)
        Dim n As Integer
        If s = "ALL" OrElse Not Integer.TryParse(s, n) Then Return 25
        Return n
    End Function

    Private Function IncludeDraft() As Boolean
        Return Convert.ToString(_chkDraft.SelectedItem) <> "RELEASED ONLY"
    End Function

    Private Function AsAt() As Date
        Dim d As Date
        If Not Date.TryParse(_txtAsAt.Text.Trim(), d) Then d = Date.Today
        Return d
    End Function

End Class
