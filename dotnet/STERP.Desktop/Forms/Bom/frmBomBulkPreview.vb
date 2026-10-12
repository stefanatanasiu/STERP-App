Imports STERP.Business.Bom
Imports STERP.Common

''' <summary>Engineering replacement preview; never exposes APPLY or approval.</summary>
Public Class frmBomBulkPreview
    Inherits ListForm

    Private ReadOnly _svc As New BomBulkService()
    Private _changeRef As TextBox
    Private _oldItem As TextBox
    Private _newItem As TextBox
    Private _effectiveDate As TextBox
    Private _scope As ComboBox
    Private _scopeItem As TextBox
    Private _drafts As ComboBox
    Private ReadOnly _answer As New TextBox() With {
        .Name = "txtPreviewAnswer", .Dock = DockStyle.Bottom, .Height = 76,
        .Multiline = True, .ReadOnly = True, .ScrollBars = ScrollBars.Vertical}

    Public Sub New()
        Text = "Bulk Change Preview"
        ClientSize = New Size(1000, 520)
        Grid.Name = "grdBulkPreview"
        Controls.Add(_answer)
    End Sub

    Protected Overrides Sub BuildFilters()
        _changeRef = FilterEdit("&Change reference:", 180) : _changeRef.Name = "txtChangeRef"
        _oldItem = FilterEdit("&Old component:", 150) : _oldItem.Name = "txtOldItem"
        _newItem = FilterEdit("&Replacement:", 150) : _newItem.Name = "txtNewItem"
        _effectiveDate = FilterEdit("&Effective date:", 90) : _effectiveDate.Name = "txtEffectiveDate"
        _scope = FilterList("&Scope:", 90, "ITEM", "BOMREF", "PARENT") : _scope.Name = "cboScope"
        _scopeItem = FilterEdit("&Bill / parent:", 170) : _scopeItem.Name = "txtScopeItem"
        _drafts = FilterList("&Include:", 160, "RELEASED AND DRAFT", "RELEASED ONLY")
        _drafts.Name = "cboIncludeDraft"
        _effectiveDate.Text = Fmt.Date(SterpSession.Current.BusinessDate)
    End Sub

    Protected Overrides Sub BuildTools()
        AddTool("Preview (F10)", "ENGINE", AddressOf PreviewClicked).Name = "btnPreview"
    End Sub

    Protected Overrides Sub LoadData()
        Populate(Nothing)
        _answer.Text = ""
        SetStatus("Enter the change reference, source and replacement. F10 previews only; no bills are changed.")
    End Sub

    Protected Overrides Sub OnConfirm()
        PreviewClicked(Nothing, EventArgs.Empty)
    End Sub

    Private Sub PreviewClicked(ByVal sender As Object, ByVal e As EventArgs)
        Populate(Nothing)
        _answer.Text = ""
        Dim effective As Date? = Fmt.ParseDate(_effectiveDate.Text)
        If Not effective.HasValue Then
            SetStatus("Enter a valid effective date in dd/MM/yyyy form.", "!  WARNING")
            Return
        End If
        Guard(Sub()
                  Dim rows As DataTable = Nothing
                  Dim result = _svc.Preview(_changeRef.Text.Trim().ToUpperInvariant(),
                      _oldItem.Text.Trim().ToUpperInvariant(), _newItem.Text.Trim().ToUpperInvariant(),
                      effective.Value, CStr(_scope.SelectedItem), _scopeItem.Text.Trim().ToUpperInvariant(),
                      _drafts.SelectedIndex = 0, rows)
                  Populate(rows)
                  _answer.Text = _svc.Summary & vbCrLf & _svc.Refusal & vbCrLf & result.Display
                  SetStatus("PREVIEW ONLY - " & _svc.TargetCount & " target(s), " & _svc.FailedCount &
                      " failed, " & _svc.AppliedCount & " applied. " & result.ReasonCode,
                      If(result.Succeeded, "OK", "*** REFUSED"))
              End Sub)
    End Sub
End Class
