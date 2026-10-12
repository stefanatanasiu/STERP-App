Imports STERP.Business
Imports STERP.Common

''' <summary>
''' Metadata driven file maintenance: list grid on top, edit panel below, driven by
''' core.MaintDef / core.MaintColumn.  Add, change and retire only - a row that other
''' documents refer to is never deleted, its retire flag is cleared (COM-013).
''' Opened from a menu entry with FormClass = frmGenericMaint and FormArgument = the
''' maintenance code, e.g. CORE.CURRENCY.
''' </summary>
Partial Public Class frmGenericMaint
    Inherits BaseForm

    Private ReadOnly _svc As New MaintService()
    Private ReadOnly _editors As New Dictionary(Of String, Control)
    Private _mode As String = ""          ' "" browse, ADD, CHANGE
    Private _retireColumn As String = ""

    Public Sub New()
        InitializeComponent()
        GridHelper.Style(grdList)
    End Sub

    Protected Overrides Sub OnLoad(ByVal e As EventArgs)
        MyBase.OnLoad(e)
        BuildToolbar()
        If Not Guard(Sub() LoadDefinition()) Then Return
        LoadList()
    End Sub

    Private Sub BuildToolbar()
        UiStyle.Tool(tbMaint, "New", "NEW", AddressOf NewClicked)
        UiStyle.Tool(tbMaint, "Save (F10)", "SAVE", AddressOf SaveClicked)
        UiStyle.Tool(tbMaint, "Retire", "RETIRE", AddressOf RetireClicked)
        tbMaint.Items.Add(New ToolStripSeparator())
        UiStyle.Tool(tbMaint, "Find (F2)", "FIND", AddressOf FindClicked)
        UiStyle.Tool(tbMaint, "Refresh (F5)", "REFRESH", AddressOf RefreshClicked)
        tbMaint.Items.Add(New ToolStripSeparator())
        UiStyle.Tool(tbMaint, "Close (F3)", "CLOSE", AddressOf CloseClicked)
    End Sub


    ' ------------------------------------------------------------------ metadata
    Private Sub LoadDefinition()
        If FormArgument Is Nothing OrElse FormArgument.Trim().Length = 0 Then
            Throw New ApplicationException("No maintenance code was supplied for this menu entry.")
        End If
        If Not _svc.LoadDefinition(FormArgument.Trim()) Then
            Throw New ApplicationException("Maintenance definition " & FormArgument & " is not registered.")
        End If
        Me.Text = Convert.ToString(_svc.Definition("Title")) & "  [" & FormArgument & "]"
        _retireColumn = Fmt.Str(ServiceBase.Col(_svc.Definition, "RetireColumn"))
        lblHelp.Text = Fmt.Str(ServiceBase.Col(_svc.Definition, "HelpText"))
        BuildEditors()
    End Sub

    ''' <summary>One labelled control per editable column, two columns of fields.</summary>
    Private Sub BuildEditors()
        pnlEdit.Controls.Clear()
        _editors.Clear()
        Dim x As Integer = 8, y As Integer = 8, n As Integer = 0
        For Each c As DataRow In _svc.Columns.Rows
            If Not Fmt.Bool(c("IsInEdit")) Then Continue For
            Dim name As String = Convert.ToString(c("ColumnName"))
            Dim dt As String = UCase(Fmt.Str(c("DataType")))
            UiStyle.Caption(pnlEdit, Convert.ToString(c("Caption")) & ":", x, y, 118)
            Dim ed As Control
            Select Case dt
                Case "BOOL"
                    ed = UiStyle.Tick(pnlEdit, "", x + 122, y, 20)
                Case "DATE"
                    Dim d As New DateTimePicker()
                    d.Format = DateTimePickerFormat.Custom
                    d.CustomFormat = "dd/MM/yyyy"
                    d.ShowCheckBox = True
                    d.SetBounds(x + 122, y, 110, 20)
                    pnlEdit.Controls.Add(d)
                    ed = d
                Case Else
                    Dim w As Integer = If(dt = "TEXT", 200, 120)
                    Dim box As TextBox = UiStyle.Edit(pnlEdit, x + 122, y, w, Fmt.Int(ServiceBase.Col(c, "MaxLength", 0)))
                    If dt <> "TEXT" AndAlso dt <> "CODE" Then box.TextAlign = HorizontalAlignment.Right
                    ed = box
                    If Fmt.Str(ServiceBase.Col(c, "LookupProc")).Length > 0 Then
                        Dim b As Button = UiStyle.Push(pnlEdit, "F2", x + 122 + w + 2, y - 1, 26)
                        b.Tag = New String() {name, Fmt.Str(c("LookupProc")), Fmt.Str(ServiceBase.Col(c, "LookupArg")),
                                              Convert.ToString(c("Caption"))}
                        AddHandler b.Click, AddressOf LookupClicked
                    End If
            End Select
            ed.Tag = c
            _editors(name) = ed
            n += 1
            y += 25
            If n Mod 8 = 0 Then
                x += 390
                y = 8
            End If
        Next
        SetEditEnabled(False)
    End Sub

    ' --------------------------------------------------------------------- data
    Private Sub LoadList()
        Guard(Sub()
                  Dim t As DataTable = _svc.List(FormArgument, txtSearch.Text.Trim())
                  _mode = ""
                  SetEditEnabled(False)
                  GridHelper.Bind(grdList, t)
                  For Each c As DataRow In _svc.Columns.Rows
                      GridHelper.Decorate(grdList, Convert.ToString(c("ColumnName")),
                                          Convert.ToString(c("Caption")), Fmt.Str(c("DataType")),
                                          Fmt.Int(c("Width")))
                      If Not Fmt.Bool(c("IsInList")) AndAlso Not Fmt.Bool(c("IsKey")) Then
                          If grdList.Columns.Contains(Convert.ToString(c("ColumnName"))) Then
                              grdList.Columns(Convert.ToString(c("ColumnName"))).Visible = False
                          End If
                      End If
                  Next
                  GridHelper.FillWidth(grdList)
                  grdList_SelectionChanged(grdList, EventArgs.Empty)
                  SetStatus(t.Rows.Count & " record(s).", "")
              End Sub)
    End Sub

    Private Sub grdList_SelectionChanged(ByVal sender As Object, ByVal e As EventArgs) Handles grdList.SelectionChanged
        If _mode = "ADD" Then Return
        Dim r As DataRow = If(grdList.Rows.Count = 0, Nothing, GridHelper.SelectedRow(grdList))
        If r Is Nothing Then
            _mode = ""
            For Each editor As Control In _editors.Values
                SetValue(editor, Nothing)
            Next
            SetEditEnabled(False)
            Return
        End If
        For Each kv As KeyValuePair(Of String, Control) In _editors
            If r.Table.Columns.Contains(kv.Key) Then SetValue(kv.Value, r(kv.Key))
        Next
        _mode = "CHANGE"
        SetEditEnabled(True)
    End Sub

    Private Sub SetEditEnabled(ByVal enabled As Boolean)
        For Each kv As KeyValuePair(Of String, Control) In _editors
            Dim c As DataRow = DirectCast(kv.Value.Tag, DataRow)
            Dim isKey As Boolean = Fmt.Bool(c("IsKey"))
            kv.Value.Enabled = enabled AndAlso Not Fmt.Bool(c("IsReadOnly")) AndAlso
                               (Not isKey OrElse _mode = "ADD")
        Next
        pnlEdit.Enabled = enabled
    End Sub

    Private Shared Sub SetValue(ByVal ed As Control, ByVal value As Object)
        If TypeOf ed Is CheckBox Then
            DirectCast(ed, CheckBox).Checked = Fmt.Bool(value)
        ElseIf TypeOf ed Is DateTimePicker Then
            Dim d As DateTimePicker = DirectCast(ed, DateTimePicker)
            If value Is Nothing OrElse IsDBNull(value) Then
                d.Checked = False
            Else
                d.Value = Convert.ToDateTime(value)
                d.Checked = True
            End If
        Else
            ed.Text = Fmt.Str(value)
        End If
    End Sub

    Private Shared Function GetValue(ByVal ed As Control) As String
        If TypeOf ed Is CheckBox Then Return If(DirectCast(ed, CheckBox).Checked, "1", "0")
        If TypeOf ed Is DateTimePicker Then
            Dim d As DateTimePicker = DirectCast(ed, DateTimePicker)
            Return If(d.Checked, d.Value.ToString("yyyy-MM-dd"), Nothing)
        End If
        Return ed.Text.Trim()
    End Function

    ' ------------------------------------------------------------------ commands
    Private Sub NewClicked(ByVal sender As Object, ByVal e As EventArgs)
        _mode = "ADD"
        For Each kv As KeyValuePair(Of String, Control) In _editors
            Dim c As DataRow = DirectCast(kv.Value.Tag, DataRow)
            Dim dflt As String = Fmt.Str(ServiceBase.Col(c, "DefaultValue"))
            If kv.Key = "CompanyCode" AndAlso dflt = "" Then dflt = SterpSession.Current.CompanyCode
            SetValue(kv.Value, dflt)
        Next
        SetEditEnabled(True)
        SetStatus("Enter the new record and press F10 to save.", "ADD")
    End Sub

    Private Sub SaveClicked(ByVal sender As Object, ByVal e As EventArgs)
        OnConfirm()
    End Sub

    Protected Overrides Sub OnConfirm()
        If _mode = "" Then
            SetStatus("Select a record or press New first.", "")
            Return
        End If
        Dim values As New Dictionary(Of String, String)
        For Each kv As KeyValuePair(Of String, Control) In _editors
            Dim c As DataRow = DirectCast(kv.Value.Tag, DataRow)
            If Fmt.Bool(c("IsReadOnly")) Then Continue For
            Dim v As String = GetValue(kv.Value)
            If Fmt.Bool(c("IsRequired")) AndAlso (v Is Nothing OrElse v.Length = 0) Then
                ShowError("Enter a value for " & Convert.ToString(c("Caption")) & ".", "UI0001")
                kv.Value.Focus()
                Return
            End If
            values(kv.Key) = v
        Next
        Guard(Sub()
                  Dim msg As String = _svc.Save(FormArgument, _mode, values)
                  If msg <> "" Then
                      ShowError(msg, "")
                  Else
                      SetStatus("Record saved.", "OK")
                      LoadList()
                  End If
              End Sub)
    End Sub

    Private Sub RetireClicked(ByVal sender As Object, ByVal e As EventArgs)
        If _retireColumn = "" Then
            ShowError("Records on this file cannot be withdrawn.", "UI0002")
            Return
        End If
        If GridHelper.SelectedRow(grdList) Is Nothing Then Return
        If Not Confirm("Withdraw the selected record from use?" & vbCrLf &
                       "Existing documents that refer to it are not affected.") Then Return
        Dim values As New Dictionary(Of String, String)
        For Each kv As KeyValuePair(Of String, Control) In _editors
            If Fmt.Bool(DirectCast(kv.Value.Tag, DataRow)("IsKey")) Then values(kv.Key) = GetValue(kv.Value)
        Next
        Guard(Sub()
                  Dim msg As String = _svc.Save(FormArgument, "RETIRE", values)
                  If msg <> "" Then ShowError(msg, "") Else LoadList()
              End Sub)
    End Sub

    Private Sub LookupClicked(ByVal sender As Object, ByVal e As EventArgs)
        Dim info() As String = DirectCast(DirectCast(sender, Button).Tag, String())
        Dim code As String = frmLookup.Pick(Me, info(1), info(2), info(3))
        If code <> "" Then _editors(info(0)).Text = code
    End Sub

    Protected Overrides Sub OnLookup()
        Dim b As Button = TryCast(Me.ActiveControl, Button)
        For Each kv As KeyValuePair(Of String, Control) In _editors
            If kv.Value Is Me.ActiveControl Then
                Dim c As DataRow = DirectCast(kv.Value.Tag, DataRow)
                If Fmt.Str(ServiceBase.Col(c, "LookupProc")).Length > 0 Then
                    Dim code As String = frmLookup.Pick(Me, Fmt.Str(c("LookupProc")),
                        Fmt.Str(ServiceBase.Col(c, "LookupArg")), Convert.ToString(c("Caption")))
                    If code <> "" Then kv.Value.Text = code
                    Return
                End If
            End If
        Next
        MyBase.OnLookup()
    End Sub

    Private Sub FindClicked(ByVal sender As Object, ByVal e As EventArgs)
        txtSearch.Focus()
        txtSearch.SelectAll()
    End Sub

    Private Sub RefreshClicked(ByVal sender As Object, ByVal e As EventArgs)
        LoadList()
    End Sub

    Private Sub CloseClicked(ByVal sender As Object, ByVal e As EventArgs)
        CloseForm()
    End Sub

    Protected Overrides Sub OnRefreshKey()
        LoadList()
    End Sub

    Private Sub txtSearch_KeyDown(ByVal sender As Object, ByVal e As KeyEventArgs) Handles txtSearch.KeyDown
        If e.KeyCode = Keys.Enter Then
            LoadList()
            e.Handled = True
        End If
    End Sub

    Private Sub btnSearch_Click(ByVal sender As Object, ByVal e As EventArgs) Handles btnSearch.Click
        LoadList()
    End Sub

End Class
