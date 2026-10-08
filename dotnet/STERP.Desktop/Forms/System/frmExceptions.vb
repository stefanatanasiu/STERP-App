Imports STERP.Business
Imports STERP.Common

''' <summary>Exception worklist: owner, age, priority, status and resolution (COM-017).</summary>
Public Class frmExceptions
    Inherits ListForm

    Private ReadOnly _svc As New ExceptionService()
    Private cboStatus As ComboBox
    Private cboSeverity As ComboBox
    Private txtOwner As TextBox

    Public Sub New()
        Me.Text = "Exception Worklist"
    End Sub

    Protected Overrides Sub BuildFilters()
        cboStatus = FilterList("&Status:", 120, "OPEN", "ACKNOWLEDGED", "RESOLVED", "ALL")
        cboSeverity = FilterList("Se&verity:", 100, "ALL", "HOLD", "WARNING", "ERROR")
        txtOwner = FilterEdit("&Owner:", 90)
        AddHandler cboStatus.SelectedIndexChanged, Sub(s As Object, e As EventArgs) LoadData()
        AddHandler cboSeverity.SelectedIndexChanged, Sub(s As Object, e As EventArgs) LoadData()
    End Sub

    Protected Overrides Sub BuildTools()
        AddSeparator()
        AddTool("Resolve (F10)", "POST", AddressOf ResolveClicked)
        AddSeparator()
    End Sub

    Protected Overrides Sub LoadData()
        Guard(Sub() Populate(_svc.Worklist(Convert.ToString(cboStatus.SelectedItem),
                                       Convert.ToString(cboSeverity.SelectedItem),
                                       txtOwner.Text.Trim())))
    End Sub

    Protected Overrides Sub OnConfirm()
        ResolveClicked(Nothing, EventArgs.Empty)
    End Sub

    Private Sub ResolveClicked(ByVal sender As Object, ByVal e As EventArgs)
        Dim id As Object = GridHelper.Selected(Grid, "ExceptionId")
        If id Is Nothing Then
            SetStatus("Select an exception first.", "")
            Return
        End If
        Dim note As String = InputBox("Resolution note for exception " & Fmt.Int(id) & ":",
                                      "STERP - Resolve Exception", "")
        If note Is Nothing OrElse note.Trim().Length = 0 Then Return
        Guard(Sub()
                  Dim msg As String = _svc.Resolve(Fmt.Int(id), note)
                  If msg <> "" Then
                      ShowError(msg, "")
                  Else
                      SetStatus("Exception resolved.", "OK")
                      LoadData()
                  End If
              End Sub)
    End Sub

End Class
