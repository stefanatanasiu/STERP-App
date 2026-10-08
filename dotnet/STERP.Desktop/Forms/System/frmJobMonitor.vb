Imports STERP.Business
Imports STERP.Common

''' <summary>Batch job monitor over core.JobRun; opens the spool file of a run.</summary>
Public Class frmJobMonitor
    Inherits ListForm

    Private ReadOnly _svc As New JobService()
    Private cboStatus As ComboBox
    Private txtFrom As TextBox

    Public Sub New()
        Me.Text = "Job Monitor"
        Me.ClientSize = New Size(900, 420)
    End Sub

    Protected Overrides Sub BuildFilters()
        cboStatus = FilterList("&Status:", 120, "ALL", "RUNNING", "COMPLETED", "WARNING", "FAILED")
        txtFrom = FilterEdit("Fro&m:", 80)
        txtFrom.Text = Fmt.Date(Date.Today.AddDays(-7))
        AddHandler cboStatus.SelectedIndexChanged, Sub(s As Object, e As EventArgs) LoadData()
    End Sub

    Protected Overrides Sub BuildTools()
        AddSeparator()
        AddTool("View spool", "PRINT", AddressOf ViewClicked)
        AddSeparator()
    End Sub

    Protected Overrides Sub LoadData()
        Dim d As Date? = Fmt.ParseDate(txtFrom.Text)
        Guard(Sub() Populate(_svc.Runs(If(d.HasValue, d.Value, Date.Today.AddDays(-7)),
                                   Convert.ToString(cboStatus.SelectedItem))))
    End Sub

    Private Sub ViewClicked(ByVal sender As Object, ByVal e As EventArgs)
        Dim f As String = Fmt.Str(GridHelper.Selected(Grid, "SpoolFile"))
        If f = "" Then f = Fmt.Str(GridHelper.Selected(Grid, "LogFile"))
        If f = "" Then
            SetStatus("This run produced no spool file.", "")
            Return
        End If
        Dim main As frmMain = TryCast(Me.MdiParent, frmMain)
        If main IsNot Nothing Then main.OpenForm("frmSpoolViewer", f)
    End Sub

End Class
