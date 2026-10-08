Imports STERP.Business
Imports STERP.Common

''' <summary>
''' Stock count entry and the approved adjustment.  Counted quantities are recorded against
''' the frozen book quantity; the variance is only turned into movements by the COUNTADJ batch
''' run, which refuses to commit without the count approval permission.
''' REQ: ITM-018, FIN-022
''' </summary>
Public Class frmStockCount
    Inherits ListForm

    Private ReadOnly _svc As New StockService()
    Private ReadOnly _rpt As New ReportService()
    Private _txtRef As TextBox
    Private _cboStatus As ComboBox

    Public Sub New()
        Me.Text = "Stock Count and Adjustment"
    End Sub

    Protected Overrides Sub BuildFilters()
        _txtRef = FilterEdit("Count &ref:", 140)
        _cboStatus = FilterList("&Status:", 110, "OPEN", "COUNTED", "RECOUNT", "APPROVED", "POSTED", "ALL")
        If FormArgument <> "" Then _txtRef.Text = FormArgument
        AddHandler _cboStatus.SelectedIndexChanged, Sub(s As Object, e As EventArgs) LoadData()
    End Sub

    Protected Overrides Sub BuildTools()
        AddSeparator()
        AddTool("Enter count (F10)", "SAVE", AddressOf EnterClicked)
        AddTool("Count sheet", "PRINT", AddressOf SheetClicked)
        AddTool("Approve adjustment", "POST", AddressOf ApproveClicked)
        AddSeparator()
    End Sub

    Protected Overrides Sub LoadData()
        Guard(Sub()
                  If _txtRef.Text.Trim() = "" Then
                      Populate(_svc.Counts(Convert.ToString(_cboStatus.SelectedItem)))
                      SetStatus("Enter a count reference to see its lines.", "")
                  Else
                      Populate(_svc.CountLines(_txtRef.Text.Trim().ToUpperInvariant()))
                  End If
              End Sub)
    End Sub

    Protected Overrides Sub OnConfirm()
        EnterClicked(Nothing, EventArgs.Empty)
    End Sub

    Private Sub EnterClicked(ByVal sender As Object, ByVal e As EventArgs)
        Dim countId As Object = GridHelper.Selected(Grid, "CountId")
        Dim lineNum As Object = GridHelper.Selected(Grid, "LineNum")
        If countId Is Nothing OrElse lineNum Is Nothing Then
            SetStatus("Select a count line first.", "!  WARNING")
            Return
        End If
        Dim item As String = Fmt.Str(GridHelper.Selected(Grid, "ItemCode"))
        Dim qty As String = InputBox("Counted quantity for " & item & " (book quantity " &
                                     Fmt.Str(GridHelper.Selected(Grid, "ExpectedQty")) & "):",
                                     "STERP - Stock Count", Fmt.Str(GridHelper.Selected(Grid, "CountedQty")))
        If qty Is Nothing OrElse qty.Trim() = "" Then Return
        Guard(Sub()
                  Dim msg As String = _svc.SaveCount(countId, lineNum, qty.Trim())
                  If msg <> "" Then
                      ShowError(msg, "")
                  Else
                      SetStatus("Counted quantity recorded for " & item & ".", "OK")
                      LoadData()
                  End If
              End Sub)
    End Sub

    Private Sub SheetClicked(ByVal sender As Object, ByVal e As EventArgs)
        RunCountAdj("PREVIEW")
    End Sub

    Private Sub ApproveClicked(ByVal sender As Object, ByVal e As EventArgs)
        If Not Confirm("Post the approved adjustment for count " & _txtRef.Text.Trim() & "?") Then Return
        RunCountAdj("COMMIT")
    End Sub

    ''' <summary>The count sheet and the approved adjustment are the same batch program (COUNTADJ).</summary>
    Private Sub RunCountAdj(ByVal mode As String)
        If _txtRef.Text.Trim() = "" Then
            SetStatus("Enter the count reference first.", "!  WARNING")
            Return
        End If
        Guard(Sub()
                  Dim args As New Dictionary(Of String, String)
                  args("CO") = SterpSession.Current.CompanyCode
                  args("SITE") = SterpSession.Current.SiteCode
                  args("COUNT") = _txtRef.Text.Trim().ToUpperInvariant()
                  args("USER") = SterpSession.Current.UserCode
                  args("MODE") = mode
                  Dim rr As ReportRun = _rpt.Run("ITM.COUNTADJ", "COUNTADJ", args)
                  SetStatus("COUNTADJ " & LCase(mode) & " finished with return code " & rr.ReturnCode & ".",
                            If(rr.Succeeded, "OK", "*** ERROR"))
                  LoadData()
              End Sub)
    End Sub

End Class
