Imports STERP.Business
Imports STERP.CobolInterop
Imports STERP.Common

''' <summary>
''' The nine box VAT return: every box is an aggregation computed by VATRET9, a box drills
''' down to the records behind it, VATDET answers for a VAT code and VATERRC decides how an
''' error must be corrected.  The submission is written to an MTD file, not sent to HMRC.
''' REQ: FIN-016, FIN-017, FIN-018, FIN-025, FIN-026, FIN-027
''' </summary>
Public Class frmVatReturn
    Inherits BaseForm

    Private Shared ReadOnly _captions As String() = New String() {
        "1 VAT due on sales:", "2 VAT due on acquisitions:", "3 Total VAT due:",
        "4 VAT reclaimed on purchases:", "5 Net VAT to pay or reclaim:", "6 Total sales ex VAT:",
        "7 Total purchases ex VAT:", "8 Supplies to EU members:", "9 Acquisitions from EU members:"}

    Private ReadOnly _svc As New VatService()
    Private ReadOnly _box(8) As TextBox
    Private _txtPeriod As TextBox
    Private _cboBasis As ComboBox
    Private _grid As DataGridView

    Public Sub New()
        Me.Text = "VAT Return - Nine Boxes"
        Me.ClientSize = New Size(830, 520)
        UiStyle.Caption(Me, "VAT &period key:", 8, 8, 90)
        _txtPeriod = UiStyle.Edit(Me, 100, 8, 90)
        UiStyle.Caption(Me, "&Basis:", 202, 8, 40)
        _cboBasis = UiStyle.Picker(Me, 244, 6, 90)
        _cboBasis.Items.AddRange(New String() {"INVOICE", "CASH"})
        _cboBasis.SelectedIndex = 0
        AddHandler UiStyle.Push(Me, "&Prepare (F5)", 344, 6, 86).Click, AddressOf PrepareClicked
        AddHandler UiStyle.Push(Me, "&Drill down", 436, 6, 80).Click, AddressOf DrillClicked
        AddHandler UiStyle.Push(Me, "Fi&nalise", 522, 6, 70).Click, AddressOf FinaliseClicked
        AddHandler UiStyle.Push(Me, "&Submit to file (F10)", 598, 6, 126).Click, AddressOf SubmitClicked
        AddHandler UiStyle.Push(Me, "&Error correction", 730, 6, 94).Click, AddressOf CorrectionClicked
        For i As Integer = 0 To 8
            Dim x As Integer = If(i < 5, 8, 424), y As Integer = 42 + (i Mod 5) * 26
            UiStyle.Caption(Me, _captions(i), x, y, 190)
            _box(i) = UiStyle.Edit(Me, x + 196, y, 120)
            _box(i).ReadOnly = True
            _box(i).TextAlign = HorizontalAlignment.Right
            _box(i).BackColor = UiStyle.Face
        Next
        _grid = New DataGridView()
        _grid.SetBounds(8, 180, 814, 310)
        _grid.Anchor = AnchorStyles.Top Or AnchorStyles.Left Or AnchorStyles.Right Or AnchorStyles.Bottom
        GridHelper.Style(_grid)
        Me.Controls.Add(_grid)
    End Sub

    Protected Overrides Sub OnLoad(ByVal e As EventArgs)
        MyBase.OnLoad(e)
        _txtPeriod.Text = FormArgument
        If _txtPeriod.Text = "" Then
            Dim t As DataTable = Nothing
            Guard(Sub() t = _svc.VatPeriods())
            If t IsNot Nothing AndAlso t.Rows.Count > 0 Then _txtPeriod.Text = Fmt.Str(t.Rows(0)("VatPeriodKey"))
        End If
        If _txtPeriod.Text <> "" Then PrepareClicked(Nothing, EventArgs.Empty)
    End Sub

    Protected Overrides Sub OnRefreshKey()
        PrepareClicked(Nothing, EventArgs.Empty)
    End Sub

    Protected Overrides Sub OnConfirm()
        SubmitClicked(Nothing, EventArgs.Empty)
    End Sub

    ''' <summary>F2 asks VATDET for the rate and treatment of a VAT code (FIN-016, FIN-017).</summary>
    Protected Overrides Sub OnLookup()
        Dim code As String = InputBox("VAT code to look up at today's tax point:", "STERP - VAT Code", "S")
        If code Is Nothing OrElse code.Trim() = "" Then Return
        Guard(Sub()
                  Dim r As CobolResult = _svc.Rate(code.Trim().ToUpperInvariant(), SterpSession.Current.BusinessDate)
                  If ShowResult(r) Then SetStatus("VAT code " & code.Trim().ToUpperInvariant() & ": " & _svc.Answer, "OK")
              End Sub)
    End Sub

    ''' <summary>Box 3 = box 1 + box 2 and box 5 = box 3 - box 4 are VATRET9's arithmetic (FIN-025).</summary>
    Private Sub PrepareClicked(ByVal sender As Object, ByVal e As EventArgs)
        If PeriodKey() = "" Then
            SetStatus("Enter a VAT period key first.", "!  WARNING")
            Return
        End If
        Guard(Sub()
                  Dim r As CobolResult = _svc.Prepare(PeriodKey(), Convert.ToString(_cboBasis.SelectedItem))
                  For i As Integer = 1 To 9
                      _box(i - 1).Text = Fmt.Money(_svc.BoxValue(i))
                  Next
                  GridHelper.Bind(_grid, Nothing)
                  If r.IsNotFound Then
                      SetStatus("VATRET9 did not answer for " & PeriodKey() & " (" & r.ReasonCode &
                                "); the boxes come from fin.usp_VatBoxes.", "!  WARNING")
                      Return
                  End If
                  If Not ShowResult(r) Then Return
                  SetStatus("VRN " & _svc.VatRegNo & ", " & _svc.StartDate & " to " & _svc.EndDate & ", filing due " &
                            _svc.FilingDue & ".  " & _svc.RecordCount & " record(s), " & _svc.LateCount &
                            " late.  Return " & _svc.ReturnStatus & ", net " & Fmt.Money(_svc.BoxValue(5)) &
                            If(_svc.IsRepayment, " REPAYMENT", " PAYABLE") & ".",
                            If(_svc.LateCount > 0, "!  WARNING", "OK"))
              End Sub)
    End Sub

    ''' <summary>Every box total drills back to the source documents behind it (FIN-026).</summary>
    Private Sub DrillClicked(ByVal sender As Object, ByVal e As EventArgs)
        Dim boxNo As Integer = 0
        For i As Integer = 0 To 8
            If Me.ActiveControl Is _box(i) Then boxNo = i + 1
        Next
        If boxNo = 0 Then
            SetStatus("Put the cursor in the box to drill into, then press Drill down.", "!  WARNING")
            Return
        End If
        Guard(Sub()
                  Dim r As CobolResult = _svc.Drill(PeriodKey(), boxNo)
                  GridHelper.Bind(_grid, _svc.Drilled)
                  GridHelper.FillWidth(_grid)
                  If _svc.Drilled Is Nothing Then
                      SetStatus("No records stand behind box " & boxNo & " for " & PeriodKey() & ".  " & r.Display, "")
                  Else
                      SetStatus(_svc.Drilled.Rows.Count & " record(s) behind box " & boxNo & " of " & PeriodKey() &
                                ", total " & Fmt.Money(_svc.BoxValue(boxNo)) & ".", "OK")
                  End If
              End Sub)
    End Sub

    Private Sub FinaliseClicked(ByVal sender As Object, ByVal e As EventArgs)
        If Not Confirm("Finalise the VAT return for " & PeriodKey() & "?  The figures and their detail are frozen.") Then Return
        Guard(Sub()
                  If ShowResult(_svc.Record("FINALISE", PeriodKey(), "", "")) Then PrepareClicked(Nothing, EventArgs.Empty)
              End Sub)
    End Sub

    ''' <summary>The written file is the submission and its name is the receipt reference (FIN-018).</summary>
    Private Sub SubmitClicked(ByVal sender As Object, ByVal e As EventArgs)
        If Not Confirm("Write the MTD submission file for " & PeriodKey() & " and record the submission?") Then Return
        Guard(Sub()
                  Dim path As String = _svc.WriteSubmissionFile(PeriodKey())
                  Dim name As String = System.IO.Path.GetFileName(path)
                  Dim r As CobolResult = _svc.Record("SUBMIT", PeriodKey(), name, "WRITTEN TO FILE " & path)
                  If Not ShowResult(r) Then
                      SetStatus("The file was written to " & path & " but the return was not marked submitted.", "!  WARNING")
                      Return
                  End If
                  Info("The MTD submission file has been written to:" & vbCrLf & vbCrLf & path & vbCrLf & vbCrLf &
                       "Send it to HMRC through the approved filing service.", "VAT Submission")
                  SetStatus("Submitted " & PeriodKey() & ", receipt reference " & name & ".", "OK")
              End Sub)
    End Sub

    ''' <summary>VATERRC decides between the next return and a separate disclosure (FIN-027).</summary>
    Private Sub CorrectionClicked(ByVal sender As Object, ByVal e As EventArgs)
        Dim amount As String = InputBox("Net VAT error found in an earlier period (negative if VAT was overpaid):",
                                        "STERP - VAT Error Correction", "")
        If amount Is Nothing OrElse amount.Trim() = "" Then Return
        Guard(Sub()
                  Dim r As CobolResult = _svc.AssessError(PeriodKey(), PeriodKey(), Fmt.Dec(amount), False)
                  If ShowResult(r) Then SetStatus("Error of " & Fmt.Money(amount) & ": " & _svc.Answer, "OK")
              End Sub)
    End Sub

    Private Function PeriodKey() As String
        Return _txtPeriod.Text.Trim().ToUpperInvariant()
    End Function

End Class
