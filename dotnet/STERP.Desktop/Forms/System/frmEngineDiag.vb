Imports STERP.Business
Imports STERP.Common
Imports STERP.CobolInterop

''' <summary>
''' Engine diagnostics: LINK to PING, list the generated program directory and run a
''' COMMAREA mapping test with a hex dump (COM-011).
''' </summary>
Partial Public Class frmEngineDiag
    Inherits BaseForm

    Private ReadOnly _eng As New EngineService()

    Public Sub New()
        InitializeComponent()
        txtDump.Font = UiStyle.FixedFont
        lstPrograms.Font = UiStyle.FixedFont
    End Sub

    Protected Overrides Sub OnLoad(ByVal e As EventArgs)
        MyBase.OnLoad(e)
        UiStyle.Tool(tbDiag, "Ping (F5)", "ENGINE", AddressOf PingClicked)
        UiStyle.Tool(tbDiag, "COMMAREA test (F10)", "RUN", AddressOf EchoClicked)
        UiStyle.Tool(tbDiag, "Copybook map", "FIND", AddressOf MapClicked)
        tbDiag.Items.Add(New ToolStripSeparator())
        UiStyle.Tool(tbDiag, "Close (F3)", "CLOSE", Sub(s As Object, a As EventArgs) CloseForm())
        txtAmount.Text = "1234.56"
        PingClicked(Nothing, EventArgs.Empty)
        EchoClicked(Nothing, EventArgs.Empty)
    End Sub


    Private Sub PingClicked(ByVal sender As Object, ByVal e As EventArgs)
        Guard(Sub()
                  Dim r As CobolResult = _eng.Ping()
                  lblEngine.Text = If(_eng.EngineVersion = "", "(engine unavailable)", _eng.EngineVersion)
                  lblDb.Text = _eng.DatabaseName
                  lblTime.Text = _eng.ServerTime
                  lblDbVersion.Text = _eng.DatabaseVersion
                  lblRc.Text = r.ReturnCode.ToString("00") & "  " & r.Severity
                  lblMessage.Text = r.Display
                  lstPrograms.Items.Clear()
                  For Each p As String In _eng.ProgramNames()
                      lstPrograms.Items.Add(p)
                  Next
                  lblCount.Text = lstPrograms.Items.Count & " program(s) in the engine directory"
                  SetStatus("Engine " & r.Severity, If(r.IsOk, "OK", "!"))
              End Sub)
    End Sub

    Protected Overrides Sub OnRefreshKey()
        PingClicked(Nothing, EventArgs.Empty)
    End Sub

    Protected Overrides Sub OnConfirm()
        EchoClicked(Nothing, EventArgs.Empty)
    End Sub

    Private Sub EchoClicked(ByVal sender As Object, ByVal e As EventArgs)
        Guard(Sub()
                  Dim amount As Decimal
                  Decimal.TryParse(txtAmount.Text.Trim(), amount)
                  Dim dump As String = ""
                  Dim r As CobolResult = _eng.EchoTest(amount, dump)
                  txtDump.Text = ("PING returned " & r.ToString() & vbCrLf & vbCrLf & dump) _
                                 .Replace(vbLf, vbCrLf).Replace(vbCr & vbCr, vbCr)
                  SetStatus("COMMAREA test complete, " & r.Severity, If(r.IsOk, "OK", "!"))
              End Sub)
    End Sub

    Private Sub MapClicked(ByVal sender As Object, ByVal e As EventArgs)
        Dim book As String = InputBox("Copybook to map (from bin\x86\copy):",
                                      "STERP - Copybook map", "PINGCA")
        If book Is Nothing OrElse book.Trim().Length = 0 Then Return
        Guard(Sub() txtDump.Text = _eng.Layout(book.Trim()).Replace(vbLf, vbCrLf).Replace(vbCr & vbCr, vbCr))
    End Sub

End Class
