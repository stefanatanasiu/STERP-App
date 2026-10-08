Imports STERP.Business
Imports STERP.Common

''' <summary>Product, engine and database identification.</summary>
Partial Public Class frmAbout
    Inherits Form

    Public Sub New()
        InitializeComponent()
        UiStyle.ApplyForm(Me)
    End Sub

    Private Sub frmAbout_Load(ByVal sender As Object, ByVal e As EventArgs) Handles MyBase.Load
        Dim eng As New EngineService()
        Dim s As SterpSession = SterpSession.Current
        Dim sb As New Text.StringBuilder()
        Try
            eng.Ping()
        Catch ex As Exception
            SterpLog.Warn("About ping: " & ex.Message)
        End Try
        sb.AppendLine("Workstation      : " & Environment.MachineName & "   (" & Environment.UserName & ")")
        sb.AppendLine("Signed on as     : " & s.Title & "   " & s.DisplayName)
        sb.AppendLine("Company / site   : " & s.CompanyCode & " / " & s.SiteCode & "   " & s.CompanyName)
        sb.AppendLine("Business date    : " & Fmt.Date(s.BusinessDate) & "    Period " & s.PeriodText)
        sb.AppendLine()
        sb.AppendLine("Transaction engine : " & If(eng.EngineVersion = "", "not available", eng.EngineVersion))
        sb.AppendLine("Engine programs    : " & eng.ProgramCount())
        sb.AppendLine("Database           : " & eng.DatabaseName & "   " & eng.ServerTime)
        sb.AppendLine("Database version   : " & eng.DatabaseVersion)
        sb.AppendLine()
        sb.AppendLine("Program directory:")
        Dim line As String = "   "
        For Each p As String In eng.ProgramNames()
            If line.Length + p.Length > 70 Then
                sb.AppendLine(line)
                line = "   "
            End If
            line &= p.PadRight(10)
        Next
        sb.AppendLine(line)
        txtDetail.Text = sb.ToString().Replace(vbLf, vbCrLf).Replace(vbCr & vbCr, vbCr)
        txtDetail.Select(0, 0)
    End Sub

    Private Sub btnOk_Click(ByVal sender As Object, ByVal e As EventArgs) Handles btnOk.Click
        Me.Close()
    End Sub

End Class
