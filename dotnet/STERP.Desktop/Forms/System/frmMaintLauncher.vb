Imports STERP.Business
Imports STERP.Common

''' <summary>Index of every generic maintenance screen the user may open.</summary>
Public Class frmMaintLauncher
    Inherits ListForm

    Private ReadOnly _svc As New MaintService()

    Public Sub New()
        Me.Text = "File Maintenance"
        Me.ClientSize = New Size(520, 420)
    End Sub

    Protected Overrides Sub BuildTools()
        AddTool("Open", "RUN", AddressOf OpenClicked)
    End Sub

    Protected Overrides Sub LoadData()
        Guard(Sub() Populate(_svc.Definitions()))
    End Sub

    Private Sub OpenClicked(ByVal sender As Object, ByVal e As EventArgs)
        Dim code As Object = GridHelper.Selected(Grid, "MaintCode")
        If code Is Nothing Then Return
        Dim main As frmMain = TryCast(Me.MdiParent, frmMain)
        If main IsNot Nothing Then main.OpenForm("frmGenericMaint", Convert.ToString(code))
    End Sub

    Private Sub GridDoubleClick(ByVal sender As Object, ByVal e As EventArgs) Handles Grid.DoubleClick
        OpenClicked(sender, e)
    End Sub

End Class
