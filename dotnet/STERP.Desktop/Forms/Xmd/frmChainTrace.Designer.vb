Imports System.Drawing

''' <summary>
''' Designer half of the chain trace enquiry.  The filter strip, the grid and the status line
''' all belong to the base list form; only the window itself is described here, in the house
''' style of every other STERP enquiry.
''' </summary>
Partial Public Class frmChainTrace

    Private components As System.ComponentModel.IContainer = Nothing

    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing AndAlso components IsNot Nothing Then components.Dispose()
        MyBase.Dispose(disposing)
    End Sub

    Private Sub InitializeComponent()
        Me.SuspendLayout()
        Me.AutoScaleMode = Windows.Forms.AutoScaleMode.None
        Me.ClientSize = New Size(1024, 700)
        Me.MinimumSize = New Size(760, 460)
        Me.Name = "frmChainTrace"
        Me.Text = "Business Chain Trace"
        Me.ResumeLayout(False)
    End Sub

End Class
