''' <summary>Start-up banner shown while the COBOL engine initialises.</summary>
Partial Public Class frmSplash
    Inherits Form

    Public Sub New()
        InitializeComponent()
        Me.Font = UiStyle.BodyFont
    End Sub

    ''' <summary>
    ''' The designer sets ClientSize before the border style, so the frame is taken out
    ''' of the client area and the progress line is cut off; ask for it again once the
    ''' window really exists.
    ''' </summary>
    Protected Overrides Sub OnLoad(ByVal e As EventArgs)
        MyBase.OnLoad(e)
        Me.ClientSize = New Size(422, 162)
    End Sub

    ''' <summary>Progress message at the foot of the banner.</summary>
    Public Sub ShowStep(ByVal text As String)
        lblStep.Text = text
        lblStep.Refresh()
        Application.DoEvents()
    End Sub

End Class
