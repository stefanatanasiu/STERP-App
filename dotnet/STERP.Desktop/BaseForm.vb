Imports STERP.Business
Imports STERP.Common
Imports STERP.CobolInterop

''' <summary>
''' Base class of every STERP window.  Gives the classic look, the standard function
''' keys (F1 help, F2 lookup, F3/Esc close, F5 refresh, F10 confirm/post), a status
''' message line and message boxes that always show the reason code (COM-017, COM-024).
''' </summary>
Public Class BaseForm
    Inherits Form

    ''' <summary>Value of core.MenuItem.FormArgument for the entry that opened this form.</summary>
    Public Property FormArgument As String = ""

    Private _statusStrip As StatusStrip
    Private _statusLabel As ToolStripStatusLabel
    Private _cueLabel As ToolStripStatusLabel

    Public Sub New()
        UiStyle.ApplyForm(Me)
        Me.KeyPreview = True
        Me.StartPosition = FormStartPosition.WindowsDefaultLocation
    End Sub

    Protected Overrides Sub OnLoad(ByVal e As EventArgs)
        MyBase.OnLoad(e)
        If Not Me.DesignMode Then
            BuildStatusLine()
            FitToWorkspace()
        End If
    End Sub

    ''' <summary>
    ''' Keeps an MDI child inside the workspace.  Classic business screens never open
    ''' larger than the frame or hang off the bottom right corner (COM-024).
    ''' </summary>
    Private Sub FitToWorkspace()
        If Me.MdiParent Is Nothing Then Return
        Dim client As MdiClient = Nothing
        For Each c As Control In Me.MdiParent.Controls
            Dim m As MdiClient = TryCast(c, MdiClient)
            If m IsNot Nothing Then client = m
        Next
        If client Is Nothing Then Return
        Dim area As Size = client.ClientSize
        Me.Size = New Size(Math.Max(Math.Min(Me.Width, area.Width - 4), 320),
                           Math.Max(Math.Min(Me.Height, area.Height - 4), 220))
        Me.Left = Math.Max(0, Math.Min(Me.Left, area.Width - Me.Width))
        Me.Top = Math.Max(0, Math.Min(Me.Top, area.Height - Me.Height))
    End Sub

    ''' <summary>Re-applies the classic palette once Load has built the screen (COM-024).</summary>
    Protected Overrides Sub OnShown(ByVal e As EventArgs)
        MyBase.OnShown(e)
        If Not Me.DesignMode Then UiStyle.ApplyPalette(Me)
    End Sub

    Private Sub BuildStatusLine()
        If _statusStrip IsNot Nothing OrElse Me.IsMdiContainer Then Return
        _statusStrip = New StatusStrip()
        _statusStrip.RenderMode = ToolStripRenderMode.System
        _statusStrip.SizingGrip = False
        _cueLabel = New ToolStripStatusLabel("") With {.AutoSize = False, .Width = 70,
                                                       .TextAlign = ContentAlignment.MiddleLeft,
                                                       .BorderSides = ToolStripStatusLabelBorderSides.Right,
                                                       .BorderStyle = Border3DStyle.SunkenOuter}
        _statusLabel = New ToolStripStatusLabel("") With {.Spring = True,
                                                          .TextAlign = ContentAlignment.MiddleLeft}
        _statusStrip.Items.Add(_cueLabel)
        _statusStrip.Items.Add(_statusLabel)
        ' added last so the docking pass gives it the bottom edge before the Fill control
        Me.Controls.Add(_statusStrip)
    End Sub

    ' --------------------------------------------------------------- function keys
    Protected Overrides Sub OnKeyDown(ByVal e As KeyEventArgs)
        Select Case e.KeyCode
            Case Keys.F1
                OnHelpKey() : e.Handled = True
            Case Keys.F2
                OnLookup() : e.Handled = True
            Case Keys.F3
                CloseForm() : e.Handled = True
            Case Keys.Escape
                If Not e.Shift Then CloseForm() : e.Handled = True
            Case Keys.F5
                OnRefreshKey() : e.Handled = True
            Case Keys.F10
                OnConfirm() : e.Handled = True
        End Select
        If Not e.Handled Then MyBase.OnKeyDown(e)
    End Sub

    ''' <summary>F2 - open a lookup for the field with the focus.</summary>
    Protected Overridable Sub OnLookup()
        SetStatus("No lookup is available for this field.")
    End Sub

    ''' <summary>F5 - re-read the data.</summary>
    Protected Overridable Sub OnRefreshKey()
    End Sub

    ''' <summary>F10 - confirm / post the work in progress.</summary>
    Protected Overridable Sub OnConfirm()
    End Sub

    ''' <summary>F1 - module help.</summary>
    Protected Overridable Sub OnHelpKey()
        Info(SterpConfig.ProductName & vbCrLf & vbCrLf &
             "Screen: " & Me.Text & vbCrLf &
             "F2 lookup   F3/Esc close   F5 refresh   F10 confirm" & vbCrLf & vbCrLf &
             "Printed documentation is held by the systems team.", "Help")
    End Sub

    ''' <summary>Overridden by data entry forms that need to ask about unsaved changes.</summary>
    Protected Overridable Function CanClose() As Boolean
        Return True
    End Function

    Protected Sub CloseForm()
        If CanClose() Then Me.Close()
    End Sub

    Protected Overrides Sub OnFormClosing(ByVal e As FormClosingEventArgs)
        If e.CloseReason = CloseReason.UserClosing AndAlso Not CanClose() Then e.Cancel = True
        MyBase.OnFormClosing(e)
    End Sub

    ' ------------------------------------------------------------- status and busy
    Protected Sub SetStatus(ByVal text As String)
        SetStatus(text, "")
    End Sub

    ''' <summary>COM-024: the cue is text, never colour alone.</summary>
    Protected Sub SetStatus(ByVal text As String, ByVal cue As String)
        If _statusLabel Is Nothing Then Return
        _cueLabel.Text = cue
        _statusLabel.Text = text
        _statusStrip.Refresh()
    End Sub

    Protected Sub Busy(ByVal busy As Boolean)
        Me.Cursor = If(busy, Cursors.WaitCursor, Cursors.Default)
        If busy Then
            If _cueLabel IsNot Nothing AndAlso _cueLabel.Text <> "BUSY" Then
                _restCue = _cueLabel.Text
                _restText = _statusLabel.Text
            End If
            SetStatus("Working...", "BUSY")
        Else
            SetStatus("")
        End If
        Application.DoEvents()
    End Sub

    Private _restCue As String = ""
    Private _restText As String = ""

    ''' <summary>
    ''' Puts the status line back the way the screen left it once a service call has
    ''' finished; a window that still reads "BUSY / Working..." when it is idle tells
    ''' the operator the wrong thing (COM-024).
    ''' </summary>
    Private Sub ClearBusy()
        If _cueLabel IsNot Nothing AndAlso _cueLabel.Text = "BUSY" Then SetStatus(_restText, _restCue)
    End Sub

    ' -------------------------------------------------------------- message boxes
    Protected Sub Info(ByVal text As String, ByVal title As String)
        MessageBox.Show(Me, text, "STERP - " & title, MessageBoxButtons.OK, MessageBoxIcon.Information)
    End Sub

    Protected Sub ShowError(ByVal text As String)
        ShowError(text, "")
    End Sub

    Protected Sub ShowError(ByVal text As String, ByVal reasonCode As String)
        SetStatus(text, "*** ERROR")
        MessageBox.Show(Me, Tagged(text, reasonCode), "STERP - Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
    End Sub

    Protected Sub ShowWarning(ByVal text As String, ByVal reasonCode As String)
        SetStatus(text, "!  WARNING")
        MessageBox.Show(Me, Tagged(text, reasonCode), "STERP - Warning", MessageBoxButtons.OK, MessageBoxIcon.Warning)
    End Sub

    ''' <summary>RC 06 - the instruction needs an authority the user does not hold.</summary>
    Protected Sub ShowHold(ByVal text As String, ByVal reasonCode As String)
        SetStatus(text, "## HOLD")
        MessageBox.Show(Me, Tagged(text, reasonCode) & vbCrLf & vbCrLf &
                        "The document has been placed on hold and an approval has been requested.",
                        "STERP - Hold", MessageBoxButtons.OK, MessageBoxIcon.Warning)
    End Sub

    Protected Function Confirm(ByVal text As String) As Boolean
        Return MessageBox.Show(Me, text, "STERP - Confirm",
                               MessageBoxButtons.YesNo, MessageBoxIcon.Question) = DialogResult.Yes
    End Function

    ''' <summary>Shows a COBOL outcome with the right severity; True when work may continue.</summary>
    Protected Function ShowResult(ByVal r As CobolResult) As Boolean
        If r Is Nothing Then Return False
        Select Case r.ReturnCode
            Case 0
                SetStatus(If(r.Message.Trim().Length > 0, r.Message, "Completed."), "OK")
                Return True
            Case 4
                ShowWarning(r.Message, r.ReasonCode)
                Return True
            Case 6
                ShowHold(r.Message, r.ReasonCode)
                Return False
            Case Else
                ShowError(r.Message, r.ReasonCode)
                Return False
        End Select
    End Function

    ''' <summary>Runs a service call, turning any failure into a message box.</summary>
    Protected Function Guard(ByVal work As Action) As Boolean
        Try
            Busy(True)
            work()
            Return True
        Catch ex As STERP.Data.BusinessRuleException
            ShowError(ex.Message, ex.Code)
            Return False
        Catch ex As Exception
            SterpLog.Error(Me.Name, ex)
            ShowError(ex.Message, "SYS0001")
            Return False
        Finally
            Me.Cursor = Cursors.Default
            ClearBusy()
        End Try
    End Function

    Private Shared Function Tagged(ByVal text As String, ByVal reasonCode As String) As String
        If reasonCode Is Nothing OrElse reasonCode.Trim().Length = 0 Then Return text
        Return text & vbCrLf & vbCrLf & "Reason code: " & reasonCode.Trim()
    End Function

End Class
