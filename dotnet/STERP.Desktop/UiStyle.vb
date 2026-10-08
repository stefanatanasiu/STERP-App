Imports System.Drawing.Drawing2D

''' <summary>
''' The house style of the STERP client (COM-024): Windows Classic, Microsoft Sans
''' Serif 8.25pt, grey 3D controls, no visual styles and no image files - the toolbar
''' glyphs are drawn with GDI+ at start-up.
''' </summary>
Public NotInheritable Class UiStyle

    Public Shared ReadOnly BodyFont As New Font("Microsoft Sans Serif", 8.25!)
    Public Shared ReadOnly BoldFont As New Font("Microsoft Sans Serif", 8.25!, FontStyle.Bold)
    Public Shared ReadOnly FixedFont As New Font("Courier New", 9.0!)
    Public Shared ReadOnly TitleFont As New Font("Microsoft Sans Serif", 14.0!, FontStyle.Bold)

    ' Classic Windows palette.  Windows 10/11 paint COLOR_BTNFACE almost white and
    ' COLOR_HIGHLIGHT bright blue, which destroys the period look, so the shell carries
    ' its own copy of the Windows 2000 "Standard" scheme (COM-024).
    Public Shared ReadOnly Face As Color = Color.FromArgb(212, 208, 200)
    Public Shared ReadOnly FaceText As Color = Color.FromArgb(0, 0, 0)
    Public Shared ReadOnly WindowFace As Color = Color.FromArgb(255, 255, 255)
    Public Shared ReadOnly Shadow As Color = Color.FromArgb(128, 128, 128)
    Public Shared ReadOnly Workspace As Color = Color.FromArgb(128, 128, 128)
    Public Shared ReadOnly Chosen As Color = Color.FromArgb(0, 0, 128)
    Public Shared ReadOnly ChosenText As Color = Color.FromArgb(255, 255, 255)
    Public Shared ReadOnly Disabled As Color = Color.FromArgb(128, 128, 128)

    Public Const RowHeight As Integer = 17

    Public Shared Sub ApplyForm(ByVal f As Form)
        f.Font = BodyFont
        f.BackColor = Face
        f.ForeColor = FaceText
        ApplyPalette(f)
    End Sub

    ''' <summary>
    ''' Repaints a control tree in the classic palette.  Buttons, group boxes and tick
    ''' boxes are moved off FlatStyle.System because the system renderer ignores
    ''' BackColor and would paint them in the current Windows theme colour.
    ''' </summary>
    Public Shared Sub ApplyPalette(ByVal parent As Control)
        For Each c As Control In parent.Controls
            If TypeOf c Is MdiClient Then
                c.BackColor = Workspace
            ElseIf TypeOf c Is DataGridView Then
                ' GridHelper.Style owns the grid palette
            ElseIf TypeOf c Is TextBoxBase Then
                Dim t As TextBoxBase = DirectCast(c, TextBoxBase)
                ' a read-only entry field is a classic greyed box, but a multi-line pane
                ' (log, spool file, hex dump, about box) stays a white document window
                t.BackColor = If(t.ReadOnly AndAlso Not t.Multiline, Face, WindowFace)
                t.ForeColor = FaceText
            ElseIf TypeOf c Is ListBox OrElse TypeOf c Is ListView OrElse TypeOf c Is TreeView Then
                c.BackColor = WindowFace
                c.ForeColor = FaceText
            ElseIf TypeOf c Is ToolStrip Then
                c.BackColor = Face
                c.ForeColor = FaceText
                ' ToolStrip, MenuStrip and StatusStrip set their own font in the constructor
                ' (Segoe UI on Windows 10/11), so the classic face has to be forced back on
                ' them and on every item they own (COM-024)
                c.Font = BodyFont
                For Each it As ToolStripItem In DirectCast(c, ToolStrip).Items
                    ClassicItem(it)
                Next
            Else
                c.BackColor = Face
                c.ForeColor = FaceText
            End If

            Dim b As ButtonBase = TryCast(c, ButtonBase)
            If b IsNot Nothing Then
                b.FlatStyle = FlatStyle.Standard
                b.UseVisualStyleBackColor = False
            End If
            Dim grp As GroupBox = TryCast(c, GroupBox)
            If grp IsNot Nothing Then grp.FlatStyle = FlatStyle.Standard

            ApplyPalette(c)
        Next
    End Sub

    ''' <summary>Puts one toolbar, menu or status item back into the classic face.</summary>
    Private Shared Sub ClassicItem(ByVal it As ToolStripItem)
        it.Font = BodyFont
        it.ForeColor = FaceText
        Dim drop As ToolStripDropDownItem = TryCast(it, ToolStripDropDownItem)
        If drop Is Nothing Then Return
        For Each sub_ As ToolStripItem In drop.DropDownItems
            ClassicItem(sub_)
        Next
    End Sub

    ' ------------------------------------------------------------- control factory
    Public Shared Function Caption(ByVal parent As Control, ByVal text As String,
                                   ByVal x As Integer, ByVal y As Integer, ByVal w As Integer) As Label
        Dim l As New Label()
        l.Text = text : l.SetBounds(x, y + 3, w, 15) : l.AutoSize = False
        l.TextAlign = ContentAlignment.TopLeft
        parent.Controls.Add(l)
        Return l
    End Function

    Public Shared Function Edit(ByVal parent As Control, ByVal x As Integer, ByVal y As Integer,
                                ByVal w As Integer, Optional ByVal maxLen As Integer = 0) As TextBox
        Dim t As New TextBox()
        t.SetBounds(x, y, w, 20) : t.BorderStyle = BorderStyle.Fixed3D
        t.BackColor = WindowFace : t.ForeColor = FaceText
        If maxLen > 0 Then t.MaxLength = maxLen
        parent.Controls.Add(t)
        Return t
    End Function

    Public Shared Function Picker(ByVal parent As Control, ByVal x As Integer, ByVal y As Integer,
                                  ByVal w As Integer) As ComboBox
        Dim c As New ComboBox()
        c.SetBounds(x, y, w, 21) : c.DropDownStyle = ComboBoxStyle.DropDownList
        c.FlatStyle = FlatStyle.Standard
        c.BackColor = WindowFace : c.ForeColor = FaceText
        parent.Controls.Add(c)
        Return c
    End Function

    Public Shared Function Tick(ByVal parent As Control, ByVal text As String,
                                ByVal x As Integer, ByVal y As Integer, ByVal w As Integer) As CheckBox
        Dim c As New CheckBox()
        c.Text = text : c.SetBounds(x, y, w, 18) : c.FlatStyle = FlatStyle.Standard
        c.BackColor = Face : c.ForeColor = FaceText
        parent.Controls.Add(c)
        Return c
    End Function

    Public Shared Function Push(ByVal parent As Control, ByVal text As String, ByVal x As Integer,
                                ByVal y As Integer, Optional ByVal w As Integer = 80) As Button
        Dim b As New Button()
        b.Text = text : b.SetBounds(x, y, w, 23) : b.FlatStyle = FlatStyle.Standard
        b.UseVisualStyleBackColor = False
        b.BackColor = Face : b.ForeColor = FaceText
        parent.Controls.Add(b)
        Return b
    End Function

    Public Shared Function Box(ByVal parent As Control, ByVal text As String, ByVal x As Integer,
                               ByVal y As Integer, ByVal w As Integer, ByVal h As Integer) As GroupBox
        Dim g As New GroupBox()
        g.Text = text : g.SetBounds(x, y, w, h) : g.FlatStyle = FlatStyle.Standard
        g.BackColor = Face : g.ForeColor = FaceText
        parent.Controls.Add(g)
        Return g
    End Function

    ''' <summary>Adds a classic 16x16 glyph button to a toolbar.</summary>
    Public Shared Function Tool(ByVal bar As ToolStrip, ByVal text As String, ByVal glyphName As String,
                                ByVal handler As EventHandler) As ToolStripButton
        Dim b As New ToolStripButton(text, Glyph(glyphName), handler)
        b.DisplayStyle = ToolStripItemDisplayStyle.ImageAndText
        b.ImageTransparentColor = Color.Transparent
        bar.Items.Add(b)
        Return b
    End Function

    ' --------------------------------------------------------------- toolbar glyphs
    ''' <summary>16x16 glyphs drawn in code so the client needs no image resources.</summary>
    Public Shared Function Glyph(ByVal kind As String) As Image
        Dim bmp As New Bitmap(16, 16)
        Using g As Graphics = Graphics.FromImage(bmp)
            g.SmoothingMode = SmoothingMode.None
            g.Clear(Color.Transparent)
            Dim dark As New Pen(Color.FromArgb(64, 64, 64))
            Select Case kind.ToUpperInvariant()
                Case "NEW"
                    g.FillRectangle(Brushes.White, 3, 1, 9, 13)
                    g.DrawRectangle(dark, 3, 1, 9, 13)
                    g.DrawLine(Pens.Gray, 5, 4, 10, 4) : g.DrawLine(Pens.Gray, 5, 7, 10, 7)
                    g.DrawLine(Pens.Gray, 5, 10, 8, 10)
                Case "SAVE"
                    g.FillRectangle(New SolidBrush(Color.FromArgb(0, 0, 128)), 1, 1, 14, 14)
                    g.FillRectangle(Brushes.White, 4, 2, 8, 5)
                    g.FillRectangle(Brushes.Gainsboro, 3, 9, 10, 5)
                Case "RETIRE"
                    g.DrawLine(New Pen(Color.Maroon, 2), 3, 3, 12, 12)
                    g.DrawLine(New Pen(Color.Maroon, 2), 12, 3, 3, 12)
                Case "REFRESH"
                    g.DrawArc(New Pen(Color.FromArgb(0, 96, 0), 2), 2, 2, 11, 11, 40, 280)
                    g.FillPolygon(New SolidBrush(Color.FromArgb(0, 96, 0)),
                                  New Point() {New Point(11, 0), New Point(15, 5), New Point(9, 5)})
                Case "FIND"
                    g.DrawEllipse(New Pen(Color.FromArgb(0, 0, 128), 2), 2, 2, 8, 8)
                    g.DrawLine(New Pen(Color.FromArgb(0, 0, 128), 2), 9, 9, 14, 14)
                Case "PRINT"
                    g.FillRectangle(Brushes.White, 4, 1, 8, 4) : g.DrawRectangle(dark, 4, 1, 8, 4)
                    g.FillRectangle(Brushes.DimGray, 1, 5, 14, 6)
                    g.FillRectangle(Brushes.White, 4, 9, 8, 6) : g.DrawRectangle(dark, 4, 9, 8, 6)
                Case "POST"
                    g.DrawLines(New Pen(Color.FromArgb(0, 112, 0), 3),
                                New Point() {New Point(2, 8), New Point(6, 13), New Point(14, 2)})
                Case "REJECT"
                    g.DrawLines(New Pen(Color.Maroon, 3),
                                New Point() {New Point(3, 3), New Point(13, 13)})
                    g.DrawLines(New Pen(Color.Maroon, 3),
                                New Point() {New Point(13, 3), New Point(3, 13)})
                Case "RUN"
                    g.FillPolygon(New SolidBrush(Color.FromArgb(0, 96, 0)),
                                  New Point() {New Point(3, 1), New Point(14, 8), New Point(3, 15)})
                Case "CLOSE"
                    g.FillRectangle(Brushes.Gainsboro, 2, 2, 12, 12) : g.DrawRectangle(dark, 2, 2, 12, 12)
                    g.DrawLine(New Pen(Color.Black, 2), 5, 5, 11, 11)
                    g.DrawLine(New Pen(Color.Black, 2), 11, 5, 5, 11)
                Case "ENGINE"
                    g.FillRectangle(Brushes.Gainsboro, 1, 3, 14, 10) : g.DrawRectangle(dark, 1, 3, 14, 10)
                    g.FillRectangle(Brushes.Lime, 3, 5, 3, 3) : g.FillRectangle(Brushes.Gold, 7, 5, 3, 3)
                    g.DrawLine(Pens.Gray, 3, 10, 12, 10)
                Case Else
                    g.FillRectangle(Brushes.Gainsboro, 2, 2, 12, 12) : g.DrawRectangle(dark, 2, 2, 12, 12)
            End Select
        End Using
        Return bmp
    End Function

End Class

''' <summary>
''' Windows 10/11 draw menus white with a drop shadow.  STERP has always had grey
''' menus with a navy highlight, so the shell installs this renderer process-wide.
''' </summary>
Public Class ClassicRenderer
    Inherits ToolStripSystemRenderer

    Private Shared ReadOnly Navy As New SolidBrush(UiStyle.Chosen)
    Private Shared ReadOnly Grey As New SolidBrush(UiStyle.Face)

    Protected Overrides Sub OnRenderToolStripBackground(ByVal e As ToolStripRenderEventArgs)
        e.Graphics.Clear(UiStyle.Face)
    End Sub

    Protected Overrides Sub OnRenderMenuItemBackground(ByVal e As ToolStripItemRenderEventArgs)
        Dim r As New Rectangle(Point.Empty, e.Item.Size)
        If e.Item.Selected AndAlso e.Item.Enabled Then
            e.Graphics.FillRectangle(Navy, r)
        Else
            e.Graphics.FillRectangle(Grey, r)
        End If
    End Sub

    Protected Overrides Sub OnRenderItemText(ByVal e As ToolStripItemTextRenderEventArgs)
        If Not e.Item.Enabled Then
            e.TextColor = UiStyle.Disabled
        ElseIf e.Item.Selected AndAlso TypeOf e.Item Is ToolStripMenuItem Then
            e.TextColor = Color.White
        Else
            e.TextColor = UiStyle.FaceText
        End If
        MyBase.OnRenderItemText(e)
    End Sub

    Protected Overrides Sub OnRenderToolStripBorder(ByVal e As ToolStripRenderEventArgs)
        If TypeOf e.ToolStrip Is ToolStripDropDown Then
            ControlPaint.DrawBorder3D(e.Graphics, New Rectangle(Point.Empty, e.ToolStrip.Size),
                                      Border3DStyle.Raised)
        Else
            MyBase.OnRenderToolStripBorder(e)
        End If
    End Sub

    Protected Overrides Sub OnRenderSeparator(ByVal e As ToolStripSeparatorRenderEventArgs)
        Dim s As Size = e.Item.Size
        If e.Vertical Then
            ControlPaint.DrawBorder3D(e.Graphics, New Rectangle(s.Width \ 2 - 1, 3, 2, s.Height - 6),
                                      Border3DStyle.Etched, Border3DSide.Left)
        Else
            ControlPaint.DrawBorder3D(e.Graphics, New Rectangle(2, s.Height \ 2 - 1, s.Width - 4, 2),
                                      Border3DStyle.Etched, Border3DSide.Top)
        End If
    End Sub

    Protected Overrides Sub OnRenderArrow(ByVal e As ToolStripArrowRenderEventArgs)
        e.ArrowColor = If(e.Item.Selected AndAlso e.Item.Enabled, Color.White, UiStyle.FaceText)
        MyBase.OnRenderArrow(e)
    End Sub

    ''' <summary>Turns a drop-down into a classic grey, shadowless menu.</summary>
    Public Shared Sub Apply(ByVal item As ToolStripDropDownItem)
        item.DropDown.BackColor = UiStyle.Face
        item.DropDown.DropShadowEnabled = False
        item.DropDown.RenderMode = ToolStripRenderMode.ManagerRenderMode
        For Each child As ToolStripItem In item.DropDownItems
            Dim sub_ As ToolStripDropDownItem = TryCast(child, ToolStripDropDownItem)
            If sub_ IsNot Nothing AndAlso sub_.HasDropDownItems Then Apply(sub_)
        Next
    End Sub

End Class
