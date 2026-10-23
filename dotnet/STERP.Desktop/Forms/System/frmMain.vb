Imports System.Reflection
Imports STERP.Business
Imports STERP.Common

''' <summary>
''' The MDI shell.  Menus are built from core.MenuItem (already filtered by
''' permission); each entry names the VB class to instantiate by reflection and an
''' optional argument.  Entries with no FormClass and an argument starting with "$"
''' are built-in shell commands.
''' </summary>
Partial Public Class frmMain
    Inherits Form

    Private ReadOnly _menuSvc As New MenuService()
    Private ReadOnly _engine As New EngineService()

    Public Sub New()
        InitializeComponent()
        UiStyle.ApplyForm(Me)
    End Sub

    Private Sub frmMain_Load(ByVal sender As Object, ByVal e As EventArgs) Handles MyBase.Load
        Dim s As SterpSession = SterpSession.Current
        Me.Text = SterpConfig.ProductName & "  [" & s.CompanyCode & "] " & s.CompanyName
        ' the MDI client is a separate control and keeps its own colour
        For Each c As Control In Me.Controls
            Dim client As MdiClient = TryCast(c, MdiClient)
            If client IsNot Nothing Then client.BackColor = UiStyle.Workspace
        Next
        BuildMenu()
        BuildToolbar()
        RefreshStatus()
        UiStyle.ApplyPalette(Me)
        tmrStatus.Start()
        SetMessage("Ready.")
    End Sub

    ' ------------------------------------------------------------------- menu bar
    Private Sub BuildMenu()
        mnuBar.Items.Clear()
        Dim data As DataTable = _menuSvc.Load()
        If data Is Nothing OrElse data.Rows.Count = 0 Then data = BuiltInMenu()
        For Each top As DataRow In data.Select("ParentMenuCode is null", "SortOrder")
            Dim item As New ToolStripMenuItem(Convert.ToString(top("Caption")))
            item.Tag = Convert.ToString(top("MenuCode"))
            AddChildren(item, data, Convert.ToString(top("MenuCode")))
            If item.DropDownItems.Count > 0 Then
                ClassicRenderer.Apply(item)
                mnuBar.Items.Add(item)
            End If
            If Convert.ToString(top("MenuCode")) = "WINDOW" Then mnuBar.MdiWindowListItem = item
        Next
        If mnuBar.Items.Count = 0 Then mnuBar.Items.Add(New ToolStripMenuItem("(no menu entries)"))
    End Sub

    Private Sub AddChildren(ByVal parent As ToolStripMenuItem, ByVal data As DataTable, ByVal code As String)
        For Each r As DataRow In data.Select("ParentMenuCode = '" & code.Replace("'", "''") & "'", "SortOrder")
            Dim item As New ToolStripMenuItem(Convert.ToString(r("Caption")))
            item.Tag = r
            AddHandler item.Click, AddressOf MenuItemClicked
            Dim sc As String = Convert.ToString(r("Shortcut"))
            If sc.Length > 0 Then item.ShortcutKeyDisplayString = sc
            AddChildren(item, data, Convert.ToString(r("MenuCode")))
            parent.DropDownItems.Add(item)
        Next
    End Sub

    Private Sub MenuItemClicked(ByVal sender As Object, ByVal e As EventArgs)
        Dim r As DataRow = TryCast(DirectCast(sender, ToolStripMenuItem).Tag, DataRow)
        If r Is Nothing Then Return
        Dim cls As String = Convert.ToString(r("FormClass")).Trim()
        Dim arg As String = Convert.ToString(r("FormArgument")).Trim()
        If cls.Length = 0 Then
            RunCommand(arg)
        Else
            OpenForm(cls, arg)
        End If
    End Sub

    ''' <summary>Opens (or re-activates) an MDI child by class name.</summary>
    Public Sub OpenForm(ByVal className As String, ByVal argument As String)
        Try
            For Each open As Form In Me.MdiChildren
                Dim bf As BaseForm = TryCast(open, BaseForm)
                If open.GetType().Name = className AndAlso
                   (bf Is Nothing OrElse String.Equals(bf.FormArgument, argument)) Then
                    open.Activate()
                    Return
                End If
            Next
            Dim t As Type = Assembly.GetExecutingAssembly().GetType("STERP.Desktop." & className, False, True)
            If t Is Nothing Then
                MessageBox.Show(Me, "STERP-E-0004: Screen " & className & " is not installed in this release.",
                                "STERP - Menu", MessageBoxButtons.OK, MessageBoxIcon.Warning)
                Return
            End If
            Dim f As Form = DirectCast(Activator.CreateInstance(t), Form)
            Dim b As BaseForm = TryCast(f, BaseForm)
            If b IsNot Nothing Then b.FormArgument = argument
            f.MdiParent = Me
            f.Show()
            SetMessage(f.Text)
        Catch ex As Exception
            SterpLog.Error("OpenForm " & className, ex)
            MessageBox.Show(Me, "STERP-E-0005: " & className & " could not be opened." & vbCrLf & ex.Message,
                            "STERP - Menu", MessageBoxButtons.OK, MessageBoxIcon.Error)
        End Try
    End Sub

    ''' <summary>Built-in shell commands used by the Window, Help and File menus.</summary>
    Private Sub RunCommand(ByVal command As String)
        Select Case command.ToUpperInvariant()
            Case "$CASCADE" : Me.LayoutMdi(MdiLayout.Cascade)
            Case "$TILEH" : Me.LayoutMdi(MdiLayout.TileHorizontal)
            Case "$TILEV" : Me.LayoutMdi(MdiLayout.TileVertical)
            Case "$ARRANGE" : Me.LayoutMdi(MdiLayout.ArrangeIcons)
            Case "$CLOSEALL"
                For Each f As Form In Me.MdiChildren
                    f.Close()
                Next
            Case "$ABOUT"
                Using a As New frmAbout()
                    a.ShowDialog(Me)
                End Using
            Case "$HELP"
                MessageBox.Show(Me,
                    SterpConfig.ProductName & vbCrLf & vbCrLf &
                    "F2 lookup      F3 / Esc close window" & vbCrLf &
                    "F5 refresh     F10 confirm or post" & vbCrLf & vbCrLf &
                    "Printed manuals are held by the systems team; the on-line help" & vbCrLf &
                    "compiler was retired in 2011.", "STERP - Help", MessageBoxButtons.OK, MessageBoxIcon.Information)
            Case "$PRINTSETUP"
                Using d As New PrintDialog()
                    d.ShowDialog(Me)
                End Using
            Case "$COMPANY", "$RUNAS"
                ChangeSignOn()
            Case "$EXIT" : Me.Close()
            Case Else : SetMessage("Command " & command & " is not available.")
        End Select
    End Sub

    ' -------------------------------------------------------------------- toolbar
    ''' <summary>
    ''' File > Continue as User / Select Company / Site.  The sign-on dialog changes the shared
    ''' session as the operator works in it, so the context is snapshotted first: Cancel puts it
    ''' back untouched, and a real change of user, company, site or date closes every window
    ''' opened under the old context (their data and permissions belong to it).  If a window
    ''' refuses to close, the old context is restored and nothing changes.
    ''' </summary>
    Private Sub ChangeSignOn()
        Dim before As SterpSession = SterpSession.Current.Snapshot()
        Dim ok As Boolean
        Using l As New frmLogin()
            ok = (l.ShowDialog(Me) = DialogResult.OK)
        End Using
        If Not ok Then
            SterpSession.Current.RestoreFrom(before)
            SetMessage("Sign-on unchanged - still " & before.Title & " in " & before.CompanyCode & "/" & before.SiteCode & ".")
            Return
        End If
        If SterpSession.Current.ContextDiffers(before) Then
            Dim after As SterpSession = SterpSession.Current.Snapshot()
            ' windows close (and ask about unsaved work) under the context they were opened in
            SterpSession.Current.RestoreFrom(before)
            For Each f As Form In Me.MdiChildren
                f.Close()
            Next
            If Me.MdiChildren.Length > 0 Then
                SetMessage("Sign-on unchanged - close the open windows first (" & Me.MdiChildren.Length & " still open).")
                Return
            End If
            SterpSession.Current.RestoreFrom(after)
        End If
        Dim s As SterpSession = SterpSession.Current
        Me.Text = SterpConfig.ProductName & "  [" & s.CompanyCode & "] " & s.CompanyName
        BuildMenu()
        RefreshStatus()
        SetMessage("Signed on as " & s.Title & " in " & s.CompanyCode & "/" & s.SiteCode & ".")
    End Sub

    ' -------------------------------------------------------------------- toolbar
    Private Sub BuildToolbar()
        AddButton("Approvals", "POST", "frmApprovals")
        AddButton("Exceptions", "RETIRE", "frmExceptions")
        AddButton("Audit", "FIND", "frmAudit")
        tbMain.Items.Add(New ToolStripSeparator())
        AddButton("Reports", "PRINT", "frmReportLauncher")
        AddButton("Spool", "NEW", "frmSpoolViewer")
        AddButton("Jobs", "RUN", "frmJobMonitor")
        tbMain.Items.Add(New ToolStripSeparator())
        AddButton("File maintenance", "SAVE", "frmMaintLauncher")
        AddButton("Engine", "ENGINE", "frmEngineDiag")
        ' the CUA keys are stated on the frame so they are on view from every screen
        tbMain.Items.Add(New ToolStripSeparator())
        Dim keys As New ToolStripLabel("F1 Help   F2 Lookup   F3 Close   F5 Refresh   F10 Post")
        keys.ForeColor = UiStyle.FaceText
        keys.Alignment = ToolStripItemAlignment.Right
        tbMain.Items.Add(keys)
    End Sub

    Private Sub AddButton(ByVal text As String, ByVal glyph As String, ByVal className As String)
        Dim b As ToolStripButton = UiStyle.Tool(tbMain, text, glyph, Nothing)
        b.Tag = className
        AddHandler b.Click, Sub(s As Object, e As EventArgs) OpenForm(className, "")
    End Sub

    ' ----------------------------------------------------------------- status bar
    Public Sub SetMessage(ByVal text As String)
        sbMessage.Text = text
    End Sub

    Private Sub RefreshStatus()
        Dim s As SterpSession = SterpSession.Current
        sbUser.Text = "User: " & s.Title
        sbCompany.Text = "Co: " & s.CompanyCode
        sbSite.Text = "Site: " & s.SiteCode
        sbPeriod.Text = "Period: " & If(s.PeriodText = "", "-", s.PeriodText & " " & s.PeriodStatus)
        sbDate.Text = Fmt.Date(s.BusinessDate)
        sbEngine.Text = "Engine: " & _engine.Status()
    End Sub

    Private Sub tmrStatus_Tick(ByVal sender As Object, ByVal e As EventArgs) Handles tmrStatus.Tick
        sbCaps.Text = If(Control.IsKeyLocked(Keys.CapsLock), "CAPS", "")
        sbNum.Text = If(Control.IsKeyLocked(Keys.NumLock), "NUM", "")
        _ticks += 1
        If _ticks Mod 60 = 0 Then sbEngine.Text = "Engine: " & _engine.Status()
    End Sub

    Private _ticks As Integer

    ''' <summary>Fallback menu used when core.MenuItem has not been seeded yet.</summary>
    Private Shared Function BuiltInMenu() As DataTable
        Dim t As New DataTable("MenuItem")
        t.Columns.Add("MenuCode") : t.Columns.Add("ParentMenuCode") : t.Columns.Add("Caption")
        t.Columns.Add("FormClass") : t.Columns.Add("FormArgument") : t.Columns.Add("Shortcut")
        t.Columns.Add("SortOrder", GetType(Integer))
        Dim rows() As String = {
            "FILE||&File||||10", "SYSTEM||&System||||20", "RPT||&Reports||||120",
            "WINDOW||&Window||||130", "HELP||&Help||||140",
            "FILE.COMPANY|FILE|Select &Company / Site...||$COMPANY||10",
            "FILE.EXIT|FILE|E&xit||$EXIT||90",
            "SYS.MAINT|SYSTEM|File Main&tenance...|frmMaintLauncher|||10",
            "SYS.APPROVAL|SYSTEM|&Approvals Inbox...|frmApprovals|||20",
            "SYS.EXCEPTION|SYSTEM|&Exception Worklist...|frmExceptions|||30",
            "SYS.AUDIT|SYSTEM|A&udit Enquiry...|frmAudit|||40",
            "SYS.JOBS|SYSTEM|&Job Monitor...|frmJobMonitor|||50",
            "SYS.ENGINE|SYSTEM|Engine &Diagnostics...|frmEngineDiag|||60",
            "RPT.LAUNCH|RPT|&Run Report...|frmReportLauncher|||10",
            "RPT.SPOOL|RPT|&Spool File Viewer...|frmSpoolViewer|||20",
            "WIN.CASCADE|WINDOW|&Cascade||$CASCADE||10",
            "WIN.TILEH|WINDOW|Tile &Horizontally||$TILEH||20",
            "WIN.TILEV|WINDOW|Tile &Vertically||$TILEV||30",
            "WIN.CLOSEALL|WINDOW|C&lose All||$CLOSEALL||50",
            "HELP.ABOUT|HELP|&About STERP - Stefan's ERP...||$ABOUT||90"}
        For Each line As String In rows
            Dim p() As String = line.Split("|"c)
            Dim r As DataRow = t.NewRow()
            r("MenuCode") = p(0)
            r("ParentMenuCode") = If(p(1) = "", DBNull.Value, CObj(p(1)))
            r("Caption") = p(2) : r("FormClass") = p(3) : r("FormArgument") = p(4)
            r("Shortcut") = p(5) : r("SortOrder") = Integer.Parse(p(6))
            t.Rows.Add(r)
        Next
        Return t
    End Function

End Class
