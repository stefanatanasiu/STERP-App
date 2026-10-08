Imports System.Collections.Generic
Imports System.Drawing.Imaging
Imports System.IO
Imports System.Linq
Imports System.Reflection
Imports System.Runtime.InteropServices
Imports System.Text
Imports System.Text.RegularExpressions
Imports System.Threading
Imports STERP.Business
Imports STERP.Common
Imports STERP.CobolInterop

''' <summary>
''' Unattended screen harness used by build\ui-smoke.ps1 ("STERP.EXE /qa-screens &lt;dir&gt;").
''' Signs on as SYSADM, opens every core.MenuItem FormClass in turn, writes a PNG of each
''' MDI child and a tab separated inspection report of the classic-look rules in COM-024
''' (clipped or overlapping controls, stray fonts and colours, grids without headers or
''' rows, missing mnemonics and tab order, forms that throw on open).  Never shipped to a
''' user: the switch is undocumented and the process exits when the sweep finishes.
''' </summary>
Public NotInheritable Class QaScreens

    Private Const Pw As UInteger = 2UI          ' PW_RENDERFULLCONTENT

    <DllImport("user32.dll")>
    Private Shared Function EnumThreadWindows(ByVal tid As UInteger, ByVal cb As EnumProc, ByVal p As IntPtr) As Boolean
    End Function
    <DllImport("user32.dll")>
    Private Shared Function GetClassName(ByVal h As IntPtr, ByVal s As StringBuilder, ByVal n As Integer) As Integer
    End Function
    <DllImport("user32.dll")>
    Private Shared Function PostMessage(ByVal h As IntPtr, ByVal m As UInteger, ByVal w As IntPtr, ByVal l As IntPtr) As IntPtr
    End Function
    <DllImport("kernel32.dll")>
    Private Shared Function GetCurrentThreadId() As UInteger
    End Function
    Private Delegate Function EnumProc(ByVal h As IntPtr, ByVal l As IntPtr) As Boolean

    Private Shared _uiThread As UInteger
    Private Shared _killer As Timer
    Private Shared ReadOnly _log As New List(Of String)()
    Private Shared _dir As String

    ''' <summary>Entry point called from Program.Main; returns a process exit code.</summary>
    Public Shared Function Run(ByVal outDir As String, ByVal company As String) As Integer
        _dir = outDir
        Directory.CreateDirectory(_dir)
        For Each old As String In Directory.GetFiles(_dir, "*.png")
            File.Delete(old)
        Next
        _uiThread = GetCurrentThreadId()
        ' any message box raised by a screen would stall the sweep for ever
        _killer = New Timer(AddressOf DismissDialogs, Nothing, 1500, 1200)

        Dim login As New LoginService()
        Dim msg As String = login.SignOnAs("SYSADM")
        If msg <> "" Then
            Note("SIGNON", "", "FAIL", msg)
            Flush()
            Return 2
        End If
        Dim s As SterpSession = SterpSession.Current
        If s.Access.Count = 0 Then
            Note("SIGNON", "", "FAIL", "SYSADM has no company/site access")
            Flush()
            Return 2
        End If
        s.SelectCompanySite(s.Access(0))
        If Not String.IsNullOrEmpty(company) Then
            For Each cs As CompanySite In s.Access
                If String.Equals(cs.CompanyCode, company, StringComparison.OrdinalIgnoreCase) Then
                    s.SelectCompanySite(cs)
                    Exit For
                End If
            Next
        End If
        login.LoadPeriod(Date.Today)
        Note("SIGNON", "", "OK", s.UserCode & " " & s.CompanyCode & "/" & s.SiteCode & " period " & s.PeriodText)

        Dim main As New frmMain()
        main.StartPosition = FormStartPosition.Manual
        main.Location = New Point(0, 0)
        main.Size = New Size(1024, 720)
        main.Show()
        Pump(1200)
        Shot(main, "00-main-mdi")
        Inspect(main, "frmMain", "")

        For Each pair As String() In Screens()
            Capture(main, pair(0), pair(1))
        Next
        Dialogs(main)

        ' one last shot of the shell with a child open, for the documentation
        main.OpenForm("frmJournalEnquiry", "")
        Pump(900)
        Shot(main, "00-main-mdi-with-child")
        For Each c As Form In main.MdiChildren
            c.Close()
        Next

        Pump(300)
        main.Close()
        Flush()
        Return 0
    End Function

    ''' <summary>Distinct FormClass / FormArgument pairs from the database menu.</summary>
    Private Shared Function Screens() As List(Of String())
        Dim list As New List(Of String())()
        Dim seen As New List(Of String)()
        Dim data As DataTable = New MenuService().Load()
        If data Is Nothing Then Return list
        For Each r As DataRow In data.Rows
            Dim cls As String = Convert.ToString(r("FormClass")).Trim()
            If cls.Length = 0 Then Continue For
            Dim arg As String = Convert.ToString(r("FormArgument")).Trim()
            Dim key As String = (cls & "|" & arg).ToUpperInvariant()
            If seen.Contains(key) Then Continue For
            seen.Add(key)
            list.Add(New String() {cls, arg})
        Next
        list.Sort(Function(a, b) String.Compare(a(0) & "|" & a(1), b(0) & "|" & b(1), StringComparison.OrdinalIgnoreCase))
        Return list
    End Function

    Private Shared Sub Capture(ByVal main As frmMain, ByVal cls As String, ByVal arg As String)
        Dim name As String = Safe(cls & If(arg = "", "", "-" & arg))
        Try
            main.OpenForm(cls, arg)
            Pump(850)
            Dim child As Form = Nothing
            For Each f As Form In main.MdiChildren
                If f.GetType().Name = cls Then child = f
            Next
            If child Is Nothing Then
                Note(cls, arg, "FAIL", "no MDI child was created")
                Return
            End If
            Shot(child, name)
            Note(cls, arg, "OK", child.Text)
            Inspect(child, cls, arg)
            child.Close()
            Pump(150)
        Catch ex As Exception
            Note(cls, arg, "FAIL", ex.GetType().Name & ": " & ex.Message)
        End Try
    End Sub

    ''' <summary>Sign-on, about box and lookup are dialogs, not MDI children.</summary>
    Private Shared Sub Dialogs(ByVal main As frmMain)
        For Each cls As String In New String() {"frmLogin", "frmAbout", "frmSplash"}
            Try
                Dim t As Type = Assembly.GetExecutingAssembly().GetType("STERP.Desktop." & cls, False, True)
                If t Is Nothing Then Continue For
                Dim f As Form = DirectCast(Activator.CreateInstance(t), Form)
                f.StartPosition = FormStartPosition.Manual
                f.Location = New Point(1100, 40)
                f.Show()
                Pump(700)
                Shot(f, Safe(cls))
                Note(cls, "", "OK", f.Text)
                Inspect(f, cls, "")
                f.Close()
            Catch ex As Exception
                Note(cls, "", "FAIL", ex.Message)
            End Try
        Next
    End Sub

    ' ------------------------------------------------------------------- inspection
    Private Shared Sub Inspect(ByVal f As Form, ByVal cls As String, ByVal arg As String)
        Dim found As New List(Of String)()
        Walk(f, f, found)
        If f.Text Is Nothing OrElse f.Text.Trim().Length = 0 Then found.Add("no window caption")
        Dim tabs As Integer = 0, maxTab As Integer = 0
        CountTabs(f, tabs, maxTab)
        If tabs > 2 AndAlso maxTab = 0 Then found.Add("tab order not set (" & tabs & " tab stops all TabIndex 0)")
        Dim dialog As Boolean = (cls = "frmLogin" OrElse cls = "frmAbout" OrElse cls = "frmSplash" OrElse cls = "frmLookup")
        If Not dialog AndAlso Not Hints(f) Then found.Add("keys: no function key hint on the screen")
        For Each d As String In found
            Note(cls, arg, "DEFECT", d)
        Next
    End Sub

    Private Shared Sub Walk(ByVal parent As Control, ByVal root As Form, ByVal found As List(Of String))
        Dim kids As New List(Of Control)()
        For Each c As Control In parent.Controls
            If c.Visible Then kids.Add(c)
        Next
        For Each c As Control In kids
            Dim id As String = If(c.Name = "", c.GetType().Name, c.Name)

            If c.Dock = DockStyle.None AndAlso Not TypeOf parent Is MdiClient Then
                Dim r As Rectangle = parent.ClientRectangle
                Dim sc As ScrollableControl = TryCast(parent, ScrollableControl)
                If sc IsNot Nothing AndAlso sc.AutoScroll Then
                    r = New Rectangle(r.Location, New Size(Math.Max(r.Width, sc.DisplayRectangle.Width),
                                                           Math.Max(r.Height, sc.DisplayRectangle.Height)))
                End If
                If c.Right > r.Right + 1 OrElse c.Bottom > r.Bottom + 1 OrElse c.Left < -1 OrElse c.Top < -1 Then
                    found.Add("clipped: " & id & " " & c.Bounds.ToString() & " outside " & r.ToString())
                End If
            End If

            Dim nm As String = c.Font.Name
            If nm <> "Microsoft Sans Serif" AndAlso nm <> "MS Sans Serif" AndAlso nm <> "Courier New" Then
                found.Add("font: " & id & " uses " & nm & " " & c.Font.SizeInPoints.ToString("0.##"))
            ElseIf Not (TypeOf c Is Label) AndAlso nm = "Microsoft Sans Serif" AndAlso
                   Math.Abs(c.Font.SizeInPoints - 8.25!) > 0.01! Then
                found.Add("font: " & id & " at " & c.Font.SizeInPoints.ToString("0.##") & "pt")
            End If

            Dim b As ButtonBase = TryCast(c, ButtonBase)
            If b IsNot Nothing Then
                If b.FlatStyle <> FlatStyle.Standard AndAlso b.FlatStyle <> FlatStyle.System Then
                    found.Add("style: " & id & " FlatStyle=" & b.FlatStyle.ToString())
                End If
                If TypeOf b Is Button AndAlso b.Text.Length > 1 AndAlso b.Text.IndexOf("&"c) < 0 AndAlso
                   Not Regex.IsMatch(b.Text.Trim(), "^F\d+$") Then
                    found.Add("mnemonic: button '" & b.Text & "' (" & id & ") has no accelerator")
                End If
            End If

            Dim g As DataGridView = TryCast(c, DataGridView)
            If g IsNot Nothing Then
                If Not g.ColumnHeadersVisible Then found.Add("grid: " & id & " has no column headers")
                If g.Columns.Count = 0 Then found.Add("grid: " & id & " has no columns")
                If g.Rows.Count = 0 Then found.Add("grid: " & id & " is empty")
            End If
            Dim lv As ListView = TryCast(c, ListView)
            If lv IsNot Nothing Then
                If lv.View <> View.Details Then found.Add("list: " & id & " is not a details list")
                If lv.View = View.Details AndAlso lv.Columns.Count = 0 Then found.Add("list: " & id & " has no column headers")
                If lv.Items.Count = 0 Then found.Add("list: " & id & " is empty")
            End If

            If Not (TypeOf c Is DataGridView) Then
                If Not Classic(c.BackColor) Then found.Add("colour: " & id & " BackColor " & Describe(c.BackColor))
                If c.ForeColor.ToArgb() <> Color.Black.ToArgb() AndAlso Not Classic(c.ForeColor) Then
                    found.Add("colour: " & id & " ForeColor " & Describe(c.ForeColor))
                End If
            End If

            Walk(c, root, found)
        Next

        ' overlapping siblings inside one container.  A group box drawn behind controls
        ' that were added to the form rather than to the box is the house layout, not a
        ' defect, so containers take no part in the comparison.
        For i As Integer = 0 To kids.Count - 1
            If IsBox(kids(i)) Then Continue For
            For j As Integer = i + 1 To kids.Count - 1
                If IsBox(kids(j)) Then Continue For
                If kids(i).Dock <> DockStyle.None OrElse kids(j).Dock <> DockStyle.None Then Continue For
                Dim o As Rectangle = Rectangle.Intersect(kids(i).Bounds, kids(j).Bounds)
                If o.Width > 2 AndAlso o.Height > 2 Then
                    found.Add("overlap: " & Id(kids(i)) & " and " & Id(kids(j)) & " share " & o.Width & "x" & o.Height & "px")
                End If
            Next
        Next
    End Sub

    Private Shared Function IsBox(ByVal c As Control) As Boolean
        Return TypeOf c Is GroupBox OrElse TypeOf c Is Panel OrElse TypeOf c Is TabControl OrElse
               TypeOf c Is SplitContainer OrElse TypeOf c Is ToolStrip
    End Function

    Private Shared Function Id(ByVal c As Control) As String
        If c.Name <> "" Then Return c.Name
        Return c.GetType().Name & " '" & If(c.Text, "").Trim() & "'"
    End Function

    Private Shared Sub CountTabs(ByVal parent As Control, ByRef tabs As Integer, ByRef maxTab As Integer)
        For Each c As Control In parent.Controls
            If c.Visible AndAlso c.TabStop AndAlso Not TypeOf c Is Label Then
                tabs += 1
                If c.TabIndex > maxTab Then maxTab = c.TabIndex
            End If
            CountTabs(c, tabs, maxTab)
        Next
    End Sub

    ''' <summary>True when the screen tells the operator about at least one CUA key.</summary>
    Private Shared Function Hints(ByVal parent As Control) As Boolean
        For Each c As Control In parent.Controls
            Dim t As String = If(c.Text, "")
            If t.IndexOf("F2", StringComparison.Ordinal) >= 0 OrElse t.IndexOf("F5", StringComparison.Ordinal) >= 0 OrElse
               t.IndexOf("F10", StringComparison.Ordinal) >= 0 OrElse t.IndexOf("F3", StringComparison.Ordinal) >= 0 Then Return True
            Dim ts As ToolStrip = TryCast(c, ToolStrip)
            If ts IsNot Nothing Then
                For Each it As ToolStripItem In ts.Items
                    Dim x As String = If(it.Text, "")
                    If x.IndexOf("F2", StringComparison.Ordinal) >= 0 OrElse x.IndexOf("F5", StringComparison.Ordinal) >= 0 OrElse
                       x.IndexOf("F10", StringComparison.Ordinal) >= 0 OrElse x.IndexOf("F3", StringComparison.Ordinal) >= 0 Then Return True
                Next
            End If
            If Hints(c) Then Return True
        Next
        Return False
    End Function

    Private Shared Function Classic(ByVal c As Color) As Boolean
        Dim a As Integer = c.ToArgb()
        Return a = UiStyle.Face.ToArgb() OrElse a = UiStyle.WindowFace.ToArgb() OrElse
               a = UiStyle.Shadow.ToArgb() OrElse a = UiStyle.Chosen.ToArgb() OrElse
               a = UiStyle.ChosenText.ToArgb() OrElse a = Color.Black.ToArgb() OrElse
               a = SystemColors.Control.ToArgb() OrElse a = SystemColors.Window.ToArgb() OrElse
               a = SystemColors.ControlText.ToArgb() OrElse a = Color.Transparent.ToArgb()
    End Function

    Private Shared Function Describe(ByVal c As Color) As String
        Return c.R & "," & c.G & "," & c.B
    End Function

    ' ----------------------------------------------------------------------- plumbing
    Private Shared Sub Shot(ByVal f As Form, ByVal name As String)
        Try
            Using bmp As New Bitmap(Math.Max(f.Width, 8), Math.Max(f.Height, 8))
                f.DrawToBitmap(bmp, New Rectangle(0, 0, bmp.Width, bmp.Height))
                bmp.Save(Path.Combine(_dir, name & ".png"), ImageFormat.Png)
            End Using
        Catch ex As Exception
            Note(name, "", "FAIL", "screenshot: " & ex.Message)
        End Try
    End Sub

    Private Shared Sub Pump(ByVal ms As Integer)
        Dim until As Date = Date.UtcNow.AddMilliseconds(ms)
        While Date.UtcNow < until
            Application.DoEvents()
            Thread.Sleep(25)
        End While
    End Sub

    Private Shared Sub Note(ByVal cls As String, ByVal arg As String, ByVal kind As String, ByVal text As String)
        _log.Add(cls & vbTab & arg & vbTab & kind & vbTab & If(text, "").Replace(vbTab, " ").Replace(vbCrLf, " "))
        Console.WriteLine(kind.PadRight(7) & " " & (cls & If(arg = "", "", " " & arg)).PadRight(34) & " " & text)
    End Sub

    Private Shared Sub Flush()
        Try
            _killer.Dispose()
        Catch
        End Try
        File.WriteAllLines(Path.Combine(_dir, "qa-report.tsv"),
                           New String() {"FormClass" & vbTab & "Argument" & vbTab & "Kind" & vbTab & "Detail"}.
                           Concat(_log).ToArray())
    End Sub

    ''' <summary>Closes any modal box a screen puts up so the sweep cannot stall.</summary>
    Private Shared Sub DismissDialogs(ByVal state As Object)
        Try
            EnumThreadWindows(_uiThread,
                Function(h As IntPtr, l As IntPtr)
                    Dim sb As New StringBuilder(64)
                    GetClassName(h, sb, 64)
                    If sb.ToString() = "#32770" Then PostMessage(h, &H10UI, IntPtr.Zero, IntPtr.Zero)
                    Return True
                End Function, IntPtr.Zero)
        Catch
        End Try
    End Sub

    Private Shared Function Safe(ByVal s As String) As String
        Dim sb As New StringBuilder()
        For Each ch As Char In s
            sb.Append(If(Char.IsLetterOrDigit(ch) OrElse ch = "-"c, ch, "_"c))
        Next
        Return sb.ToString()
    End Function

End Class
