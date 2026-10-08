Imports System.Drawing.Printing
Imports System.IO
Imports STERP.Common

''' <summary>
''' Spool file viewer - 132 column report listings in Courier New, paged on the form
''' feed character the COBOL Report Writer emits.  FormArgument may hold the path.
''' </summary>
Partial Public Class frmSpoolViewer
    Inherits BaseForm

    Private _pages As New List(Of String)
    Private _page As Integer
    Private _file As String = ""
    Private _printPage As Integer
    Private WithEvents _doc As New PrintDocument()

    Public Sub New()
        InitializeComponent()
        txtPage.Font = UiStyle.FixedFont
    End Sub

    Protected Overrides Sub OnLoad(ByVal e As EventArgs)
        MyBase.OnLoad(e)
        UiStyle.Tool(tbSpool, "Open", "NEW", AddressOf OpenClicked)
        UiStyle.Tool(tbSpool, "Prev", "REFRESH", AddressOf PrevClicked)
        UiStyle.Tool(tbSpool, "Next", "RUN", AddressOf NextClicked)
        tbSpool.Items.Add(New ToolStripSeparator())
        UiStyle.Tool(tbSpool, "Find (F2)", "FIND", AddressOf FindClicked)
        UiStyle.Tool(tbSpool, "Print", "PRINT", AddressOf PrintClicked)
        UiStyle.Tool(tbSpool, "Save As", "SAVE", AddressOf SaveClicked)
        tbSpool.Items.Add(New ToolStripSeparator())
        UiStyle.Tool(tbSpool, "Close (F3)", "CLOSE", Sub(s As Object, a As EventArgs) CloseForm())
        If FormArgument IsNot Nothing AndAlso FormArgument.Trim().Length > 0 Then
            LoadFile(FormArgument.Trim())
        Else
            OpenClicked(Nothing, EventArgs.Empty)
        End If
    End Sub


    Public Sub LoadFile(ByVal spoolFile As String)
        Guard(Sub()
                  If Not File.Exists(spoolFile) Then
                      ShowError("Spool file " & spoolFile & " no longer exists.", "SPL0001")
                      Return
                  End If
                  _file = spoolFile
                  _pages.Clear()
                  For Each p As String In File.ReadAllText(spoolFile).Split(ChrW(12))
                      If p.Trim().Length > 0 Then _pages.Add(p.TrimStart(ControlChars.Lf, ControlChars.Cr))
                  Next
                  If _pages.Count = 0 Then _pages.Add("(the report produced no output)")
                  _page = 0
                  Me.Text = "Spool - " & Path.GetFileName(spoolFile)
                  ShowPage()
              End Sub)
    End Sub

    Private Sub ShowPage()
        txtPage.Text = _pages(_page).Replace(vbLf, vbCrLf).Replace(vbCr & vbCr, vbCr)
        txtPage.Select(0, 0)
        lblPage.Text = "Page " & (_page + 1) & " of " & _pages.Count
        SetStatus(_file, "")
    End Sub

    Private Sub OpenClicked(ByVal sender As Object, ByVal e As EventArgs)
        Using d As New OpenFileDialog()
            d.InitialDirectory = Path.Combine(SterpConfig.RunDir, "spool")
            d.Filter = "Spool files (*.lst)|*.lst|Log files (*.log)|*.log|All files (*.*)|*.*"
            If d.ShowDialog(Me) = DialogResult.OK Then LoadFile(d.FileName)
        End Using
    End Sub

    Private Sub PrevClicked(ByVal sender As Object, ByVal e As EventArgs)
        If _page > 0 Then
            _page -= 1
            ShowPage()
        End If
    End Sub

    Private Sub NextClicked(ByVal sender As Object, ByVal e As EventArgs)
        If _page < _pages.Count - 1 Then
            _page += 1
            ShowPage()
        End If
    End Sub

    Protected Overrides Sub OnLookup()
        FindClicked(Nothing, EventArgs.Empty)
    End Sub

    Private Sub FindClicked(ByVal sender As Object, ByVal e As EventArgs)
        Dim text As String = InputBox("Text to find (the search continues over every page):",
                                      "STERP - Find in spool file", "")
        If text Is Nothing OrElse text.Trim().Length = 0 Then Return
        For i As Integer = 0 To _pages.Count - 1
            Dim p As Integer = (_page + i) Mod _pages.Count
            Dim at As Integer = _pages(p).IndexOf(text, StringComparison.OrdinalIgnoreCase)
            If at >= 0 Then
                _page = p
                ShowPage()
                txtPage.Select(Math.Max(0, txtPage.Text.IndexOf(text, StringComparison.OrdinalIgnoreCase)), text.Length)
                txtPage.ScrollToCaret()
                txtPage.Focus()
                SetStatus("Found on page " & (p + 1) & ".", "OK")
                Return
            End If
        Next
        SetStatus("'" & text & "' was not found in this spool file.", "!")
    End Sub

    Private Sub SaveClicked(ByVal sender As Object, ByVal e As EventArgs)
        If _file = "" Then Return
        Using d As New SaveFileDialog()
            d.FileName = Path.GetFileName(_file)
            d.Filter = "Spool files (*.lst)|*.lst|Text files (*.txt)|*.txt|All files (*.*)|*.*"
            If d.ShowDialog(Me) = DialogResult.OK Then
                File.Copy(_file, d.FileName, True)
                SetStatus("Saved to " & d.FileName, "OK")
            End If
        End Using
    End Sub

    Private Sub PrintClicked(ByVal sender As Object, ByVal e As EventArgs)
        If _pages.Count = 0 Then Return
        Using d As New PrintDialog()
            d.Document = _doc
            If d.ShowDialog(Me) <> DialogResult.OK Then Return
        End Using
        _printPage = 0
        _doc.DocumentName = Path.GetFileName(_file)
        _doc.Print()
        SetStatus("Sent to " & _doc.PrinterSettings.PrinterName, "OK")
    End Sub

    Private Sub Doc_PrintPage(ByVal sender As Object, ByVal e As PrintPageEventArgs) Handles _doc.PrintPage
        Using f As New Font("Courier New", 7.0!)
            e.Graphics.DrawString(_pages(_printPage), f, Brushes.Black, e.MarginBounds)
        End Using
        _printPage += 1
        e.HasMorePages = (_printPage < _pages.Count)
    End Sub

    Protected Overrides Sub OnKeyDown(ByVal e As KeyEventArgs)
        If e.KeyCode = Keys.PageDown AndAlso e.Control Then
            NextClicked(Nothing, EventArgs.Empty)
            e.Handled = True
        ElseIf e.KeyCode = Keys.PageUp AndAlso e.Control Then
            PrevClicked(Nothing, EventArgs.Empty)
            e.Handled = True
        End If
        If Not e.Handled Then MyBase.OnKeyDown(e)
    End Sub

End Class
