Imports STERP.Business
Imports STERP.CobolInterop
Imports STERP.Common

''' <summary>
''' Manual GL journal entry.  The screen collects a header and its lines, has GLVALID check
''' each line as it is added, JNLENTRY validate and stage the whole journal and JNLPOST post
''' it under one correlation id, so a repeat is never a second posting.
''' REQ: FIN-002, FIN-003, FIN-004, FIN-005, COM-015
''' </summary>
Public Class frmJournalEntry
    Inherits BaseForm

    Private Shared ReadOnly _headSpec As String()() = New String()() {
        New String() {"REF", "Journal &ref:", "150"}, New String() {"TYPE", "T&ype:", "90"},
        New String() {"POSTDATE", "&Posting date:", "90"}, New String() {"DOCDATE", "D&ocument date:", "90"},
        New String() {"CCY", "C&urrency:", "60"}, New String() {"EVIDENCE", "&Evidence ref:", "250"},
        New String() {"DESC", "Descri&ption:", "250"}}

    Private Shared ReadOnly _lineSpec As String()() = New String()() {
        New String() {"AccountCode", "&Account:", "110"}, New String() {"CostCentre", "&Cost centre:", "90"},
        New String() {"Project", "Pro&ject:", "90"}, New String() {"VatCode", "V&AT code:", "50"},
        New String() {"Debit", "De&bit:", "90"}, New String() {"Credit", "Cre&dit:", "90"},
        New String() {"Narrative", "&Narrative:", "250"}}

    Private ReadOnly _svc As New JournalService()
    Private ReadOnly _head As New Dictionary(Of String, TextBox)
    Private ReadOnly _line As New Dictionary(Of String, TextBox)
    Private ReadOnly _lines As DataTable = JournalService.NewLineTable()
    Private _grid As DataGridView
    Private _staged As Boolean

    Public Sub New()
        Me.Text = "Journal Entry"
        Me.ClientSize = New Size(860, 530)
        Build(_headSpec, _head, 8, 4)
        UiStyle.Caption(Me, "Journal line - F2 looks up an account:", 8, 112, 300)
        Build(_lineSpec, _line, 12, 132)
        ' the action row sits between the line fields and the list so it cannot collide
        ' with a second or third column of entry fields
        AddHandler UiStyle.Push(Me, "&Add line", 12, 232, 90).Click, AddressOf AddLineClicked
        AddHandler UiStyle.Push(Me, "&Remove", 108, 232, 90).Click, AddressOf RemoveClicked
        AddHandler UiStyle.Push(Me, "Va&lidate", 204, 232, 90).Click, AddressOf ValidateClicked
        AddHandler UiStyle.Push(Me, "P&ost (F10)", 300, 232, 90).Click, AddressOf PostClicked
        _grid = New DataGridView()
        _grid.SetBounds(8, 262, 844, 230)
        _grid.Anchor = AnchorStyles.Top Or AnchorStyles.Left Or AnchorStyles.Right Or AnchorStyles.Bottom
        GridHelper.Style(_grid)
        Me.Controls.Add(_grid)
    End Sub

    Private Sub Build(ByVal spec As String()(), ByVal into As Dictionary(Of String, TextBox),
                      ByVal x0 As Integer, ByVal y0 As Integer)
        Dim x As Integer = x0, y As Integer = y0, n As Integer = 0
        For Each f As String() In spec
            UiStyle.Caption(Me, f(1), x, y, 86)
            into(f(0)) = UiStyle.Edit(Me, x + 88, y, CInt(f(2)))
            n += 1
            y += 26
            If n Mod 4 = 0 Then x += 352 : y = y0
        Next
    End Sub

    Protected Overrides Sub OnLoad(ByVal e As EventArgs)
        MyBase.OnLoad(e)
        _head("TYPE").Text = If(FormArgument <> "", FormArgument, "GL")
        _head("CCY").Text = SterpSession.Current.BaseCurrency
        _head("POSTDATE").Text = Fmt.Date(SterpSession.Current.BusinessDate)
        _head("DOCDATE").Text = _head("POSTDATE").Text
        _svc.StartNew()
        GridHelper.Bind(_grid, _lines)
        SetStatus("Enter the header, add the lines, then Validate and press F10 to post.  Period " &
                  SterpSession.Current.PeriodText & ".")
    End Sub

    Protected Overrides Sub OnLookup()
        Dim code As String = frmLookup.Pick(Me, "core.usp_UiLookup", "FIN.ACCOUNT", "Account")
        If code <> "" Then _line("AccountCode").Text = code
    End Sub

    Protected Overrides Sub OnConfirm()
        PostClicked(Nothing, EventArgs.Empty)
    End Sub

    Protected Overrides Function CanClose() As Boolean
        Return _lines.Rows.Count = 0 OrElse Confirm("Abandon this unposted journal?")
    End Function

    ''' <summary>GLVALID checks the account and its dimensions before the line is accepted (FIN-002, FIN-003).</summary>
    Private Sub AddLineClicked(ByVal sender As Object, ByVal e As EventArgs)
        Dim account As String = _line("AccountCode").Text.Trim().ToUpperInvariant()
        Dim debit As Decimal = Fmt.Dec(_line("Debit").Text), credit As Decimal = Fmt.Dec(_line("Credit").Text)
        If account = "" OrElse (debit = 0D AndAlso credit = 0D) Then
            SetStatus("A line needs an account and either a debit or a credit.", "!  WARNING")
            Return
        End If
        Guard(Sub()
                  Dim r As CobolResult = _svc.ValidateLine(account, _line("CostCentre").Text.Trim().ToUpperInvariant(),
                                                           _line("Project").Text.Trim().ToUpperInvariant(),
                                                           _line("VatCode").Text.Trim().ToUpperInvariant(),
                                                           If(debit > 0D, "DR", "CR"), If(debit > 0D, debit, credit))
                  If r.IsError OrElse r.IsSevere Then
                      ShowError(r.Message, r.ReasonCode)
                      Return
                  End If
                  Dim row As DataRow = _lines.NewRow()
                  For Each f As String() In _lineSpec
                      row(f(0)) = If(f(0) = "Debit" OrElse f(0) = "Credit", CObj(Fmt.Dec(_line(f(0)).Text)),
                                     CObj(_line(f(0)).Text.Trim()))
                  Next
                  row("AccountCode") = account
                  _lines.Rows.Add(row)
                  _staged = False
                  For Each box As TextBox In _line.Values
                      box.Text = ""
                  Next
                  GridHelper.Bind(_grid, _lines)
                  SetStatus("Line " & _lines.Rows.Count & " added - " & account & " " & _svc.AccountName &
                            ".  Debits " & Fmt.Money(Total("Debit")) & ", credits " & Fmt.Money(Total("Credit")) &
                            ".", "OK")
              End Sub)
    End Sub

    Private Sub RemoveClicked(ByVal sender As Object, ByVal e As EventArgs)
        Dim row As DataRow = GridHelper.SelectedRow(_grid)
        If row Is Nothing Then
            SetStatus("Select the line to remove first.", "!  WARNING")
            Return
        End If
        row.Delete()
        _lines.AcceptChanges()
        _staged = False
        GridHelper.Bind(_grid, _lines)
        SetStatus(_lines.Rows.Count & " line(s) left.", "")
    End Sub

    ''' <summary>JNLENTRY VALIDAT reports the balance, the period and every bad line (FIN-004).</summary>
    Private Sub ValidateClicked(ByVal sender As Object, ByVal e As EventArgs)
        Guard(Sub()
                  Dim r As CobolResult = _svc.Prepare("VALIDAT", Header(), _lines)
                  If _svc.LineResults IsNot Nothing Then GridHelper.Bind(_grid, _svc.LineResults)
                  If Not ShowResult(r) Then Return
                  SetStatus("Debits " & Fmt.Money(_svc.TotalDebit) & ", credits " & Fmt.Money(_svc.TotalCredit) &
                            ", out of balance " & Fmt.Money(_svc.OutOfBalance) & ", period " & _svc.PeriodStatus &
                            ", " & _svc.ErrorCount & " error(s).", If(_svc.ErrorCount = 0, "OK", "!  WARNING"))
              End Sub)
    End Sub

    ''' <summary>SAVE stages the lines, POST commits them under the same correlation id (FIN-005).</summary>
    Private Sub PostClicked(ByVal sender As Object, ByVal e As EventArgs)
        If _lines.Rows.Count < 2 Then
            SetStatus("A journal needs at least two lines.", "!  WARNING")
            Return
        End If
        If Not Confirm("Post this journal of " & Fmt.Money(Total("Debit")) & " each side?") Then Return
        Guard(Sub()
                  If Not _staged Then
                      If Not ShowResult(_svc.Prepare("SAVE", Header(), _lines)) Then Return
                      _staged = True
                  End If
                  Dim r As CobolResult = _svc.Post(Header())
                  If Not ShowResult(r) Then Return
                  SetStatus("Journal " & _svc.JournalNumber & " posted, " & Fmt.Money(_svc.TotalDebit) & " each side." &
                            If(_svc.WasDuplicate, "  (already posted under this correlation id)", ""), "OK")
                  _lines.Clear()
                  _staged = False
                  _svc.StartNew()
                  GridHelper.Bind(_grid, _lines)
              End Sub)
    End Sub

    Private Function Header() As Dictionary(Of String, String)
        Dim v As New Dictionary(Of String, String)
        For Each f As String() In _headSpec
            v(f(0)) = _head(f(0)).Text.Trim()
        Next
        Return v
    End Function

    Private Function Total(ByVal column As String) As Decimal
        Return Fmt.Dec(_lines.Compute("SUM(" & column & ")", ""))
    End Function

End Class
