Imports System.Collections.Generic
Imports System.Linq
Imports STERP.Business
Imports STERP.CobolInterop
Imports STERP.Common

''' <summary>
''' Bank reconciliation: statement entries on the left, the cash book on the right and the
''' bridge from the book balance to the statement balance on the status line.  BANKREC ties
''' n statement entries to m book entries and refuses unequal totals without an explanation.
''' REQ: FIN-013, FIN-014, FIN-015
''' </summary>
Public Class frmBankRecon
    Inherits BaseForm

    Private ReadOnly _svc As New BankService()
    Private _txtBank As TextBox
    Private _txtAsAt As TextBox
    Private _chkOutstanding As CheckBox
    Private _grdStatement As DataGridView
    Private _grdBook As DataGridView

    Public Sub New()
        Me.Text = "Bank Reconciliation"
        Me.ClientSize = New Size(880, 500)
        UiStyle.Caption(Me, "&Bank account:", 8, 8, 84)
        _txtBank = UiStyle.Edit(Me, 94, 8, 120)
        UiStyle.Caption(Me, "&As at:", 226, 8, 44)
        _txtAsAt = UiStyle.Edit(Me, 272, 8, 90)
        _chkOutstanding = UiStyle.Tick(Me, "&Outstanding book entries only", 374, 9, 190)
        _chkOutstanding.Checked = True
        AddHandler UiStyle.Push(Me, "&Refresh (F5)", 570, 6, 90).Click, Sub(s As Object, e As EventArgs) LoadData()
        AddHandler UiStyle.Push(Me, "&Match (F10)", 666, 6, 90).Click, AddressOf MatchClicked
        AddHandler UiStyle.Push(Me, "&Unmatch", 762, 6, 90).Click, AddressOf UnmatchClicked
        _grdStatement = Side("Statement entries:", 8, False)
        _grdBook = Side("Cash book entries:", 444, True)
    End Sub

    ''' <summary>One captioned, multi-select grid; both sides of the reconciliation look the same.</summary>
    Private Function Side(ByVal caption As String, ByVal x As Integer, ByVal anchorRight As Boolean) As DataGridView
        UiStyle.Caption(Me, caption, x, 36, 180)
        Dim g As New DataGridView()
        g.SetBounds(x, 56, 428, 414)
        g.Anchor = AnchorStyles.Top Or AnchorStyles.Left Or AnchorStyles.Bottom
        If anchorRight Then g.Anchor = g.Anchor Or AnchorStyles.Right
        GridHelper.Style(g)
        g.MultiSelect = True
        Me.Controls.Add(g)
        Return g
    End Function

    Protected Overrides Sub OnLoad(ByVal e As EventArgs)
        MyBase.OnLoad(e)
        _txtAsAt.Text = Fmt.Date(SterpSession.Current.BusinessDate)
        _txtBank.Text = FormArgument
        If _txtBank.Text = "" Then
            Dim t As DataTable = Nothing
            Guard(Sub() t = _svc.Accounts())
            If t IsNot Nothing AndAlso t.Rows.Count > 0 Then _txtBank.Text = Fmt.Str(t.Rows(0)("BankAccountCode"))
        End If
        If _txtBank.Text <> "" Then LoadData()
    End Sub

    Protected Overrides Sub OnRefreshKey()
        LoadData()
    End Sub

    Protected Overrides Sub OnConfirm()
        MatchClicked(Nothing, EventArgs.Empty)
    End Sub

    Protected Overrides Sub OnLookup()
        Dim code As String = frmLookup.Pick(Me, "core.usp_UiLookup", "FIN.BANK", "Bank account")
        If code <> "" Then
            _txtBank.Text = code
            LoadData()
        End If
    End Sub

    ''' <summary>Both sides plus the bridge: book balance, outstanding items, statement balance (FIN-013).</summary>
    Private Sub LoadData()
        Guard(Sub()
                  Dim account As String = Bank()
                  Dim parsed As Date? = Fmt.ParseDate(_txtAsAt.Text)
                  Dim asAt As Date = If(parsed.HasValue, parsed.Value, SterpSession.Current.BusinessDate)
                  GridHelper.Bind(_grdStatement, _svc.StatementLines(account, "ALL"))
                  GridHelper.Bind(_grdBook, _svc.BookLines(account, asAt, _chkOutstanding.Checked))
                  _svc.Bridge(account, asAt)
                  SetStatus("Book " & Fmt.Money(_svc.BookBalance) & " | outstanding receipts " &
                            Fmt.Money(_svc.OutstandingReceipts) & " | payments " & Fmt.Money(_svc.OutstandingPayments) &
                            " | statement " & Fmt.Money(_svc.StatementBalance) & " | bridged " &
                            Fmt.Money(_svc.BridgedBalance) & " | unexplained " & Fmt.Money(_svc.Unexplained) &
                            " | " & _svc.UnmatchedCount & " unmatched", If(_svc.Unexplained = 0D, "OK", "!  WARNING"))
              End Sub)
    End Sub

    ''' <summary>Ties the selected statement entries to the selected book entries (FIN-014).</summary>
    Private Sub MatchClicked(ByVal sender As Object, ByVal e As EventArgs)
        Dim statement As List(Of Decimal()) = Picked(_grdStatement, "StatementLineId", "Amount")
        Dim book As List(Of Decimal()) = Picked(_grdBook, "JournalLineId", "SignedGBP")
        If statement.Count = 0 OrElse book.Count = 0 Then
            SetStatus("Select at least one entry on each side.  Hold Ctrl to pick several.", "!  WARNING")
            Return
        End If
        Dim narrative As String = ""
        Dim difference As Decimal = statement.Sum(Function(x) x(1)) - book.Sum(Function(x) x(1))
        If difference <> 0D Then
            narrative = InputBox("The two sides differ by " & Fmt.Money(difference) &
                                 ".  Explain the difference in writing, or cancel:", "STERP - Grouped Match", "")
            If narrative Is Nothing OrElse narrative.Trim() = "" Then
                SetStatus("Unequal totals are only reconciled with a written explanation.", "!  WARNING")
                Return
            End If
        End If
        Guard(Sub()
                  Dim r As CobolResult = _svc.Match(Bank(), statement, book, narrative.Trim())
                  If Not ShowResult(r) Then Return
                  SetStatus(_svc.MatchType & " match of " & _svc.MatchedCount & " entries: statement " &
                            Fmt.Money(_svc.StatementTotal) & " against book " & Fmt.Money(_svc.BookTotal) &
                            ", difference " & Fmt.Money(_svc.Difference) & ".", "OK")
                  LoadData()
              End Sub)
    End Sub

    ''' <summary>Reverses a match group; the statement evidence stays on file (FIN-014).</summary>
    Private Sub UnmatchClicked(ByVal sender As Object, ByVal e As EventArgs)
        Dim matchId As Object = GridHelper.Selected(_grdBook, "ReconMatchId")
        If matchId Is Nothing Then
            SetStatus("Select a reconciled cash book entry first.", "!  WARNING")
            Return
        End If
        Dim reason As String = InputBox("Why is match group " & Fmt.Str(matchId) & " being reopened?",
                                        "STERP - Unmatch", "")
        If reason Is Nothing OrElse reason.Trim() = "" Then
            SetStatus("Reopening a match needs a recorded reason.", "!  WARNING")
            Return
        End If
        Guard(Sub()
                  If ShowResult(_svc.Unmatch(Bank(), matchId, reason.Trim())) Then LoadData()
              End Sub)
    End Sub

    Private Shared Function Picked(ByVal g As DataGridView, ByVal idColumn As String,
                                   ByVal amountColumn As String) As List(Of Decimal())
        Dim chosen As New List(Of Decimal())
        For Each r As DataGridViewRow In g.SelectedRows
            Dim v As DataRowView = TryCast(r.DataBoundItem, DataRowView)
            If v Is Nothing OrElse Not v.Row.Table.Columns.Contains(idColumn) Then Continue For
            chosen.Add(New Decimal() {Fmt.Dec(v.Row(idColumn)), Fmt.Dec(v.Row(amountColumn))})
        Next
        Return chosen
    End Function

    Private Function Bank() As String
        Return _txtBank.Text.Trim().ToUpperInvariant()
    End Function

End Class
