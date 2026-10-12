Imports STERP.Business
Imports STERP.CobolInterop
Imports STERP.Common

''' <summary>
''' Supplier verification: whether a supplier may be ordered from or paid, and the state of its
''' bank records.  A bank change always needs a second pair of eyes, so a clerk who tries to
''' approve a change they prepared themselves is refused by the engine and told so here.
''' REQ: AP-001, AP-002, AP-024, AP-027
''' </summary>
Public Class frmSupplierVerify
    Inherits ListForm

    Private ReadOnly _svc As New SupplierService()
    Private _txtSupplier As TextBox
    Private _cboView As ComboBox
    Private _showingBanks As Boolean
    Private _bankSupplier As String = ""

    Public Sub New()
        Me.Text = "Supplier Verification"
    End Sub

    Protected Overrides Sub BuildFilters()
        _txtSupplier = FilterEdit("S&upplier:", 150)
        _cboView = FilterList("&View:", 120, "ALL", "HOLD", "UNVERIFIED")
        If FormArgument <> "" Then _txtSupplier.Text = FormArgument
        AddHandler _cboView.SelectedIndexChanged, Sub(s As Object, e As EventArgs) LoadData()
    End Sub

    Protected Overrides Sub BuildTools()
        AddSeparator()
        AddTool("Verify (F10)", "ENGINE", AddressOf VerifyClicked)
        AddTool("Payable?", "FIND", AddressOf PayableClicked)
        AddSeparator()
        AddTool("Bank records", "FIND", AddressOf BanksClicked)
        AddTool("Change bank", "EDIT", AddressOf ChangeClicked)
        AddTool("CoP check", "RUN", AddressOf CopClicked)
        AddTool("Record callback", "RUN", AddressOf CallbackClicked)
        AddTool("Approve bank", "POST", AddressOf ApproveClicked)
        AddSeparator()
    End Sub

    Protected Overrides Sub LoadData()
        _showingBanks = False
        Guard(Sub()
                  Populate(_svc.Suppliers(Supplier(), Convert.ToString(_cboView.SelectedItem)))
                  SetStatus(Grid.Rows.Count & " supplier(s).", "")
              End Sub)
    End Sub

    Protected Overrides Sub OnConfirm()
        VerifyClicked(Nothing, EventArgs.Empty)
    End Sub

    ''' <summary>APSUPVAL owns the answer; the screen only reports it.</summary>
    Private Sub VerifyClicked(ByVal sender As Object, ByVal e As EventArgs)
        Dim code As String = Chosen()
        If code = "" Then Return
        Guard(Sub()
                  Dim r As CobolResult = _svc.Validate(code)
                  If Not ShowResult(r) Then Return
                  SetStatus(code & ": balance " & Fmt.Money(_svc.Balance) & " over " & _svc.OpenItems &
                            " open item(s) | terms " & _svc.TermDays & " days | purchase " &
                            If(_svc.CanPurchase, "ALLOWED", "BLOCKED") & " | payment " &
                            If(_svc.CanPay, "ALLOWED", "BLOCKED"),
                            If(_svc.CanPurchase AndAlso _svc.CanPay, "OK", "## HOLD"))
              End Sub)
    End Sub

    Private Sub PayableClicked(ByVal sender As Object, ByVal e As EventArgs)
        Dim code As String = Chosen()
        If code = "" Then Return
        Guard(Sub()
                  Dim r As CobolResult = _svc.PaymentCheck(code)
                  If Not ShowResult(r) Then Return
                  SetStatus(code & " may " & If(_svc.CanPay, "be paid. ", "NOT be paid. ") & _svc.Note,
                            If(_svc.CanPay, "OK", "## HOLD"))
              End Sub)
    End Sub

    Private Sub BanksClicked(ByVal sender As Object, ByVal e As EventArgs)
        Dim code As String = Chosen()
        If code = "" Then Return
        Guard(Sub()
                  _bankSupplier = code
                  _showingBanks = True
                  Populate(_svc.Banks(code))
                  SetStatus("Bank records for " & code & ". A change stays unusable until an " &
                            "independent approver accepts it.", "")
              End Sub)
    End Sub

    Private Sub CopClicked(ByVal sender As Object, ByVal e As EventArgs)
        If Not RequireBankRow() Then Return
        Dim id As Integer = BankId()
        Dim v As Dictionary(Of String, String) = ApInputDialog.Collect(Me, "Record Confirmation of Payee",
            New String()() {
                New String() {"RESULT", "Result (MATCH/CLOSEMATCH/NOMATCH/UNAVAILABLE)", "", "12"},
                New String() {"REFERENCE", "CoP evidence reference", "", "60"},
                New String() {"DECISION", "Authorised decision (required unless MATCH)", "", "200"}})
        If v Is Nothing Then Return
        Guard(Sub()
                  Dim r As CobolResult = _svc.ConfirmationOfPayee(_bankSupplier, id,
                      v("RESULT").ToUpperInvariant(), v("REFERENCE"), v("DECISION"))
                  If Not ShowResult(r) Then Return
                  SetStatus("Confirmation of Payee: " & _svc.CopResult & ". " & _svc.Note,
                            If(_svc.CopResult = "MATCH", "OK", "!  WARNING"))
                  Populate(_svc.Banks(_bankSupplier))
              End Sub)
    End Sub

    Private Sub ChangeClicked(ByVal sender As Object, ByVal e As EventArgs)
        Dim code As String = Chosen()
        If code = "" Then Return
        Dim v As Dictionary(Of String, String) = ApInputDialog.Collect(Me, "Prepare GB / GBP Bacs bank change",
            New String()() {
                New String() {"BANKREF", "New unique bank reference (maximum 10 characters)", "", "10"},
                New String() {"BENEFICIARY", "Beneficiary name", "", "120"},
                New String() {"SORTCODE", "Sort code (six digits)", "", "6"},
                New String() {"ACCOUNT", "Account number (eight digits)", "", "8"},
                New String() {"BANKNAME", "Bank name", "", "60"},
                New String() {"SOURCE", "Request source (EMAIL/PORTAL/LETTER/CALLBACK)", "", "30"}})
        If v Is Nothing Then Return
        Guard(Sub()
                  Dim r As CobolResult = _svc.PrepareBank(code, v("BANKREF").ToUpperInvariant(),
                      v("BENEFICIARY"), v("SORTCODE"), v("ACCOUNT"), v("BANKNAME"),
                      v("SOURCE").ToUpperInvariant())
                  If Not ShowResult(r) Then Return
                  _bankSupplier = code
                  _showingBanks = True
                  Populate(_svc.Banks(code))
                  SetStatus("Bank change " & _svc.BankId & " is " & _svc.BankStatus &
                            ". Record CoP and an independent callback, then obtain independent approval.",
                            "!  PENDING")
              End Sub)
    End Sub

    Private Sub CallbackClicked(ByVal sender As Object, ByVal e As EventArgs)
        If Not RequireBankRow() Then Return
        Dim id As Integer = BankId()
        Dim v As Dictionary(Of String, String) = ApInputDialog.Collect(Me, "Record independent callback",
            New String()() {
                New String() {"CONTACT", "Contact already held on record (not change request)", "", "120"},
                New String() {"ONFILE", "Contact was already on file? Enter Y or N", "", "1"},
                New String() {"NOTE", "Independent callback evidence / outcome", "", "300"}})
        If v Is Nothing Then Return
        If Not Confirm("Record an independent callback by " & SterpSession.Current.UserCode &
                       "? Do not use a contact supplied with the change request.") Then Return
        Guard(Sub()
                  Dim r As CobolResult = _svc.RecordCallback(_bankSupplier, id, v("CONTACT"),
                      v("ONFILE").ToUpperInvariant() = "Y", v("NOTE"))
                  If Not ShowResult(r) Then Return
                  Populate(_svc.Banks(_bankSupplier))
                  SetStatus("Callback recorded; bank status " & _svc.BankStatus &
                            ". Independent approval is still required.", "OK")
              End Sub)
    End Sub

    ''' <summary>Approval is the engine's decision.  Approving your own change comes back as an
    ''' error and is shown as one - it is never silently ignored (AP-027, D-19).</summary>
    Private Sub ApproveClicked(ByVal sender As Object, ByVal e As EventArgs)
        If Not RequireBankRow() Then Return
        If Not Confirm("Approve this bank change for " & _bankSupplier & "?") Then Return
        Guard(Sub()
                  Dim r As CobolResult = _svc.ApproveBank(_bankSupplier, BankId(), "APPROVE")
                  If Not ShowResult(r) Then
                      SetStatus("Approval refused: " & r.Display, "*** ERROR")
                      Return
                  End If
                  Populate(_svc.Banks(_bankSupplier))
                  SetStatus("Bank record approved, status " & _svc.BankStatus & ".", "OK")
              End Sub)
    End Sub

    Private Function RequireBankRow() As Boolean
        If Not _showingBanks OrElse _bankSupplier = "" Then
            SetStatus("Show the bank records of a supplier first.", "!  WARNING")
            Return False
        End If
        If BankId() = 0 Then
            SetStatus("Select a bank record first.", "!  WARNING")
            Return False
        End If
        Return True
    End Function

    Private Function BankId() As Integer
        Return Fmt.Int(GridHelper.Selected(Grid, "SupplierBankId"))
    End Function

    Private Function Chosen() As String
        If _showingBanks Then Return _bankSupplier
        Dim code As String = Fmt.Str(GridHelper.Selected(Grid, "SupplierCode"))
        If code = "" Then code = Supplier()
        If code = "" Then SetStatus("Select a supplier first.", "!  WARNING")
        Return code
    End Function

    Private Function Supplier() As String
        Return _txtSupplier.Text.Trim().ToUpperInvariant()
    End Function

End Class

''' <summary>Small keyed capture dialog shared by the two purchase-ledger list screens.</summary>
Friend NotInheritable Class ApInputDialog
    Public Shared Function Collect(ByVal owner As Form, ByVal title As String,
                                   ByVal fields As String()()) As Dictionary(Of String, String)
        Using dialog As New Form()
            dialog.Text = title
            dialog.FormBorderStyle = FormBorderStyle.FixedDialog
            dialog.StartPosition = FormStartPosition.CenterParent
            dialog.MinimizeBox = False
            dialog.MaximizeBox = False
            dialog.ClientSize = New Size(710, fields.Length * 30 + 55)
            Dim edits As New Dictionary(Of String, TextBox)
            Dim y As Integer = 12
            For Each field As String() In fields
                UiStyle.Caption(dialog, field(1) & ":", 10, y, 370)
                Dim edit As TextBox = UiStyle.Edit(dialog, 390, y, 305)
                edit.Name = field(0)
                edit.MaxLength = Integer.Parse(field(3))
                edit.Text = field(2)
                edits(field(0)) = edit
                y += 30
            Next
            Dim ok As Button = UiStyle.Push(dialog, "&OK", 500, y + 5, 90)
            ok.DialogResult = DialogResult.OK
            Dim cancel As Button = UiStyle.Push(dialog, "&Cancel", 600, y + 5, 90)
            cancel.DialogResult = DialogResult.Cancel
            dialog.AcceptButton = ok
            dialog.CancelButton = cancel
            If dialog.ShowDialog(owner) <> DialogResult.OK Then Return Nothing
            Dim values As New Dictionary(Of String, String)
            For Each entry As KeyValuePair(Of String, TextBox) In edits
                values(entry.Key) = entry.Value.Text.Trim()
            Next
            Dim invalid As String = WidthError(fields, values)
            If invalid <> "" Then
                MessageBox.Show(owner, invalid, "STERP - Input", MessageBoxButtons.OK, MessageBoxIcon.Error)
                Return Nothing
            End If
            Return values
        End Using
    End Function

    Friend Shared Function WidthError(ByVal fields As String()(),
                                     ByVal values As Dictionary(Of String, String)) As String
        For Each field As String() In fields
            If values(field(0)).Length > Integer.Parse(field(3)) Then
                Return field(0) & " must be at most " & field(3) & " characters. Nothing was recorded."
            End If
        Next
        Return ""
    End Function
End Class
