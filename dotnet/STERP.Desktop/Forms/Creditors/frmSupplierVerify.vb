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
        AddTool("CoP check", "RUN", AddressOf CopClicked)
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
        Guard(Sub()
                  Dim r As CobolResult = _svc.ConfirmationOfPayee(_bankSupplier, BankId())
                  If Not ShowResult(r) Then Return
                  SetStatus("Confirmation of Payee: " & _svc.CopResult & ". " & _svc.Note,
                            If(_svc.CopResult = "MATCH", "OK", "!  WARNING"))
                  Populate(_svc.Banks(_bankSupplier))
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
