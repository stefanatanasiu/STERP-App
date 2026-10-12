Imports STERP.Business
Imports STERP.CobolInterop
Imports STERP.Common

''' <summary>
''' Purchase invoice register: what has been captured, how far it has been matched, whether it
''' may be paid and, when it may not, the reason.  Registration, the duplicate test and the
''' tolerance arithmetic all belong to APINVREG and APMATCH.
''' REQ: AP-003, AP-004, AP-006, AP-008, AP-023
''' </summary>
Public Class frmInvoiceRegister
    Inherits ListForm

    Private ReadOnly _svc As New InvoiceService()
    Private _txtFilter As TextBox
    Private _cboView As ComboBox

    Public Sub New()
        Me.Text = "Purchase Invoice Register"
    End Sub

    Protected Overrides Sub BuildFilters()
        _txtFilter = FilterEdit("Supplier or &reference:", 170)
        _cboView = FilterList("&View:", 120, "ALL", "BLOCKED", "UNMATCHED", "UNPAID", "OVERDUE")
        If FormArgument <> "" Then _txtFilter.Text = FormArgument
        AddHandler _cboView.SelectedIndexChanged, Sub(s As Object, e As EventArgs) LoadData()
    End Sub

    Protected Overrides Sub BuildTools()
        AddSeparator()
        AddTool("Match (F10)", "ENGINE", AddressOf MatchClicked)
        AddTool("Duplicate check", "FIND", AddressOf DuplicateClicked)
        AddTool("Open items", "RUN", AddressOf OpenItemsClicked)
        AddSeparator()
    End Sub

    Protected Overrides Sub LoadData()
        Guard(Sub()
                  Populate(_svc.Register(_txtFilter.Text.Trim(), Convert.ToString(_cboView.SelectedItem)))
                  SetStatus(Grid.Rows.Count & " document(s) in the register.", "")
              End Sub)
    End Sub

    Protected Overrides Sub OnConfirm()
        MatchClicked(Nothing, EventArgs.Empty)
    End Sub

    ''' <summary>APMATCH re-matches every line and reports the outcome; a line outside tolerance
    ''' leaves the invoice blocked for payment (AP-006, AP-008).</summary>
    Private Sub MatchClicked(ByVal sender As Object, ByVal e As EventArgs)
        Dim reg As String = Selected("RegisterNumber")
        If reg = "" Then Return
        Guard(Sub()
                  Dim r As CobolResult = _svc.MatchAll(reg)
                  If Not ShowResult(r) Then
                      SetStatus(reg & " could not be matched: " & r.Display, "*** ERROR")
                      Return
                  End If
                  LoadData()
                  SetStatus(reg & ": " & _svc.LinesMatched & " line(s) matched, " & _svc.LinesVariance &
                            " with a variance, " & _svc.LinesUnmatched & " unmatched | invoice is " &
                            _svc.MatchState, If(_svc.LinesVariance + _svc.LinesUnmatched = 0, "OK", "!  WARNING"))
              End Sub)
    End Sub

    ''' <summary>The duplicate test the clerk runs before keying a document again (AP-004).</summary>
    Private Sub DuplicateClicked(ByVal sender As Object, ByVal e As EventArgs)
        Dim row As DataRow = GridHelper.SelectedRow(Grid)
        If row Is Nothing Then
            SetStatus("Select a document first.", "!  WARNING")
            Return
        End If
        Dim supplier As String = Fmt.Str(GridHelper.Selected(Grid, "SupplierCode"))
        Dim docRef As String = Fmt.Str(GridHelper.Selected(Grid, "SupplierDocRef"))
        Dim docType As String = Fmt.Str(GridHelper.Selected(Grid, "DocumentType"))
        Dim gross As Decimal = Fmt.Dec(GridHelper.Selected(Grid, "StatedGross"))
        Dim raw As Object = GridHelper.Selected(Grid, "DocumentDate")
        Dim docDate As Date = If(raw Is Nothing OrElse IsDBNull(raw), SterpSession.Current.BusinessDate, Convert.ToDateTime(raw))
        Guard(Sub()
                  Dim r As CobolResult = _svc.DuplicateCheck(supplier, docRef, gross, docDate, docType)
                  If _svc.DuplicateFound Then
                      ShowWarning("This document is already registered as " & _svc.DuplicateRegister &
                                  ". Registering it again would pay the supplier twice.", r.ReasonCode)
                      SetStatus("Duplicate of " & _svc.DuplicateRegister & ".", "*** ERROR")
                  Else
                      SetStatus("No exact duplicate; " & _svc.SimilarCount & " similar document(s) to judge. " &
                                _svc.Note, If(_svc.SimilarCount = 0, "OK", "!  WARNING"))
                  End If
              End Sub)
    End Sub

    Private Sub OpenItemsClicked(ByVal sender As Object, ByVal e As EventArgs)
        Dim supplier As String = Fmt.Str(GridHelper.Selected(Grid, "SupplierCode"))
        Guard(Sub()
                  Populate(_svc.OpenItems(supplier, "OPEN"))
                  SetStatus("Open items for " & If(supplier = "", "the whole ledger", supplier) &
                            ". Press F5 for the register.", "")
              End Sub)
    End Sub

    Private Function Selected(ByVal column As String) As String
        Dim v As String = Fmt.Str(GridHelper.Selected(Grid, column))
        If v = "" Then SetStatus("Select a document first.", "!  WARNING")
        Return v
    End Function

End Class
