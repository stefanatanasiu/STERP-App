Imports STERP.Business
Imports STERP.CobolInterop
Imports STERP.Common

''' <summary>
''' Posted journal enquiry: headers with their approval, reversal and evidence trail, the
''' lines behind one journal, and the linked reversal of a posting that was wrong.  Posted
''' money is never altered, so a correction is always a second, linked entry.
''' REQ: FIN-004, FIN-006, FIN-023
''' </summary>
Public Class frmJournalEnquiry
    Inherits ListForm

    Private ReadOnly _svc As New JournalService()
    Private _txtNumber As TextBox
    Private _txtAccount As TextBox
    Private _cboStatus As ComboBox

    Public Sub New()
        Me.Text = "Journal Enquiry"
    End Sub

    Protected Overrides Sub BuildFilters()
        _txtNumber = FilterEdit("&Journal:", 150)
        _txtAccount = FilterEdit("&Account:", 110)
        _cboStatus = FilterList("&Status:", 110, "ALL", "POSTED", "DRAFT", "SUBMITTED", "APPROVED", "REJECTED", "VOID")
        If FormArgument <> "" Then _txtNumber.Text = FormArgument
        AddHandler _cboStatus.SelectedIndexChanged, Sub(s As Object, e As EventArgs) LoadData()
    End Sub

    Protected Overrides Sub BuildTools()
        AddSeparator()
        AddTool("Lines", "FIND", AddressOf LinesClicked)
        AddTool("Reverse (F10)", "REJECT", AddressOf ReverseClicked)
        AddSeparator()
    End Sub

    Protected Overrides Sub LoadData()
        Guard(Sub()
                  Dim status As String = Convert.ToString(_cboStatus.SelectedItem)
                  Populate(_svc.Enquiry(_txtNumber.Text.Trim().ToUpperInvariant(),
                                        If(status = "ALL", "", status),
                                        _txtAccount.Text.Trim().ToUpperInvariant()))
              End Sub)
    End Sub

    Protected Overrides Sub OnConfirm()
        ReverseClicked(Nothing, EventArgs.Empty)
    End Sub

    ''' <summary>Drill from a header to the lines that make it up (FIN-023).</summary>
    Private Sub LinesClicked(ByVal sender As Object, ByVal e As EventArgs)
        Dim id As Object = GridHelper.Selected(Grid, "JournalId")
        If id Is Nothing Then
            SetStatus("Select a journal first.", "!  WARNING")
            Return
        End If
        Guard(Sub()
                  Populate(_svc.Lines(id))
                  SetStatus("Lines of journal " & Fmt.Str(GridHelper.Selected(Grid, "JournalNumber")) &
                            ".  Press F5 to go back to the headers.", "")
              End Sub)
    End Sub

    ''' <summary>JNLPOST REVERSE writes a linked contra entry; nothing posted is edited (FIN-006).</summary>
    Private Sub ReverseClicked(ByVal sender As Object, ByVal e As EventArgs)
        Dim number As String = Fmt.Str(GridHelper.Selected(Grid, "JournalNumber"))
        If number = "" Then
            SetStatus("Select a posted journal first.", "!  WARNING")
            Return
        End If
        Dim reason As String = InputBox("Why is journal " & number & " being reversed?",
                                        "STERP - Journal Reversal", "")
        If reason Is Nothing OrElse reason.Trim() = "" Then
            SetStatus("A reversal needs a recorded reason.", "!  WARNING")
            Return
        End If
        Guard(Sub()
                  Dim r As CobolResult = _svc.Reverse(number, 100D, reason.Trim())
                  If Not ShowResult(r) Then Return
                  SetStatus("Journal " & number & " reversed by " & _svc.JournalNumber & ", " &
                            Fmt.Money(_svc.TotalDebit) & " each side.", "OK")
                  LoadData()
              End Sub)
    End Sub

End Class
