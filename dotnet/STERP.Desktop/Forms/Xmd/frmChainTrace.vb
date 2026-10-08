Imports System.Data
Imports STERP.Business.Xmd
Imports STERP.CobolInterop
Imports STERP.Common

''' <summary>
''' Business chain trace.  One document reference in, the ordered steps of its order to cash,
''' procure to pay or assembly chain out.  CHAINTRC returns at most 24 steps, so a long chain
''' is reported as truncated rather than silently shortened; returns, credits and reversals
''' keep their own line and their own kind, so a reversal is never read as a cancellation.
''' REQ: XMD-001, XMD-002, XMD-003, XMD-004
''' </summary>
Public Class frmChainTrace
    Inherits ListForm

    Private ReadOnly _svc As New XmdService()
    Private _cboType As ComboBox
    Private _txtRef As TextBox

    Public Sub New()
        InitializeComponent()
    End Sub

    Protected Overrides Sub BuildFilters()
        _cboType = FilterList("Document &type:", 130, "", "ORDER", "PO", "KIT", "INVOICE",
                              "DESPATCH", "RECEIPT", "PAYMENT", "RETURN")
        _txtRef = FilterEdit("&Reference:", 180)
        If FormArgument <> "" Then _txtRef.Text = FormArgument
    End Sub

    Protected Overrides Sub BuildTools()
        AddSeparator()
        AddTool("Trace (F10)", "ENGINE", AddressOf TraceClicked)
        AddTool("Whole chain", "FIND", AddressOf ListClicked)
        AddSeparator()
    End Sub

    Protected Overrides Sub LoadData()
        If _txtRef Is Nothing Then Return
        If Code(_txtRef) = "" Then
            SetStatus("Enter a document reference, then Trace (F10).", "")
            Return
        End If
        TraceClicked(Nothing, EventArgs.Empty)
    End Sub

    Protected Overrides Sub OnConfirm()
        TraceClicked(Nothing, EventArgs.Empty)
    End Sub

    ''' <summary>The engine answer: the chain exactly as CHAINTRC walks it.</summary>
    Private Sub TraceClicked(ByVal sender As Object, ByVal e As EventArgs)
        If Code(_txtRef) = "" Then
            SetStatus("A chain trace needs a document reference.", "!  WARNING")
            Return
        End If
        Guard(Sub()
                  Dim r As CobolResult = _svc.ChainTrace(DocType(), Code(_txtRef))
                  If _svc.Steps IsNot Nothing Then Populate(_svc.Steps)
                  If Not ShowResult(r) Then Return
                  SetStatus(Describe(), StatusTag())
              End Sub)
    End Sub

    ''' <summary>The same chain from rpt.usp_ChainTrace, which is not capped at 24 steps.</summary>
    Private Sub ListClicked(ByVal sender As Object, ByVal e As EventArgs)
        If Code(_txtRef) = "" Then
            SetStatus("A chain trace needs a document reference.", "!  WARNING")
            Return
        End If
        If Not XmdService.IsSetTraceable(DocType()) Then
            SetStatus("The whole chain is walked from an ORDER, a PO or a KIT - choose one.",
                      "!  WARNING")
            Return
        End If
        Guard(Sub()
                  Dim rows As DataTable = _svc.ChainTraceList(DocType(), Code(_txtRef))
                  If rows Is Nothing Then
                      SetStatus("rpt.usp_ChainTrace is not deployed on this database.", "!  WARNING")
                      Return
                  End If
                  Populate(rows)
                  SetStatus("rpt.usp_ChainTrace returned " & rows.Rows.Count &
                            " step(s) for " & Code(_txtRef) & ".", "OK")
              End Sub)
    End Sub

    Private Function Describe() As String
        Dim s As String = _svc.Text("CT-CHAIN-NAME").Trim()
        If s = "" Then s = _svc.Text("CT-CHAIN-TYPE").Trim()
        Return s & ": " & CInt(_svc.Num("CT-STEP-CNT")) & " step(s), party " &
               _svc.Text("CT-PARTY").Trim() & ", value " & Fmt.Money(_svc.Num("CT-TOTAL-GBP")) &
               ", " & CInt(_svc.Num("CT-REVERSALS")) & " reversal(s)" &
               If(_svc.Text("CT-COMPLETE") = "Y", ", complete", "") &
               If(_svc.Text("CT-BROKEN") = "Y", ", *** BROKEN CHAIN", "") &
               If(_svc.Text("CT-TRUNCATED") = "Y",
                  ", *** TRUNCATED AT " & XmdService.MaxSteps & " STEPS", "")
    End Function

    Private Function StatusTag() As String
        If _svc.Text("CT-BROKEN") = "Y" OrElse _svc.Text("CT-TRUNCATED") = "Y" Then
            Return "!  WARNING"
        End If
        Return "OK"
    End Function

    Private Function DocType() As String
        Return Convert.ToString(_cboType.SelectedItem).Trim().ToUpperInvariant()
    End Function

    Private Shared Function Code(ByVal t As TextBox) As String
        Return t.Text.Trim().ToUpperInvariant()
    End Function

End Class
