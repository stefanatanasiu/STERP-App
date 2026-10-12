Imports System.Collections.Generic
Imports STERP.Business
Imports STERP.CobolInterop
Imports STERP.Common

''' <summary>
''' Goods receipt and inspection.  VALIDATE reads the outstanding lines of the order into the
''' COMMAREA, the counted quantities are written back into the same buffer and F10 confirms the
''' delivery - RECEIPT decides ownership, tolerance and what is quarantined, INSPECT decides
''' what is accepted, rejected, reworked or scrapped.  The screen never computes any of it.
''' REQ: PO-009, PO-010, PO-011, PO-014, PO-025
''' </summary>
Public Class frmGoodsReceipt
    Inherits BaseForm

    Private Shared ReadOnly _head As String()() = New String()() {
        New String() {"ORDER", "&Order:", "150"}, New String() {"DELNOTE", "&Delivery note:", "150"},
        New String() {"PHYSDATE", "&Arrived:", "90"}, New String() {"WHS", "&Warehouse:", "90"},
        New String() {"EVIDENCE", "Shipping &evidence:", "200"}, New String() {"IMPORTENTRY", "&Import entry:", "150"}}

    Private Shared ReadOnly _count As String()() = New String()() {
        New String() {"COUNTED", "Co&unted:", "90"}, New String() {"LOC", "&Location:", "90"},
        New String() {"LOT", "Lo&t:", "110"}, New String() {"SERIAL", "&Serial:", "110"}}

    Private Shared ReadOnly _insp As String()() = New String()() {
        New String() {"INSPECTED", "Ins&pected:", "80"}, New String() {"ACCEPTED", "A&ccepted:", "80"},
        New String() {"REJECTED", "&Rejected:", "80"}, New String() {"REWORK", "Re&work:", "80"},
        New String() {"REASONCODE", "Reason cod&e:", "90"}, New String() {"CONCESSION", "Concession re&f:", "140"},
        New String() {"GOODLOC", "Good locatio&n:", "90"}}

    Private ReadOnly _svc As New GoodsReceiptService()
    Private ReadOnly _edit As New Dictionary(Of String, TextBox)
    Private _cboOwner As ComboBox
    Private _cboDisp As ComboBox
    Private _grid As DataGridView
    Private _loadedHeader As Dictionary(Of String, String)

    Public Sub New()
        Me.Text = "Goods Receipt and Inspection"
        Me.ClientSize = New Size(920, 580)
        Dim y As Integer = 8
        For Each f As String() In _head
            UiStyle.Caption(Me, f(1), 8, y, 110)
            _edit(f(0)) = UiStyle.Edit(Me, 120, y, CInt(f(2)))
            y += 24
        Next
        UiStyle.Caption(Me, "Ownershi&p passes:", 330, 8, 110)
        _cboOwner = UiStyle.Picker(Me, 444, 6, 150)
        _cboOwner.Items.AddRange(New String() {"RECEIPT", "INCOTERMS", "DROPSHIP"})
        _cboOwner.SelectedIndex = 0
        Dim b1 As Button = UiStyle.Push(Me, "&Validate", 330, 36, 90)
        AddHandler b1.Click, AddressOf ValidateClicked
        Dim b2 As Button = UiStyle.Push(Me, "Co&unt line", 426, 36, 90)
        AddHandler b2.Click, AddressOf CountClicked
        Dim b3 As Button = UiStyle.Push(Me, "Re&verse", 522, 36, 90)
        AddHandler b3.Click, AddressOf ReverseClicked
        Dim countBox As GroupBox = UiStyle.Box(Me, "Counted quantity for the selected line", 620, 2, 292, 136)
        Dim cy As Integer = 22
        For Each f As String() In _count
            UiStyle.Caption(Me, f(1), 630, cy, 80)
            _edit(f(0)) = UiStyle.Edit(Me, 714, cy, CInt(f(2)))
            cy += 24
        Next
        Dim inspectionBox As GroupBox = UiStyle.Box(Me, "Inspection of a quarantined line", 8, 150, 904, 136)
        Dim ix As Integer = 18, iy As Integer = 172
        For Each f As String() In _insp
            UiStyle.Caption(Me, f(1), ix, iy, 92)
            _edit(f(0)) = UiStyle.Edit(Me, ix + 94, iy, CInt(f(2)))
            iy += 24
            ' two columns only - a third would be drawn over the disposition block
            If iy > 244 Then
                ix += 300
                iy = 172
            End If
        Next
        UiStyle.Caption(Me, "Dispositio&n:", 618, 172, 86)
        _cboDisp = UiStyle.Picker(Me, 706, 170, 140)
        _cboDisp.Items.AddRange(New String() {"ACCEPT", "REJECT", "REWORK", "CONCESSION", "SCRAP", "RETURN"})
        _cboDisp.SelectedIndex = 0
        UiStyle.Caption(Me, "Re&ason:", 618, 196, 86)
        _edit("REASON") = UiStyle.Edit(Me, 706, 194, 190)
        Dim b4 As Button = UiStyle.Push(Me, "Record &inspection", 706, 220, 140)
        AddHandler b4.Click, AddressOf InspectClicked
        _grid = New DataGridView()
        _grid.SetBounds(8, 294, 904, 242)
        _grid.Anchor = AnchorStyles.Top Or AnchorStyles.Left Or AnchorStyles.Right Or AnchorStyles.Bottom
        GridHelper.Style(_grid)
        Me.Controls.Add(_grid)
        countBox.SendToBack()
        inspectionBox.SendToBack()
    End Sub

    Protected Overrides Sub OnLoad(ByVal e As EventArgs)
        MyBase.OnLoad(e)
        _edit("PHYSDATE").Text = Fmt.Date(SterpSession.Current.BusinessDate)
        _edit("WHS").Text = "MAIN"
        If FormArgument <> "" Then _edit("ORDER").Text = FormArgument
        SetStatus("Validate the delivery note against the order, count each line, then F10 to confirm it.")
    End Sub

    Protected Overrides Sub OnLookup()
        If Me.ActiveControl Is _edit("ORDER") Then
            Dim c As String = frmLookup.Pick(Me, "po.usp_OpenOrders", SterpSession.Current.CompanyCode, "Open order")
            If c <> "" Then _edit("ORDER").Text = c
        Else
            MyBase.OnLookup()
        End If
    End Sub

    Protected Overrides Sub OnRefreshKey()
        ValidateClicked(Nothing, EventArgs.Empty)
    End Sub

    ''' <summary>Reads the outstanding order lines into the COMMAREA (PO-009, PO-014, PO-025).</summary>
    Private Sub ValidateClicked(ByVal sender As Object, ByVal e As EventArgs)
        _loadedHeader = Nothing
        GridHelper.Bind(_grid, Nothing)
        Guard(Sub()
                  Dim v As Dictionary(Of String, String) = Values()
                  v("OWNERSHIP") = Convert.ToString(_cboOwner.SelectedItem)
                  Dim r As CobolResult = _svc.Validate(v)
                  GridHelper.Bind(_grid, _svc.Lines())
                  If _grid.Rows.Count > 0 Then _loadedHeader = Header()
                  If Not ShowResult(r) Then Return
                  Dim txt As String = _grid.Rows.Count & " outstanding line(s) on " & v("ORDER")
                  If _svc.WasDuplicate Then txt &= "   *** this delivery note has been received before"
                  SetStatus(txt, If(_svc.WasDuplicate, "!  WARNING", "OK"))
              End Sub)
    End Sub

    ''' <summary>Writes one counted quantity back into the validated COMMAREA (PO-009).</summary>
    Private Sub CountClicked(ByVal sender As Object, ByVal e As EventArgs)
        If Not HeaderReady() Then Return
        If _grid.CurrentRow Is Nothing Then
            SetStatus("Select the line you have counted first.", "!  WARNING")
            Return
        End If
        Guard(Sub()
                  Dim i As Integer = _grid.CurrentRow.Index + 1
                  Dim v As Dictionary(Of String, String) = Values()
                  _svc.Count(i, PoMap.D(v, "COUNTED"), PoMap.S(v, "LOC"), PoMap.S(v, "LOT"), PoMap.S(v, "SERIAL"))
                  GridHelper.Bind(_grid, _svc.Lines())
                  If _grid.Rows.Count >= i Then _grid.CurrentCell = _grid.Rows(i - 1).Cells(0)
                  SetStatus("Line " & i & " counted as " & PoMap.D(v, "COUNTED") &
                            ".  Count the rest, then F10 to confirm the delivery.", "OK")
              End Sub)
    End Sub

    ''' <summary>F10 confirms the delivery; RECEIPT applies the tolerances and posts (PO-009, PO-011).</summary>
    Protected Overrides Sub OnConfirm()
        If Not HeaderReady() Then Return
        If Not Confirm("Confirm this delivery against " & _edit("ORDER").Text.Trim() & "?") Then Return
        Guard(Sub()
                  Dim r As CobolResult = _svc.Confirm("CONFIRM")
                  GridHelper.Bind(_grid, _svc.Lines())
                  If Not ShowResult(r) Then Return
                  _loadedHeader = Nothing
                  SetStatus("Receipt " & _svc.ReceiptRef & " " & _svc.Status & ": " & _svc.LinesAccepted &
                            " accepted, " & _svc.LinesQuarantined & " quarantined, " & _svc.LinesHeld &
                            " held.  GRNI " & Fmt.Money(_svc.GrniGBP) & ", price variance " &
                            Fmt.Money(_svc.PpvGBP) & ", journal " & _svc.JournalNumber & ".",
                            If(_svc.LinesHeld > 0, "!  WARNING", "OK"))
              End Sub)
    End Sub

    ''' <summary>Inspection disposition of a quarantined line (PO-010).</summary>
    Private Sub InspectClicked(ByVal sender As Object, ByVal e As EventArgs)
        Guard(Sub()
                  Dim v As Dictionary(Of String, String) = Values()
                  v("RECEIPT") = _svc.ReceiptRef
                  v("DISPOSITION") = Convert.ToString(_cboDisp.SelectedItem)
                  v("LINE") = If(_grid.CurrentRow Is Nothing, "1", Convert.ToString(_grid.CurrentRow.Index + 1))
                  Dim r As CobolResult = _svc.Inspect(v)
                  If Not ShowResult(r) Then Return
                  Dim ia As CommArea = _svc.Inspection
                  SetStatus("Inspection recorded: " & Fmt.Str(ia.GetDecimal("IN-TO-GOOD-QTY")) & " to good stock, " &
                            Fmt.Str(ia.GetDecimal("IN-TO-SCRAP-QTY")) & " scrapped (" &
                            Fmt.Money(ia.GetDecimal("IN-SCRAP-VALUE-GBP")) & "), " &
                            Fmt.Str(ia.GetDecimal("IN-HELD-FOR-RET-QTY")) & " held for return.  Quality status " &
                            ia.GetString("IN-QUALITY-STATUS") & ".", "OK")
              End Sub)
    End Sub

    ''' <summary>Reversal of a confirmed receipt with its reason (PO-009).</summary>
    Private Sub ReverseClicked(ByVal sender As Object, ByVal e As EventArgs)
        If _svc.ReceiptRef = "" Then
            SetStatus("There is no receipt on this screen to reverse.", "!  WARNING")
            Return
        End If
        If Not Confirm("Reverse receipt " & _svc.ReceiptRef & "?") Then Return
        Guard(Sub()
                  Dim r As CobolResult = _svc.Reverse(_svc.ReceiptRef, _edit("REASON").Text.Trim())
                  If Not ShowResult(r) Then Return
                  SetStatus("Receipt " & _svc.ReceiptRef & " reversed; journal " & _svc.JournalNumber & ".", "OK")
              End Sub)
    End Sub

    Private Function Values() As Dictionary(Of String, String)
        Dim v As New Dictionary(Of String, String)
        For Each k As String In _edit.Keys
            v(k) = _edit(k).Text
        Next
        Return v
    End Function

    Private Function Header() As Dictionary(Of String, String)
        Dim values As New Dictionary(Of String, String)
        For Each field As String() In _head
            values(field(0)) = _edit(field(0)).Text.Trim()
        Next
        values("OWNERSHIP") = Convert.ToString(_cboOwner.SelectedItem)
        Return values
    End Function

    Private Function HeaderReady() As Boolean
        If _loadedHeader Is Nothing Then
            SetStatus("Validate the delivery against an order before counting or confirming.", "!  WARNING")
            Return False
        End If
        For Each field As KeyValuePair(Of String, String) In Header()
            If Not String.Equals(field.Value, _loadedHeader(field.Key), StringComparison.OrdinalIgnoreCase) Then
                SetStatus("The receipt header has changed. Validate it again before counting or confirming.", "!  WARNING")
                Return False
            End If
        Next
        Return True
    End Function
End Class
