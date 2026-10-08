Imports System.Collections.Generic
Imports STERP.Business
Imports STERP.CobolInterop
Imports STERP.Common

''' <summary>
''' Purchase order entry, approval and amendment.  Everything that decides is COBOL: POPRICE
''' explains where the price came from, BUDGCHK gives the warn/block verdict measured after the
''' purchase, POSUPCHK and PAYTERMC gate the supplier and the payment terms, POAPPRV approves
''' the exact version of the order and POORDER refuses to issue an order that is not approved.
''' An amendment that changes value, supplier or terms invalidates the approval - the screen
''' only reports it.
''' REQ: PO-003, PO-004, PO-005, PO-006, PO-007, PO-008, PO-021, PO-024, PO-026, PO-027
''' </summary>
Public Class frmPurchaseOrder
    Inherits BaseForm

    Private Shared ReadOnly _head As String()() = New String()() {
        New String() {"ORDER", "&Order:", "150"}, New String() {"SUPPLIER", "&Supplier:", "120"},
        New String() {"CCY", "C&urrency:", "50"}, New String() {"TERMS", "Pa&yment terms:", "90"},
        New String() {"INCO", "&Incoterms:", "50"}, New String() {"PLACE", "Named p&lace:", "200"},
        New String() {"BUDGET", "&Budget code:", "110"}, New String() {"SUPPREF", "Supplier re&f:", "150"}}

    Private Shared ReadOnly _line As String()() = New String()() {
        New String() {"ITEM", "I&tem:", "170"}, New String() {"DESC", "Descri&ption:", "250"},
        New String() {"QTY", "&Quantity:", "90"}, New String() {"UOM", "U&nit:", "60"},
        New String() {"PRICE", "Unit pri&ce:", "90"}, New String() {"DISCPCT", "Disc &%:", "60"},
        New String() {"DUE", "Due &date:", "90"}, New String() {"ACCOUNT", "&Account:", "90"},
        New String() {"CC", "Cost c&entre:", "90"}, New String() {"DESTSITE", "To si&te:", "60"}}

    Private ReadOnly _svc As New PurchaseOrderService()
    Private ReadOnly _edit As New Dictionary(Of String, TextBox)
    Private _cboType As ComboBox
    Private _cboAmend As ComboBox
    Private _txtAmendValue As TextBox
    Private _txtReason As TextBox
    Private _grid As DataGridView

    Public Sub New()
        Me.Text = "Purchase Order Entry and Approval"
        Me.ClientSize = New Size(900, 600)
        UiStyle.Caption(Me, "T&ype:", 8, 8, 60)
        _cboType = UiStyle.Picker(Me, 100, 6, 120)
        _cboType.Items.AddRange(New String() {"STANDARD", "BLANKET", "CALLOFF", "SERVICE", "SUBCONTRACT", "DROPSHIP"})
        _cboType.SelectedIndex = 0
        Dim y As Integer = 34
        For Each f As String() In _head
            UiStyle.Caption(Me, f(1), 8, y, 90)
            _edit(f(0)) = UiStyle.Edit(Me, 100, y, CInt(f(2)))
            y += 24
        Next
        UiStyle.Box(Me, "Line", 470, 2, 420, 262)
        Dim ly As Integer = 22
        For Each f As String() In _line
            UiStyle.Caption(Me, f(1), 480, ly, 100)
            _edit(f(0)) = UiStyle.Edit(Me, 584, ly, CInt(f(2)))
            ly += 24
        Next
        Buttons(y + 4, ly + 4)
        UiStyle.Box(Me, "Amendment", 8, 272, 882, 58)
        UiStyle.Caption(Me, "&Field:", 18, 292, 46)
        _cboAmend = UiStyle.Picker(Me, 66, 290, 120)
        _cboAmend.Items.AddRange(New String() {"QUANTITY", "PRICE", "DATE", "SUPPLIER", "TERMS", "DESCRIPTION"})
        _cboAmend.SelectedIndex = 0
        UiStyle.Caption(Me, "Ne&w value:", 196, 292, 70)
        _txtAmendValue = UiStyle.Edit(Me, 270, 290, 110)
        UiStyle.Caption(Me, "Rea&son:", 390, 292, 50)
        _txtReason = UiStyle.Edit(Me, 442, 290, 300)
        Dim amend As Button = UiStyle.Push(Me, "A&mend", 752, 289, 90)
        AddHandler amend.Click, AddressOf AmendClicked
        _grid = New DataGridView()
        _grid.SetBounds(8, 336, 882, 220)
        _grid.Anchor = AnchorStyles.Top Or AnchorStyles.Left Or AnchorStyles.Right Or AnchorStyles.Bottom
        GridHelper.Style(_grid)
        Me.Controls.Add(_grid)
    End Sub

    Private Sub Buttons(ByVal hy As Integer, ByVal ly As Integer)
        Dim b1 As Button = UiStyle.Push(Me, "Ne&w order", 100, hy, 96)
        AddHandler b1.Click, AddressOf NewClicked
        Dim b2 As Button = UiStyle.Push(Me, "Chec&k budget", 202, hy, 100)
        AddHandler b2.Click, AddressOf BudgetClicked
        Dim b3 As Button = UiStyle.Push(Me, "Supplier c&hecks", 308, hy, 110)
        AddHandler b3.Click, AddressOf ChecksClicked
        Dim b4 As Button = UiStyle.Push(Me, "Explain &price", 480, ly, 110)
        AddHandler b4.Click, AddressOf PriceClicked
        Dim b5 As Button = UiStyle.Push(Me, "&Add line", 596, ly, 90)
        AddHandler b5.Click, AddressOf AddClicked
        Dim b6 As Button = UiStyle.Push(Me, "Appro&val", 692, ly, 90)
        AddHandler b6.Click, AddressOf ApprovalClicked
    End Sub

    Protected Overrides Sub OnLoad(ByVal e As EventArgs)
        MyBase.OnLoad(e)
        _edit("CCY").Text = SterpSession.Current.BaseCurrency
        _edit("DESTSITE").Text = SterpSession.Current.SiteCode
        _edit("DUE").Text = Fmt.Date(SterpSession.Current.BusinessDate.AddDays(14))
        If FormArgument <> "" Then _edit("ORDER").Text = FormArgument
        SetStatus("F5 lists the open orders, F10 issues the order once it has been approved.")
        OnRefreshKey()
    End Sub

    Protected Overrides Sub OnLookup()
        If Me.ActiveControl Is _edit("ITEM") Then
            Dim c As String = frmLookup.Pick(Me, "core.usp_UiLookup", "ITM.ITEM", "Item")
            If c <> "" Then _edit("ITEM").Text = c
        Else
            MyBase.OnLookup()
        End If
    End Sub

    ''' <summary>F5 shows the open orders of the supplier, or this order's own lines (PO-024).</summary>
    Protected Overrides Sub OnRefreshKey()
        Guard(Sub()
                  If _svc.Lines.Rows.Count > 0 Then
                      GridHelper.Bind(_grid, _svc.Lines)
                  Else
                      GridHelper.Bind(_grid, _svc.OpenOrders(V("SUPPLIER"), V("ORDER"), False))
                  End If
              End Sub)
    End Sub

    Private Sub NewClicked(ByVal sender As Object, ByVal e As EventArgs)
        Guard(Sub()
                  Dim v As Dictionary(Of String, String) = Values()
                  v("TYPE") = Convert.ToString(_cboType.SelectedItem)
                  If Not ShowResult(_svc.Create(v)) Then Return
                  _edit("ORDER").Text = _svc.OrderNumber
                  GridHelper.Bind(_grid, _svc.Lines)
                  SetStatus("Order " & _svc.OrderNumber & " created (" & _svc.Status & ").", "OK")
              End Sub)
    End Sub

    ''' <summary>Adds a line; an empty price lets POPRICE fill it in (PO-003, PO-004, PO-008).</summary>
    Private Sub AddClicked(ByVal sender As Object, ByVal e As EventArgs)
        Guard(Sub()
                  If _svc.OrderNumber = "" Then NewClicked(sender, e)
                  If _svc.OrderNumber = "" Then Return
                  Dim r As CobolResult = _svc.AddLine(Values())
                  If Not ShowResult(r) Then Return
                  GridHelper.Bind(_grid, _svc.Lines)
                  _edit("ITEM").Text = "" : _edit("DESC").Text = "" : _edit("QTY").Text = "" : _edit("PRICE").Text = ""
                  SetStatus("Line " & _svc.LineNum & " added, priced from " & _svc.PriceSource &
                            ".  Order net " & Fmt.Money(_svc.NetAmount) & ", gross " & Fmt.Money(_svc.GrossAmount) &
                            ", commitment " & Fmt.Money(_svc.CommitmentGBP) & ".", "OK")
              End Sub)
    End Sub

    ''' <summary>Where the price comes from and the break ladder behind it (PO-004).</summary>
    Private Sub PriceClicked(ByVal sender As Object, ByVal e As EventArgs)
        Guard(Sub()
                  Dim r As CobolResult = _svc.Explain(V("SUPPLIER"), V("ITEM"), PoMap.D(Values(), "QTY"),
                                                      V("UOM"), V("CCY"))
                  If Not ShowResult(r) Then Return
                  Dim p As CommArea = _svc.Price
                  _edit("PRICE").Text = Fmt.Str(p.GetDecimal("PR-UNIT-PRICE"))
                  GridHelper.Bind(_grid, _svc.Breaks(V("SUPPLIER"), V("ITEM")))
                  SetStatus(p.GetString("PR-PRICE-BASIS") & ": " & Fmt.Money(p.GetDecimal("PR-UNIT-PRICE"),
                            p.GetString("PR-CURRENCY")) & " per " & Fmt.Str(p.GetDecimal("PR-PRICE-PER")) &
                            ", break at " & Fmt.Str(p.GetDecimal("PR-BREAK-QTY-USED")) & ", net " &
                            Fmt.Money(p.GetDecimal("PR-NET-VALUE"), p.GetString("PR-CURRENCY")) & "  [" &
                            p.GetString("PR-SOURCE-TEXT") & "]", "OK")
              End Sub)
    End Sub

    ''' <summary>The budget verdict, measured after this purchase (PO-005).</summary>
    Private Sub BudgetClicked(ByVal sender As Object, ByVal e As EventArgs)
        Guard(Sub()
                  Dim v As Dictionary(Of String, String) = Values()
                  v("NETGBP") = Fmt.Str(If(_svc.NetAmount <> 0D, _svc.NetAmount, PoMap.D(v, "QTY") * PoMap.D(v, "PRICE")))
                  Dim r As CobolResult = _svc.CheckBudget(v)
                  Dim b As CommArea = _svc.Budget
                  Dim txt As String = b.GetString("BC-OUTCOME") & " - " & Fmt.Pct(b.GetDecimal("BC-USED-AFTER-PCT")) &
                      " of the budget is used after this order (warn at " & Fmt.Pct(b.GetDecimal("BC-WARN-PCT")) &
                      ", block at " & Fmt.Pct(b.GetDecimal("BC-BLOCK-PCT")) & "), " &
                      Fmt.Money(b.GetDecimal("BC-AVAILABLE-GBP")) & " available"
                  If b.GetInt("BC-RELATED-CNT") > 0 Then
                      txt &= ", including " & b.GetInt("BC-RELATED-CNT") & " related document(s) worth " &
                             Fmt.Money(b.GetDecimal("BC-RELATED-GBP"))
                  End If
                  If b.GetString("BC-OUTCOME") = "BLOCK" Then
                      ShowHold(txt, r.ReasonCode)
                      SetStatus(txt, "## HOLD")
                  ElseIf b.GetString("BC-OUTCOME") = "WARN" Then
                      SetStatus(txt, "!  WARNING")
                  Else
                      SetStatus(txt, "OK")
                  End If
              End Sub)
    End Sub

    ''' <summary>Due diligence and payment terms policy before issue (PO-026, PO-027).</summary>
    Private Sub ChecksClicked(ByVal sender As Object, ByVal e As EventArgs)
        Guard(Sub()
                  Dim dd As CommArea = _svc.Diligence(V("SUPPLIER"), _svc.NetAmount)
                  Dim pt As CommArea = _svc.Terms(V("SUPPLIER"), V("TERMS"), _svc.NetAmount)
                  Dim txt As String = "Diligence " & dd.GetString("DD-OVERALL") & " (" & dd.GetInt("DD-FAIL-CNT") &
                      " failed, " & dd.GetInt("DD-MISSING-CNT") & " missing, " & dd.GetInt("DD-EXPIRED-CNT") &
                      " expired)   Terms " & pt.GetInt("PT-TERMS-DAYS") & " days against a limit of " &
                      pt.GetInt("PT-EFFECTIVE-LIMIT") & " (" & pt.GetString("PT-LIMIT-SOURCE") & ")"
                  If dd.GetString("DD-MISSING-LIST") <> "" Then txt &= "   missing: " & dd.GetString("DD-MISSING-LIST")
                  Dim cue As String = "OK"
                  If dd.GetString("DD-CAN-ORDER") <> "Y" Then cue = "## HOLD"
                  If pt.GetString("PT-NEEDS-EXCEPTION") = "Y" Then
                      cue = "!  WARNING"
                      txt &= "   terms exceed the policy by " & pt.GetInt("PT-EXCESS-DAYS") & " day(s) - an approved exception is needed"
                  End If
                  SetStatus(txt, cue)
              End Sub)
    End Sub

    ''' <summary>Request or give the approval of this exact order version (PO-006).</summary>
    Private Sub ApprovalClicked(ByVal sender As Object, ByVal e As EventArgs)
        Guard(Sub()
                  Dim order As String = If(_svc.OrderNumber = "", V("ORDER"), _svc.OrderNumber)
                  Dim r As CobolResult = _svc.Approval("CHECK", order, "", "")
                  If _svc.ApprovalStatus <> "PENDING" AndAlso _svc.ApprovalStatus <> "APPROVED" Then
                      r = _svc.Approval("REQUEST", order, "", _txtReason.Text.Trim())
                      If Not ShowResult(r) Then Return
                      SetStatus("Approval requested for " & order & " (" & Fmt.Money(_svc.AmountGBP) &
                                ", permission " & _svc.RequiredPermission & ").", "OK")
                      Return
                  End If
                  If _svc.ApprovalStatus = "PENDING" Then
                      If Not Confirm("Approve order " & order & " for " & Fmt.Money(_svc.AmountGBP) & "?") Then Return
                      r = _svc.Approval("DECIDE", order, "APPROVE", _txtReason.Text.Trim())
                      If Not ShowResult(r) Then Return
                  End If
                  SetStatus("Order " & order & " approval is " & _svc.ApprovalStatus & ".", "OK")
              End Sub)
    End Sub

    ''' <summary>An amendment; POORDER decides whether it is material (PO-007).</summary>
    Private Sub AmendClicked(ByVal sender As Object, ByVal e As EventArgs)
        Guard(Sub()
                  Dim order As String = If(_svc.OrderNumber = "", V("ORDER"), _svc.OrderNumber)
                  Dim line As Integer = PoMap.I(Values(), "LINENUM")
                  If line = 0 Then line = _svc.LineNum
                  Dim r As CobolResult = _svc.Amend(order, line, Convert.ToString(_cboAmend.SelectedItem),
                                                    _txtAmendValue.Text.Trim(), _txtReason.Text.Trim())
                  If Not ShowResult(r) Then Return
                  If _svc.NeedsReapproval Then
                      SetStatus("Amendment " & Convert.ToString(_cboAmend.SelectedItem) & " recorded.  This is a " &
                                "material change: the approval has been invalidated and the order must be approved again.",
                                "!  WARNING")
                  Else
                      SetStatus("Amendment recorded; the approval still stands.  Order status " & _svc.Status & ".", "OK")
                  End If
              End Sub)
    End Sub

    ''' <summary>F10 issues the order to the supplier (PO-006).</summary>
    Protected Overrides Sub OnConfirm()
        Dim order As String = If(_svc.OrderNumber = "", V("ORDER"), _svc.OrderNumber)
        If order = "" Then
            SetStatus("There is no order to issue.", "!  WARNING")
            Return
        End If
        If Not Confirm("Issue order " & order & " to the supplier?") Then Return
        Guard(Sub()
                  Dim r As CobolResult = _svc.Act("ISSUE", order, Values())
                  If Not ShowResult(r) Then Return
                  SetStatus("Order " & order & " is " & _svc.Status & ", committed " &
                            Fmt.Money(_svc.CommitmentGBP) & ".", "OK")
              End Sub)
    End Sub

    Private Function Values() As Dictionary(Of String, String)
        Dim v As New Dictionary(Of String, String)
        For Each k As String In _edit.Keys
            v(k) = _edit(k).Text
        Next
        Return v
    End Function

    Private Function V(ByVal key As String) As String
        Return _edit(key).Text.Trim().ToUpperInvariant()
    End Function
End Class
