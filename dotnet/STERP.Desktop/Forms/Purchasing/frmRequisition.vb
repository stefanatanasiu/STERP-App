Imports System.Collections.Generic
Imports STERP.Business
Imports STERP.CobolInterop
Imports STERP.Common

''' <summary>
''' Purchase requisition capture.  The screen collects the demand, POREQ decides whether it may
''' be raised, BUDGCHK answers the budget question and SOURCSEL shows which supplier the buyer
''' is expected to use and what the premium is if another one is chosen.
''' REQ: PO-001, PO-002, PO-005
''' </summary>
Public Class frmRequisition
    Inherits BaseForm

    Private Shared ReadOnly _head As String()() = New String()() {
        New String() {"REQ", "&Requisition:", "140"}, New String() {"PURPOSE", "&Purpose:", "300"},
        New String() {"NEEDEDBY", "Needed &by:", "90"},
        New String() {"CC", "&Cost centre:", "90"}, New String() {"PROJECT", "Pro&ject:", "90"},
        New String() {"BUDGET", "B&udget code:", "110"}, New String() {"DEMANDREF", "Demand re&f:", "140"}}

    Private Shared ReadOnly _line As String()() = New String()() {
        New String() {"ITEM", "&Item:", "180"}, New String() {"DESC", "Descrip&tion:", "260"},
        New String() {"QTY", "&Quantity:", "90"}, New String() {"UOM", "U&nit:", "60"},
        New String() {"PRICE", "Est. &price:", "90"}, New String() {"SUPPLIER", "Suggested &supplier:", "100"},
        New String() {"ACCOUNT", "&Account:", "90"}}

    Private ReadOnly _svc As New RequisitionService()
    Private ReadOnly _edit As New Dictionary(Of String, TextBox)
    Private _cboDemand As ComboBox
    Private _grid As DataGridView
    Private _btnSource As Button

    Public Sub New()
        Me.Text = "Purchase Requisition"
        Me.ClientSize = New Size(840, 520)
        UiStyle.Caption(Me, "&Demand:", 8, 8, 70)
        _cboDemand = UiStyle.Picker(Me, 100, 6, 130)
        _cboDemand.Items.AddRange(New String() {"MANUAL", "SALES", "KIT", "REPLENISH", "PROJECT"})
        _cboDemand.SelectedIndex = 0
        Dim y As Integer = 34
        For Each f As String() In _head
            UiStyle.Caption(Me, f(1), 8, y, 90)
            _edit(f(0)) = UiStyle.Edit(Me, 100, y, CInt(f(2)))
            y += 25
        Next
        UiStyle.Box(Me, "Line", 420, 30, 406, 190)
        Dim ly As Integer = 50
        For Each f As String() In _line
            UiStyle.Caption(Me, f(1), 430, ly, 110)
            _edit(f(0)) = UiStyle.Edit(Me, 544, ly, CInt(f(2)))
            ly += 24
        Next
        _btnSource = UiStyle.Push(Me, "Chec&k this source", 430, ly + 2, 150)
        AddHandler _btnSource.Click, AddressOf SourceClicked
        Dim add As Button = UiStyle.Push(Me, "&Add line", 590, ly + 2, 90)
        AddHandler add.Click, AddressOf AddClicked
        _grid = New DataGridView()
        _grid.SetBounds(8, 248, 818, 228)
        _grid.Anchor = AnchorStyles.Top Or AnchorStyles.Left Or AnchorStyles.Right Or AnchorStyles.Bottom
        GridHelper.Style(_grid)
        Me.Controls.Add(_grid)
    End Sub

    Protected Overrides Sub OnLoad(ByVal e As EventArgs)
        MyBase.OnLoad(e)
        _edit("NEEDEDBY").Text = Fmt.Date(SterpSession.Current.BusinessDate.AddDays(14))
        SetStatus("Enter the demand, add the lines, then F10 to submit it for approval.")
        If FormArgument <> "" Then
            _edit("REQ").Text = FormArgument
            OnRefreshKey()
        End If
    End Sub

    Protected Overrides Sub OnLookup()
        If Me.ActiveControl Is _edit("ITEM") Then
            Dim c As String = frmLookup.Pick(Me, "core.usp_UiLookup", "ITM.ITEM", "Item")
            If c <> "" Then _edit("ITEM").Text = c
        Else
            MyBase.OnLookup()
        End If
    End Sub

    ''' <summary>Adds a line, creating the requisition header on the first one (PO-001).</summary>
    Private Sub AddClicked(ByVal sender As Object, ByVal e As EventArgs)
        Guard(Sub()
                  Dim v As Dictionary(Of String, String) = Values()
                  If _svc.ReqNumber = "" AndAlso PoMap.S(v, "REQ") <> "" Then _svc.Act("ENQUIRE", PoMap.S(v, "REQ"))
                  If _svc.ReqNumber = "" Then
                      v("DEMANDTYPE") = Convert.ToString(_cboDemand.SelectedItem)
                      If Not ShowResult(_svc.Create(v)) Then Return
                  End If
                  Dim r As CobolResult = _svc.AddLine(v)
                  If Not ShowResult(r) Then Return
                  _edit("REQ").Text = _svc.ReqNumber
                  GridHelper.Bind(_grid, _svc.Lines)
                  _edit("ITEM").Text = "" : _edit("DESC").Text = "" : _edit("QTY").Text = ""
                  SetStatus(_svc.ReqNumber & "  " & _svc.Status & "   total " & Fmt.Money(_svc.TotalGBP) &
                            "   budget " & BudgetText(), BudgetCue())
              End Sub)
    End Sub

    ''' <summary>
    ''' Checks the suggested supplier against the approved source register and reports the lead
    ''' time, the minimum order and what was last paid.  SOURCSEL answers for a named supplier,
    ''' so an unapproved or suspended one comes back as its own refusal, never as a blank
    ''' line (PO-002).
    ''' </summary>
    Private Sub SourceClicked(ByVal sender As Object, ByVal e As EventArgs)
        Guard(Sub()
                  Dim v As Dictionary(Of String, String) = Values()
                  v("REQ") = _svc.ReqNumber
                  Dim ca As CommArea = _svc.Source("CHECK", v)
                  If _svc.LastSource.IsNotFound OrElse _svc.LastSource.IsError OrElse _svc.LastSource.IsSevere Then
                      SetStatus(_svc.LastSource.Display, "!  WARNING")
                      Return
                  End If
                  Dim t As String = ca.GetString("SS-SUPPLIER") & "  lead time " & ca.GetInt("SS-LEAD-TIME-DAYS") &
                      " day(s), min order " & Fmt.Qty(ca.GetDecimal("SS-MIN-ORDER-QTY"), ca.GetString("SS-UOM-CODE")) &
                      ", last paid " & Fmt.Money(ca.GetDecimal("SS-LAST-PAID-PRICE"), ca.GetString("SS-SRC-CURRENCY"))
                  If ca.GetString("SS-SRC-RESTRICTION") <> "" Then t &= "   RESTRICTION: " & ca.GetString("SS-SRC-RESTRICTION")
                  SetStatus(t, If(_svc.LastSource.IsWarning OrElse _svc.LastSource.IsHold, "!  WARNING", "OK"))
              End Sub)
    End Sub

    ''' <summary>F10 submits the requisition for approval.</summary>
    Protected Overrides Sub OnConfirm()
        If _svc.ReqNumber = "" Then
            SetStatus("Add at least one line first.", "!  WARNING")
            Return
        End If
        If Not Confirm("Submit requisition " & _svc.ReqNumber & " for approval?") Then Return
        Guard(Sub()
                  Dim r As CobolResult = _svc.Act("SUBMIT", _svc.ReqNumber)
                  If Not ShowResult(r) Then Return
                  SetStatus("Requisition " & _svc.ReqNumber & " is " & _svc.Status & ".  Budget " & BudgetText(), BudgetCue())
              End Sub)
    End Sub

    ''' <summary>F5 opens the requisition in the Requisition field, or re-reads this one (PO-001).</summary>
    Protected Overrides Sub OnRefreshKey()
        Dim no As String = PoMap.S(Values(), "REQ")
        If no = "" Then no = _svc.ReqNumber
        If no = "" Then
            SetStatus("Type a requisition number and press F5, or add a line to raise a new one.", "!  WARNING")
            Return
        End If
        Guard(Sub()
                  Dim r As CobolResult = _svc.Act("ENQUIRE", no)
                  If Not ShowResult(r) Then Return
                  _edit("REQ").Text = _svc.ReqNumber
                  GridHelper.Bind(_grid, _svc.Lines)
                  SetStatus(_svc.ReqNumber & "  " & _svc.Status & "   " & Fmt.Money(_svc.TotalGBP), "")
              End Sub)
    End Sub

    Private Function Values() As Dictionary(Of String, String)
        Dim v As New Dictionary(Of String, String)
        For Each k As String In _edit.Keys
            v(k) = _edit(k).Text
        Next
        Return v
    End Function

    Private Function BudgetText() As String
        If _svc.BudgetOutcome = "" Then Return "not checked"
        Return _svc.BudgetOutcome & " at " & Fmt.Pct(_svc.BudgetUsedPct) & " used, " &
               Fmt.Money(_svc.BudgetAvailGBP) & " left"
    End Function

    Private Function BudgetCue() As String
        If _svc.BudgetOutcome = "BLOCK" Then Return "## HOLD"
        If _svc.BudgetOutcome = "WARN" Then Return "!  WARNING"
        Return "OK"
    End Function
End Class
