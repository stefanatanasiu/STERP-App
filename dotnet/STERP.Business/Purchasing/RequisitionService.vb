Imports System.Collections.Generic
Imports STERP.CobolInterop
Imports STERP.Common

''' <summary>
''' Requisition facade - capture, submission, approval and conversion to an order, plus the
''' approved-source check a buyer makes before choosing a supplier.  Not one rule lives here:
''' POREQ owns the requisition life cycle, BUDGCHK the budget answer and SOURCSEL the source.
''' REQ: PO-001, PO-002, PO-005
''' </summary>
Public Class RequisitionService
    Inherits ServiceBase

    Public Property ReqNumber As String = ""
    Public Property Status As String = ""
    Public Property Outcome As String = ""
    Public Property TotalGBP As Decimal
    Public Property LineValueGBP As Decimal
    Public Property LineNum As Integer
    Public Property BudgetOutcome As String = ""
    Public Property BudgetUsedPct As Decimal
    Public Property BudgetAvailGBP As Decimal
    Public Property OrderNumber As String = ""
    Public Property LastSource As CobolResult
    ''' <summary>Lines captured in this sitting - the COMMAREA carries one line at a time.</summary>
    Public ReadOnly Property Lines As New DataTable("ReqLines")

    Public Sub New()
        PoMap.Columns(Lines, "LineNum:INT", "ItemCode", "Description", "Qty:DEC", "UomCode",
                      "EstPrice:DEC", "LineValueGBP:DEC", "SuggestedSupplier", "BudgetOutcome")
    End Sub

    ''' <summary>Header capture; purpose and demand reference stay with the requisition (PO-001).</summary>
    Public Function Create(ByVal v As Dictionary(Of String, String)) As CobolResult
        Dim ca As CommArea = Area(v)
        ca.SetString("RQ-REQUESTER", If(PoMap.S(v, "REQUESTER") = "", User, PoMap.S(v, "REQUESTER")))
        ca.SetString("RQ-PURPOSE", PoMap.T(v, "PURPOSE"))
        ca.SetDate("RQ-REQ-DATE", Session.BusinessDate)
        ca.SetDate("RQ-NEEDED-BY", PoMap.Dt(v, "NEEDEDBY", Session.BusinessDate))
        ca.SetString("RQ-DEMAND-TYPE", If(PoMap.S(v, "DEMANDTYPE") = "", "MANUAL", PoMap.S(v, "DEMANDTYPE")))
        ca.SetString("RQ-DEMAND-REF", PoMap.S(v, "DEMANDREF"))
        Dim r As CobolResult = Link("POREQ", ca, "CREATE", Session.NewCorrelationId())
        Read(ca)
        Lines.Rows.Clear()
        Return r
    End Function

    ''' <summary>One requisition line; POREQ answers with its budget verdict (PO-001, PO-005).</summary>
    Public Function AddLine(ByVal v As Dictionary(Of String, String)) As CobolResult
        Dim ca As CommArea = Area(v)
        ca.SetString("RQ-REQ-NUMBER", ReqNumber)
        ca.SetString("RQ-ITEM-CODE", PoMap.S(v, "ITEM"))
        ca.SetString("RQ-DESCRIPTION", PoMap.T(v, "DESC"))
        ca.SetDecimal("RQ-QUANTITY", PoMap.D(v, "QTY"))
        ca.SetString("RQ-UOM-CODE", PoMap.S(v, "UOM"))
        ca.SetDecimal("RQ-EST-UNIT-PRICE", PoMap.D(v, "PRICE"))
        ca.SetString("RQ-SUGGEST-SUPP", PoMap.S(v, "SUPPLIER"))
        ca.SetString("RQ-ACCOUNT-CODE", PoMap.S(v, "ACCOUNT"))
        ca.SetDate("RQ-LINE-NEEDED-BY", PoMap.Dt(v, "NEEDEDBY", Session.BusinessDate))
        Dim r As CobolResult = Link("POREQ", ca, "ADDLINE")
        Read(ca)
        If r.Succeeded Then
            Lines.Rows.Add(LineNum, PoMap.S(v, "ITEM"), PoMap.T(v, "DESC"), PoMap.D(v, "QTY"),
                           PoMap.S(v, "UOM"), PoMap.D(v, "PRICE"), LineValueGBP,
                           PoMap.S(v, "SUPPLIER"), BudgetOutcome)
        End If
        Return r
    End Function

    ''' <summary>SUBMIT / CANCEL / ENQUIRE - the function name is the verb POREQ understands.</summary>
    Public Function Act(ByVal func As String, ByVal reqNo As String) As CobolResult
        Dim ca As CommArea = Area(Nothing)
        ca.SetString("RQ-REQ-NUMBER", If(reqNo = "", ReqNumber, reqNo))
        Dim r As CobolResult = Link("POREQ", ca, func)
        Read(ca)
        Return r
    End Function

    ''' <summary>Approval decision; a requester may not approve their own demand (PO-005).</summary>
    Public Function Decide(ByVal reqNo As String, ByVal decision As String, ByVal reason As String) As CobolResult
        Dim ca As CommArea = Area(Nothing)
        ca.SetString("RQ-REQ-NUMBER", reqNo)
        ca.SetString("RQ-DECISION", decision)
        ca.SetString("RQ-APPROVER", User)
        ca.SetString("RQ-REASON", reason)
        Dim r As CobolResult = Link("POREQ", ca, "APPROVE")
        Read(ca)
        Return r
    End Function

    ''' <summary>Turns an approved line into order demand, carrying the requisition reference (PO-001).</summary>
    Public Function ConvertLine(ByVal reqNo As String, ByVal line As Integer, ByVal qty As Decimal,
                                ByVal orderNo As String) As CobolResult
        Dim ca As CommArea = Area(Nothing)
        ca.SetString("RQ-REQ-NUMBER", reqNo)
        ca.SetInt("RQ-LINE-NUM", line)
        ca.SetDecimal("RQ-CONVERT-QTY", qty)
        ca.SetString("RQ-ORDER-NUMBER", orderNo)
        Dim r As CobolResult = Link("POREQ", ca, "CONVERT", Session.NewCorrelationId())
        Read(ca)
        Return r
    End Function

    ''' <summary>Approved source, lead time and the premium over the cheapest quote (PO-002).</summary>
    Public Function Source(ByVal func As String, ByVal v As Dictionary(Of String, String)) As CommArea
        Dim ca As CommArea = NewArea("SOURCSELCA")
        ca.SetString("SS-REQ-NUMBER", PoMap.S(v, "REQ"))
        ca.SetString("SS-ITEM-CODE", PoMap.S(v, "ITEM"))
        ca.SetString("SS-SUPPLIER", PoMap.S(v, "SUPPLIER"))
        ca.SetDecimal("SS-QUANTITY", PoMap.D(v, "QTY"))
        ca.SetString("SS-UOM-CODE", PoMap.S(v, "UOM"))
        ca.SetString("SS-BUYER", User)
        ca.SetDate("SS-NEEDED-BY", PoMap.Dt(v, "NEEDEDBY", Session.BusinessDate))
        ca.SetDate("SS-EVENT-DATE", Session.BusinessDate)
        LastSource = Link("SOURCSEL", ca, func)
        Return ca
    End Function

    Private Function Area(ByVal v As Dictionary(Of String, String)) As CommArea
        Dim ca As CommArea = NewArea("POREQCA")
        If v IsNot Nothing Then
            ca.SetString("RQ-COST-CENTRE", PoMap.S(v, "CC"))
            ca.SetString("RQ-PROJECT", PoMap.S(v, "PROJECT"))
            ca.SetString("RQ-JOB-REF", PoMap.S(v, "JOB"))
            ca.SetString("RQ-BUDGET-CODE", PoMap.S(v, "BUDGET"))
        End If
        Return ca
    End Function

    Private Sub Read(ByVal ca As CommArea)
        If ca.GetString("RQ-REQ-NUMBER") <> "" Then ReqNumber = ca.GetString("RQ-REQ-NUMBER")
        Status = ca.GetString("RQ-STATUS")
        Outcome = ca.GetString("RQ-OUTCOME")
        TotalGBP = ca.GetDecimal("RQ-TOTAL-GBP")
        LineValueGBP = ca.GetDecimal("RQ-LINE-VALUE-GBP")
        LineNum = ca.GetInt("RQ-LINE-NUM")
        BudgetOutcome = ca.GetString("RQ-BUDGET-OUTCOME")
        BudgetUsedPct = ca.GetDecimal("RQ-BUDGET-USED-PCT")
        BudgetAvailGBP = ca.GetDecimal("RQ-BUDGET-AVAIL-GBP")
        OrderNumber = ca.GetString("RQ-ORDER-NUMBER")
    End Sub
End Class
