Imports System.Data
Imports STERP.Business.Kits
Imports STERP.CobolInterop
Imports STERP.Common

''' <summary>
''' Trial kitting.  KITTRIAL answers feasibility, shortages and the limiting requirement for a
''' stated output quantity and date; nothing is reserved, issued or posted by this screen.
''' REQ: KIT-006, KIT-007, KIT-009, KIT-022
''' </summary>
Public Class frmKitTrial
    Inherits ListForm

    Private ReadOnly _svc As New KitService()
    Private _txtKit As TextBox
    Private _txtItem As TextBox
    Private _txtQty As TextBox
    Private _txtDate As TextBox
    Private _cboSupply As ComboBox

    Public Sub New()
        Me.Text = "Trial Kitting"
    End Sub

    Protected Overrides Sub BuildFilters()
        _txtKit = FilterEdit("&Kit:", 130)
        _txtItem = FilterEdit("&Parent:", 150)
        _txtQty = FilterEdit("&Qty:", 70)
        _txtDate = FilterEdit("&By:", 80)
        _cboSupply = FilterList("&Supply:", 130, "FREE STOCK", "+ INBOUND", "+ SUBASSEMBLIES")
        _txtQty.Text = "1"
        _txtDate.Text = SterpSession.Current.BusinessDate.ToString("dd/MM/yyyy")
        If FormArgument <> "" Then _txtKit.Text = FormArgument
    End Sub

    Protected Overrides Sub BuildTools()
        AddSeparator()
        AddTool("Trial (F10)", "ENGINE", AddressOf TrialClicked)
        AddTool("Max build", "RUN", AddressOf MaxClicked)
        AddSeparator()
    End Sub

    Protected Overrides Sub LoadData()
        If _txtKit Is Nothing Then Return
        SetStatus("Enter a kit or a parent item and press Trial (F10).  Nothing is committed.", "")
    End Sub

    Protected Overrides Sub OnConfirm()
        TrialClicked(Nothing, EventArgs.Empty)
    End Sub

    ''' <summary>Non-committing feasibility for the stated quantity and date (KIT-006).</summary>
    Private Sub TrialClicked(ByVal sender As Object, ByVal e As EventArgs)
        If Not Asked() Then Return
        Guard(Sub()
                  Dim r As CobolResult = _svc.Trial(Code(_txtKit), Code(_txtItem), "",
                                                    SterpSession.Current.SiteCode, Qty(), [When](),
                                                    Inbound(), Subassemblies())
                  If _svc.Lines IsNot Nothing Then Populate(_svc.Lines)
                  If ShowResult(r) Then Report("Trial")
              End Sub)
    End Sub

    ''' <summary>The whole parents the eligible stock supports, and what limits it (KIT-006).</summary>
    Private Sub MaxClicked(ByVal sender As Object, ByVal e As EventArgs)
        If Not Asked() Then Return
        Guard(Sub()
                  Dim r As CobolResult = _svc.MaxBuild(Code(_txtKit), Code(_txtItem), "",
                                                       SterpSession.Current.SiteCode, [When](), Inbound())
                  If ShowResult(r) Then Report("Max build")
              End Sub)
    End Sub

    Private Sub Report(ByVal what As String)
        SetStatus(what & ": buildable " & Fmt.Qty(_svc.Num("KT-MAX-BUILDABLE"), "") & " | " & _svc.Text("KT-COMPLETENESS") &
                  " | " & _svc.Num("KT-SHORTAGE-CNT") & " shortage(s)" &
                  If(_svc.Text("KT-LIMITING-ITEM") <> "", " | limited by " & _svc.Text("KT-LIMITING-ITEM"), "") &
                  If(_svc.Text("KT-ACTION-HINT") <> "", " | " & _svc.Text("KT-ACTION-HINT"), ""),
                  If((_svc.Text("KT-FEASIBLE") = "Y"), "OK", "!  WARNING"))
    End Sub

    Private Function Asked() As Boolean
        If Code(_txtKit) <> "" OrElse Code(_txtItem) <> "" Then Return True
        SetStatus("Enter a kit reference or a parent item first.", "!  WARNING")
        Return False
    End Function

    Private Function Qty() As Decimal
        Dim q As Decimal
        If Not Decimal.TryParse(_txtQty.Text.Trim(), q) Then q = 1D
        Return q
    End Function

    Private Function [When]() As Date
        Dim d As Date
        If Not Date.TryParse(_txtDate.Text.Trim(), d) Then d = SterpSession.Current.BusinessDate
        Return d
    End Function

    Private Function Inbound() As Boolean
        Return _cboSupply.SelectedIndex >= 1
    End Function

    Private Function Subassemblies() As Boolean
        Return _cboSupply.SelectedIndex >= 2
    End Function

    Private Shared Function Code(ByVal t As TextBox) As String
        Return t.Text.Trim().ToUpperInvariant()
    End Function

End Class
