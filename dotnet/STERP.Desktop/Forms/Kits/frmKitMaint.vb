Imports System.Data
Imports STERP.Business.Kits
Imports STERP.CobolInterop
Imports STERP.Common

''' <summary>
''' Kit maintenance: the execution kit worklist with its readiness, and the lifecycle actions
''' that KITCREAT owns - review, release (which freezes the revisions and assumptions) and the
''' kind check that decides whether this parent may ever be received into stock.
''' REQ: KIT-001, KIT-002, KIT-003, KIT-004, KIT-008, KIT-022, KIT-023, KIT-024
''' </summary>
Public Class frmKitMaint
    Inherits ListForm

    Private ReadOnly _svc As New KitService()
    Private ReadOnly _exec As New KitExecutionService()
    Private _txtKit As TextBox
    Private _txtSite As TextBox
    Private _cboStatus As ComboBox

    Public Sub New()
        Me.Text = "Kit Maintenance"
    End Sub

    Protected Overrides Sub BuildFilters()
        _txtKit = FilterEdit("&Kit:", 150)
        _txtSite = FilterEdit("S&ite:", 60)
        _cboStatus = FilterList("S&tatus:", 120, "ALL", "DRAFT", "RELEASED", "PARTISSUED",
                                "COMPLETE", "CLOSED", "CANCELLED")
        _txtSite.Text = SterpSession.Current.SiteCode
        If FormArgument <> "" Then _txtKit.Text = FormArgument
        AddHandler _cboStatus.SelectedIndexChanged, Sub(s As Object, e As EventArgs) LoadData()
    End Sub

    Protected Overrides Sub BuildTools()
        AddSeparator()
        AddTool("Lines", "FIND", AddressOf LinesClicked)
        AddTool("Review (F10)", "ENGINE", AddressOf ReviewClicked)
        AddTool("Release", "RUN", AddressOf ReleaseClicked)
        AddTool("Kind check", "ENGINE", AddressOf KindClicked)
        AddTool("Commercial sets", "ENGINE", AddressOf SetsClicked)
        AddSeparator()
    End Sub

    Protected Overrides Sub LoadData()
        Guard(Sub() Populate(_svc.Worklist(Code(_txtSite), Status(), Code(_txtKit))))
    End Sub

    ''' <summary>KITCREAT ENQUIRE plus the GETLINE window: requirements against actuals (KIT-003).</summary>
    Private Sub LinesClicked(ByVal sender As Object, ByVal e As EventArgs)
        If Not Picked() Then Return
        Guard(Sub()
                  Dim r As CobolResult = _svc.Enquire(KitRef())
                  If _svc.Lines IsNot Nothing Then Populate(_svc.Lines)
                  If ShowResult(r) Then Report("Kit " & KitRef())
              End Sub)
    End Sub

    Private Sub ReviewClicked(ByVal sender As Object, ByVal e As EventArgs)
        If Not Picked() Then Return
        Guard(Sub()
                  Dim r As CobolResult = _svc.Review(KitRef())
                  If ShowResult(r) Then Report("Reviewed")
              End Sub)
    End Sub

    ''' <summary>Release freezes the bill and item revisions for this kit (KIT-004).</summary>
    Private Sub ReleaseClicked(ByVal sender As Object, ByVal e As EventArgs)
        If Not Picked() Then Return
        If Not Confirm("Release " & KitRef() & "?  The revisions and assumptions are then fixed.") Then Return
        Guard(Sub()
                  Dim r As CobolResult = _svc.Release(KitRef())
                  If ShowResult(r) Then
                      Report("Released")
                      LoadData()
                  End If
              End Sub)
    End Sub

    ''' <summary>A commercial kit parent can never be received as a stocked assembly (KIT-001).</summary>
    Private Sub KindClicked(ByVal sender As Object, ByVal e As EventArgs)
        If Not Picked() Then Return
        Guard(Sub()
                  Dim kind As String = Fmt.Str(GridHelper.Selected(Grid, "KitKind"))
                  Dim item As String = Fmt.Str(GridHelper.Selected(Grid, "ParentItemCode"))
                  Dim r As CobolResult = _svc.KindCheck(KitRef(), kind, item)
                  If ShowResult(r) Then
                      SetStatus("Kind " & kind & " - may be received into stock: " &
                                If(_svc.Text("KC-MAY-RECEIVE") = "Y", "YES", "NO"), If(_svc.Text("KC-MAY-RECEIVE") = "Y", "OK", "!  WARNING"))
                  End If
              End Sub)
    End Sub

    ''' <summary>KITCOMM proves the delivered sets of a commercial kit (KIT-023).</summary>
    Private Sub SetsClicked(ByVal sender As Object, ByVal e As EventArgs)
        If Not Picked() Then Return
        Guard(Sub()
                  Dim item As String = Fmt.Str(GridHelper.Selected(Grid, "ParentItemCode"))
                  Dim r As CobolResult = _exec.Commercial("SETS", item, "", KitRef(), 0, 1D, 0D, 0, 0D)
                  If _exec.Lines IsNot Nothing Then Populate(_exec.Lines)
                  If ShowResult(r) Then
                      SetStatus("Complete sets " & Fmt.Qty(_exec.Num("KV-COMPLETE-SETS"), "") &
                                " | limiting " & _exec.Text("KV-LIMIT-ITEM") &
                                " | despatchable: " & If(_exec.Text("KV-MAY-DESPATCH") = "Y", "YES", "NO"),
                                If(_exec.Text("KV-MAY-DESPATCH") = "Y", "OK", "!  WARNING"))
                  End If
              End Sub)
    End Sub

    Protected Overrides Sub OnConfirm()
        ReviewClicked(Nothing, EventArgs.Empty)
    End Sub

    Private Sub Report(ByVal what As String)
        SetStatus(what & " - status " & _svc.Text("KC-STATUS") & ", " & _svc.Text("KC-COMPLETENESS") & ", " &
                  _svc.Num("KC-SHORTAGE-CNT") & " shortage(s), WIP " & Fmt.Money(_svc.Num("KC-WIP-VALUE")) &
                  If(_svc.Text("KC-WARNING-TEXT") <> "", " | " & _svc.Text("KC-WARNING-TEXT"), ""),
                  If(_svc.Num("KC-SHORTAGE-CNT") = 0D, "OK", "!  WARNING"))
    End Sub

    Private Function KitRef() As String
        Dim sel As String = Fmt.Str(GridHelper.Selected(Grid, "KitRef"))
        Return If(sel <> "", sel, Code(_txtKit))
    End Function

    Private Function Picked() As Boolean
        If KitRef() <> "" Then Return True
        SetStatus("Select a kit row, or type a kit reference.", "!  WARNING")
        Return False
    End Function

    Private Function Status() As String
        Dim s As String = Convert.ToString(_cboStatus.SelectedItem)
        Return If(s = "ALL", "", s)
    End Function

    Private Shared Function Code(ByVal t As TextBox) As String
        Return t.Text.Trim().ToUpperInvariant()
    End Function

End Class
