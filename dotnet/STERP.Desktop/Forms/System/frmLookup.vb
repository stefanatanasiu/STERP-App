Imports STERP.Business
Imports STERP.Common

''' <summary>
''' Generic F2 lookup: runs a stored procedure that accepts @CompanyCode, @SiteCode,
''' @Search and @Argument and returns a result set whose first column is the code.
''' </summary>
Partial Public Class frmLookup
    Inherits Form

    Private ReadOnly _proc As String
    Private ReadOnly _arg As String
    Private ReadOnly _svc As New LookupService()

    ''' <summary>Code picked by the operator, or "" when the dialog was cancelled.</summary>
    Public SelectedCode As String = ""

    Public Sub New(ByVal procedure As String, ByVal argument As String, ByVal caption As String)
        InitializeComponent()
        UiStyle.ApplyForm(Me)
        GridHelper.Style(grdList)
        _proc = procedure
        _arg = argument
        Me.Text = "Lookup - " & caption
    End Sub

    Private Sub frmLookup_Load(ByVal sender As Object, ByVal e As EventArgs) Handles MyBase.Load
        Search()
    End Sub

    Private Sub Search()
        Try
            Me.Cursor = Cursors.WaitCursor
            GridHelper.Bind(grdList, _svc.Find(_proc, txtSearch.Text.Trim(), _arg))
            lblCount.Text = grdList.Rows.Count & " record(s).  Double-click or press Enter to select."
        Catch ex As Exception
            lblCount.Text = "*** " & ex.Message
        Finally
            Me.Cursor = Cursors.Default
        End Try
    End Sub

    Private Sub btnSearch_Click(ByVal sender As Object, ByVal e As EventArgs) Handles btnSearch.Click
        Search()
    End Sub

    Private Sub btnOk_Click(ByVal sender As Object, ByVal e As EventArgs) Handles btnOk.Click
        If grdList.CurrentRow Is Nothing OrElse grdList.Columns.Count = 0 Then Return
        Dim v As Object = grdList.CurrentRow.Cells(0).Value
        SelectedCode = If(v Is Nothing OrElse IsDBNull(v), "", Convert.ToString(v).Trim())
        Me.DialogResult = DialogResult.OK
        Me.Close()
    End Sub

    Private Sub grdList_DoubleClick(ByVal sender As Object, ByVal e As EventArgs) Handles grdList.DoubleClick
        btnOk_Click(sender, e)
    End Sub

    Private Sub txtSearch_KeyDown(ByVal sender As Object, ByVal e As KeyEventArgs) Handles txtSearch.KeyDown
        If e.KeyCode = Keys.Enter Then
            Search()
            e.Handled = True
        End If
    End Sub

    ''' <summary>Opens the lookup and returns the chosen code ("" when cancelled).</summary>
    Public Shared Function Pick(ByVal owner As IWin32Window, ByVal procedure As String,
                                ByVal argument As String, ByVal caption As String) As String
        Using f As New frmLookup(procedure, argument, caption)
            If f.ShowDialog(owner) = DialogResult.OK Then Return f.SelectedCode
        End Using
        Return ""
    End Function

End Class
