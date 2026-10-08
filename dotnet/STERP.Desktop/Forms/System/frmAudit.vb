Imports STERP.Business
Imports STERP.Common

''' <summary>Audit trail enquiry over core.AuditLog (COM-013).</summary>
Public Class frmAudit
    Inherits ListForm

    Private ReadOnly _svc As New AuditService()
    Private txtEntity As TextBox
    Private txtKey As TextBox
    Private txtUser As TextBox
    Private txtFrom As TextBox
    Private txtTo As TextBox

    Public Sub New()
        Me.Text = "Audit Enquiry"
        Me.ClientSize = New Size(900, 440)
    End Sub

    Protected Overrides Sub BuildFilters()
        txtEntity = FilterEdit("&Entity:", 110)
        txtKey = FilterEdit("&Key:", 110)
        txtUser = FilterEdit("&User:", 80)
        txtFrom = FilterEdit("Fro&m:", 80)
        txtTo = FilterEdit("&To:", 80)
        txtFrom.Text = Fmt.Date(SterpSession.Current.BusinessDate.AddMonths(-1))
        txtTo.Text = Fmt.Date(SterpSession.Current.BusinessDate)
    End Sub

    Protected Overrides Sub LoadData()
        Dim d1 As Date? = Fmt.ParseDate(txtFrom.Text)
        Dim d2 As Date? = Fmt.ParseDate(txtTo.Text)
        Guard(Sub() Populate(_svc.Enquire(txtEntity.Text.Trim(), txtKey.Text.Trim(),
                                      If(d1.HasValue, d1.Value, Date.Today.AddYears(-5)),
                                      If(d2.HasValue, d2.Value, Date.Today),
                                      txtUser.Text.Trim())))
    End Sub

End Class
