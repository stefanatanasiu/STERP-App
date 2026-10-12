using System;
using System.Collections.Generic;
using System.Data;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Threading;
using System.Windows.Forms;
using STERP.Business;
using STERP.CobolInterop;
using STERP.Common;

namespace STERP.Tests
{
    /// <summary>Regression checks for the native GUI findings in FUNCTIONAL-QA-LEDGER.</summary>
    public static class GuiRegressionTests
    {
        private const BindingFlags Members =
            BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.DeclaredOnly;

        private static void Sta(Action test)
        {
            Exception failure = null;
            var thread = new Thread(() =>
            {
                try { test(); }
                catch (Exception ex) { failure = ex; }
            });
            thread.SetApartmentState(ApartmentState.STA);
            thread.Start();
            thread.Join();
            if (failure != null) throw failure;
        }

        private static Form Screen(string name, bool filters = false)
        {
            var assembly = Assembly.LoadFrom(Path.Combine(SterpConfig.BinDir, "STERP.exe"));
            var type = assembly.GetType("STERP.Desktop." + name, true);
            var form = (Form)Activator.CreateInstance(type);
            Call(form, "BuildStatusLine");
            if (filters) Call(form, "BuildFilters");
            return form;
        }

        private static FieldInfo Field(object target, string name)
        {
            for (var type = target.GetType(); type != null; type = type.BaseType)
            {
                var field = type.GetField(name, Members) ?? type.GetField("_" + name, Members);
                if (field != null) return field;
            }
            throw new MissingFieldException(target.GetType().FullName, name);
        }

        private static T Get<T>(object target, string name)
        {
            return (T)Field(target, name).GetValue(target);
        }

        private static object Call(object target, string name, params object[] arguments)
        {
            for (var type = target.GetType(); type != null; type = type.BaseType)
            {
                var method = type.GetMethods(Members | BindingFlags.Static)
                    .SingleOrDefault(m => m.Name == name && m.GetParameters().Length == arguments.Length);
                if (method == null) continue;
                try { return method.Invoke(method.IsStatic ? null : target, arguments); }
                catch (TargetInvocationException ex)
                {
                    if (ex.InnerException != null) throw ex.InnerException;
                    throw;
                }
            }
            throw new MissingMethodException(target.GetType().FullName, name);
        }

        private static string Status(Form form)
        {
            return Get<ToolStripStatusLabel>(form, "_statusLabel").Text;
        }

        private static DataTable StaleRows()
        {
            var rows = new DataTable();
            rows.Columns.Add("PreviousAccount");
            rows.Rows.Add("C0001");
            return rows;
        }

        private static void Bind(DataGridView grid, DataTable rows)
        {
            grid.BindingContext = new BindingContext();
            var handle = grid.Handle;
            grid.DataSource = rows;
        }

        private static void Context(string user, string site = "RDG")
        {
            SterpSession.Current.UserCode = user;
            SterpSession.Current.CompanyCode = "STC";
            SterpSession.Current.SiteCode = site;
            SterpSession.Current.BusinessDate = new DateTime(2026, 10, 12);
        }

        [StTest("gui: job monitor filters use actual stored statuses", Reqs = "COM-018,XMD-007")]
        public static void JobMonitorUsesStoredStatuses()
        {
            Sta(() =>
            {
                Context("FINCTRL");
                using (var form = Screen("frmJobMonitor", true))
                {
                    var filter = Get<ComboBox>(form, "cboStatus");
                    St.True(filter.Items.Cast<string>().SequenceEqual(
                        new[] { "ALL", "RUNNING", "OK", "WARNING", "FAILED", "ABENDED" }),
                        "all persisted job states are selectable, without the nonexistent COMPLETED state");
                    Get<TextBox>(form, "txtFrom").Text = "01/01/2000";
                    filter.SelectedItem = "OK";
                    var rows = Get<DataGridView>(form, "Grid").DataSource as DataTable;
                    St.True(rows != null, "the selected successful-job filter loads its result");
                    St.Eq(new JobService().Runs(new DateTime(2000, 1, 1), "OK").Rows.Count, rows.Rows.Count,
                        "the GUI retrieves exactly the successful runs");
                    St.True(rows.Rows.Cast<DataRow>().All(row => Fmt.Str(row["Status"]) == "OK"),
                        "the successful filter does not include warning/failure/running rows");
                }
            });
        }

        [StTest("gui: blank customer refresh clears every account result", Reqs = "AR-021,COM-024")]
        public static void CustomerBlankRefresh()
        {
            Sta(() =>
            {
                using (var form = Screen("frmCustomerEnquiry"))
                {
                    Get<Label>(form, "lblCustomerName").Text = "Pennine Cycles";
                    var metrics = Get<Dictionary<string, Label>>(form, "Metric");
                    foreach (var label in metrics.Values) label.Text = "stale";
                    Bind(Get<DataGridView>(form, "grdAged"), StaleRows());
                    Bind(Get<DataGridView>(form, "grdItems"), StaleRows());
                    Get<Label>(form, "lblItemsCaption").Text = "Old statement";

                    Call(form, "LoadData");

                    St.Eq("", Get<Label>(form, "lblCustomerName").Text, "old customer name is cleared");
                    St.True(metrics.Values.All(label => label.Text == ""), "all account metrics are cleared");
                    St.Eq(null, Get<DataGridView>(form, "grdAged").DataSource, "old ageing is unbound");
                    St.Eq(null, Get<DataGridView>(form, "grdItems").DataSource, "old open items are unbound");
                    St.Eq("Open items, oldest due date first:",
                        Get<Label>(form, "lblItemsCaption").Text, "statement caption is reset");
                }
            });
        }

        [StTest("gui: blank receipt refresh clears allocations and preview totals", Reqs = "AR-009,COM-024")]
        public static void ReceiptBlankRefresh()
        {
            Sta(() =>
            {
                using (var form = Screen("frmReceiptEntry"))
                {
                    var rows = StaleRows();
                    Field(form, "_items").SetValue(form, rows);
                    Bind(Get<DataGridView>(form, "grdItems"), rows);
                    var totals = Get<Dictionary<string, Label>>(form, "Total");
                    foreach (var label in totals.Values) label.Text = "stale";

                    Call(form, "LoadItems");

                    St.Eq(null, Get<DataTable>(form, "_items"), "old allocation buffer is discarded");
                    St.Eq(null, Get<DataGridView>(form, "grdItems").DataSource, "old allocation grid is unbound");
                    St.True(totals.Values.All(label => label.Text == ""), "all preview totals are cleared");
                    St.Eq(false, (bool)Call(form, "Ready"), "a failed refresh cannot preview or post");
                }
            });
        }

        [StTest("gui: receipt actions require the loaded customer and currency", Reqs = "AR-009,AR-010")]
        public static void ReceiptLoadedContext()
        {
            Context("ARCLERK");
            Sta(() =>
            {
                using (var form = Screen("frmReceiptEntry"))
                {
                    Get<TextBox>(form, "txtCustomer").Text = "C0001";
                    Get<ComboBox>(form, "cboCurrency").SelectedItem = "GBP";
                    Get<TextBox>(form, "txtAmount").Text = "8196.00";
                    var grid = Get<DataGridView>(form, "grdItems");
                    grid.BindingContext = new BindingContext();
                    var handle = grid.Handle;
                    Call(form, "LoadItems");
                    St.Eq(true, (bool)Call(form, "Ready"), "the loaded customer/currency is ready");

                    Get<TextBox>(form, "txtCustomer").Text = "C0004";
                    St.Eq(false, (bool)Call(form, "Ready"), "changed customer requires a reload");
                    Get<TextBox>(form, "txtCustomer").Text = " c0001 ";
                    St.Eq(true, (bool)Call(form, "Ready"), "normalised account identity is unchanged");
                    Get<ComboBox>(form, "cboCurrency").SelectedItem = "EUR";
                    St.Eq(false, (bool)Call(form, "Ready"), "changed currency requires a reload");
                }
            });
        }

        [StTest("gui: a recorded receipt cannot be posted again without reloading", Reqs = "AR-010")]
        public static void ReceiptAlreadyPosted()
        {
            Sta(() =>
            {
                using (var form = Screen("frmReceiptEntry"))
                {
                    Field(form, "_items").SetValue(form, StaleRows());
                    Field(form, "_posted").SetValue(form, true);
                    Get<TextBox>(form, "txtAmount").Text = "100";

                    St.Eq(false, (bool)Call(form, "Ready"), "the already-recorded receipt is not sent again");
                }
            });
        }

        [StTest("gui: promise toolbar uses the keyboard promise workflow", Reqs = "AR-016,COM-024")]
        public static void PromiseToolbar()
        {
            Sta(() =>
            {
                using (var form = Screen("frmCreditControl"))
                {
                    Get<ToolStripButton>(form, "tsbPromise").PerformClick();
                    St.Eq("Select an overdue item first.", Status(form), "the shared action checks its selection");
                }
            });
        }

        [StTest("gui: invalid explosion inputs clear rather than calculate substitutes", Reqs = "BOM-012,COM-024")]
        public static void ExplosionInvalidInputs()
        {
            Context("ENGINEER");
            Sta(() =>
            {
                using (var form = Screen("frmBomExplosion", true))
                {
                    Get<TextBox>(form, "_txtItem").Text = "FG-CAKE-CELEB-8IN";
                    Get<TextBox>(form, "_txtAsAt").Text = "12/10/2026";
                    var grid = Get<DataGridView>(form, "Grid");
                    foreach (var quantity in new[] { "0", "-1", "bad", "" })
                    {
                        Bind(grid, StaleRows());
                        Get<TextBox>(form, "_txtQty").Text = quantity;

                        Call(form, "ExplodeClicked", null, EventArgs.Empty);

                        St.Eq(null, grid.DataSource, "invalid quantity leaves no calculated rows: " + quantity);
                        St.True(Status(form).Contains("positive"), "quantity refusal is explicit: " + quantity);
                    }
                    Get<TextBox>(form, "_txtQty").Text = "12";
                    Get<TextBox>(form, "_txtAsAt").Text = "32/10/2026";
                    Bind(grid, StaleRows());
                    Call(form, "ExplodeClicked", null, EventArgs.Empty);
                    St.Eq(null, grid.DataSource, "an invalid date leaves no calculated rows");
                    St.True(Status(form).Contains("date"), "date refusal is explicit");
                }
            });
        }

        [StTest("gui: empty BOM enquiries remove old results and parent selection", Reqs = "BOM-012,BOM-023")]
        public static void BomBlankRefresh()
        {
            Sta(() =>
            {
                foreach (var name in new[] { "frmBomExplosion", "frmBomWhereUsed" })
                {
                    using (var form = Screen(name, true))
                    {
                        var grid = Get<DataGridView>(form, "Grid");
                        Bind(grid, StaleRows());
                        Call(form, "LoadData");
                        St.Eq(null, grid.DataSource, name + " removes the old result set");
                        St.Eq(null, grid.CurrentRow, name + " removes the old actionable selection");
                    }
                }
            });
        }

        [StTest("gui: cost comparison retains its selected action target", Reqs = "CST-007,CST-012")]
        public static void CostSetComparisonContext()
        {
            Context("COSTACC");
            Sta(() =>
            {
                using (var form = Screen("frmCostSet", true))
                {
                    var sets = new DataTable();
                    sets.Columns.Add("CostSet");
                    sets.Rows.Add("FY2027STD");
                    var grid = Get<DataGridView>(form, "Grid");
                    Bind(grid, sets);
                    grid.CurrentCell = grid.Rows[0].Cells[0];
                    Get<TextBox>(form, "_txtCompare").Text = "FY2026STD";

                    Call(form, "CompareClicked", null, EventArgs.Empty);

                    St.Eq("FY2027STD", (string)Call(form, "Chosen"), "comparison rows retain their owning set");
                    St.True(Status(form).Contains("Compared FY2027STD with FY2026STD"),
                        "the comparison reports its actual target");
                    Call(form, "ImpactClicked", null, EventArgs.Empty);
                    St.True(Status(form).Contains("Impact FY2027STD:"), "refresh does not rename the action");
                    St.Eq("FY2027STD", (string)Call(form, "Chosen"), "the refreshed list keeps the action selection");
                }
            });
        }

        [StTest("gui: goods receipt editors are above decorative groups", Reqs = "PO-009,PO-010,COM-024")]
        public static void GoodsReceiptEditorLayering()
        {
            Sta(() =>
            {
                using (var form = Screen("frmGoodsReceipt"))
                {
                    var editors = Get<Dictionary<string, TextBox>>(form, "_edit");
                    foreach (var group in form.Controls.OfType<GroupBox>())
                    {
                        foreach (var editor in editors.Values.Where(e => group.Bounds.IntersectsWith(e.Bounds)))
                        {
                            St.True(form.Controls.GetChildIndex(editor) < form.Controls.GetChildIndex(group),
                                editor.AccessibleName + " is above " + group.Text);
                        }
                    }
                }
            });
        }

        [StTest("gui: goods receipt offers only engine-supported ownership bases", Reqs = "PO-014,PO-018")]
        public static void GoodsReceiptOwnership()
        {
            Sta(() =>
            {
                using (var form = Screen("frmGoodsReceipt"))
                {
                    var owner = Get<ComboBox>(form, "_cboOwner");
                    St.Eq(3, owner.Items.Count, "unsupported ownership aliases are not offered");
                    St.Eq("RECEIPT", Convert.ToString(owner.SelectedItem), "normal receipt is the default");
                    St.True(owner.Items.Cast<string>().SequenceEqual(new[] { "RECEIPT", "INCOTERMS", "DROPSHIP" }),
                        "the UI codes match RECEIPTCA and the SQL constraint");
                }
            });
        }

        [StTest("gui: receipt traceability retains its specific reason and loaded lines", Reqs = "PO-009,PO-025")]
        public static void GoodsReceiptTraceabilityReason()
        {
            Context("WHSE", "BHM");
            var service = new GoodsReceiptService();
            var values = new Dictionary<string, string>
            {
                { "ORDER", "PO00000004" }, { "WHS", "MAIN" }, { "DELNOTE", "QA-TRACE-ONLY" }
            };
            var result = service.Validate(values);

            St.Eq(8, result.ReturnCode, "the untraced receipt is refused");
            St.Eq("RCP0004", result.ReasonCode, "the lot-required refusal is not overwritten");
            St.True(result.Message.Contains("lot number"), "the receiver is told which input to correct");
            St.True(service.Lines().Rows.Count > 0, "the loaded lines remain available for counting");
            service.Count(1, 40m, "GOODSIN", "QA-TRACE-ONLY", "");
            St.Eq("QA-TRACE-ONLY", Fmt.Str(service.Lines().Rows[0]["RC-L-LOT"]), "the traceability can be corrected");
        }

        [StTest("gui: VAT box selection survives drill-button focus", Reqs = "FIN-026,COM-024")]
        public static void VatMouseDrillFocus()
        {
            Context("VATOWNER");
            Sta(() =>
            {
                using (var form = Screen("frmVatReturn"))
                {
                    form.ShowInTaskbar = false;
                    form.Show();
                    Get<TextBox>(form, "_txtPeriod").Text = "26A2";
                    Call(form, "PrepareClicked", null, EventArgs.Empty);
                    var boxes = Get<TextBox[]>(form, "_box");
                    var button = form.Controls.OfType<Button>().Single(b => b.Text.Replace("&", "") == "Drill down");
                    boxes[0].Select();
                    button.Select();
                    St.Eq(button, form.ActiveControl, "the button, not the value box, now has focus");
                    button.PerformClick();
                    var service = Get<VatService>(form, "_svc");
                    St.True(service.Drilled != null && service.Drilled.Rows.Count > 0,
                        "ordinary drill-button activation loads the selected box's documents");
                    St.True(Status(form).Contains("behind box 1 of 26A2"), "the selected VAT box is reported");
                    Call(form, "PrepareClicked", null, EventArgs.Empty);
                    St.Eq(0, Get<int>(form, "_selectedBox"), "preparation clears the old box selection");
                    form.Close();
                }
            });
        }

        [StTest("gui: duplicate checks identify both seeded invoices and credit notes", Reqs = "AP-004")]
        public static void ApDocumentDuplicates()
        {
            Context("APCLERK", "BHM");
            var service = new InvoiceService();
            var documents = service.Register("S0001", "ALL");
            foreach (var type in new[] { "INVOICE", "CREDITNOTE" })
            {
                var row = documents.Rows.Cast<DataRow>().First(r => Fmt.Str(r["DocumentType"]) == type);
                var result = service.DuplicateCheck(Fmt.Str(row["SupplierCode"]),
                    Fmt.Str(row["SupplierDocRef"]), Fmt.Dec(row["StatedGross"]),
                    Convert.ToDateTime(row["DocumentDate"]), type);
                St.True(service.DuplicateFound, "the exact " + type + " is identified");
                St.Eq(Fmt.Str(row["RegisterNumber"]), service.DuplicateRegister, "the correct register reference is returned");
                St.Eq("APR0010", result.ReasonCode, "the duplicate retains the engine's refusal");
            }
        }

        [StTest("gui: payment totals load the selected proposal instead of reporting zero", Reqs = "AP-011")]
        public static void PaymentProposalTotals()
        {
            Context("APCLERK", "BHM");
            var service = new PaymentService();
            var proposal = service.Proposals().Rows.Cast<DataRow>().First(r => Fmt.Dec(r["TotalOutflowGBP"]) > 0m);
            var reference = Fmt.Str(proposal["ProposalRef"]);
            var result = service.Totals(reference);
            St.True(result.Succeeded, "known proposal totals answer");
            St.Eq(Fmt.Int(proposal["ProposalId"]), result.Area.GetInt("PP-PROPOSAL-ID"), "real proposal identity is loaded");
            St.Eq(Fmt.Dec(proposal["CashPrincipalGBP"]), service.CashPrincipal, "stored cash principal");
            St.Eq(Fmt.Dec(proposal["CreditsAppliedGBP"]), service.CreditsApplied, "stored credits");
            St.Eq(Fmt.Dec(proposal["TotalOutflowGBP"]), service.TotalOutflow, "stored non-zero outflow");
            St.Eq(Fmt.Str(proposal["Status"]), service.ProposalStatus, "proposal status is retained");
            St.Eq(Fmt.Int(proposal["ExcludedCount"]), service.ExcludedCount, "stored exclusion count");
            St.Eq(service.Lines(reference).Rows.Count, result.Area.GetInt("PP-LINE-CNT"), "the actual proposal lines are read");
            result = service.Totals("QA-UNKNOWN-PROPOSAL");
            St.True(result.IsError, "an unknown reference is an explicit refusal, not success-shaped zero totals");
            St.Eq(0m, service.TotalOutflow, "a refusal removes the previous proposal's amount");
            St.Eq("", service.ProposalStatus, "a refusal removes the previous status");
        }

        [StTest("gui: goods receipt derives supplier and currency from its site-scoped order", Reqs = "PO-009,PO-014")]
        public static void GoodsReceiptOrderIdentity()
        {
            Context("WHSE", "BHM");
            var service = new GoodsReceiptService();
            service.Validate(new Dictionary<string, string>
            {
                { "ORDER", "PO00000004" }, { "WHS", "MAIN" }, { "LOT", "QA-IDENTITY-ONLY" }
            });
            var area = Get<CommArea>(service, "_area");
            St.True(area.GetString("RC-SUPPLIER") != "", "the screen's absent supplier input is derived from the order");
            St.Eq("GBP", area.GetString("RC-CURRENCY"), "the order currency is preserved");
            Context("WHSE", "RDG");
            var otherSite = new GoodsReceiptService();
            var result = otherSite.Validate(new Dictionary<string, string> { { "ORDER", "PO00000004" } });
            St.True(result.ReturnCode >= 8, "a different site's order is refused");
            St.Eq(0, otherSite.Lines().Rows.Count, "a different site's delivery lines are not loaded");
        }

        [StTest("gui: maintenance refresh synchronizes a single selected record and clears empty results",
            Reqs = "COM-013,COM-024,BOM-004")]
        public static void MaintenanceSelectedRecord()
        {
            Context("ENGINEER");
            Sta(() =>
            {
                using (var form = Screen("frmGenericMaint"))
                {
                    form.GetType().GetProperty("FormArgument").SetValue(form, "BOM.HEADER", null);
                    Call(form, "LoadDefinition");
                    var grid = Get<DataGridView>(form, "grdList");
                    grid.BindingContext = new BindingContext();
                    var handle = grid.Handle;
                    var search = Get<TextBox>(form, "txtSearch");
                    var editors = Get<Dictionary<string, Control>>(form, "_editors");
                    search.Text = "BOM-DEMO-B4TC";

                    Call(form, "LoadList");

                    St.Eq(1, grid.Rows.Count, "the filtered draft has exactly one result");
                    St.Eq("CHANGE", Get<string>(form, "_mode"), "the selected record is editable");
                    St.True(editors["Description"].Enabled, "non-key content can be edited");
                    St.Eq(((DataRowView)grid.CurrentRow.DataBoundItem)["Description"].ToString(),
                        editors["Description"].Text,
                        "the editor belongs to the selected record");
                    St.True(editors.Values.Where(e => Convert.ToBoolean(((DataRow)e.Tag)["IsKey"]))
                        .All(e => !e.Enabled), "existing primary keys remain protected");
                    St.Eq(null, Call(form, "GetValue", editors["BatchSize"]),
                        "an unset optional numeric field is posted as null, not an empty numeric string");
                    St.Eq("", Call(form, "GetValue", editors["ParentRevision"]),
                        "an empty code retains its existing empty-string semantics");
                    editors["BatchSize"].Text = "0";
                    St.Eq("0", Call(form, "GetValue", editors["BatchSize"]),
                        "an explicitly keyed numeric zero is not replaced by null");

                    search.Text = "QA-NO-SUCH-BILL";
                    Call(form, "LoadList");

                    St.Eq(0, grid.Rows.Count, "an unknown search has no results");
                    St.Eq("", Get<string>(form, "_mode"), "no record remains in change mode");
                    St.Eq("", editors["Description"].Text, "previous record content is cleared");
                    St.True(editors.Values.All(e => !e.Enabled), "no-result editors are disabled");

                    Call(form, "NewClicked", null, EventArgs.Empty);
                    St.Eq("ADD", Get<string>(form, "_mode"), "the new-record action enters ADD mode");
                    St.True(editors.Values.OfType<DateTimePicker>().All(d => !d.Checked),
                        "dates with no metadata default are unset on a new record");
                    search.Text = "BOM-DEMO-B4TC";
                    Call(form, "LoadList");

                    St.Eq("CHANGE", Get<string>(form, "_mode"), "refresh abandons ADD for the selected record");
                    St.True(editors["Description"].Enabled, "the refreshed record is editable");
                }
            });
        }

        [StTest("gui: despatch refuses invalid dates without substituting the business date",
            Reqs = "SO-012,COM-024")]
        public static void DespatchDateValidation()
        {
            Context("WHSE");
            Sta(() =>
            {
                using (var form = Screen("frmDespatchConfirm"))
                {
                    var lines = new DataTable();
                    lines.Columns.Add("DespatchQty", typeof(decimal));
                    lines.Rows.Add(1m);
                    Field(form, "_lines").SetValue(form, lines);
                    Field(form, "_loadedPick").SetValue(form, "QA-PICK");
                    Field(form, "_loadedOrder").SetValue(form, "QA-ORDER");
                    Get<TextBox>(form, "txtPickRef").Text = "QA-PICK";
                    Get<TextBox>(form, "txtOrder").Text = "QA-ORDER";
                    var date = Get<TextBox>(form, "txtDespatchDate");

                    foreach (var invalid in new[] { "", "32/10/2026", "not a date" })
                    {
                        date.Text = invalid;
                        St.Eq(null, Call(form, "DespatchDate"), "invalid date is not replaced with today");
                        Call(form, "OnConfirm");
                        St.True(Status(form).Contains("valid despatch date"),
                            "the operator receives an explicit date refusal before confirmation");
                    }

                    date.Text = "15/10/2026";
                    St.Eq(new DateTime(2026, 10, 15), Call(form, "DespatchDate"),
                        "the valid entered date is preserved");
                }
            });
        }
    }
}
