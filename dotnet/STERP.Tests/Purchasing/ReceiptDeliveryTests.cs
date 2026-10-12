using System;
using System.Data.SqlClient;
using STERP.Common;

namespace STERP.Tests.Purchasing
{
    public static class ReceiptDeliveryTests
    {
        private sealed class Fixture : IDisposable
        {
            private readonly SqlConnection connection;
            private readonly SqlTransaction transaction;
            public readonly string Reference = "QAG" + Guid.NewGuid().ToString("N").Substring(0, 20);

            public Fixture(bool legacy = false)
            {
                connection = new SqlConnection(SterpConfig.SqlConnectionString);
                connection.Open();
                transaction = connection.BeginTransaction();
                Run("INSERT po.PurchaseOrder (CompanyCode,SiteCode,OrderNumber,SupplierCode,OrderDate,Status) " +
                    "VALUES ('STC','BHM',@r,'S0004','20261012','ISSUED'); " +
                    "INSERT po.PurchaseOrderLine (PurchaseOrderId,CompanyCode,OrderNumber,LineNum,ItemCode," +
                    "Description,LineType,OrderedQty,UnitPrice) SELECT PurchaseOrderId,'STC',@r,1," +
                    "'QA-SERVICE','GUI receipt regression','SERVICE',4,0 FROM po.PurchaseOrder WHERE OrderNumber=@r; " +
                    "INSERT po.PurchaseSchedule (PurchaseOrderLineId,CompanyCode,OrderNumber,LineNum,ScheduleNo,Quantity,RequestedDate) " +
                    "SELECT PurchaseOrderLineId,'STC',@r,1,1,4,'20261012' FROM po.PurchaseOrderLine WHERE OrderNumber=@r; " +
                    "INSERT po.Receipt (CompanyCode,SiteCode,ReceiptRef,SupplierCode,PurchaseOrderNumber,PhysicalDate,PostingDate,Status) " +
                    "VALUES ('STC','BHM',@r,'S0004',@r,'20261012','20261012','" + (legacy ? "CONFIRMED" : "DRAFT") + "'); " +
                    "INSERT po.ReceiptLine (ReceiptId,LineNum,PurchaseOrderNumber,PurchaseOrderLine,ScheduleNo,ItemCode," +
                    "ReceivedQty,StockQty,AcceptedQty) SELECT ReceiptId,1,@r,1,1,'QA-SERVICE',4,0,3 " +
                    "FROM po.Receipt WHERE ReceiptRef=@r;");
            }

            public object Run(string sql)
            {
                using (var command = new SqlCommand(sql, connection, transaction))
                {
                    command.Parameters.AddWithValue("@r", Reference);
                    return command.ExecuteScalar();
                }
            }

            public void State(string state)
            {
                Run("EXEC po.usp_ReceiptDeliveryState 'STC','BHM',@r,'" + state + "',0,NULL,'BUYER';");
            }

            public decimal Quantity(string table, string column)
            {
                return Convert.ToDecimal(Run("SELECT " + column + " FROM po." + table + " WHERE OrderNumber=@r"));
            }

            public void Dispose()
            {
                if (transaction.Connection != null) transaction.Rollback();
                transaction.Dispose();
                connection.Dispose();
            }
        }

        [StTest("po: receipt delivery quantities, identity and replay are consistent", Reqs = "PO-009,PO-024")]
        public static void ConfirmationAndReplay()
        {
            using (var fixture = new Fixture())
            {
                fixture.State("CONFIRMED");
                St.Eq(4m, fixture.Quantity("PurchaseOrderLine", "ReceivedQty"), "order received quantity");
                St.Eq(3m, fixture.Quantity("PurchaseOrderLine", "AcceptedQty"), "order accepted quantity");
                St.Eq(4m, fixture.Quantity("PurchaseSchedule", "ReceivedQty"), "schedule received quantity");
                St.Eq("COMPLETE", Convert.ToString(fixture.Run(
                    "SELECT Status FROM po.PurchaseSchedule WHERE OrderNumber=@r")), "completed schedule");
                St.Eq("FULLRECEIVED", Convert.ToString(fixture.Run(
                    "SELECT DeliveryStatus FROM po.PurchaseOrder WHERE OrderNumber=@r")), "completed order delivery");
                St.Eq(1, Convert.ToInt32(fixture.Run(
                    "SELECT COUNT(*) FROM po.ReceiptLine r JOIN po.PurchaseOrderLine p " +
                    "ON p.PurchaseOrderLineId=r.PurchaseOrderLineId WHERE p.OrderNumber=@r")),
                    "receipt identifies the actual order line");
                fixture.State("CONFIRMED");
                St.Eq(4m, fixture.Quantity("PurchaseOrderLine", "ReceivedQty"), "replay does not receive twice");
                fixture.Run("UPDATE l SET AcceptedQty=4 FROM po.ReceiptLine l JOIN po.Receipt r " +
                            "ON r.ReceiptId=l.ReceiptId WHERE r.ReceiptRef=@r;");
                St.Eq(4m, fixture.Quantity("PurchaseOrderLine", "AcceptedQty"), "inspection acceptance reaches the order");
                fixture.State("REVERSED");
                St.Eq(0m, fixture.Quantity("PurchaseOrderLine", "ReceivedQty"), "reversal restores order outstanding");
                St.Eq(0m, fixture.Quantity("PurchaseOrderLine", "AcceptedQty"), "reversal restores accepted quantity");
                St.Eq(0m, fixture.Quantity("PurchaseSchedule", "ReceivedQty"), "reversal restores schedule quantity");
                St.Eq("OPEN", Convert.ToString(fixture.Run(
                    "SELECT Status FROM po.PurchaseSchedule WHERE OrderNumber=@r")), "reversal reopens schedule");
                St.Eq("NOTRECEIVED", Convert.ToString(fixture.Run(
                    "SELECT DeliveryStatus FROM po.PurchaseOrder WHERE OrderNumber=@r")), "reversal resets delivery status");
                fixture.State("REVERSED");
                St.Eq(0m, fixture.Quantity("PurchaseOrderLine", "ReceivedQty"), "reversal replay does not subtract twice");
            }
        }

        [StTest("po: historical unlinked receipts do not reverse unapplied quantities", Reqs = "PO-009")]
        public static void LegacyReversal()
        {
            using (var fixture = new Fixture(true))
            {
                fixture.State("REVERSED");
                St.Eq(0m, fixture.Quantity("PurchaseOrderLine", "ReceivedQty"), "legacy order quantity remains zero");
                St.Eq(0m, fixture.Quantity("PurchaseSchedule", "ReceivedQty"), "legacy schedule quantity remains zero");
                St.Eq("REVERSED", Convert.ToString(fixture.Run(
                    "SELECT Status FROM po.Receipt WHERE ReceiptRef=@r")), "legacy receipt is reversed");
            }
        }

        [StTest("po: receipt delivery cannot cross site boundaries", Reqs = "PO-009,COM-001")]
        public static void SiteBoundary()
        {
            using (var fixture = new Fixture())
            {
                try
                {
                    fixture.Run("EXEC po.usp_ReceiptDeliveryState 'STC','RDG',@r,'CONFIRMED';");
                    St.Fail("a BHM receipt must not confirm at RDG");
                }
                catch (SqlException error)
                {
                    St.Eq(50812, error.Number, "cross-site confirmation explicitly refuses the caller");
                }
            }
        }
    }
}
