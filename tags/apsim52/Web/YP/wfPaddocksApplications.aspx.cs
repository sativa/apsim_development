using System;
using System.Collections;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Web;
using System.Web.SessionState;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.HtmlControls;

namespace YP2006
{
	/// <summary>
	/// Summary description for wfPaddocksApplications.
	/// </summary>
	public class wfPaddocksApplications : System.Web.UI.Page
	{
		protected System.Web.UI.WebControls.Label lblNitrogen;
		protected System.Data.DataSet dsIrrigation;
		protected System.Data.DataTable dtIrrigation;
		protected System.Data.DataColumn dcIrrigationID;
		protected System.Data.DataColumn dcIrrigationDate;
		protected System.Data.DataColumn dcIrrigationAmount;
		protected System.Data.DataColumn dcIrrigationEfficency;
		protected System.Data.DataSet dsNitrogen;
		protected System.Data.DataTable dtNitrogen;
		protected System.Data.DataColumn dcID;
		protected System.Data.DataColumn dcApplicationDate;
		protected System.Data.DataColumn dcRate;
		protected Janus.Web.GridEX.GridEX grdIrrigation;
		protected Janus.Web.GridEX.GridEX grdNitrogen;
		protected System.Web.UI.WebControls.Label Label2;
		protected System.Web.UI.WebControls.LinkButton btnPaddocksInformation;
		protected System.Web.UI.WebControls.LinkButton btnPaddocksCrop;
		protected System.Web.UI.WebControls.LinkButton btnPaddocksSoil;
		protected System.Web.UI.WebControls.LinkButton btnPaddocksApplictions;
		protected System.Web.UI.WebControls.LinkButton btnPaddocksRainfall;
		protected System.Web.UI.WebControls.Panel pnlPaddock;
		protected System.Web.UI.WebControls.Label lblHeading;
		protected System.Web.UI.WebControls.LinkButton btnGrowersReports;
		protected System.Web.UI.WebControls.LinkButton btnGrowersPaddocks;
		protected System.Web.UI.WebControls.LinkButton btnManageReports;
		protected System.Web.UI.WebControls.Label lblYieldProphet;
		protected System.Web.UI.WebControls.Button btnCancel;
		protected System.Web.UI.WebControls.Button btnSave;
		protected System.Web.UI.WebControls.Panel pnlBottomBorder;
		protected System.Web.UI.WebControls.Button btnCancelTwo;
		protected System.Web.UI.WebControls.Button btnSaveTwo;
		protected System.Web.UI.WebControls.Image imgBanner;
		protected System.Web.UI.WebControls.LinkButton btnPersonalDetails;
		protected System.Web.UI.WebControls.LinkButton btnManageItems;
		protected System.Web.UI.WebControls.LinkButton btnMainMenu;
		protected System.Web.UI.WebControls.Panel pnlNavigationMenu;
		protected System.Web.UI.WebControls.ImageButton imgHelpIrrigation;
		protected System.Web.UI.WebControls.ImageButton imgHelpNitrogen;
		protected System.Web.UI.WebControls.ImageButton btnHelpPaddockApplicationsPage;
		protected System.Web.UI.WebControls.Label lblIrrigation;
	


		#region Web Form Designer generated code
		override protected void OnInit(EventArgs e)
		{
			System.Threading.Thread.CurrentThread.CurrentCulture = System.Globalization.CultureInfo.CreateSpecificCulture("en-AU");
			InitializeComponent();
			base.OnInit(e);
		}
		
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{    
			this.dsIrrigation = new System.Data.DataSet();
			this.dtIrrigation = new System.Data.DataTable();
			this.dcIrrigationID = new System.Data.DataColumn();
			this.dcIrrigationDate = new System.Data.DataColumn();
			this.dcIrrigationAmount = new System.Data.DataColumn();
			this.dcIrrigationEfficency = new System.Data.DataColumn();
			this.dsNitrogen = new System.Data.DataSet();
			this.dtNitrogen = new System.Data.DataTable();
			this.dcID = new System.Data.DataColumn();
			this.dcApplicationDate = new System.Data.DataColumn();
			this.dcRate = new System.Data.DataColumn();
			((System.ComponentModel.ISupportInitialize)(this.dsIrrigation)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dtIrrigation)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dsNitrogen)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dtNitrogen)).BeginInit();
			this.btnPersonalDetails.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnManageReports.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnManageItems.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnGrowersPaddocks.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnGrowersReports.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnMainMenu.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnPaddocksRainfall.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnPaddocksApplictions.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnPaddocksSoil.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnPaddocksCrop.Click += new System.EventHandler(this.NavigationButtonClick);
			this.btnPaddocksInformation.Click += new System.EventHandler(this.NavigationButtonClick);
			this.grdIrrigation.UpdatingCell += new Janus.Web.GridEX.UpdatingCellEventHandler(this.grd_UpdatingCell);
			this.grdNitrogen.UpdatingCell += new Janus.Web.GridEX.UpdatingCellEventHandler(this.grd_UpdatingCell);
			this.btnSave.Click += new System.EventHandler(this.btnSave_Click);
			this.btnCancel.Click += new System.EventHandler(this.btnCancel_Click);
			this.btnSaveTwo.Click += new System.EventHandler(this.btnSave_Click);
			this.btnCancelTwo.Click += new System.EventHandler(this.btnCancel_Click);
			// 
			// dsIrrigation
			// 
			this.dsIrrigation.DataSetName = "NewDataSet";
			this.dsIrrigation.Locale = new System.Globalization.CultureInfo("en-AU");
			this.dsIrrigation.Tables.AddRange(new System.Data.DataTable[] {
																			  this.dtIrrigation});
			// 
			// dtIrrigation
			// 
			this.dtIrrigation.Columns.AddRange(new System.Data.DataColumn[] {
																				this.dcIrrigationID,
																				this.dcIrrigationDate,
																				this.dcIrrigationAmount,
																				this.dcIrrigationEfficency});
			this.dtIrrigation.TableName = "Irrigation";
			// 
			// dcIrrigationID
			// 
			this.dcIrrigationID.Caption = "ID";
			this.dcIrrigationID.ColumnName = "ID";
			// 
			// dcIrrigationDate
			// 
			this.dcIrrigationDate.Caption = "Date";
			this.dcIrrigationDate.ColumnName = "Date";
			this.dcIrrigationDate.DataType = typeof(System.DateTime);
			// 
			// dcIrrigationAmount
			// 
			this.dcIrrigationAmount.Caption = "Amount (mm/ha)";
			this.dcIrrigationAmount.ColumnName = "Amount";
			// 
			// dcIrrigationEfficency
			// 
			this.dcIrrigationEfficency.Caption = "Efficency (%)";
			this.dcIrrigationEfficency.ColumnName = "Efficency";
			// 
			// dsNitrogen
			// 
			this.dsNitrogen.DataSetName = "NewDataSet";
			this.dsNitrogen.Locale = new System.Globalization.CultureInfo("en-US");
			this.dsNitrogen.Tables.AddRange(new System.Data.DataTable[] {
																			this.dtNitrogen});
			// 
			// dtNitrogen
			// 
			this.dtNitrogen.Columns.AddRange(new System.Data.DataColumn[] {
																			  this.dcID,
																			  this.dcApplicationDate,
																			  this.dcRate});
			this.dtNitrogen.TableName = "Nitrogen";
			// 
			// dcID
			// 
			this.dcID.ColumnName = "ID";
			// 
			// dcApplicationDate
			// 
			this.dcApplicationDate.ColumnName = "ApplicationDate";
			this.dcApplicationDate.DataType = typeof(System.DateTime);
			// 
			// dcRate
			// 
			this.dcRate.ColumnName = "Rate";
			this.Load += new System.EventHandler(this.Page_Load);
			((System.ComponentModel.ISupportInitialize)(this.dsIrrigation)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dtIrrigation)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dsNitrogen)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dtNitrogen)).EndInit();

		}
		#endregion


		#region Form Functions
		//-------------------------------------------------------------------------
		//Fills the nitrogen application grid with the vales stored in the database
		//-------------------------------------------------------------------------	
		private void FillNitrogenGrid()
		{
			try
			{
				DataTable dtNitrogenApplications =
					DataAccessClass.GetPaddocksFertiliserApplications("Nitrogen", 
					Session["SelectedPaddockName"].ToString(), FunctionsClass.GetActiveUserName());
				
				DataRow drNitrogen;
				int iMaxNumberOfRows = 5;
				foreach(DataRow drNitrogenApplication in dtNitrogenApplications.Rows)
				{
					drNitrogen = dsNitrogen.Tables["Nitrogen"].NewRow();
					drNitrogen["Rate"] = drNitrogenApplication["Rate"];
					drNitrogen["ApplicationDate"] = DateTime.ParseExact(drNitrogenApplication["ApplicationDate"].ToString(), "yyyy-MM-dd", null);
					dsNitrogen.Tables["Nitrogen"].Rows.Add(drNitrogen);
				}
				for(int iIndex = dsNitrogen.Tables["Nitrogen"].Rows.Count; iIndex < iMaxNumberOfRows; iIndex++)
				{
					drNitrogen = dsNitrogen.Tables["Nitrogen"].NewRow();
					dsNitrogen.Tables["Nitrogen"].Rows.Add(drNitrogen);
				}
				this.DataBind();
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}
		}
		//-------------------------------------------------------------------------
		//Fills the irrigation application grid with the vales stored in the database
		//-------------------------------------------------------------------------	
		private void FillIrrigationGrid()
		{
			try
			{
				//Gets the data and converts it from xml format to a datatable
				DataTable dtIrrigationApplication =
					DataAccessClass.GetPaddocksIrrigationApplications(Session["SelectedPaddockName"].ToString(), 
					FunctionsClass.GetActiveUserName());
				//Ensure that a sample has been added
				DataRow drIrrigation;
				int iMaxNumberOfRows = 10;
				foreach(DataRow drIrrigationApplication in dtIrrigationApplication.Rows)
				{
					drIrrigation = dsIrrigation.Tables["Irrigation"].NewRow();
					drIrrigation["Amount"] = drIrrigationApplication["Amount"];
					drIrrigation["Date"] = DateTime.ParseExact(drIrrigationApplication["ApplicationDate"].ToString(), "yyyy-MM-dd", null);
					drIrrigation["Efficency"] = drIrrigationApplication["Efficency"];
					dsIrrigation.Tables["Irrigation"].Rows.Add(drIrrigation);
				}
				for(int iIndex = dsIrrigation.Tables["Irrigation"].Rows.Count; iIndex < iMaxNumberOfRows; iIndex++)
				{
					drIrrigation = dsIrrigation.Tables["Irrigation"].NewRow();
					dsIrrigation.Tables["Irrigation"].Rows.Add(drIrrigation);
				}
				this.DataBind();
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}
		}
		//-------------------------------------------------------------------------
		//Saves the values from the grid into the database
		//-------------------------------------------------------------------------
		private void SaveNitrogenApplications()
		{
			try
			{
					Janus.Web.GridEX.GridEXRow grdRow;
					grdNitrogen.UpdateOnLeave = true;
					DataAccessClass.DeletePaddocksFertiliserApplications("Nitrogen", 
						Session["SelectedPaddockName"].ToString(), FunctionsClass.GetActiveUserName());
					for(int iIndex = 0; iIndex < grdNitrogen.RowCount; iIndex++)
					{
						grdRow = grdNitrogen.GetRow(iIndex);
						//If there is data in the dataTable then save it to the database
						if(grdRow.Cells["Rate"].Value != null && grdRow.Cells["ApplicationDate"].Value != null)
						{
							DataAccessClass.InsertFertiliserApplication((DateTime.ParseExact(grdRow.Cells["ApplicationDate"].Text, "dd/MM/yyyy", null)).ToString("yyyy-MM-dd"), 
								grdRow.Cells["Rate"].Text, "Nitrogen", Session["SelectedPaddockName"].ToString(), FunctionsClass.GetActiveUserName());
						}
					}
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}
		}
		//-------------------------------------------------------------------------
		//Saves the values from the grid into the database
		//-------------------------------------------------------------------------
		private void SaveIrrigationApplications()
		{
			try
			{
				Janus.Web.GridEX.GridEXRow grdRow;
				grdIrrigation.UpdateOnLeave = true;
				DataAccessClass.DeletePaddocksIrrigationApplications(Session["SelectedPaddockName"].ToString(), 
					FunctionsClass.GetActiveUserName());
				for(int iIndex = 0; iIndex < grdNitrogen.RowCount; iIndex++)
				{
					grdRow = grdIrrigation.GetRow(iIndex);
					//If there is data in the dataTable then save it to the database
					if(grdRow.Cells["Amount"].Value != null && grdRow.Cells["Date"].Value != null && grdRow.Cells["Efficency"].Value != null)
					{
						DataAccessClass.InsertIrrigationApplication((DateTime.ParseExact(grdRow.Cells["Date"].Text, "dd/MM/yyyy", null)).ToString("yyyy-MM-dd"), 
							grdRow.Cells["Amount"].Text, grdRow.Cells["Efficency"].Text, Session["SelectedPaddockName"].ToString(), FunctionsClass.GetActiveUserName());
					}
				}
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void SaveApplications()
		{
			if(FunctionsClass.IsReadOnly() == false)
			{
				SaveNitrogenApplications();
				SaveIrrigationApplications();
			}
			else
				FunctionsClass.DisplayMessage(Page, FunctionsClass.ReturnReadOnlyMessage());
		}
		//-------------------------------------------------------------------------
		#endregion


		#region Form Events
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void Page_Load(object sender, System.EventArgs e)
		{
			if(!IsPostBack)
			{
				//Checks to ensure that only valid users are permitted to view the page
				FunctionsClass.CheckSession();
				FunctionsClass.CheckForGrowerLevelPriviledges();

				FillNitrogenGrid();
				FillIrrigationGrid();
				FunctionsClass.SetHeadingString(lblHeading);
				FunctionsClass.SetNavigationMenu(btnGrowersPaddocks, btnGrowersReports, 
					btnManageItems, btnManageReports);
				FunctionsClass.SetDisplayBanner(imgBanner);
				HelpClass.SetHelpForPaddockApplicationPage(imgHelpNitrogen, 
					imgHelpIrrigation, btnHelpPaddockApplicationsPage);
			}
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void NavigationButtonClick(object sender, System.EventArgs e)
		{
			Server.Transfer(((LinkButton)sender).CommandName);
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnSave_Click(object sender, System.EventArgs e)
		{
			SaveApplications();
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnCancel_Click(object sender, System.EventArgs e)
		{
		Server.Transfer("wfPaddocksApplications.aspx");
		}

		//-------------------------------------------------------------------------
		//On the update of the grid cell, it runs a check to make sure that the 
		//values entered are valid
		//-------------------------------------------------------------------------
		private void grd_UpdatingCell(object sender, Janus.Web.GridEX.UpdatingCellEventArgs e)
		{
			if(e.Column.Key == "Rate" || e.Column.Key == "Amount" || e.Column.Key == "Efficency")
			{
				if(e.Value != null)
				{
					if(InputValidationClass.IsInputAPositiveDecimal(e.Value.ToString()) == false)
					{
						e.Value = "0";
					}
				}
			}
		}
		//-------------------------------------------------------------------------
		#endregion

	}//END OF CLASS
}//END OF NAMESPACE
