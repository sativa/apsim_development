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

namespace YieldProphet
{
	/// <summary>
	/// Summary description for wfGenerateIrrigationComparisonReport.
	/// </summary>
	public class wfGenerateIrrigationComparisonReport : System.Web.UI.Page
	{
		protected System.Web.UI.WebControls.ImageButton btnSaveImg;
		protected System.Web.UI.WebControls.ImageButton btnCancelImg;
		protected System.Web.UI.WebControls.Button btnSave;
		protected System.Web.UI.WebControls.LinkButton btnCancel;
		protected System.Web.UI.WebControls.Label lblReportName;
		protected System.Web.UI.WebControls.TextBox edtReportName;
		protected System.Data.DataSet dsNitrogen;
		protected System.Data.DataTable dtNitrogen;
		protected System.Data.DataColumn dcID;
		protected System.Data.DataColumn dcApplicationDate;
		protected System.Data.DataColumn dcRate;
		protected System.Data.DataSet dsIrrigation;
		protected System.Data.DataTable dtIrrigation;
		protected System.Data.DataColumn dcIrrigationID;
		protected System.Data.DataColumn dcIrrigationDate;
		protected System.Data.DataColumn dcAmount;
		protected System.Data.DataColumn dcIrrigationEfficency;
		protected Janus.Web.GridEX.GridEX grdNitrogenThree;
		protected Janus.Web.GridEX.GridEX grdNitrogenTwo;
		protected Janus.Web.GridEX.GridEX grdNitrogenOne;
		protected Janus.Web.GridEX.GridEX grdIrrigationOne;
		protected System.Web.UI.WebControls.Label lblScenarioOneApplications;
		protected System.Web.UI.WebControls.Label lblScenarioTwoApplications;
		protected System.Web.UI.WebControls.Label lblScenarioThreeApplications;
		protected Janus.Web.GridEX.GridEX grdIrrigationTwo;
		protected Janus.Web.GridEX.GridEX grdIrrigationThree;
		protected System.Web.UI.WebControls.Label Label1;
		protected System.Web.UI.WebControls.Panel pnlTop;
	

		#region Web Form Designer generated code
		override protected void OnInit(EventArgs e)
		{
			System.Globalization.DateTimeFormatInfo.CurrentInfo.ShortDatePattern = "dd/MM/yyyy";
			InitializeComponent();
			base.OnInit(e);
		}
		
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{    
			this.dsNitrogen = new System.Data.DataSet();
			this.dtNitrogen = new System.Data.DataTable();
			this.dcID = new System.Data.DataColumn();
			this.dcApplicationDate = new System.Data.DataColumn();
			this.dcRate = new System.Data.DataColumn();
			this.dsIrrigation = new System.Data.DataSet();
			this.dtIrrigation = new System.Data.DataTable();
			this.dcIrrigationID = new System.Data.DataColumn();
			this.dcIrrigationDate = new System.Data.DataColumn();
			this.dcAmount = new System.Data.DataColumn();
			this.dcIrrigationEfficency = new System.Data.DataColumn();
			((System.ComponentModel.ISupportInitialize)(this.dsNitrogen)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dtNitrogen)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dsIrrigation)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dtIrrigation)).BeginInit();
			this.btnCancel.Click += new System.EventHandler(this.btnCancel_Click);
			this.btnSave.Click += new System.EventHandler(this.btnSave_Click);
			this.btnCancelImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnCancelImg_Click);
			this.btnSaveImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnSaveImg_Click);
			this.grdNitrogenOne.UpdatingCell += new Janus.Web.GridEX.UpdatingCellEventHandler(this.grd_UpdatingCell);
			this.grdIrrigationOne.UpdatingCell += new Janus.Web.GridEX.UpdatingCellEventHandler(this.grd_UpdatingCell);
			this.grdNitrogenTwo.UpdatingCell += new Janus.Web.GridEX.UpdatingCellEventHandler(this.grd_UpdatingCell);
			this.grdNitrogenThree.UpdatingCell += new Janus.Web.GridEX.UpdatingCellEventHandler(this.grd_UpdatingCell);
			this.grdIrrigationTwo.UpdatingCell += new Janus.Web.GridEX.UpdatingCellEventHandler(this.grd_UpdatingCell);
			this.grdIrrigationThree.UpdatingCell += new Janus.Web.GridEX.UpdatingCellEventHandler(this.grd_UpdatingCell);
			// 
			// dsNitrogen
			// 
			this.dsNitrogen.DataSetName = "NewDataSet";
			this.dsNitrogen.Locale = new System.Globalization.CultureInfo("en-AU");
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
			this.dcID.Caption = "ID";
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
																																				this.dcAmount,
																																				this.dcIrrigationEfficency});
			this.dtIrrigation.TableName = "Irrigation";
			// 
			// dcIrrigationID
			// 
			this.dcIrrigationID.ColumnName = "ID";
			// 
			// dcIrrigationDate
			// 
			this.dcIrrigationDate.Caption = "Date";
			this.dcIrrigationDate.ColumnName = "Date";
			this.dcIrrigationDate.DataType = typeof(System.DateTime);
			// 
			// dcAmount
			// 
			this.dcAmount.ColumnName = "Amount";
			// 
			// dcIrrigationEfficency
			// 
			this.dcIrrigationEfficency.ColumnName = "Efficency";
			this.Load += new System.EventHandler(this.Page_Load);
			((System.ComponentModel.ISupportInitialize)(this.dsNitrogen)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dtNitrogen)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dsIrrigation)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dtIrrigation)).EndInit();

		}
		#endregion



		#region Form Functions
		//-------------------------------------------------------------------------
		//Stores the report type selection from the previous page in view state
		//variables.
		//-------------------------------------------------------------------------
		private void StoreReportSelection()
		{
			try
			{
				wfEditPaddock EditPaddock = (wfEditPaddock) Context.Handler;
				ViewState["ReportType"] = EditPaddock.ReturnReportType();
				ViewState["EmailConParFiles"] = EditPaddock.ReturnEmailConParFiles();
				edtReportName.Text = Session["SelectedPaddockName"].ToString() +" "+ ViewState["ReportType"].ToString();
			}
			catch(Exception)
			{}
		}	
		//-------------------------------------------------------------------------
		//Intitialise the grids to contain 5 blank rows
		//-------------------------------------------------------------------------
		private void InitialiseGrids()
		{
			DataRow drNitrogen;
			int iMaxNumberOfRows = 5;
			for(int iIndex = dsNitrogen.Tables["Nitrogen"].Rows.Count; iIndex < iMaxNumberOfRows; iIndex++)
			{
				drNitrogen = dsNitrogen.Tables["Nitrogen"].NewRow();
				drNitrogen["ID"] = 0;	
				dsNitrogen.Tables["Nitrogen"].Rows.Add(drNitrogen);
			}
			DataRow drIrrigation;
			iMaxNumberOfRows = 10;
			for(int iIndex = dsIrrigation.Tables["Irrigation"].Rows.Count; iIndex < iMaxNumberOfRows; iIndex++)
			{
				drIrrigation = dsIrrigation.Tables["Irrigation"].NewRow();
				drIrrigation["ID"] = 0;	
				dsIrrigation.Tables["Irrigation"].Rows.Add(drIrrigation);
			}
			this.DataBind();
		}
		//-------------------------------------------------------------------------
		//A report is generated and sent to the apsim run machine
		//-------------------------------------------------------------------------
		private void GenerateReport()
		{
			try
			{
				//Check that their is a report name
				if(edtReportName.Text != "")
				{
					//Check that the name of the report won't cause a problem when it is stored in the file system
					if(InputValidationClass.IsInputAValidFileLocationString(edtReportName.Text) == true)
					{
						DataTable dtPaddockDetails = 
							DataAccessClass.GetDetailsOfPaddock(Session["SelectedPaddockName"].ToString(), 
							FunctionsClass.GetActiveUserName());
						if(dtPaddockDetails.Rows.Count > 0)
						{
							string szCropType = dtPaddockDetails.Rows[0]["CropType"].ToString();
							//Generate a data table that stores the values particular to the nitrogen report
							DataTable dtOtherValues = 
								ReportClass.CreateIrrigationComparisonOtherValues(ReturnNitrogenDataSet(), 
									ReturnIrrigationDataSet());
							//Generate the files needed to generate a report and then email these files to the ApsimRun machine
							if(EmailClass.SendReportEmail(edtReportName.Text, szCropType, 
								ViewState["ReportType"].ToString(), (bool)ViewState["EmailConParFiles"], dtOtherValues) == true)
							{
								Server.Transfer("wfReportGenerated.aspx");
							}
							else
								throw new Exception("Error requesting report");
						}
						else
							throw new Exception("Can not access crop type");
					}
					else
						throw new Exception("Report Description contains invalid characters. Please remove any of the following characters \\\\ / : * \" ? \\' # < > |");
				}
				else
					throw new Exception("Please enter a report name");
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}
		}
		//-------------------------------------------------------------------------
		//Returns a dataset holding all the nitrogen scenarios as tables
		//-------------------------------------------------------------------------
		private DataSet ReturnNitrogenDataSet()
		{
			DataSet dsNitrogenScenarios = dsNitrogen.Copy();
			dsNitrogenScenarios.Tables.Clear();
			dsNitrogenScenarios.Tables.Add(ReturnNitrogenDataTable(grdNitrogenOne, "ScenarioOne"));
			dsNitrogenScenarios.Tables.Add(ReturnNitrogenDataTable(grdNitrogenTwo, "ScenarioTwo"));
			dsNitrogenScenarios.Tables.Add(ReturnNitrogenDataTable(grdNitrogenThree, "ScenarioThree"));
			return dsNitrogenScenarios;
		}
		//-------------------------------------------------------------------------
		//Returns the data from the selected nitrogen grid in the form of a datatable
		//-------------------------------------------------------------------------
		private DataTable ReturnNitrogenDataTable(Janus.Web.GridEX.GridEX grdSelectedGrid, string szTableName)
			{
			DataTable dtNitrogenScenario = dsNitrogen.Tables["Nitrogen"].Copy();
			dtNitrogenScenario.TableName = szTableName;
			DataRow drNitrogen;
			try
				{
				Janus.Web.GridEX.GridEXRow grdRow;
				for(int iIndex = 0; iIndex < grdSelectedGrid.RowCount; iIndex++)
					{	
					grdRow = grdSelectedGrid.GetRow(iIndex);
					//If there is data in the dataTable then save it to the database
					if(grdRow.Cells["ApplicationDate"].Value != null && grdRow.Cells["Rate"].Value != null)
						{		
						drNitrogen = dtNitrogenScenario.NewRow();
						drNitrogen["ApplicationDate"] = grdRow.Cells["ApplicationDate"].Text;
						drNitrogen["Rate"] = grdRow.Cells["Rate"].Text;
						dtNitrogenScenario.Rows.Add(drNitrogen);
						}
					}
				}
			catch(Exception)
				{
				FunctionsClass.DisplayMessage(Page, "Error storing nitrogen data");
				}
			return dtNitrogenScenario;
			}
		//-------------------------------------------------------------------------
		//Returns a dataset holding all the nitrogen scenarios as tables
		//-------------------------------------------------------------------------
		private DataSet ReturnIrrigationDataSet()
		{
			DataSet dsIrrigationScenarios = dsIrrigation.Copy();
			dsIrrigationScenarios.Tables.Clear();
			dsIrrigationScenarios.Tables.Add(ReturnIrrigationDataTable(grdIrrigationOne, "ScenarioOne"));
			dsIrrigationScenarios.Tables.Add(ReturnIrrigationDataTable(grdIrrigationTwo, "ScenarioTwo"));
			dsIrrigationScenarios.Tables.Add(ReturnIrrigationDataTable(grdIrrigationThree, "ScenarioThree"));
			return dsIrrigationScenarios;
		}
		//-------------------------------------------------------------------------
		//Returns the data from the selected irrigation grid in the form of a datatable
		//-------------------------------------------------------------------------
		private DataTable ReturnIrrigationDataTable(Janus.Web.GridEX.GridEX grdSelectedGrid,  string szTableName)
			{
			DataTable dtIrrigationScenario = dsIrrigation.Tables["Irrigation"].Copy();
			dtIrrigationScenario.TableName = szTableName;
			DataRow drIrrigation;
			try
				{
				Janus.Web.GridEX.GridEXRow grdRow;
				for(int iIndex = 0; iIndex < grdSelectedGrid.RowCount; iIndex++)
					{	
					grdRow = grdSelectedGrid.GetRow(iIndex);
					//If there is data in the dataTable then save it to the database
					if(grdRow.Cells["Date"].Value != null && grdRow.Cells["Amount"].Value != null &&
						grdRow.Cells["Efficency"].Value != null)
						{		
						drIrrigation = dtIrrigationScenario.NewRow();
						drIrrigation["Date"] = grdRow.Cells["Date"].Text;
						drIrrigation["Amount"] = grdRow.Cells["Amount"].Text;
						drIrrigation["Efficency"] = grdRow.Cells["Efficency"].Text;
						dtIrrigationScenario.Rows.Add(drIrrigation);
						}
					}
				}
			catch(Exception)
				{
				FunctionsClass.DisplayMessage(Page, "Error storing irrigation data");
				}
			return dtIrrigationScenario;
			}
		//-------------------------------------------------------------------------
		#endregion



		#region Form Events
		//-------------------------------------------------------------------------
		//Sets the page up and stores the values need to generate the report
		//-------------------------------------------------------------------------
		private void Page_Load(object sender, System.EventArgs e)
		{
			if (!IsPostBack)
			{	
				//View state is used to store values over post back events
				ViewState["ReportTypeID"] = "0";
				ViewState["ReportType"] = "";
				ViewState["EmailConParFiles"] = false;
				FunctionsClass.CheckSession();
				FunctionsClass.CheckForGrowerLevelPriviledges();
				InitialiseGrids();
				StoreReportSelection();

				FunctionsClass.SetControlFocus("edtReportName", this);
				btnSave.Style.Add("cursor", "hand");
			}
		}
		//-------------------------------------------------------------------------
		//When the user presses the cancel button they are transfered back to the 
		//edit paddock page
		//-------------------------------------------------------------------------
		private void btnCancel_Click(object sender, System.EventArgs e)
		{
			Server.Transfer("wfEditPaddock.aspx");
		}
		//-------------------------------------------------------------------------
		//When the user presses the cancel image they are transfered back to the 
		//edit paddock page
		//-------------------------------------------------------------------------
		private void btnCancelImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
		{
			Server.Transfer("wfEditPaddock.aspx");
		}
		//-------------------------------------------------------------------------
		//When the user presses the save button the report is generated
		//-------------------------------------------------------------------------
		private void btnSave_Click(object sender, System.EventArgs e)
		{
			GenerateReport();
		}
		//-------------------------------------------------------------------------
		//When the user presses the save image the report is generated
		//-------------------------------------------------------------------------
		private void btnSaveImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
		{
			GenerateReport();
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

		//-------------------------------------------------------------------------
	}//END CLASS
}//END NAMESPACE