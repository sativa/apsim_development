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
using Xceed.Chart.Core;
using Xceed.Chart.Server;
using CSGeneral;

namespace YieldProphet
	{
	/// <summary>
	/// Summary description for wfEditSetupPaddock.
	/// </summary>
	public class wfEditSetupPaddock : System.Web.UI.Page
		{
		protected System.Web.UI.WebControls.ImageButton btnSaveImg;
		protected System.Web.UI.WebControls.ImageButton btnCancelImg;
		protected System.Web.UI.WebControls.LinkButton btnCancel;
		protected System.Web.UI.WebControls.Label lblName;
		protected System.Web.UI.WebControls.Label lblPaddockSetup;
		protected System.Web.UI.WebControls.Label lbRegion;
		protected System.Web.UI.WebControls.Label lblWeatherStation;
		protected System.Web.UI.WebControls.Label lblSoilType;
		protected System.Web.UI.WebControls.Label lblSubSoil;
		protected System.Web.UI.WebControls.Label lblInitialConditions;
		protected System.Web.UI.WebControls.DropDownList cboSubSoil;
		protected System.Web.UI.WebControls.DropDownList cboSoilType;
		protected System.Web.UI.WebControls.DropDownList cboWeatherStation;
		protected System.Web.UI.WebControls.DropDownList cboRegion;
		protected System.Data.DataColumn dcDepth;
		protected System.Data.DataColumn dcWater;
		protected System.Data.DataColumn dcNO3;
		protected System.Data.DataColumn dcNH4;
		protected System.Web.UI.WebControls.Label lblDepthOne;
		protected System.Web.UI.WebControls.Label lblDepthTwo;
		protected System.Web.UI.WebControls.Button btnSave;
		protected System.Web.UI.WebControls.CheckBox chkLinkedRainfall;
		protected System.Web.UI.WebControls.DropDownList cboLinkedRainfall;
		protected System.Data.DataSet dsSoilSampleOne;
		protected System.Data.DataTable dtSoilSampleOne;
		protected System.Data.DataSet dsSoilSampleTwo;
		protected System.Data.DataTable dtSoilSampleTwo;
		protected System.Data.DataColumn dataColumn1;
		protected System.Data.DataColumn dcOC;
		protected System.Data.DataColumn dcEC;
		protected System.Data.DataColumn dcPH;
		protected System.Data.DataColumn dcESP;
		protected Janus.Web.GridEX.GridEX grdSoilSampleTwo;
		protected Janus.Web.GridEX.GridEX grdSoilSampleOne;
		protected System.Data.DataSet dsInitialDate;
		protected System.Data.DataTable dtInitialDate;
		protected System.Data.DataColumn dcInitialDate;
		protected Janus.Web.GridEX.GridEX grdSampleDate;
		protected System.Web.UI.WebControls.Label lblStartGrowingSeason;
		protected System.Data.DataSet dsStartOfGrowingSeason;
		protected System.Data.DataTable dtStartOfGrowingSeason;
		protected System.Data.DataColumn dcGrowingSeasonDate;
		protected Janus.Web.GridEX.GridEX grdStartOfGrowingSeason;
		protected Xceed.Chart.Server.ChartServerControl cscSoilChart;
		protected System.Web.UI.WebControls.Button btnUpdateGraph;
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
			this.dsSoilSampleOne = new System.Data.DataSet();
			this.dtSoilSampleOne = new System.Data.DataTable();
			this.dcDepth = new System.Data.DataColumn();
			this.dcWater = new System.Data.DataColumn();
			this.dcNO3 = new System.Data.DataColumn();
			this.dcNH4 = new System.Data.DataColumn();
			this.dsSoilSampleTwo = new System.Data.DataSet();
			this.dtSoilSampleTwo = new System.Data.DataTable();
			this.dataColumn1 = new System.Data.DataColumn();
			this.dcOC = new System.Data.DataColumn();
			this.dcEC = new System.Data.DataColumn();
			this.dcPH = new System.Data.DataColumn();
			this.dcESP = new System.Data.DataColumn();
			this.dsInitialDate = new System.Data.DataSet();
			this.dtInitialDate = new System.Data.DataTable();
			this.dcInitialDate = new System.Data.DataColumn();
			this.dsStartOfGrowingSeason = new System.Data.DataSet();
			this.dtStartOfGrowingSeason = new System.Data.DataTable();
			this.dcGrowingSeasonDate = new System.Data.DataColumn();
			((System.ComponentModel.ISupportInitialize)(this.dsSoilSampleOne)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dtSoilSampleOne)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dsSoilSampleTwo)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dtSoilSampleTwo)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dsInitialDate)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dtInitialDate)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dsStartOfGrowingSeason)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dtStartOfGrowingSeason)).BeginInit();
			this.btnCancel.Click += new System.EventHandler(this.btnCancel_Click);
			this.btnSave.Click += new System.EventHandler(this.btnSave_Click);
			this.btnCancelImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnCancelImg_Click);
			this.btnSaveImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnSaveImg_Click);
			this.cboRegion.SelectedIndexChanged += new System.EventHandler(this.cboRegion_SelectedIndexChanged);
			this.grdSoilSampleOne.UpdatingCell += new Janus.Web.GridEX.UpdatingCellEventHandler(this.grdSoilSample_UpdatingCell);
			this.chkLinkedRainfall.CheckedChanged += new System.EventHandler(this.chkLinkedRainfall_CheckedChanged);
			this.grdSoilSampleTwo.UpdatingCell += new Janus.Web.GridEX.UpdatingCellEventHandler(this.grdSoilSample_UpdatingCell);
			this.btnUpdateGraph.Click += new System.EventHandler(this.btnUpdateGraph_Click);
			// 
			// dsSoilSampleOne
			// 
			this.dsSoilSampleOne.DataSetName = "NewDataSet";
			this.dsSoilSampleOne.Locale = new System.Globalization.CultureInfo("en-US");
			this.dsSoilSampleOne.Tables.AddRange(new System.Data.DataTable[] {
																																				 this.dtSoilSampleOne});
			// 
			// dtSoilSampleOne
			// 
			this.dtSoilSampleOne.Columns.AddRange(new System.Data.DataColumn[] {
																																					 this.dcDepth,
																																					 this.dcWater,
																																					 this.dcNO3,
																																					 this.dcNH4});
			this.dtSoilSampleOne.TableName = "SoilSampleOne";
			// 
			// dcDepth
			// 
			this.dcDepth.Caption = "Depth";
			this.dcDepth.ColumnName = "Depth";
			// 
			// dcWater
			// 
			this.dcWater.Caption = "Starting Water";
			this.dcWater.ColumnName = "Water";
			// 
			// dcNO3
			// 
			this.dcNO3.Caption = "Starting NO3";
			this.dcNO3.ColumnName = "NO3";
			// 
			// dcNH4
			// 
			this.dcNH4.Caption = "Starting NH4";
			this.dcNH4.ColumnName = "NH4";
			// 
			// dsSoilSampleTwo
			// 
			this.dsSoilSampleTwo.DataSetName = "NewDataSet";
			this.dsSoilSampleTwo.Locale = new System.Globalization.CultureInfo("en-US");
			this.dsSoilSampleTwo.Tables.AddRange(new System.Data.DataTable[] {
																																				 this.dtSoilSampleTwo});
			// 
			// dtSoilSampleTwo
			// 
			this.dtSoilSampleTwo.Columns.AddRange(new System.Data.DataColumn[] {
																																					 this.dataColumn1,
																																					 this.dcOC,
																																					 this.dcEC,
																																					 this.dcPH,
																																					 this.dcESP});
			this.dtSoilSampleTwo.TableName = "SoilSampleTwo";
			// 
			// dataColumn1
			// 
			this.dataColumn1.Caption = "Depth";
			this.dataColumn1.ColumnName = "Depth";
			// 
			// dcOC
			// 
			this.dcOC.Caption = "OC";
			this.dcOC.ColumnName = "OC";
			// 
			// dcEC
			// 
			this.dcEC.Caption = "EC";
			this.dcEC.ColumnName = "EC";
			// 
			// dcPH
			// 
			this.dcPH.Caption = "PH";
			this.dcPH.ColumnName = "PH";
			// 
			// dcESP
			// 
			this.dcESP.Caption = "ESP";
			this.dcESP.ColumnName = "ESP";
			// 
			// dsInitialDate
			// 
			this.dsInitialDate.DataSetName = "NewDataSet";
			this.dsInitialDate.Locale = new System.Globalization.CultureInfo("en-AU");
			this.dsInitialDate.Tables.AddRange(new System.Data.DataTable[] {
																																			 this.dtInitialDate});
			// 
			// dtInitialDate
			// 
			this.dtInitialDate.Columns.AddRange(new System.Data.DataColumn[] {
																																				 this.dcInitialDate});
			this.dtInitialDate.TableName = "InitialDate";
			// 
			// dcInitialDate
			// 
			this.dcInitialDate.ColumnName = "InitialDate";
			this.dcInitialDate.DataType = typeof(System.DateTime);
			// 
			// dsStartOfGrowingSeason
			// 
			this.dsStartOfGrowingSeason.DataSetName = "NewDataSet";
			this.dsStartOfGrowingSeason.Locale = new System.Globalization.CultureInfo("en-AU");
			this.dsStartOfGrowingSeason.Tables.AddRange(new System.Data.DataTable[] {
																																								this.dtStartOfGrowingSeason});
			// 
			// dtStartOfGrowingSeason
			// 
			this.dtStartOfGrowingSeason.Columns.AddRange(new System.Data.DataColumn[] {
																																									this.dcGrowingSeasonDate});
			this.dtStartOfGrowingSeason.TableName = "StartOfGrowingSeason";
			// 
			// dcGrowingSeasonDate
			// 
			this.dcGrowingSeasonDate.ColumnName = "GrowingSeasonDate";
			this.dcGrowingSeasonDate.DataType = typeof(System.DateTime);
			this.Load += new System.EventHandler(this.Page_Load);
			((System.ComponentModel.ISupportInitialize)(this.dsSoilSampleOne)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dtSoilSampleOne)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dsSoilSampleTwo)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dtSoilSampleTwo)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dsInitialDate)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dtInitialDate)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dsStartOfGrowingSeason)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dtStartOfGrowingSeason)).EndInit();

		}
		#endregion



		#region Form Functions
		//-------------------------------------------------------------------------
		//Fills the form with the selected paddock's data from the database
		//There is a bug with dependant combo boxes, so time needs to be allowed
		//between the filling of the master combo box (regions) and the dependant
		//combo boxes (soils and met stations)
		//-------------------------------------------------------------------------
		private void FillNonDependantFormControls()
			{
			//Fills the regions combo box 
			FillRegionCombo();
			DisplayGrowersName();
			//Fills the linked rainfall combo box
			FillLinkedRainfallCombo();
			//Fills the sub soil combo box 
			FillSubSoilCombo();
			//Fills the soil sample one grid
			FillSoilSampleOneGrid();
			//Fills the soil sample two grid
			FillSoilSampleTwoGrid();
			//If ther is a soil sample record set the initial condition date
			try
				{
				DataTable dtSoilSample = DataAccessClass.GetPaddocksSoilSample("GridOne", 
					Session["SelectedPaddockName"].ToString(), FunctionsClass.GetActiveUserName());
				string szInitialDate = "";
				if(dtSoilSample.Rows.Count > 0)
					{
					szInitialDate = dtSoilSample.Rows[0]["SampleDate"].ToString();
					}
				SetInitialDate(szInitialDate);
				}
			catch(Exception E)
				{
				FunctionsClass.DisplayMessage(Page, E.Message);
				}
			}
		//-------------------------------------------------------------------------
		//Fills the form with the selected paddock's data from the database
		//-------------------------------------------------------------------------
		private void FillDependantFormControls()
			{
			try
				{
				DataTable dtPaddockDetails = 
					DataAccessClass.GetDetailsOfPaddock(Session["SelectedPaddockName"].ToString(), 
					FunctionsClass.GetActiveUserName());
				//if a region has be previously saved it is set as the selected metstation.
				cboRegion.SelectedValue = dtPaddockDetails.Rows[0]["RegionType"].ToString();
			

				//Fills the met stations combo box
				FillMetStationCombo();
				//Fills the soil types combo box
				FillSoilTypeCombo();		

				SetStartOfGrowingSeasonDate(dtPaddockDetails.Rows[0]["StartOfGrowingSeasonDate"].ToString());

				if(cboLinkedRainfall.Items.Count > 0)
					{
					//if a region has be previously saved it is set as the selected metstation.
					if(dtPaddockDetails.Rows[0]["LinkedRainfallPaddockName"].ToString() != "")
						{
						cboLinkedRainfall.SelectedItem.Text = dtPaddockDetails.Rows[0]["LinkedRainfallPaddockName"].ToString();
						cboLinkedRainfall.Enabled = true;
						chkLinkedRainfall.Checked = true;
						}
					else
						{
						cboLinkedRainfall.Enabled = false;
						chkLinkedRainfall.Checked = false;
						}
					}
				else
					{
					chkLinkedRainfall.Enabled = false;
					cboLinkedRainfall.Enabled = false;
					}
				//Set user's selections
				cboRegion.SelectedValue = dtPaddockDetails.Rows[0]["RegionType"].ToString();		
				cboSubSoil.SelectedValue = dtPaddockDetails.Rows[0]["SubSoilConstraintType"].ToString();		
				cboWeatherStation.SelectedValue = dtPaddockDetails.Rows[0]["MetStationName"].ToString();
				cboSoilType.SelectedValue = dtPaddockDetails.Rows[0]["SoilName"].ToString();
				}
			catch(Exception E)
				{
				FunctionsClass.DisplayMessage(Page, E.Message);
				}
			}
		//-------------------------------------------------------------------------
		//Set the date shown in the initial conditions date grid
		//-------------------------------------------------------------------------
		private void SetInitialDate(string szInitialDate)
			{
			DataRow drInitialDate;
			if(szInitialDate != null && szInitialDate != "")
				{
				drInitialDate = dsInitialDate.Tables["InitialDate"].NewRow();
				drInitialDate["InitialDate"] = DateTime.ParseExact(szInitialDate, "yyyy-MM-dd", null);
				dsInitialDate.Tables["InitialDate"].Rows.Add(drInitialDate);
				}
			else
				{
				drInitialDate = dsInitialDate.Tables["InitialDate"].NewRow();
				drInitialDate["InitialDate"] = DateTime.Today;
				dsInitialDate.Tables["InitialDate"].Rows.Add(drInitialDate);
				}
			this.DataBind();
			}
		//-------------------------------------------------------------------------
		//Set the date shown in the strart of growing season date grid
		//-------------------------------------------------------------------------
		private void SetStartOfGrowingSeasonDate(string szStartOfGrowingSeason)
			{
			DataRow drStartOfGrowingSeason;
			if(szStartOfGrowingSeason != null && szStartOfGrowingSeason != "")
				{
				drStartOfGrowingSeason = dsStartOfGrowingSeason.Tables["StartOfGrowingSeason"].NewRow();
				drStartOfGrowingSeason["GrowingSeasonDate"] = DateTime.ParseExact(szStartOfGrowingSeason, "yyyy-MM-dd", null);
				dsStartOfGrowingSeason.Tables["StartOfGrowingSeason"].Rows.Add(drStartOfGrowingSeason);
				}
			else
				{
				drStartOfGrowingSeason = dsStartOfGrowingSeason.Tables["StartOfGrowingSeason"].NewRow();
				drStartOfGrowingSeason["GrowingSeasonDate"] = new DateTime(DateTime.Today.Year, 4, 1);
				dsStartOfGrowingSeason.Tables["StartOfGrowingSeason"].Rows.Add(drStartOfGrowingSeason);
				}
			this.DataBind();
		}
		//-------------------------------------------------------------------------
		//Displays the grower's name and the paddock's name on a label
		//-------------------------------------------------------------------------
		private void DisplayGrowersName()
			{
			try
				{
				DataTable dtUsersDetails = DataAccessClass.GetDetailsOfUser(FunctionsClass.GetActiveUserName());
				lblName.Text = dtUsersDetails.Rows[0]["Name"].ToString()+ 
					" and paddock:  "+Session["SelectedPaddockName"].ToString();
				}
			catch(Exception E)
				{
				FunctionsClass.DisplayMessage(Page, E.Message);
				}
			}
		//-------------------------------------------------------------------------
		//Fills the regions combo box with all the regions from the database
		//-------------------------------------------------------------------------
		private void FillRegionCombo()
			{
			try
				{
				DataTable dtRegions = DataAccessClass.GetAllRegions();
				cboRegion.DataSource = dtRegions;
				cboRegion.DataTextField = "Type";
				cboRegion.DataValueField = "Type";
				cboRegion.DataBind();
				}
			catch(Exception E)
				{
				FunctionsClass.DisplayMessage(Page, E.Message);
				}
			}
		//-------------------------------------------------------------------------
		//Fills the sub soil combo box with all the sub soils types form the database
		//-------------------------------------------------------------------------
		private void FillSubSoilCombo()
			{
			try
				{
				DataTable dtSubSoilConstraints = DataAccessClass.GetAllSubSoilConstraintTypes();
				cboSubSoil.DataSource = dtSubSoilConstraints;
				cboSubSoil.DataTextField = "Type";
				cboSubSoil.DataValueField = "Type";
				cboSubSoil.DataBind();
				}
			catch(Exception E)
				{
				FunctionsClass.DisplayMessage(Page, E.Message);
				}
			}
		//-------------------------------------------------------------------------
		//Fills the met station combo box with all the met stations linked to the
		//selected region
		//-------------------------------------------------------------------------
		private void FillMetStationCombo()
			{
			//If a region is selected then fill the combo box
			if(cboRegion.SelectedValue != null && cboRegion.SelectedValue != "")
				{
				try
					{
					string szSelectedRegionID = cboRegion.SelectedValue.ToString(); 
					DataTable dtMetStations = DataAccessClass.GetMetStationsOfRegion(szSelectedRegionID);
					cboWeatherStation.DataSource = dtMetStations;
					cboWeatherStation.DataTextField = "Name";
					cboWeatherStation.DataValueField = "Name";
					cboWeatherStation.DataBind();
					}
				catch(Exception E)
					{
					FunctionsClass.DisplayMessage(Page, E.Message);
					}
				}
			//If no region is selected then display and error to the user
			else
				{
				FunctionsClass.DisplayMessage(Page, "Please select a region");
				}
			}
		//-------------------------------------------------------------------------
		//Fills the soil type combo box with all the soil types linked to the
		//selected region and to the current grower
		//-------------------------------------------------------------------------
		private void FillSoilTypeCombo()
			{
			//If a region is selected then fill the combo box
			if(cboRegion.SelectedValue != null && cboRegion.SelectedValue != "")
				{
				try
					{
					DataTable dtSoils = DataAccessClass.GetSoilsOfRegion(cboRegion.SelectedItem.Text);
					DataTable dtUsersDetails = DataAccessClass.GetDetailsOfUser(FunctionsClass.GetActiveUserName());
					foreach (DataRow drSoil in dtSoils.Rows)
						{
						if(drSoil["Name"].ToString().IndexOf("Grower soil:") != -1)
							{
							if(drSoil["Name"].ToString().IndexOf("Grower soil:"+dtUsersDetails.Rows[0]["Name"].ToString()) == -1)
								{
								drSoil.Delete();
								}
							}
						}
					cboSoilType.DataSource = dtSoils;
					cboSoilType.DataTextField = "Name";
					cboSoilType.DataValueField = "Name";
					cboSoilType.DataBind();
					}
				catch(Exception E)
					{
					FunctionsClass.DisplayMessage(Page, E.Message);
					}
				}
			//If no region is selected then display an error the user
			else
				{
				FunctionsClass.DisplayMessage(Page, "Please select a region");
				}
			}
		//-------------------------------------------------------------------------
		//Fills the linked rainfall combo box with all the paddocks from the database
		//-------------------------------------------------------------------------
		private void FillLinkedRainfallCombo()
			{
			try
				{
				DataTable dtPaddocks = 
					DataAccessClass.GetAllPaddocksForTemporalLinking(Session["SelectedPaddockName"].ToString(), 
					FunctionsClass.GetActiveUserName());
				cboLinkedRainfall.DataSource = dtPaddocks;
				cboLinkedRainfall.DataTextField = "Name";
				cboLinkedRainfall.DataValueField = "Name";
				cboLinkedRainfall.DataBind();
				}
			catch(Exception E)
				{
				FunctionsClass.DisplayMessage(Page, E.Message);
				}
			}
		//-------------------------------------------------------------------------
		//Fills the first soil sample grid with data from the database
		//-------------------------------------------------------------------------	
		private void FillSoilSampleOneGrid()
			{
			try
				{
				//Gets the data and converts it from xml format to a datatable
				DataTable dtPaddocksSoilSameple =
					DataAccessClass.GetPaddocksSoilSample("GridOne", Session["SelectedPaddockName"].ToString(), 
					FunctionsClass.GetActiveUserName());
				DataTable dtSoilSampleFromDB = new DataTable();
				if(dtPaddocksSoilSameple.Rows.Count > 0)
				{
					dtSoilSampleFromDB = SoilSampleClass.ConvertSoilSampleOneDataToDataTable(dtPaddocksSoilSameple.Rows[0]["Data"].ToString());
				}
				
				DataRow drSoilSampleLevel;
				int iMaxNumberOfRows = 8;
				//Takes the data from the datatable and assigns it to the grid
				foreach(DataRow drSoilSampleLevelFromDB in dtSoilSampleFromDB.Rows)
					{
					drSoilSampleLevel = dsSoilSampleOne.Tables["SoilSampleOne"].NewRow();
					drSoilSampleLevel["Depth"] = drSoilSampleLevelFromDB["Depth"];	
					drSoilSampleLevel["Water"] = drSoilSampleLevelFromDB["Water"];
					drSoilSampleLevel["NO3"] = drSoilSampleLevelFromDB["NO3"];
					drSoilSampleLevel["NH4"] = drSoilSampleLevelFromDB["NH4"];	
					dsSoilSampleOne.Tables["SoilSampleOne"].Rows.Add(drSoilSampleLevel);
					}
				//Appends any need blank records so that the grid displays the correct number of rows
				for(int iIndex = dsSoilSampleOne.Tables["SoilSampleOne"].Rows.Count; iIndex < iMaxNumberOfRows; iIndex++)
					{
					drSoilSampleLevel = dsSoilSampleOne.Tables["SoilSampleOne"].NewRow();
					dsSoilSampleOne.Tables["SoilSampleOne"].Rows.Add(drSoilSampleLevel);
					}
				this.DataBind();
				}
			catch(Exception E)
				{
				FunctionsClass.DisplayMessage(Page, E.Message);
				}
			}
		//-------------------------------------------------------------------------
		//Fills the second soil sample grid with data from the database
		//-------------------------------------------------------------------------
		private void FillSoilSampleTwoGrid()
			{
			try
				{
				//Gets the data and converts it from xml format to a datatable
				DataTable dtPaddocksSoilSameple =
					DataAccessClass.GetPaddocksSoilSample("GridTwo", Session["SelectedPaddockName"].ToString(), 
					FunctionsClass.GetActiveUserName());

				DataTable dtSoilSampleFromDB = new DataTable();
				if(dtPaddocksSoilSameple.Rows.Count > 0)
				{
					dtSoilSampleFromDB = SoilSampleClass.ConvertSoilSampleTwoDataToDataTable(dtPaddocksSoilSameple.Rows[0]["Data"].ToString());
				}
				
				DataRow drSoilSampleLevel;
				int iMaxNumberOfRows = 8;
				//Takes the data from the datatable and assigns it to the grid
				foreach(DataRow drSoilSampleLevelFromDB in dtSoilSampleFromDB.Rows)
					{
					drSoilSampleLevel = dsSoilSampleTwo.Tables["SoilSampleTwo"].NewRow();
					drSoilSampleLevel["Depth"] = drSoilSampleLevelFromDB["Depth"];	
					drSoilSampleLevel["OC"] = drSoilSampleLevelFromDB["OC"];
					drSoilSampleLevel["EC"] = drSoilSampleLevelFromDB["EC"];	
					drSoilSampleLevel["PH"] = drSoilSampleLevelFromDB["PH"];
					drSoilSampleLevel["ESP"] = drSoilSampleLevelFromDB["ESP"];
					dsSoilSampleTwo.Tables["SoilSampleTwo"].Rows.Add(drSoilSampleLevel);
					}
				//Appends any need blank records so that the grid displays the correct number of rows
				for(int iIndex = dsSoilSampleTwo.Tables["SoilSampleTwo"].Rows.Count; iIndex < iMaxNumberOfRows; iIndex++)
					{
					drSoilSampleLevel = dsSoilSampleTwo.Tables["SoilSampleTwo"].NewRow();
					dsSoilSampleTwo.Tables["SoilSampleTwo"].Rows.Add(drSoilSampleLevel);
					}
				this.DataBind();
				}
			catch(Exception E)
				{
				FunctionsClass.DisplayMessage(Page, E.Message);
				}
			}
		//-------------------------------------------------------------------------
		//The paddocks settings are updated, but firstly a check is run to ensure that a
		//metstation and a soil type have been selected.  If this check is passed
		//then the selected paddock's settings are updated in the database
		//-------------------------------------------------------------------------
		private void SavePaddockSetup()
			{
			if(FunctionsClass.IsGrowerOrHigher(Session["UserName"].ToString()) == true)
				{
				//If a metstation and soil type are selected then save the record
				if(cboWeatherStation.SelectedValue != "" && cboSoilType.SelectedValue != "" && 
					cboWeatherStation.SelectedValue != "None" && cboSoilType.SelectedValue != "None" &&
					grdSampleDate.GetRow(0).Cells["InitialDate"].Text != "" &&
					grdStartOfGrowingSeason.GetRow(0).Cells["GrowingSeasonDate"].Text != "")
					{
					string szStartOfGrowingSeasonDate = 
						(DateTime.ParseExact(grdStartOfGrowingSeason.GetRow(0).Cells["GrowingSeasonDate"].Text, "dd/MM/yyyy", null)).ToString("yyyy-MM-dd");
					string szLinkedTemporalPaddock = "";
					if(chkLinkedRainfall.Checked == true)
						{
						szLinkedTemporalPaddock = cboLinkedRainfall.SelectedItem.Text;
						}
					try
						{
						DataAccessClass.UpdatePaddock("", "", cboWeatherStation.SelectedItem.Text,  
							cboSoilType.SelectedItem.Text, cboSubSoil.SelectedItem.Text,
							szLinkedTemporalPaddock, szStartOfGrowingSeasonDate, 
							Session["SelectedPaddockName"].ToString(), FunctionsClass.GetActiveUserName());
						SaveSoilSampleDetails();
						Server.Transfer("wfEditPaddock.aspx");
						}
					catch(Exception E)
						{
						FunctionsClass.DisplayMessage(Page, E.Message);
						}
					}
				//If either a metstation or a soil type are not selected display an 
				//error message to the user.
				else
					{
					FunctionsClass.DisplayMessage(Page, "Please ensure that all fields contain data");
					}
				}
			else
				{
				FunctionsClass.DisplayMessage(Page, "Functionality not available to visitors");
				}
			}
		//-------------------------------------------------------------------------
		//Saves the soil sample value from the grid
		//-------------------------------------------------------------------------
		private void SaveSoilSampleDetails()
			{
			try
				{
				//Saves the data from the first grid
				string szSoilSampleData = SoilSampleClass.CreateSoilSampleOneXmlFile(ReturnSoilSampleOneDataTable());
				string szSoilSampleDate = 
					(DateTime.ParseExact(grdSampleDate.GetRow(0).Cells["InitialDate"].Text, "dd/MM/yyyy", null)).ToString("yyyy-MM-dd");
				//If the paddock doesn't have any soil sample one data stored, we insert the
				//record into the table, it the paddock does have an existing soil sample one 
				//we update the record
				DataAccessClass.SetSoilSample(szSoilSampleDate, szSoilSampleData, "GridOne", 
					Session["SelectedPaddockName"].ToString(), FunctionsClass.GetActiveUserName());

				//Saves teh data from the second grid
				szSoilSampleData = SoilSampleClass.CreateSoilSampleTwoXmlFile(ReturnSoilSampleTwoDataTable());
				//If the paddock doesn't have any soil sample two data stored, we insert the
				//record into the table, it the paddock does have an existing soil sample two 
				//we update the record
				DataAccessClass.SetSoilSample(szSoilSampleDate, szSoilSampleData, "GridTwo", 
					Session["SelectedPaddockName"].ToString(), FunctionsClass.GetActiveUserName());
				}
			catch(Exception E)
				{
				FunctionsClass.DisplayMessage(Page, E.Message);
				}
			}
		//-------------------------------------------------------------------------
		//Returns the data from the first grid in the form of a datatable
		//-------------------------------------------------------------------------
		private DataTable ReturnSoilSampleOneDataTable()
			{
			DataTable dtSoilSampleData = dsSoilSampleOne.Tables["SoilSampleOne"].Copy();
			DataRow drSoilSampleData;
			try
				{
				Janus.Web.GridEX.GridEXRow grdRow;
				for(int iIndex = 0; iIndex < grdSoilSampleOne.RowCount; iIndex++)
					{
					grdRow = grdSoilSampleOne.GetRow(iIndex);
					//If there is data in the dataTable then save it to the database
					if(grdRow.Cells["Depth"].Value != null)
						{
						drSoilSampleData = dtSoilSampleData.NewRow();
						drSoilSampleData["Depth"] = grdRow.Cells["Depth"].Text;
						drSoilSampleData["Water"] = grdRow.Cells["Water"].Text;
						drSoilSampleData["NO3"] = grdRow.Cells["NO3"].Text;
						drSoilSampleData["NH4"] = grdRow.Cells["NH4"].Text;
						dtSoilSampleData.Rows.Add(drSoilSampleData);
						}
					}
				}
			catch(Exception)
				{
				FunctionsClass.DisplayMessage(Page, "Error storing soil sample one data");
				}
				return dtSoilSampleData;
			}
		//-------------------------------------------------------------------------
		//Returns the data from the second grid in the form of a datatable
		//-------------------------------------------------------------------------
		private DataTable ReturnSoilSampleTwoDataTable()
			{
			DataTable dtSoilSampleData = dsSoilSampleTwo.Tables["SoilSampleTwo"].Copy();
			DataRow drSoilSampleData;
			try
				{
				Janus.Web.GridEX.GridEXRow grdRow;
				for(int iIndex = 0; iIndex < grdSoilSampleTwo.RowCount; iIndex++)
					{
					grdRow = grdSoilSampleTwo.GetRow(iIndex);
					//If there is data in the dataTable then save it to the database
					if(grdRow.Cells["Depth"].Value != null)
						{
						drSoilSampleData = dtSoilSampleData.NewRow();
						drSoilSampleData["Depth"] = grdRow.Cells["Depth"].Text;
						drSoilSampleData["OC"] = grdRow.Cells["OC"].Text;
						drSoilSampleData["EC"] = grdRow.Cells["EC"].Text;
						drSoilSampleData["PH"] = grdRow.Cells["PH"].Text;
						drSoilSampleData["ESP"] = grdRow.Cells["ESP"].Text;
						dtSoilSampleData.Rows.Add(drSoilSampleData);
						}
					}
				}
			catch(Exception)
				{
				FunctionsClass.DisplayMessage(Page, "Error storing soil sample two data");
				}
			return dtSoilSampleData;
			}
		//-------------------------------------------------------------------------
		#endregion


			
			
		#region Chart Function
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void DisplaySoilChart(DataTable dtSoilData)
			{
			//Initialise the chart
			cscSoilChart.Visible = true;
			//cscSoilChart.Labels[0].Text = "Depth vs Water (%)";
			Chart chtCustomChart;
			chtCustomChart = cscSoilChart.Charts.GetAt( 0 );
			chtCustomChart.Series.Clear();
			
			//Initialise the SW series and fill with data from the passed in DataTable
			LineSeries bsSWLineSeries;
			bsSWLineSeries = ( LineSeries )chtCustomChart.Series.Add( SeriesType.Line );
			bsSWLineSeries.Name = "SW";
			bsSWLineSeries.DataLabels.Mode = DataLabelsMode.None;
			bsSWLineSeries.LineBorder.Color = Color.Blue;
			bsSWLineSeries.Values.FillFromDataTable(dtSoilData, "SW");
			//Initialise the series and fill with data from the passed in DataTable
			LineSeries bsDULLineSeries;
			bsDULLineSeries = ( LineSeries )chtCustomChart.Series.Add( SeriesType.Line );
			bsDULLineSeries.Name = "DUL";
			bsDULLineSeries.DataLabels.Mode = DataLabelsMode.None;
			bsDULLineSeries.LineBorder.Color = Color.Green;
			bsDULLineSeries.Values.FillFromDataTable(dtSoilData, "DUL");
			//Initialise the series and fill with data from the passed in DataTable
			LineSeries bsLL15LineSeries;
			bsLL15LineSeries = ( LineSeries )chtCustomChart.Series.Add( SeriesType.Line );
			bsLL15LineSeries.Name = "LL15";
			bsLL15LineSeries.DataLabels.Mode = DataLabelsMode.None;
			bsLL15LineSeries.LineBorder.Color = Color.Yellow;
			bsLL15LineSeries.Values.FillFromDataTable(dtSoilData, "LL15");
			//Initialise the series and fill with data from the passed in DataTable
			LineSeries bsAirDryLineSeries;
			bsAirDryLineSeries = ( LineSeries )chtCustomChart.Series.Add( SeriesType.Line );
			bsAirDryLineSeries.Name = "AirDry";
			bsAirDryLineSeries.DataLabels.Mode = DataLabelsMode.None;
			bsAirDryLineSeries.LineBorder.Color = Color.Red;
			bsAirDryLineSeries.Values.FillFromDataTable(dtSoilData, "AirDry");
			//Initialise the series and fill with data from the passed in DataTable
			LineSeries bsSATLineSeries;
			bsSATLineSeries = ( LineSeries )chtCustomChart.Series.Add( SeriesType.Line );
			bsSATLineSeries.Name = "SAT";
			bsSATLineSeries.DataLabels.Mode = DataLabelsMode.None;
			bsSATLineSeries.LineBorder.Color = Color.HotPink;
			bsSATLineSeries.Values.FillFromDataTable(dtSoilData, "SAT");

			//Configure primary Y-Axis to display title, etc
			Axis axsWater = chtCustomChart.Axis(0);
			axsWater.Title = "Water (%)";
			axsWater.TitleText.Orientation = 0;
			axsWater.TitleText.OffsetY = -25;
			axsWater.Text.OffsetY = -5;
			//Configure the primary (bottom) X-Axis to display the dates associated with the data.
			Axis axsDepth = chtCustomChart.Axis(2);
			axsDepth.Labels.Clear();
			axsDepth.TitleText.OffsetX = -35;
			axsDepth.TitleText.Orientation = 90;
			axsDepth.Title = "Depth (mm)";
			axsDepth.Text.OffsetX = -10; 
			axsDepth.NumericScale.AutoMax = true;
			//Assign the lables to the graph
			string szXAxisLabel = "";
			foreach(DataRow drSoilData in dtSoilData.Rows)
				{
				szXAxisLabel = drSoilData["Depth"].ToString();
				axsDepth.Labels.Add(szXAxisLabel);
				}
			}
		#endregion
		



		#region Form Events
		//-------------------------------------------------------------------------
		//If the page hasn't been viewed by the user then the user's
		//permissions are checked and the page is initialised
		//-------------------------------------------------------------------------
		private void Page_Load(object sender, System.EventArgs e)
			{
			if (!IsPostBack)
				{	
				FunctionsClass.CheckSession();
				FunctionsClass.CheckForVisitorLevelPriviledges();
				FillNonDependantFormControls();
				btnSave.Style.Add("cursor", "hand");
				FillDependantFormControls();
				DataTable dtPaddocksSoilSameple =
					DataAccessClass.GetPaddocksSoilSample("GridOne", Session["SelectedPaddockName"].ToString(), 
					FunctionsClass.GetActiveUserName());
				DataTable dtSoilSampleFromDB = new DataTable();
				if(dtPaddocksSoilSameple.Rows.Count > 0)
					{
					DisplaySoilChart(SoilSampleClass.GetChartData(dtPaddocksSoilSameple.Rows[0]["Data"].ToString(),cboSoilType.SelectedItem.Text));
					}
				else
				{
					DisplaySoilChart(SoilSampleClass.GetChartData("", cboSoilType.SelectedItem.Text));
				}
				}
			}
		//-------------------------------------------------------------------------
		//When the user selects a different region, the met stations combo and the
		//soil type combo are refreshed
		//-------------------------------------------------------------------------
		private void cboRegion_SelectedIndexChanged(object sender, System.EventArgs e)
			{
			FillMetStationCombo();
			FillSoilTypeCombo();
			}
		//-------------------------------------------------------------------------
		//When the user presses the save button the paddock settings are updated
		//-------------------------------------------------------------------------
		private void btnSave_Click(object sender, System.EventArgs e)
			{
			SavePaddockSetup();
			}
		//-------------------------------------------------------------------------
		//When the user presses the save button the paddock settings are updated
		//-------------------------------------------------------------------------
		private void btnSaveImg_Click(object sender, System.Web.UI.ImageClickEventArgs e)
			{
			SavePaddockSetup();
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
		//When the user changes the status of the linked rainfall check box
		//the linked rainfall combo box is enabled or disabled
		//-------------------------------------------------------------------------
		private void chkLinkedRainfall_CheckedChanged(object sender, System.EventArgs e)
			{
			cboLinkedRainfall.Enabled = chkLinkedRainfall.Checked;
			}
		//-------------------------------------------------------------------------
		//On the update of the grid cell, it runs a check to make sure that the 
		//values entered are valid
		//-------------------------------------------------------------------------
		private void grdSoilSample_UpdatingCell(object sender, Janus.Web.GridEX.UpdatingCellEventArgs e)
			{
			if(e.Column.Key == "Depth")
				{
				if(e.Value != null)
					{
					e.Value = InputValidationClass.ValidateDepthString(e.Value.ToString());
					}
				}
			else
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
		//
		//-------------------------------------------------------------------------
		private void btnUpdateGraph_Click(object sender, System.EventArgs e)
			{
			DisplaySoilChart(SoilSampleClass.GetChartData(SoilSampleClass.CreateSoilSampleOneXmlFile(ReturnSoilSampleOneDataTable()), cboSoilType.SelectedItem.Text));
			}
		//-------------------------------------------------------------------------
		#endregion



		//-------------------------------------------------------------------------
		}//END OF CLASS
	}//END OF NAMESPACE
