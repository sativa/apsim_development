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
		protected System.Web.UI.WebControls.Calendar cldInitialConditions;
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
		protected System.Web.UI.WebControls.Panel pnlTop;
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
				}
			}

		#region Web Form Designer generated code
		override protected void OnInit(EventArgs e)
		{
			//
			// CODEGEN: This call is required by the ASP.NET Web Form Designer.
			//
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
			((System.ComponentModel.ISupportInitialize)(this.dsSoilSampleOne)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dtSoilSampleOne)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dsSoilSampleTwo)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dtSoilSampleTwo)).BeginInit();
			this.btnCancel.Click += new System.EventHandler(this.btnCancel_Click);
			this.btnSave.Click += new System.EventHandler(this.btnSave_Click);
			this.btnCancelImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnCancelImg_Click);
			this.btnSaveImg.Click += new System.Web.UI.ImageClickEventHandler(this.btnSaveImg_Click);
			this.cboRegion.SelectedIndexChanged += new System.EventHandler(this.cboRegion_SelectedIndexChanged);
			this.grdSoilSampleOne.UpdatingCell += new Janus.Web.GridEX.UpdatingCellEventHandler(this.grdSoilSample_UpdatingCell);
			this.chkLinkedRainfall.CheckedChanged += new System.EventHandler(this.chkLinkedRainfall_CheckedChanged);
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
			this.grdSoilSampleTwo.UpdatingCell += new Janus.Web.GridEX.UpdatingCellEventHandler(this.grdSoilSample_UpdatingCell);
			this.Load += new System.EventHandler(this.Page_Load);
			((System.ComponentModel.ISupportInitialize)(this.dsSoilSampleOne)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dtSoilSampleOne)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dsSoilSampleTwo)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dtSoilSampleTwo)).EndInit();

		}
		#endregion

		//-------------------------------------------------------------------------
		//Fills the form with the selected paddock's data from the database
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
			int iNumberOfSoilSamples = DataAccessClass.ReturnNumberOfSoilSamples(Session["SelectedPaddockID"].ToString(), "GridOne");
			if(iNumberOfSoilSamples > 0)
				{
				string szInitialDate = DataAccessClass.GetPaddocksSoilSampleDate(Session["SelectedPaddockID"].ToString(), "GridOne");
				cldInitialConditions.SelectedDate = DateTime.ParseExact(szInitialDate, "yyyy-MM-dd", null);
				cldInitialConditions.VisibleDate = cldInitialConditions.SelectedDate;
				}
			else
				{
				cldInitialConditions.SelectedDate = DateTime.Today;
				cldInitialConditions.VisibleDate = DateTime.Today;
				}

			}
		//-------------------------------------------------------------------------
		//Fills the form with the selected paddock's data from the database
		//-------------------------------------------------------------------------
			private void FillDependantFormControls()
				{
				try
					{
					//if a region has be previously saved it is set as the selected metstation.
					int iRegionID = DataAccessClass.GetRegionIDOfPaddock(Session["SelectedPaddockID"].ToString());
					if(iRegionID > 0)
						{
						cboRegion.SelectedValue = iRegionID.ToString();
						}
					if(cboLinkedRainfall.Items.Count > 0)
						{
						//if a region has be previously saved it is set as the selected metstation.
						int iLinkedRainfallPaddockID = DataAccessClass.GetLinkedTemporalPaddockIDOfPaddock(Session["SelectedPaddockID"].ToString());
						if(iLinkedRainfallPaddockID > 0)
							{
							cboLinkedRainfall.SelectedValue = iLinkedRainfallPaddockID.ToString();
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
					//Fills the met stations combo box
					FillMetStationCombo();
					//Fills the soil types combo box
					FillSoilTypeCombo();			
					//If a sub soil has be previously saved it is set as the selected sub soil.
					int iSubSoilConstraintID = DataAccessClass.GetSubSoilConstraintTypeIDOfPaddock(Session["SelectedPaddockID"].ToString());
					if(iSubSoilConstraintID > 0)
						{
						cboSubSoil.SelectedValue = iSubSoilConstraintID.ToString();
						}
					//if a met station has be previously saved it is set as the selected metstation.
					int iMetStationID = DataAccessClass.GetMetStationIDOfPaddock(Session["SelectedPaddockID"].ToString());
					if(iMetStationID > 0)
						{
						cboWeatherStation.SelectedValue = iMetStationID.ToString();
						}
					//if a soil type has be previously saved it is set as the selected soil type.
					int iSoilID = DataAccessClass.GetSoilIDOfPaddock(Session["SelectedPaddockID"].ToString());
					if(iSoilID > 0)
						{
						cboSoilType.SelectedValue = iSoilID.ToString();
						}
					}
				catch(Exception)
					{
					FunctionsClass.DisplayMessage(Page, "Paddock information invalid");
					}
				}
		//-------------------------------------------------------------------------
		//Displays the grower's name and the paddock's name on a label
		//-------------------------------------------------------------------------
		private void DisplayGrowersName()
			{
			int iUserID = DataAccessClass.GetUserIDOfPaddock(Session["SelectedPaddockID"].ToString());
			lblName.Text = DataAccessClass.GetNameOfUser(iUserID.ToString())+ 
				" and paddock: "+DataAccessClass.GetNameOfPaddock(Session["SelectedPaddockID"].ToString());
			}
		//-------------------------------------------------------------------------
		//Fills the regions combo box with all the regions from the database
		//-------------------------------------------------------------------------
		private void FillRegionCombo()
			{
			DataTable dtRegions = DataAccessClass.GetAllRegions();
			cboRegion.DataSource = dtRegions;
			cboRegion.DataTextField = "Type";
			cboRegion.DataValueField = "ID";
			cboRegion.DataBind();
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
				string szSelectedRegionID = cboRegion.SelectedValue.ToString(); 
				DataTable dtMetStations = DataAccessClass.GetMetStationsOfRegion(szSelectedRegionID);
				cboWeatherStation.DataSource = dtMetStations;
				cboWeatherStation.DataTextField = "Name";
				cboWeatherStation.DataValueField = "ID";
				cboWeatherStation.DataBind();
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
				int iPaddockUserID = DataAccessClass.GetUserIDOfPaddock(Session["SelectedPaddockID"].ToString());
				string szSelectedRegionID = cboRegion.SelectedValue.ToString(); 
				DataTable dtSoils = DataAccessClass.GetSoilsOfRegion(szSelectedRegionID, iPaddockUserID.ToString());
				cboSoilType.DataSource = dtSoils;
				cboSoilType.DataTextField = "Name";
				cboSoilType.DataValueField = "ID";
				cboSoilType.DataBind();
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
			int iPaddockUserID = DataAccessClass.GetUserIDOfPaddock(Session["SelectedPaddockID"].ToString());
			DataTable dtPaddocks = DataAccessClass.GetAllPaddocksForTemporalLinking(Session["SelectedPaddockID"].ToString(), iPaddockUserID.ToString());
			cboLinkedRainfall.DataSource = dtPaddocks;
			cboLinkedRainfall.DataTextField = "Name";
			cboLinkedRainfall.DataValueField = "ID";
			cboLinkedRainfall.DataBind();
			}
		//-------------------------------------------------------------------------
		//Fills the first soil sample grid with data from the database
		//-------------------------------------------------------------------------	
		private void FillSoilSampleOneGrid()
			{
			try
				{
				//Gets the data and converts it from xml format to a datatable
				string szSoilSampleData =
					DataAccessClass.GetPaddocksSoilSampleData(Session["SelectedPaddockID"].ToString(), "GridOne");
				DataTable dtSoilSampleFromDB = SoilSampleClass.ConvertSoilSampleOneDataToDataTable(szSoilSampleData);
				
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
			catch(Exception)
				{
				FunctionsClass.DisplayMessage(Page, "Soil sample information invalid");
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
				string szSoilSampleData =
					DataAccessClass.GetPaddocksSoilSampleData(Session["SelectedPaddockID"].ToString(), "GridTwo");
				DataTable dtSoilSampleFromDB = SoilSampleClass.ConvertSoilSampleTwoDataToDataTable(szSoilSampleData);
				
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
			catch(Exception)
				{
				FunctionsClass.DisplayMessage(Page, "Soil sample two information invalid");
				}
			}
		//-------------------------------------------------------------------------
		//Fills the sub soil combo box with all the sub soils types form the database
		//-------------------------------------------------------------------------
		private void FillSubSoilCombo()
			{
			DataTable dtSubSoilConstraints = DataAccessClass.GetAllSubSoilConstraintTypes();
			cboSubSoil.DataSource = dtSubSoilConstraints;
			cboSubSoil.DataTextField = "Type";
			cboSubSoil.DataValueField = "ID";
			cboSubSoil.DataBind();
			}
		//-------------------------------------------------------------------------
		//The paddocks settings are updated, but firstly a check is run to ensure that a
		//metstation and a soil type have been selected.  If this check is passed
		//then the selected paddock's settings are updated in the database
		//-------------------------------------------------------------------------
		private void SavePaddockSetup()
			{
			if(FunctionsClass.IsGrowerOrHigher(Session["UserID"].ToString()) == true)
				{
				//If a metstation and soil type are selected then save the record
				if(cboWeatherStation.SelectedValue != "" && cboSoilType.SelectedValue != "")
					{
					string szLinkedTemporalPaddockID = "0";
					if(chkLinkedRainfall.Checked == true)
					{
						szLinkedTemporalPaddockID = cboLinkedRainfall.SelectedValue;
					}
					
					DataAccessClass.UpdatePaddockSettings(cboWeatherStation.SelectedValue.ToString(),  
						cboSoilType.SelectedValue.ToString(), cboSubSoil.SelectedValue.ToString(),
						szLinkedTemporalPaddockID, Session["SelectedPaddockID"].ToString());
					SaveSoilSampleDetails();
					Server.Transfer("wfEditPaddock.aspx");
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
				string szSoilSampleDate = cldInitialConditions.SelectedDate.ToString("yyyy-MM-dd");
				int iSoilSampleOneTypeID = DataAccessClass.GetSoilSampleTypeID("GridOne");
				//If the paddock doesn't have any soil sample one data stored, we insert the
				//record into the table, it the paddock does have an existing soil sample one 
				//we update the record
				if(DataAccessClass.ReturnNumberOfSoilSamples(Session["SelectedPaddockID"].ToString(), "GridOne") == 0)
					{
					DataAccessClass.InsertSoilSample(szSoilSampleDate, szSoilSampleData, 
						Session["SelectedPaddockID"].ToString(), iSoilSampleOneTypeID.ToString());
					}
				else
					{
					DataAccessClass.UpdateSoilSample(szSoilSampleDate, szSoilSampleData, 
						Session["SelectedPaddockID"].ToString(), iSoilSampleOneTypeID.ToString());
					}
				//Saves teh data from the second grid
				szSoilSampleData = SoilSampleClass.CreateSoilSampleTwoXmlFile(ReturnSoilSampleTwoDataTable());
				szSoilSampleDate = cldInitialConditions.SelectedDate.ToString("yyyy-MM-dd");
				int iSoilSampleTwoTypeID = DataAccessClass.GetSoilSampleTypeID("GridTwo");
				//If the paddock doesn't have any soil sample two data stored, we insert the
				//record into the table, it the paddock does have an existing soil sample two 
				//we update the record
				if(DataAccessClass.ReturnNumberOfSoilSamples(Session["SelectedPaddockID"].ToString(), "GridTwo") == 0)
					{
					DataAccessClass.InsertSoilSample(szSoilSampleDate, szSoilSampleData, 
						Session["SelectedPaddockID"].ToString(), iSoilSampleTwoTypeID.ToString());
					}
				else
					{
					DataAccessClass.UpdateSoilSample(szSoilSampleDate, szSoilSampleData, 
						Session["SelectedPaddockID"].ToString(), iSoilSampleTwoTypeID.ToString());
					}
				}
			catch(Exception)
				{}
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
				FunctionsClass.DisplayMessage(Page, "Error storing soil sample two data");
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
		}//END OF CLASS
	}//END OF NAMESPACE
