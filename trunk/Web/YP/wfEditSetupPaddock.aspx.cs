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
using VBGeneral;

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
		protected System.Web.UI.WebControls.Label lblWaterType;
		protected System.Web.UI.WebControls.RadioButtonList rdbSWUnit;
		protected System.Web.UI.WebControls.Label lblRootingDepth;
		protected System.Web.UI.WebControls.TextBox edtRootingDepth;
		protected System.Web.UI.WebControls.Label lblRootingDepthUnit;
		protected System.Web.UI.WebControls.CheckBox chkDefaultRainfall;
		protected System.Web.UI.WebControls.Label InvalidSWLabel;
		protected System.Web.UI.WebControls.CheckBox chkUseEC;
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
			this.chkDefaultRainfall.CheckedChanged += new System.EventHandler(this.chkDefaultRainfall_CheckedChanged);
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
			this.chkUseEC.CheckedChanged += new System.EventHandler(this.chkUseEC_CheckedChanged);
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

				chkDefaultRainfall.Checked = Convert.ToBoolean(Convert.ToInt32(dtPaddockDetails.Rows[0]["DefaultRainfall"].ToString()));
				chkDefaultRainfall_CheckedChanged(null, null);

				chkUseEC.Checked = Convert.ToBoolean(Convert.ToInt32(dtPaddockDetails.Rows[0]["UseEC"].ToString()));
				chkUseEC_CheckedChanged(null, null);


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
					chkLinkedRainfall.Checked = false;
					cboLinkedRainfall.Enabled = false;
					}
				//Set user's selections
				cboRegion.SelectedValue = dtPaddockDetails.Rows[0]["RegionType"].ToString();		
				edtRootingDepth.Text = dtPaddockDetails.Rows[0]["RootingDepth"].ToString();		
				cboWeatherStation.SelectedValue = dtPaddockDetails.Rows[0]["MetStationName"].ToString();
				try
					{
					cboSoilType.SelectedValue = dtPaddockDetails.Rows[0]["SoilName"].ToString();
					}
				catch (Exception)
					{ }

				//Fills the soil sample one grid
				FillSoilSampleOneGrid();
				//Fills the soil sample two grid
				FillSoilSampleTwoGrid();

				//If ther is a soil sample record set the initial condition date
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
			grdSampleDate.DataBind();
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
			grdStartOfGrowingSeason.DataBind();
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
				//Ensure that a sample has been added
				if(dtPaddocksSoilSameple.Rows.Count > 0)
				{
					SoilSample Sample = new SoilSample(new APSIMData(dtPaddocksSoilSameple.Rows[0]["Data"].ToString()));
					Sample.LinkedSoil = DataAccessClass.GetSoil(cboSoilType.SelectedValue);

					double[] sw;
					if (Sample.SWUnit == SoilSample.SWUnits.Volumetric)
					{	
						rdbSWUnit.SelectedValue = "VolumetricPercent";
						sw = Sample.SW;
					}
					else
					{	
						rdbSWUnit.SelectedValue = "GravimetricPercent";
						sw = Sample.SWGrav;
					}

					DataTableUtility.AddColumn(dsSoilSampleOne.Tables["SoilSampleOne"], "Depth", 
						Sample.DepthStrings);
					DataTableUtility.AddColumn(dsSoilSampleOne.Tables["SoilSampleOne"], "Water", 
						MathUtility.Multiply_Value(sw, 100));
					DataTableUtility.AddColumn(dsSoilSampleOne.Tables["SoilSampleOne"], "NO3", 
						Sample.NO3);
					DataTableUtility.AddColumn(dsSoilSampleOne.Tables["SoilSampleOne"], "NH4", 
						Sample.NH4);

					UpdateGraph(Sample);
				}
				//Appends any need blank records so that the grid displays the correct number of rows
				while (dsSoilSampleOne.Tables["SoilSampleOne"].Rows.Count < 8)
					dsSoilSampleOne.Tables["SoilSampleOne"].Rows.Add(dsSoilSampleOne.Tables["SoilSampleOne"].NewRow());

				grdSoilSampleOne.DataBind();
				
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
				//Ensure that a sample has been added
				if(dtPaddocksSoilSameple.Rows.Count > 0)
					{
					SoilSample Sample = new SoilSample(new APSIMData(dtPaddocksSoilSameple.Rows[0]["Data"].ToString()));
				
					DataTableUtility.AddColumn(dsSoilSampleTwo.Tables["SoilSampleTwo"], "Depth", 
						Sample.DepthStrings);
					DataTableUtility.AddColumn(dsSoilSampleTwo.Tables["SoilSampleTwo"], "OC", 
						Sample.OC);
					DataTableUtility.AddColumn(dsSoilSampleTwo.Tables["SoilSampleTwo"], "EC", 
						Sample.EC);
					DataTableUtility.AddColumn(dsSoilSampleTwo.Tables["SoilSampleTwo"], "PH", 
						Sample.PH);
					}
				//Appends any need blank records so that the grid displays the correct number of rows
				while (dsSoilSampleTwo.Tables["SoilSampleTwo"].Rows.Count < 8)
					dsSoilSampleTwo.Tables["SoilSampleTwo"].Rows.Add(dsSoilSampleTwo.Tables["SoilSampleTwo"].NewRow());

				grdSoilSampleTwo.DataBind();
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
					else
						{
						szLinkedTemporalPaddock = "NONE";
						}
					//Cancel all linked rainfall options if user has selected to use default rainfall values
					if(chkDefaultRainfall.Checked == true)
						{
						szLinkedTemporalPaddock = "NONE";
						chkLinkedRainfall.Checked = false;
						}
					try
						{
						DataAccessClass.UpdatePaddock("", "", -1, cboWeatherStation.SelectedItem.Text,  
							cboSoilType.SelectedItem.Text, "", Convert.ToInt32(chkDefaultRainfall.Checked),
							szLinkedTemporalPaddock, szStartOfGrowingSeasonDate, -1, ReturnMaxRootingDepth(), -1, -1,
							-1, Convert.ToInt32(chkUseEC.Checked), Session["SelectedPaddockName"].ToString(), Session["SelectedPaddockName"].ToString(), 
							FunctionsClass.GetActiveUserName());
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
				string szSoilSampleDate = 
					(DateTime.ParseExact(grdSampleDate.GetRow(0).Cells["InitialDate"].Text, "dd/MM/yyyy", null)).ToString("yyyy-MM-dd");

				// Save soil sample 1 grid
				SoilSample Sample1 = GetSample1();
				DataAccessClass.SetSoilSample(szSoilSampleDate, Sample1.Data.XML, "GridOne", 
					Session["SelectedPaddockName"].ToString(), FunctionsClass.GetActiveUserName());


				// Save soil sample 2 grid.
				SoilSample Sample2 = GetSample2();
				DataAccessClass.SetSoilSample(szSoilSampleDate, Sample2.Data.XML, "GridTwo", 
					Session["SelectedPaddockName"].ToString(), FunctionsClass.GetActiveUserName());
				}
			catch(Exception E)
				{
				FunctionsClass.DisplayMessage(Page, E.Message);
				}
			}


		// ------------------------------------------------
		// Return an instance of SoilSample for first grid.
		// ------------------------------------------------
		private SoilSample GetSample1()
			{
			// Save soil sample 1 grid
			SoilSample Sample1 = new SoilSample();

			int NumLayers = FunctionsClass.GetNumberOfNonBlankRows(ref grdSoilSampleOne, 0);
			Sample1.DepthStrings = FunctionsClass.GetColumnAsStrings(grdSoilSampleOne, 0, NumLayers);
			
			double[] sw = FunctionsClass.GetColumnAsDoubles(grdSoilSampleOne, 1, NumLayers);
			sw = MathUtility.Divide_Value(sw, 100);
            if (rdbSWUnit.SelectedValue == "VolumetricPercent")
				Sample1.SW = sw;
			else				
				Sample1.SWGrav = sw;
			
			Sample1.NO3 = FunctionsClass.GetColumnAsDoubles(grdSoilSampleOne, 2, NumLayers);

			Sample1.NH4 = FunctionsClass.GetColumnAsDoubles(grdSoilSampleOne, 3, NumLayers);

			return Sample1;
			}


		// ------------------------------------------------
		// Return an instance of SoilSample for first grid.
		// ------------------------------------------------
		private SoilSample GetSample2()
			{
			// Save soil sample 1 grid
			int NumLayers = FunctionsClass.GetNumberOfNonBlankRows(ref grdSoilSampleTwo, 0);
			SoilSample Sample2 = new SoilSample();
			Sample2.DepthStrings = FunctionsClass.GetColumnAsStrings(grdSoilSampleTwo, 0, NumLayers);
			Sample2.OC = FunctionsClass.GetColumnAsDoubles(grdSoilSampleTwo, 1, NumLayers);
			Sample2.EC = FunctionsClass.GetColumnAsDoubles(grdSoilSampleTwo, 2, NumLayers);
			Sample2.PH = FunctionsClass.GetColumnAsDoubles(grdSoilSampleTwo, 3, NumLayers);
			return Sample2;
			}
		//-------------------------------------------------------------------------
		//Redraws the graph on the page
		//-------------------------------------------------------------------------
		private void UpdateGraph(SoilSample Sample)
			{
			try
				{
				// get current crop - if any
				DataTable dtUsersDetails = DataAccessClass.GetDetailsOfUser(FunctionsClass.GetActiveUserName());	
				string szUsersName =  dtUsersDetails.Rows[0]["Name"].ToString();
				string szPaddockName = HttpContext.Current.Session["SelectedPaddockName"].ToString();
				DataTable dtPaddocksDetails = DataAccessClass.GetDetailsOfPaddock(szPaddockName, FunctionsClass.GetActiveUserName());
				string CropType =  dtPaddocksDetails.Rows[0]["CropType"].ToString();
				if (CropType.ToLower() == "barley" || CropType.ToLower() == "none")
					CropType = "wheat";

                Chart WaterChart = cscSoilChart.Charts.GetAt( 0 );
				WaterChart.Series.Clear();		  

				// Create a sample from the first grid.
				if (Sample == null)
					Sample = GetSample1();
				Sample.LinkedSoil = DataAccessClass.GetSoil(cboSoilType.SelectedValue);
				UpdateInvalidLabel(Sample);

				// Create a SW line on chart.
				CreateNewLineSeries(WaterChart, "SW", Color.LightBlue, 3, 
									Xceed.Chart.Standard.LinePattern.Solid,
					                MathUtility.Multiply_Value(Sample.SW, 100), Sample.CumThicknessMidPoints);

				// Get the soil data from the currently selected soil.
				Soil SelectedSoil = DataAccessClass.GetSoil(cboSoilType.SelectedValue);

				// Create DUL, LL, AirDry and SAT lines on chart.
				double[] Thickness = SelectedSoil.CumThicknessMidPoints;
				CreateNewLineSeries(WaterChart, "DUL", Color.Blue, 1, 
									Xceed.Chart.Standard.LinePattern.Solid,
					                MathUtility.Multiply_Value(SelectedSoil.DUL, 100), Thickness);
				if (CropType == "")
					CreateNewLineSeries(WaterChart, "LL15", Color.Red, 1, 
										Xceed.Chart.Standard.LinePattern.Solid,
						                MathUtility.Multiply_Value(SelectedSoil.LL15, 100), Thickness);
				else
					CreateNewLineSeries(WaterChart, "LL", Color.Red, 1, 
										Xceed.Chart.Standard.LinePattern.Solid,
										MathUtility.Multiply_Value(SelectedSoil.LL(CropType), 100), Thickness);
				CreateNewLineSeries(WaterChart, "AirDry", Color.Red, 1, 
									Xceed.Chart.Standard.LinePattern.Dash,
					                MathUtility.Multiply_Value(SelectedSoil.Airdry, 100), Thickness);
				CreateNewLineSeries(WaterChart, "SAT", Color.Blue, 1, 
									Xceed.Chart.Standard.LinePattern.Dash,
					                MathUtility.Multiply_Value(SelectedSoil.SAT, 100), Thickness);

				// put on pawc and paw labels.
				double PAWC;
				double PAW = 0.0;
				if (CropType == "")
					PAWC = MathUtility.Sum(SelectedSoil.PAWC());
				else
					{
					PAWC = MathUtility.Sum(SelectedSoil.PAWC(CropType));
					PAW = MathUtility.Sum(Sample.PAW(CropType));
					}
				cscSoilChart.Labels[0].Text = "PAWC = "+Math.Round(PAWC, 0).ToString("F0")+"mm";
				if (CropType != "")
					cscSoilChart.Labels[1].Text = "PAW = "+Math.Round(PAW, 0).ToString("F0")+"mm";
				}
			catch(Exception E)
				{
				FunctionsClass.DisplayMessage(Page, E.Message);
				}
			}

		// ------------------------------------------------------------------
		// Create a new line series for specified chart given x and y values.
		// ------------------------------------------------------------------
		private void CreateNewLineSeries(Xceed.Chart.Core.Chart Chart,
			                             string SeriesName, Color SeriesColor,
										 int SeriesWidth,
										 Xceed.Chart.Standard.LinePattern SeriesPattern,			
										 double[] x, double[] y)
			{
			//Initialise the SW series and fill with data from the passed in DataTable
			LineSeries LineSeries;
			LineSeries = ( LineSeries )Chart.Series.Add( SeriesType.Line );
			LineSeries.Name = SeriesName;
			LineSeries.DataLabels.Mode = DataLabelsMode.None;
			LineSeries.LineBorder.Color = SeriesColor;
			LineSeries.LineBorder.Width = SeriesWidth;
			LineSeries.LineBorder.Pattern = SeriesPattern;
			LineSeries.Values.FillFromEnumerable(y);
			LineSeries.XValues.FillFromEnumerable(x);
			LineSeries.UseXValues = true;
			}

		//-------------------------------------------------------------------------
		//Returns the value of the root depth text box after it is checked to insure 
		//that the value is a valid integer
		//-------------------------------------------------------------------------
		private int ReturnMaxRootingDepth()
		{
			int iRootingDepth = 0;
			if(edtRootingDepth.Text != "")
			{
				if(InputValidationClass.IsInputAPositiveInteger(edtRootingDepth.Text))
				{
					iRootingDepth = Convert.ToInt32(edtRootingDepth.Text);
				}
			}
			return iRootingDepth;
		}

		private void UpdateInvalidLabel(SoilSample Sample)
			{
			InvalidSWLabel.Visible = !DataAccessClass.IsSoilSampleOk(Sample);
			}

		//-------------------------------------------------------------------------
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
		//
		//-------------------------------------------------------------------------
		private void chkDefaultRainfall_CheckedChanged(object sender, System.EventArgs e)
			{
			if(chkDefaultRainfall.Checked == true)
				{
				chkLinkedRainfall.Checked = false;
				chkLinkedRainfall.Enabled = false;
				cboLinkedRainfall.Enabled = false;
				}
			else
				{
				chkLinkedRainfall.Enabled = true;
				}
			}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void chkUseEC_CheckedChanged(object sender, System.EventArgs e)
		{
			if(chkUseEC.Checked == true)
				edtRootingDepth.Enabled = false;
			else
				edtRootingDepth.Enabled = true;
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
			UpdateGraph(null);
			}
		//-------------------------------------------------------------------------
		#endregion



		//-------------------------------------------------------------------------
		}//END OF CLASS
	}//END OF NAMESPACE
