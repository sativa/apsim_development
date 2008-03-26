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

namespace YP2006
{
	/// <summary>
	/// Summary description for wfPaddocksSoil.
	/// </summary>
	public class wfPaddocksSoil : System.Web.UI.Page
	{
		protected System.Web.UI.WebControls.Label lblRootingDepthUnit;
		protected System.Web.UI.WebControls.RadioButtonList rdbSWUnit;
		protected System.Web.UI.WebControls.DropDownList cboSoilType;
		protected System.Web.UI.WebControls.Label lblSoilType;
		protected System.Web.UI.WebControls.TextBox edtRootingDepth;
		protected System.Web.UI.WebControls.Label lblRootingDepth;
		protected System.Web.UI.WebControls.CheckBox chkUseEC;
		protected System.Web.UI.WebControls.Label lblInitialConditions;
		protected System.Web.UI.WebControls.Label lblWaterType;
		protected System.Web.UI.WebControls.Label lblDepthOne;
		protected System.Web.UI.WebControls.Button btnUpdateGraph;
		protected System.Data.DataSet dsInitialDate;
		protected System.Data.DataTable dtInitialDate;
		protected System.Data.DataColumn dcInitialDate;
		protected Janus.Web.GridEX.GridEX grdInitialDate;
		protected System.Data.DataSet dsSoilSampleOne;
		protected System.Data.DataTable dtSoilSampleOne;
		protected System.Data.DataColumn dcDepth;
		protected System.Data.DataColumn dcWater;
		protected System.Data.DataColumn dcNO3;
		protected System.Data.DataColumn dcNH4;
		protected System.Data.DataSet dsSoilSampleTwo;
		protected System.Data.DataTable dtSoilSampleTwo;
		protected System.Data.DataColumn dataColumn1;
		protected System.Data.DataColumn dcOC;
		protected System.Data.DataColumn dcEC;
		protected System.Data.DataColumn dcPH;
		protected System.Data.DataColumn dcESP;
		protected Janus.Web.GridEX.GridEX grdSoilSampleOne;
		protected Janus.Web.GridEX.GridEX grdSoilSampleTwo;
		protected Xceed.Chart.Server.ChartServerControl cscSoilChart;
		protected System.Web.UI.WebControls.LinkButton LinkButton1;
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
		protected System.Web.UI.WebControls.ImageButton btnHelpSoilType;
		protected System.Web.UI.WebControls.ImageButton btnHelpPaddockSoilPage;
		protected System.Web.UI.WebControls.ImageButton btnHelpRootingDepth;
		protected System.Web.UI.WebControls.ImageButton btnHelpEC;
		protected System.Web.UI.WebControls.ImageButton btnHelpInitialConditions;
		protected System.Web.UI.WebControls.ImageButton btnWaterType;
		protected System.Web.UI.WebControls.ImageButton btnHelpGridOne;
		protected System.Web.UI.WebControls.ImageButton btnHelpGridTwo;
		protected System.Web.UI.WebControls.ImageButton btnHelpGraph;
		protected System.Web.UI.WebControls.Label InvalidSWLabel;
	


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
			this.dsInitialDate = new System.Data.DataSet();
			this.dtInitialDate = new System.Data.DataTable();
			this.dcInitialDate = new System.Data.DataColumn();
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
			((System.ComponentModel.ISupportInitialize)(this.dsInitialDate)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dtInitialDate)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dsSoilSampleOne)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dtSoilSampleOne)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dsSoilSampleTwo)).BeginInit();
			((System.ComponentModel.ISupportInitialize)(this.dtSoilSampleTwo)).BeginInit();
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
			this.chkUseEC.CheckedChanged += new System.EventHandler(this.chkUseEC_CheckedChanged);
			this.btnSave.Click += new System.EventHandler(this.btnSave_Click);
			this.btnUpdateGraph.Click += new System.EventHandler(this.btnUpdateGraph_Click);
			this.btnCancel.Click += new System.EventHandler(this.btnCancel_Click);
			this.btnSaveTwo.Click += new System.EventHandler(this.btnSave_Click);
			this.btnCancelTwo.Click += new System.EventHandler(this.btnCancel_Click);
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
			this.Load += new System.EventHandler(this.Page_Load);
			((System.ComponentModel.ISupportInitialize)(this.dsInitialDate)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dtInitialDate)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dsSoilSampleOne)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dtSoilSampleOne)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dsSoilSampleTwo)).EndInit();
			((System.ComponentModel.ISupportInitialize)(this.dtSoilSampleTwo)).EndInit();

		}
		#endregion
	

		#region Form Functions

		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void FillForm()
		{
			DataTable dtPaddockDetails = 
				DataAccessClass.GetDetailsOfPaddock(Session["SelectedPaddockName"].ToString(), 
				FunctionsClass.GetActiveUserName());
			//Fills the soil types combo box
			FillSoilTypeCombo(dtPaddockDetails.Rows[0]["RegionType"].ToString());		

			chkUseEC.Checked = Convert.ToBoolean(Convert.ToInt32(dtPaddockDetails.Rows[0]["UseEC"].ToString()));
			chkUseEC_CheckedChanged(null, null);

			edtRootingDepth.Text = dtPaddockDetails.Rows[0]["RootingDepth"].ToString();		
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
			grdInitialDate.DataBind();
		}
		//-------------------------------------------------------------------------
		//Fills the soil type combo box with all the soil types linked to the
		//selected region and to the current grower
		//-------------------------------------------------------------------------
		private void FillSoilTypeCombo(string szRegion)
		{
			try
			{
				DataTable dtSoils = DataAccessClass.GetSoilsOfRegion(szRegion);
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
					DataTableUtility.AddColumn(dsSoilSampleTwo.Tables["SoilSampleTwo"], "CL", 
						Sample.CL);
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
			Sample2.CL = FunctionsClass.GetColumnAsDoubles(grdSoilSampleTwo, 5, NumLayers);
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
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void UpdateInvalidLabel(SoilSample Sample)
		{
			InvalidSWLabel.Visible = !DataAccessClass.IsSoilSampleOk(Sample);
		}
		//-------------------------------------------------------------------------
		//The paddocks settings are updated, but firstly a check is run to ensure that a
		//metstation and a soil type have been selected.  If this check is passed
		//then the selected paddock's settings are updated in the database
		//-------------------------------------------------------------------------
		private void SavePaddockSoil()
		{
			try
			{
				if(FunctionsClass.IsReadOnly() == false)
				{
					//If soil type is selected then save the record
					if( cboSoilType.SelectedValue != "" && cboSoilType.SelectedValue != "None" &&
						grdInitialDate.GetRow(0).Cells["InitialDate"].Text != "" 
						)
					{
						DataAccessClass.UpdatePaddock("", "", -1, "",  
							cboSoilType.SelectedItem.Text, "", -1,
							"", "", -1, ReturnMaxRootingDepth(), -1, -1,
							-1, Convert.ToInt32(chkUseEC.Checked), Session["SelectedPaddockName"].ToString(), Session["SelectedPaddockName"].ToString(), 
							FunctionsClass.GetActiveUserName());
						SaveSoilSampleDetails();
						Server.Transfer("wfPaddocksSoil.aspx");
					}
					//If soil type is not selected display an 
					//error message to the user.
					else
						throw new Exception("Please ensure that all fields contain data");
				}
				else
					throw new Exception(FunctionsClass.ReturnReadOnlyMessage());
			}
			catch(Exception E)
			{
				FunctionsClass.DisplayMessage(Page, E.Message);
			}
		}
		//-------------------------------------------------------------------------
		//Saves the soil sample value from the grid
		//-------------------------------------------------------------------------
		private void SaveSoilSampleDetails()
		{
			string szSoilSampleDate = 
				(DateTime.ParseExact(grdInitialDate.GetRow(0).Cells["InitialDate"].Text, "dd/MM/yyyy", null)).ToString("yyyy-MM-dd");

			// Save soil sample 1 grid
			SoilSample Sample1 = GetSample1();
			DataAccessClass.SetSoilSample(szSoilSampleDate, Sample1.Data.XML, "GridOne", 
				Session["SelectedPaddockName"].ToString(), FunctionsClass.GetActiveUserName());


			// Save soil sample 2 grid.
			SoilSample Sample2 = GetSample2();
			DataAccessClass.SetSoilSample(szSoilSampleDate, Sample2.Data.XML, "GridTwo", 
				Session["SelectedPaddockName"].ToString(), FunctionsClass.GetActiveUserName());
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

				HelpClass.SetHelpForPaddockSoilPage(btnHelpSoilType, 
					btnHelpPaddockSoilPage, btnHelpRootingDepth, btnHelpEC, 
					btnHelpInitialConditions, btnWaterType, btnHelpGridOne, 
					btnHelpGridTwo, btnHelpGraph);

				FillForm();
				FunctionsClass.SetHeadingString(lblHeading);
				FunctionsClass.SetNavigationMenu(btnGrowersPaddocks, btnGrowersReports, 
					btnManageItems, btnManageReports);
				FunctionsClass.SetDisplayBanner(imgBanner);
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
		private void chkUseEC_CheckedChanged(object sender, System.EventArgs e)
		{
			if(chkUseEC.Checked == true)
				edtRootingDepth.Enabled = false;
			else
				edtRootingDepth.Enabled = true;
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnUpdateGraph_Click(object sender, System.EventArgs e)
		{
			UpdateGraph(null);
		}

		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnSave_Click(object sender, System.EventArgs e)
		{
		SavePaddockSoil();
		}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void btnCancel_Click(object sender, System.EventArgs e)
		{
		Server.Transfer("wfPaddocksSoil.aspx");
		}

		//-------------------------------------------------------------------------
		#endregion
	
	}//END OF CLASS
}//END OF NAMESPACE
