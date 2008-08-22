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
using ET;
//using CSGeneral;

namespace UrbanWater
{
	/// <summary>
	/// Summary description for wfDisplayResults.
	/// </summary>
	public class wfDisplayResults : System.Web.UI.Page
	{
		protected Xceed.Chart.Server.ChartServerControl cscRainfall_SoilMoisture;
		protected System.Web.UI.WebControls.TextBox edtEvapotranspiration;
		protected System.Web.UI.WebControls.Label lblEvapotranspiration;
		protected System.Web.UI.WebControls.Label lblWatering;
		protected System.Web.UI.WebControls.TextBox edtWatering;
		protected System.Web.UI.WebControls.Label lblEvaporation;
		protected System.Web.UI.WebControls.TextBox edtEvaporation;
		protected System.Web.UI.WebControls.TextBox edtWateringNeeded;
		protected System.Web.UI.WebControls.Label lblWateringNeeded;
		protected System.Web.UI.WebControls.Label lblRainfall;
		protected System.Web.UI.WebControls.TextBox edtRainfall;
		protected System.Web.UI.WebControls.Panel pnlPeriod;
		protected System.Web.UI.WebControls.Label lblPeriod;
		protected System.Web.UI.WebControls.Panel pnlIdentification;
		protected System.Web.UI.WebControls.Label lblIdentification;
		protected Xceed.Chart.Server.ChartServerControl cscRainfall_Evaporation;
	
		//-------------------------------------------------------------------------
		//Initialises the page
		//-------------------------------------------------------------------------
		private void Page_Load(object sender, System.EventArgs e)
		{
			if(!IsPostBack)
				{
				StoreWateringValues();
				SetIdentificationLabel();
				SetPeriodLabel();
				DisplayResults();			
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
			this.Load += new System.EventHandler(this.Page_Load);

		}
		#endregion
		
		
		
		#region Display Results on From
		//-------------------------------------------------------------------------
		//Stores the watering information entered on the title page, in a ViewState
		//variable that will persist over any post backs that occur whilst on this
		//page.  This function must be called from the first call to the Page_Load
		//event, or it can not access the previous page to get the results
		//-------------------------------------------------------------------------
		private void StoreWateringValues()
			{
			try
				{
				wfUrbanWater PageOne = (wfUrbanWater) Context.Handler;
				ViewState["WateringDataTable"] = PageOne.ReturnWateringDataTable();
				}
			catch(Exception)
				{}
			}
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		public DataTable ReturnWateringDataTable()
		{
		return (DataTable)ViewState["WateringDataTable"];
		}
		
		//-------------------------------------------------------------------------
		//Sets the Identification variable to display the location, plant type and
		//soil type used for the calculations
		//-------------------------------------------------------------------------
		private void SetIdentificationLabel()
			{
			if(Session != null)
				{
				System.Text.StringBuilder sbIdentification = new System.Text.StringBuilder(); 
				sbIdentification.Append("Location: ");
				sbIdentification.Append(DataAccessClass.GetLocationName(Session["LocationID"].ToString()));
				sbIdentification.Append(" -- Plant Type: ");
				sbIdentification.Append(DataAccessClass.GetPlantTypeName(Session["PlantTypeID"].ToString()));
				sbIdentification.Append(" -- Soil Type: ");
				sbIdentification.Append(DataAccessClass.GetSoilTypeName(Session["SoilTypeID"].ToString()));
				lblIdentification.Text = sbIdentification.ToString();
				}
			}
		//---------------------------------------------------------------------
		//Sets the period label to display the period the results are generated for
		//---------------------------------------------------------------------
		private void SetPeriodLabel()
			{
			if(Session != null)
				{
				string szEndDate = Session["EndDate"].ToString();
				string szStartDate =Session["StartDate"].ToString();
				lblPeriod.Text = "For period: "+szStartDate+" to "+szEndDate;
				}
			}
		//-------------------------------------------------------------------------
		//Runs all the caluculations and then calls the functions to display the
		//results in either text or graphical format.
		//-------------------------------------------------------------------------
		private void DisplayResults()
			{
			try
				{
				//Return met data for the selected location from the last watering date to present
				DataTable dtMetData = MetCalculations.ReturnMetData(DataAccessClass.GetLocationFileName(Session["LocationID"].ToString()));		
				//Checks to make sure there is data to display
				if(dtMetData.Rows.Count > 0)
					{
					DisplayRainfallAndEvaporationChart(dtMetData);
					}	
				DataTable dtWateringAndRainfall = WateringCalculations.AddWateringToMetData(dtMetData, (DataTable)ViewState["WateringDataTable"]);
				//Return results from calculations undertaken on the returned met data
				DataTable dtETData = MetCalculations.ReturnCalculationData(dtWateringAndRainfall, DataAccessClass.GetSoilTypePAWC(Session["SoilTypeID"].ToString()), 
					DataAccessClass.GetPlantTypeRootingDepth(Session["PlantTypeID"].ToString()), DataAccessClass.GetPlantTypeCropFactor(Session["PlantTypeID"].ToString()));
				if(dtWateringAndRainfall.Rows.Count > 0)
					{
					DisplayAllTextResults(dtMetData, dtETData, dtWateringAndRainfall);
					DisplayRainfallAndSoilMoistureChart(dtMetData, dtETData);
					}
				}
			catch(Exception)
				{}
			}	
		//---------------------------------------------------------------------
		//Displays all the text results on the form
		//---------------------------------------------------------------------
		private void DisplayAllTextResults(DataTable dtMetData, DataTable dtETData, DataTable dtWateringAndRainfall)
			{
			try
				{
				//Display Total Evapotranspiration
				double dEvapotranspiration = Convert.ToDouble(dtETData.Rows[dtETData.Rows.Count-1]["CumET"].ToString());
				dEvapotranspiration = Math.Round(dEvapotranspiration, 0);			
				edtEvapotranspiration.Text = dEvapotranspiration.ToString()+"mm";
				//Display total evaporation
				edtEvaporation.Text = MetCalculations.ReturnTotalEvaporation(dtMetData).ToString()+"mm";
				//Display total rainfall
				edtRainfall.Text = MetCalculations.ReturnTotalRainfall(dtMetData).ToString()+"mm";
				//Display total watering
				edtWatering.Text = WateringCalculations.ReturnTotalWatering((DataTable)ViewState["WateringDataTable"]).ToString()+"mm";		
				//Display total watring needed
				edtWateringNeeded.Text = WateringCalculations.ReturnWateringNeeded(dtETData, DataAccessClass.GetSoilTypePAWC(Session["SoilTypeID"].ToString()), 
					DataAccessClass.GetPlantTypeRootingDepth(Session["PlantTypeID"].ToString())).ToString()+"mm";
				}
			catch(Exception)
				{}
			}
		//-------------------------------------------------------------------------
		//Returns a DataTable with that holds a specified number of records all with
		//the same value.  This is used to draw horizontal lines on the graphs
		//-------------------------------------------------------------------------
		private DataTable ReturnConstValueDataTable(int iConstantValue, int iNumberOfRecords)
			{
			DataTable dtConstValue = new DataTable();
			try
				{
				dtConstValue.Columns.Add("Value", System.Type.GetType("System.Int32"));
				DataRow drConstValue;
				for(int iIndex = 0; iIndex < iNumberOfRecords; iIndex++)
					{
					drConstValue = dtConstValue.NewRow();
					drConstValue["Value"] = iConstantValue.ToString();
					dtConstValue.Rows.Add(drConstValue);
					}
				}
			catch(Exception)
				{}
			return dtConstValue;
			}
			
		#endregion
		
			
			
		#region Rainfall and Evaporation Chart
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void DisplayRainfallAndEvaporationChart(DataTable dtSelectedData)
		{
			//Initialise the chart
			cscRainfall_Evaporation.Visible = true;
			cscRainfall_Evaporation.Labels[0].Text = "Rainfall and Evaporation";
			Chart chtCustomChart;
			chtCustomChart = cscRainfall_Evaporation.Charts.GetAt( 0 );
			chtCustomChart.Series.Clear();
			
			//Initialise the series and fill with data from the passed in DataTable
			BarSeries bsCustomBarSeries;
			bsCustomBarSeries = ( BarSeries )chtCustomChart.Series.Add( SeriesType.Bar );
			bsCustomBarSeries.Name = "Rainfall (mm)";
			bsCustomBarSeries.BarFillEffect.Color = Color.Blue;
			bsCustomBarSeries.DataLabels.Mode = DataLabelsMode.None;
			bsCustomBarSeries.Values.FillFromDataTable(dtSelectedData, "rain");

			//Initialise the series and fill with data from the passed in DataTable
			BarSeries bsCustomBarSeriesTwo;
			bsCustomBarSeriesTwo = ( BarSeries )chtCustomChart.Series.Add( SeriesType.Bar );
			bsCustomBarSeriesTwo.Name = "Watering (mm)";
			bsCustomBarSeriesTwo.BarFillEffect.Color = Color.Yellow;
			bsCustomBarSeriesTwo.DataLabels.Mode = DataLabelsMode.None;
			bsCustomBarSeriesTwo.Values.FillFromDataTable(WateringCalculations.ReturnFullWateringDataTable(dtSelectedData, (DataTable)ViewState["WateringDataTable"]), "rain");

			
			//Initialise the series and fill with data from the passed in DataTable
			LineSeries bsCustomLineSeries;
			bsCustomLineSeries = ( LineSeries )chtCustomChart.Series.Add( SeriesType.Line );
			bsCustomLineSeries.Name = "Evaporation (mm)";
			bsCustomLineSeries.DataLabels.Mode = DataLabelsMode.None;
			bsCustomLineSeries.Values.FillFromDataTable(dtSelectedData, "Evap");
			bsCustomLineSeries.DisplayOnAxis(0, false);
			bsCustomLineSeries.DisplayOnAxis(1, true);


			//Configure primary (left) Y-Axis to display title, etc
			Axis axsRainFall = chtCustomChart.Axis(0);
			axsRainFall.Title = "Rainfall (mm)";
			axsRainFall.TitleText.Orientation = 90;
			axsRainFall.TitleText.OffsetX = -25;
			axsRainFall.Text.OffsetX = -5;
			//Configure secondary (right) Y-Axis to display title, etc
			Axis axsEvaporation = chtCustomChart.Axis(1);
			axsEvaporation.Title = "Evaporation (mm)";
			axsEvaporation.TitleText.Orientation = 90;
			axsEvaporation.TitleText.OffsetX = 25;
			axsEvaporation.Text.OffsetX = 5;
			//Configure the primary (bottom) X-Axis to display the dates associated with the data.
			Axis axsDates = chtCustomChart.Axis(2);
			axsDates.Labels.Clear();
			axsDates.TitleText.OffsetY = 25;
			axsDates.Title = "Date (DD/MM)";
			axsDates.Text.OffsetY = 5; 
			//Figure out the number of labels to display with out cluttering the graph
			int iMaximumNumberOfLabels = 10;
			int iNumberOfRecords = dtSelectedData.Rows.Count;
			//Calculate the mumber of records between each label by dividing the number
			//of records by the maximum number of labels and then round the result
			//by adding 0.5
			int iCustomStep  = (int)(((double)iNumberOfRecords / iMaximumNumberOfLabels) + 0.5);
			if (iCustomStep < 1)
				iCustomStep = 1;
			axsDates.NumericScale.CustomStep = iCustomStep;
			//Assign the lables to the graph
			string szXAxisLabel = "";
			DateTime dtStartDate = DateTime.ParseExact(Session["StartDate"].ToString(), "dd/MM/yyyy", null);
			for(int iIndex = 0; iIndex < iNumberOfRecords; iIndex++)
			{
				//Start with the selected day and then add days to determine the next date to display
				szXAxisLabel = (dtStartDate.AddDays(iIndex*iCustomStep)).ToString("dd/MM");
				axsDates.Labels.Add(szXAxisLabel);
			}
		}
		#endregion
		
		
		
		#region Rainfall and Soil Moisture Chart
		//-------------------------------------------------------------------------
		//
		//-------------------------------------------------------------------------
		private void DisplayRainfallAndSoilMoistureChart(DataTable dtMetData, DataTable dtETData)
		{
			//Initialise the chart
			cscRainfall_SoilMoisture.Visible = true;
			cscRainfall_SoilMoisture.Labels[0].Text = "Rainfall and Soil Moisture";
			Chart chtCustomChart;
			chtCustomChart = cscRainfall_SoilMoisture.Charts.GetAt( 0 );
			chtCustomChart.Series.Clear();
			ChartWall backWall = chtCustomChart.Wall(ChartWallType.Back);
			backWall.FillEffect.SetGradient(Xceed.Chart.GLCore.GradientStyle.Horizontal, Xceed.Chart.GLCore.GradientVariant.Variant1, Color.Blue, Color.Red);
			
			
			//Initialise the series and fill with data from the passed in DataTable
			LineSeries lsCriticalValue;
			lsCriticalValue = ( LineSeries )chtCustomChart.Series.Add( SeriesType.Line );
			lsCriticalValue.Name = "Target Saturation Range";
			lsCriticalValue.DataLabels.Mode = DataLabelsMode.None;
			lsCriticalValue.LineBorder.Color = Color.Orange;
			lsCriticalValue.LineBorder.Width = 2;
			lsCriticalValue.Values.FillFromDataTable(ReturnConstValueDataTable(33, dtETData.Rows.Count+2), "Value");
			lsCriticalValue.DisplayOnAxis(0, false);
			lsCriticalValue.DisplayOnAxis(1, true);
			//Initialise the series and fill with data from the passed in DataTable
			LineSeries lsIdealSaturation;
			lsIdealSaturation = ( LineSeries )chtCustomChart.Series.Add( SeriesType.Line );
			lsIdealSaturation.Name = "";
			lsIdealSaturation.Legend.Mode = Xceed.Chart.Core.SeriesLegendMode.None;
			lsIdealSaturation.DataLabels.Mode = DataLabelsMode.None;
			lsIdealSaturation.LineBorder.Color = Color.Orange;
			lsIdealSaturation.LineBorder.Width = 2;
			lsIdealSaturation.Values.FillFromDataTable(ReturnConstValueDataTable(75, dtETData.Rows.Count+2), "Value");
			lsIdealSaturation.DisplayOnAxis(0, false);
			lsIdealSaturation.DisplayOnAxis(1, true);
			
			
			
			//Initialise the series and fill with data from the passed in DataTable
			BarSeries bsCustomBarSeries;
			bsCustomBarSeries = ( BarSeries )chtCustomChart.Series.Add( SeriesType.Bar );
			bsCustomBarSeries.Name = "Rainfall (mm)";
			bsCustomBarSeries.BarFillEffect.Color = Color.Blue;
			bsCustomBarSeries.DataLabels.Mode = DataLabelsMode.None;
			bsCustomBarSeries.Values.FillFromDataTable(dtMetData, "rain");

			//Initialise the series and fill with data from the passed in DataTable
			BarSeries bsCustomBarSeriesTwo;
			bsCustomBarSeriesTwo = ( BarSeries )chtCustomChart.Series.Add( SeriesType.Bar );
			bsCustomBarSeriesTwo.Name = "Watering (mm)";
			bsCustomBarSeriesTwo.BarFillEffect.Color = Color.Yellow;
			bsCustomBarSeriesTwo.DataLabels.Mode = DataLabelsMode.None;
			bsCustomBarSeriesTwo.Values.FillFromDataTable(WateringCalculations.ReturnFullWateringDataTable(dtMetData, (DataTable)ViewState["WateringDataTable"]), "rain");
			
			//Initialise the series and fill with data from the passed in DataTable
			AreaSeries asCustomAreaSeries;
			asCustomAreaSeries = ( AreaSeries )chtCustomChart.Series.Add( SeriesType.Area );
			asCustomAreaSeries.Name = "Soil Moisture (%)";
			asCustomAreaSeries.DataLabels.Mode = DataLabelsMode.None;
			asCustomAreaSeries.AreaFillEffect.Color = Color.Green;
			asCustomAreaSeries.Values.FillFromDataTable(dtETData, "PercentFull");
			asCustomAreaSeries.DisplayOnAxis(0, false);
			asCustomAreaSeries.DisplayOnAxis(1, true);
			

			//Configure primary (left) Y-Axis to display title, etc
			Axis axsRainFall = chtCustomChart.Axis(0);
			axsRainFall.Title = "Rainfall (mm)";
			axsRainFall.TitleText.Orientation = 90;
			axsRainFall.TitleText.OffsetX = -25;
			axsRainFall.Text.OffsetX = -5;
			//Configure primary (left) Y-Axis to display title, etc
			Axis axsSoilMoisture = chtCustomChart.Axis(1);
			axsSoilMoisture.Title = "Soil Moisture (%)";
			axsSoilMoisture.TitleText.Orientation = 90;
			axsSoilMoisture.TitleText.OffsetX = 25;
			axsSoilMoisture.Text.OffsetX = 5;
			//Configure the primary (bottom) X-Axis to display the dates associated with the data.
			Axis axsDates = chtCustomChart.Axis(2);
			axsDates.Labels.Clear();
			axsDates.TitleText.OffsetY = 25;
			axsDates.Title = "Date (DD/MM)";
			axsDates.Text.OffsetY = 5; 
			//Figure out the number of labels to display with out cluttering the graph
			int iMaximumNumberOfLabels = 10;
			int iNumberOfRecords = dtETData.Rows.Count;
			//Calculate the mumber of records between each label by dividing the number
			//of records by the maximum number of labels and then round the result
			//by adding 0.5
			int iCustomStep  = (int)(((double)iNumberOfRecords / iMaximumNumberOfLabels) + 0.5);
			if (iCustomStep < 1)
				iCustomStep = 1;
			axsDates.NumericScale.CustomStep = iCustomStep;
			axsDates.NumericScale.AutoMax = true;
			//Assign the lables to the graph
			string szXAxisLabel = "";
			DateTime dtStartDate = DateTime.ParseExact(Session["StartDate"].ToString(), "dd/MM/yyyy", null);
			for(int iIndex = 0; iIndex < iNumberOfRecords; iIndex++)
			{
				//Start with the selected day and then add days to determine the next date to display
				szXAxisLabel = dtStartDate.AddDays(iIndex * iCustomStep).ToString("dd/MM");
				axsDates.Labels.Add(szXAxisLabel);
			}
		}
		#endregion
		
		
		//-------------------------------------------------------------------------
		}//END CLASS
	}//END NAMESPACE
