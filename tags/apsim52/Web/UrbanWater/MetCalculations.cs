using System;
using System.Data;
using ET;
using CSGeneral;
using System.IO;
using System.Web;

namespace UrbanWater
	{
	/// <summary>
	/// Summary description for MetCalculations.
	/// </summary>
	public class MetCalculations
		{
		public MetCalculations()
			{}
		//---------------------------------------------------------------------
		// Returns a datatable containing the data extracted from the met file
		// for the selected location between the selected date and the current
		// date.
		//---------------------------------------------------------------------
		public static DataTable ReturnMetData(string szFileName)
			{
			APSIMInputFile InputFile = new APSIMInputFile();
			string szFileLocation = HttpContext.Current.Server.MapPath("/MetFiles/")+szFileName;
			DateTime dtEndDate = DateTime.ParseExact(HttpContext.Current.Session["EndDate"].ToString(), "dd/MM/yyyy", null);
			DateTime dtStartDate = DateTime.ParseExact(HttpContext.Current.Session["StartDate"].ToString(), "dd/MM/yyyy", null);
			InputFile.ReadFromFile(szFileLocation, dtStartDate, dtEndDate);
			return InputFile.Data;
			}
		//---------------------------------------------------------------------
		// Returns a dataset containing the data caculated from the passed in
		// met data.  Calculations are undertaken using values determined from
		// crop and soil type selections.
		//---------------------------------------------------------------------
		public static DataTable ReturnCalculationData(DataTable MetData, double dPAWC, double dRootingDepth, double dCropFactor)
			{	
			System.Collections.Specialized.NameValueCollection settings = (System.Collections.Specialized.NameValueCollection)System.Configuration.ConfigurationSettings.GetConfig("CSIRO/UrbanWater");
			
			double MaxPAWC = dPAWC*dRootingDepth;
			double InitPAWC = (Convert.ToDouble(settings["InitialPAWCPercent"]) * MaxPAWC);
			
			double CritPAWC = Convert.ToDouble(settings["CriticalPAWC"]);
			return EvapoTranspiration.Calc(MetData, MaxPAWC, InitPAWC, CritPAWC, dCropFactor);
			}
		//---------------------------------------------------------------------
		//Returns the last date from the met file
		//---------------------------------------------------------------------
		public static DateTime ReturnLastDateInMetFile(string szFileName)
			{
			DateTime dtLastDate = new DateTime(1, 1, 1);
			string szFileLocation = HttpContext.Current.Server.MapPath("/MetFiles/")+szFileName;
			StreamReader srMetFile = File.OpenText(szFileLocation);
			string szTempDate;
			string szLastDate = "";
			while((szTempDate = srMetFile.ReadLine()) != null)
				{
				szLastDate = szTempDate;
				}
			if(szLastDate.Length > 7)
				{
				string szYear = szLastDate.Substring(0,4);
				string szDay = szLastDate.Substring(4, 4);
				szDay = szDay.Replace(" ", "");
				szYear = szYear.Replace(" ", "");
				dtLastDate = dtLastDate.AddYears(System.Convert.ToInt32(szYear)-1);
				dtLastDate = dtLastDate.AddDays(System.Convert.ToInt32(szDay)-1);
				}
			return dtLastDate;
			}
		//---------------------------------------------------------------------
		//Returns the total rainfall from the passed in MetData datatable
		//---------------------------------------------------------------------
		public static double ReturnTotalRainfall(DataTable MetData)
			{
			double dRain = 0;
			foreach(DataRow drMetData in MetData.Rows)
				{
				dRain = dRain + Convert.ToDouble(drMetData["Rain"]);
				}
			dRain = Math.Round(dRain, 0);
			return dRain;
			}
		//---------------------------------------------------------------------
		//Returns the total evaporation from the passed in MetData datatable
		//---------------------------------------------------------------------
		public static double ReturnTotalEvaporation(DataTable MetData)
			{
			double dEvaporation = 0;
			foreach(DataRow drMetData in MetData.Rows)
				{
				dEvaporation = dEvaporation + Convert.ToDouble(drMetData["Evap"]);
				}
			dEvaporation = Math.Round(dEvaporation, 0);
			return dEvaporation;
			}
		//-------------------------------------------------------------------------
		}//END CLASS
	}//END NAMESPACE