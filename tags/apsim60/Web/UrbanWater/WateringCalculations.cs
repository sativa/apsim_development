using System;
using System.Data;
using System.Web;
using System.Web.UI;

namespace UrbanWater
	{
	/// <summary>
	/// Summary description for WateringCalculations.
	/// </summary>
	public class WateringCalculations
		{
		public WateringCalculations()
			{
			}
		//---------------------------------------------------------------------
		//Returns the total amount of watering between the selected dates
		//---------------------------------------------------------------------
		public static double ReturnTotalWatering(DataTable dtWatering)
			{

			DateTime dtEndDate = DateTime.ParseExact(HttpContext.Current.Session["EndDate"].ToString(), "dd/MM/yyyy", null);
			DateTime dtStartDate = DateTime.ParseExact(HttpContext.Current.Session["StartDate"].ToString(), "dd/MM/yyyy", null);
			DateTime dtWateringDate;
			double dWatering = 0;
			foreach(DataRow drWatering in dtWatering.Rows)
				{
				dtWateringDate = DateTime.ParseExact(drWatering["Date"].ToString(), "dd/MM/yyyy", null);
				if(dtWateringDate <= dtEndDate && dtWateringDate >= dtStartDate)
					{
					dWatering = dWatering + Convert.ToDouble(drWatering["Amount"]);
					}
				}		
			return dWatering;
			}
		//---------------------------------------------------------------------
		//Adds the watering events as rainfall events to the MetData DataTable
		//This is used so that the calculations that find the stress level,
		//evapotranspiration etc use data that contain the correct amount of 
		//water that the plants have received
		//---------------------------------------------------------------------
		public static DataTable AddWateringToMetData(DataTable MetData, DataTable dtWatering)
			{
			DataTable dtWateringAndRainfall = MetData.Copy();
			double dRainfall = 0;
			foreach(DataRow drWatering in dtWatering.Rows)
				{
				foreach(DataRow drWateringAndRainfall in dtWateringAndRainfall.Rows)
					{
					if((DateTime)drWateringAndRainfall["Date"] == DateTime.ParseExact(drWatering["Date"].ToString(), "dd/MM/yyyy", null))
						{
						dRainfall =Convert.ToDouble(drWateringAndRainfall["rain"]);
						dRainfall = dRainfall + Convert.ToInt32(drWatering["Amount"]);
						drWateringAndRainfall["rain"] = dRainfall;
						dRainfall = 0;
						}
					}
				}
				return dtWateringAndRainfall;
			}
		//---------------------------------------------------------------------
		//Returns a Watering DataTable with the same number of records as the
		//MetData DataTable.  To achieve this blank records are entered where
		//no watering occured.  This is used to display the watering information 
		//on the graphs
		//---------------------------------------------------------------------
		public static DataTable ReturnFullWateringDataTable(DataTable MetData, DataTable dtWatering)
			{
			DataTable dtFullWatering = new DataTable();
			dtFullWatering = MetData.Copy();
			foreach(DataRow drFullWatering in dtFullWatering.Rows)
				{
				drFullWatering["rain"] = 0.0;
				foreach(DataRow drWatering in dtWatering.Rows)
					{
					if((DateTime)drFullWatering["Date"] == DateTime.ParseExact(drWatering["Date"].ToString(), "dd/MM/yyyy", null))
						{
						drFullWatering["rain"] = Convert.ToDouble(drWatering["Amount"]);;
						}
					}
				}
			return dtFullWatering;
			}
		//---------------------------------------------------------------------
		//Calculates and returns the amount of watering needed to return the soil 
		//to the specified saturation level as stated in the Web.Config file
		//---------------------------------------------------------------------	
		public static double ReturnWateringNeeded(DataTable ETData, double dSoilPAWC, double dRootingDepth)
			{
			double dWateringNeeded = 0.0;
			DataRow drLastETDay = ETData.Rows[ETData.Rows.Count-1];
			System.Collections.Specialized.NameValueCollection settings = (System.Collections.Specialized.NameValueCollection)System.Configuration.ConfigurationSettings.GetConfig("CSIRO/UrbanWater");
			double dMaxPAWC = dSoilPAWC*dRootingDepth;
			double dPAWC = Convert.ToDouble(drLastETDay["PAWC"].ToString());
			double dAcutalET = Convert.ToDouble(drLastETDay["ActualET"].ToString());
			double dRequiredSaturation = Convert.ToDouble(settings["RequiredSaturation"]);
			dWateringNeeded = ((dRequiredSaturation*dMaxPAWC) - dPAWC + dAcutalET);
			if(dWateringNeeded < 0)
				{
				dWateringNeeded = 0.0;
				}
			else
				{
				dWateringNeeded = Math.Round(dWateringNeeded, 0);
				}
			return dWateringNeeded;
			}
		//-------------------------------------------------------------------------
		}//END OF CLASS
	}//END OF NAMESPACE
