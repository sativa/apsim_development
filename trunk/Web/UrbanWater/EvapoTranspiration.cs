using System;
using System.Data;
namespace ET
	{
	public struct ETParams
		{
		public float CropFactor;						// 0-1
		public float CritPAWC;							// %
		public float RootingDepth;					// mm
		public float PAWC;								// %
		public float InitialPAW;							// %
		}

	/// <summary>
	/// Class for calculating ET between 2 dates.
	/// </summary>
	public class EvapoTranspiration
		{

		// -----------------------------------------------------
		// Calculate ET for all rows in the specified met data.
		// Assumes that the met data has columns called:
		//			date, rain, evap
		// -----------------------------------------------------
		static public DataTable Calc(DataTable MetData, double MaxPAWC, double InitPAWC, double CritPAWC, double CropFactor)
			{
			DataTable ETData = new DataTable("ETData");
					
			// create all columns
			ETData.Columns.Add(new DataColumn("Date", System.Type.GetType("System.DateTime")));
			ETData.Columns.Add(new DataColumn("Stress", System.Type.GetType("System.Double")));
			ETData.Columns.Add(new DataColumn("ActualET", System.Type.GetType("System.Double")));
			ETData.Columns.Add(new DataColumn("CumET", System.Type.GetType("System.Double")));
			ETData.Columns.Add(new DataColumn("PercentFull", System.Type.GetType("System.Double")));
			ETData.Columns.Add(new DataColumn("PAWC", System.Type.GetType("System.Double")));

			// Loop through all records of met data and add a row to our ET table.
			
			double PAWC = InitPAWC;
			double ET = 0.0;
			double CumET = 0.0;
			for (int Row = 0; Row < MetData.Rows.Count; Row++)
				{
				DataRow Today = MetData.Rows[Row];
				DateTime Day = Convert.ToDateTime(Today["Date"]);
				double Evap = Convert.ToDouble(Today["Evap"]);
				double Rain = Convert.ToDouble(Today["Rain"]);
				
				DataRow NewRow = ETData.NewRow();

				// do our water balance.
				PAWC = Math.Min(PAWC - ET + Rain, MaxPAWC); 
				
				//ADDED BY STEPHEN
				PAWC = Math.Max(PAWC, 0);

				double Stress = 1.0;
				if (PAWC < (CritPAWC*MaxPAWC))
					Stress = PAWC / (CritPAWC*MaxPAWC);
				double PET = Evap * CropFactor; 
				ET = PET * Stress;
				CumET += ET;
				double PercentFull = PAWC / MaxPAWC * 100.0;

				// Store results in our ET data table.
				NewRow["Date"] = Today["date"];
				NewRow["Stress"] = Stress;
				NewRow["ActualET"] = ET;
				NewRow["CumET"] = CumET;
				NewRow["PercentFull"] = PercentFull;
				NewRow["PAWC"] = PAWC;
				ETData.Rows.Add(NewRow);
				}
			return ETData;
			}



		}
	}
