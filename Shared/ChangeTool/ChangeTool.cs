using System;
using CSGeneral;
using VBGeneral;

namespace ChangeTool
	{
	// ------------------------------------------
	// This class converts an APSIM file from one 
	// version to the 'current' version
	// ------------------------------------------
	public class APSIMChangeTool
		{
		private static int CurrentVersion = 2;	   
		private delegate void UpgraderDelegate(APSIMData Data);

		// ------------------------------------------
		// Upgrade the specified data
		// to the 'current' version
		// ------------------------------------------
		public static void Upgrade(APSIMData Data)
			{
			// Get version number of data.
			int DataVersion = 1;
			if (Data.AttributeExists("version"))
				DataVersion = Convert.ToInt32(Data.Attribute("version"));

			// Upgrade from version 1 to 2.
			if (DataVersion == 1)
				Upgrade(Data, new UpgraderDelegate(UpdateToVersion2));            

			// All finished upgrading - write version number out.
			Data.SetAttribute("version", CurrentVersion.ToString());
			}


		// ------------------------------------------------
		// Upgrade the data using the specified 'upgrader'
		// ------------------------------------------------
		private static void Upgrade(APSIMData Data, UpgraderDelegate Upgrader)
			{
			foreach (APSIMData Child in Data.get_Children(null))
				{
				if (Child.Type.ToLower() == "area" 
					|| Child.Type.ToLower() == "folder"
					|| Child.Type.ToLower() == "simulation")
					Upgrade(Child, Upgrader);  // recursion
				else
					Upgrader(Child);
				}
			}
               

		// -----------------------------
		// Upgrade the data to version 2.
		// -----------------------------
		private static void UpdateToVersion2(APSIMData Data)
			{
			if (Data.Type.ToLower() == "soil")
				{
				Soil MySoil = new Soil(Data);
				MySoil.UpgradeToVersion2();
				}
			}	

	


		}
	}
