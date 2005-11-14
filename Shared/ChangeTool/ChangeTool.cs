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
			if (DataVersion < 2)
				Upgrade(Data, new UpgraderDelegate(UpdateToVersion2));            

			// Upgrade from version 2 to 3.
			if (DataVersion < 3)
				Upgrade(Data, new UpgraderDelegate(UpdateToVersion3));

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
				double[] thickness = MySoil.Thickness;
				MySoil.UpgradeToVersion2();
				}
			else if (Data.Type.ToLower() == "registrations")
				Data.Name = "global";
			else if (Data.Type.ToLower() == "outputfile")
				{
				APSIMData OutputFileDescription = null;
				foreach (APSIMData Child in Data.get_Children(null))
					{
					if (Child.Type.ToLower() == "outputfiledescription")
						OutputFileDescription = Child;
					}
                if (OutputFileDescription != null)
					{
					APSIMData Variables = OutputFileDescription.Child("variables");
					if (Variables != null)
						RemoveDataOutsidePaddock(Variables, "variable");
					APSIMData Events = OutputFileDescription.Child("events");
					if (Events != null)
						RemoveDataOutsidePaddock(Events, "event");
					}
				}
			}	

		// -----------------------------
		// Upgrade the data to version 3.
		// -----------------------------
		private static void UpdateToVersion3(APSIMData Data)
			{
			if (Data.Type.ToLower() == "soil")
				{
				Soil MySoil = new Soil(Data);
				MySoil.UpgradeToVersion3();
				}
			}	


		// ------------------------------------------
		// Remove all 'data outside paddock' from all 
		// children of specified data.
		// ------------------------------------------
		private static void RemoveDataOutsidePaddock(APSIMData Parent, string ChildType)
			{
			foreach (APSIMData Child in Parent.get_Children(ChildType))
				{
				if (Child.Attribute("module").ToLower() == "data outside paddock")
					Child.SetAttribute("module", "global");
				if (Child.Name.ToLower().IndexOf("data outside paddock.") == 0)
					Child.Name = Child.Name.Remove(0, 21);
				}
			}


		}
	}
