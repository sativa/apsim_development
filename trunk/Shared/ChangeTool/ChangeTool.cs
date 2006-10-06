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
		private static int CurrentVersion = 5;	   
		private delegate void UpgraderDelegate(APSIMData Data);

		// ------------------------------------------
		// Upgrade the specified data
		// to the 'current' version
		// ------------------------------------------
        public static void Upgrade(APSIMData Data)
            {
            if (Data != null)
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

                // Upgrade from version 3 to 4.
                if (DataVersion < 4)
                    Upgrade(Data, new UpgraderDelegate(UpdateToVersion4));

                // Upgrade from version 4 to 5.
                if (DataVersion < 5)
                    Upgrade(Data, new UpgraderDelegate(UpdateToVersion5));

                // All finished upgrading - write version number out.
                Data.SetAttribute("version", CurrentVersion.ToString());
                }
            }


		// ------------------------------------------------
		// Upgrade the data using the specified 'upgrader'
		// ------------------------------------------------
		private static void Upgrade(APSIMData Data, UpgraderDelegate Upgrader)
			{
            string[] ChildNames = Data.ChildNames(null);
			foreach (string ChildName in ChildNames)
				{
                APSIMData Child = Data.Child(ChildName);
                if (Child != null)
                    {
    		        if (Child.Type.ToLower() == "area" 
					   || Child.Type.ToLower() == "folder"
				   	   || Child.Type.ToLower() == "simulation"
                       || Child.Type.ToLower() == "manager"
                       || Child.Type.ToLower() == "outputfile")
				   	   Upgrade(Child, Upgrader);  // recursion
				    else
					   Upgrader(Child);
                    }
				}
			}
               

		// -----------------------------
		// Upgrade the data to version 2.
		// -----------------------------
		private static void UpdateToVersion2(APSIMData Data)
			{
			if (Data.Type.ToLower() == "soil")
				{
                CSGeneral.Soil MySoil = new CSGeneral.Soil(Data);
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
                CSGeneral.Soil MySoil = new CSGeneral.Soil(Data);
				MySoil.UpgradeToVersion3();
				}
			else if (Data.Type.ToLower() == "sample")
				{
				SoilSample MySample = new SoilSample(Data);
				MySample.UpgradeToVersion3();
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

        // -----------------------------
        // Upgrade the data to version 4.
        // -----------------------------
        private static void UpdateToVersion4(APSIMData Data)
            {
            if (Data.Type.ToLower() == "rule")
                {
                foreach (APSIMData category in Data.get_Children("category"))
                    {
                    foreach (APSIMData property in category.get_Children("property"))
                        {
                        APSIMData NewProperty = new APSIMData(property.Name, "");
                        NewProperty.SetAttribute("type", property.Attribute("type"));

                        if (property.AttributeExists("croppropertyname"))
                            NewProperty.SetAttribute("croppropertyname", property.Attribute("croppropertyname"));

                        if (property.AttributeExists("listvalues"))
                            NewProperty.SetAttribute("listvalues", property.Attribute("listvalues"));

                        NewProperty.SetAttribute("description", property.Attribute("description"));
                        NewProperty.Value = property.Attribute("value");
                        category.Delete(property.Name);
                        category.Add(NewProperty);
                        }
                    }
                }
            }

        // -----------------------------
        // Upgrade the data to version 5.
        // -----------------------------
        private static void UpdateToVersion5(APSIMData Data)
            {
            if (Data.Type.ToLower() == "outputfiledescription")
                {
                APSIMData outputfiledescription = Data;
                if (outputfiledescription.Attribute("shortcut") == "")
                    {
                    string[] VGNames = outputfiledescription.ChildNames("variables");
                    foreach (string VGName in VGNames)
                        {
                        APSIMData VariablesGroup = outputfiledescription.Child(VGName);

                        string[] VNames = VariablesGroup.ChildNames("variable");
                        foreach (string VName in VNames)
                            {
                            APSIMData Variable = VariablesGroup.Child(VName);
                            if (Variable.Attribute("name") != Variable.Attribute("variablename"))
                                Variable.Name = Variable.Attribute("variablename") + " as " + Variable.Attribute("name");
                            if (Variable.Attribute("arrayspec").Trim() != "")
                                Variable.Name += Variable.Attribute("arrayspec");
                            string ComponentName = Variable.Attribute("module");
                            if (ComponentName.ToLower() == "global")
                                ComponentName = "";
                            if (ComponentName != "")
                                Variable.Name = ComponentName + "." + Variable.Name;
                            Variable.SetAttribute("array", "?");
                            Variable.DeleteAttribute("ModuleType");
                            Variable.DeleteAttribute("arrayspec");
                            Variable.DeleteAttribute("module");
                            Variable.DeleteAttribute("variablename");
                            }
                        VariablesGroup.Name = outputfiledescription.Name;
                        VariablesGroup.Parent.Parent.Add(VariablesGroup);
                        }

                    string[] EGNames = outputfiledescription.ChildNames("events");
                    foreach (string EGName in EGNames)
                        {
                        APSIMData EventsGroup = outputfiledescription.Child(EGName);
                        string[] EventNames = EventsGroup.ChildNames("event");
                        foreach (string EventName in EventNames)
                            {
                            APSIMData Event = EventsGroup.Child(EventName);
                            string ComponentName;
                            string NewEventName;

                            if (Event.Name.IndexOf('.') != -1 )
                                {
                                ComponentName = Event.Name.Substring(0, Event.Name.IndexOf('.'));
                                NewEventName = Event.Name.Substring(Event.Name.IndexOf('.')+1);
                                }
                            else
                                {
                                NewEventName = Event.Name;
                                ComponentName = Event.Attribute("module");
                                }

                            if (ComponentName.ToLower() == "global")
                                ComponentName = "";

                            if (ComponentName != "")
                                Event.Name = ComponentName + "." + NewEventName;
                            else
                                Event.Name = NewEventName;

                            Event.DeleteAttribute("ModuleType");
                            Event.DeleteAttribute("module");
                            Event.DeleteAttribute("eventname");
                            }
                        EventsGroup.Name = outputfiledescription.Name + " Events";
                        EventsGroup.Parent.Parent.Add(EventsGroup);
                        }
                    }
                else
                    {
                    APSIMData VariablesGroup = Data.Parent.Add(new APSIMData("variables", outputfiledescription.Attribute("shortcut")));
                    VariablesGroup.SetAttribute("shortcut", outputfiledescription.Attribute("shortcut"));

                    APSIMData EventsGroup = Data.Parent.Add(new APSIMData("events", outputfiledescription.Attribute("shortcut") + " Events"));
                    EventsGroup.SetAttribute("shortcut", outputfiledescription.Attribute("shortcut") + " Events");
                    }
                outputfiledescription.Parent.Delete(outputfiledescription.Name);
                }
            }

            	


		}
	}
