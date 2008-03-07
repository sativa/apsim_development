using System;
using CSGeneral;
using VBGeneral;
using Soils;
using System.Xml;

namespace ApsimFile
	{
	// ------------------------------------------
	// This class converts an APSIM file from one 
	// version to the 'current' version
	// ------------------------------------------
	public class APSIMChangeTool
		{
		public static int CurrentVersion = 13;	   
		private delegate void UpgraderDelegate(XmlNode Data, Configuration Config);

		// ------------------------------------------
		// Upgrade the specified data
		// to the 'current' version. Returns true
        // if something was upgraded.
		// ------------------------------------------
        public static bool Upgrade(XmlNode Data)
            {
            if (Data != null)
                {
                Configuration Config = new Configuration("ApsimUI");

                // Get version number of data.
                int DataVersion = 1;
                if (XmlHelper.Attribute(Data, "version") != "")
                    DataVersion = Convert.ToInt32(XmlHelper.Attribute(Data, "version"));

                // Upgrade from version 1 to 2.
                if (DataVersion < 2)
                    Upgrade(Data, new UpgraderDelegate(UpdateToVersion2), Config);

                // Upgrade from version 2 to 3.
                if (DataVersion < 3)
                    Upgrade(Data, new UpgraderDelegate(UpdateToVersion3), Config);

                // Upgrade from version 3 to 4.
                if (DataVersion < 4)
                    Upgrade(Data, new UpgraderDelegate(UpdateToVersion4), Config);

                // Upgrade from version 4 to 5.
                if (DataVersion < 5)
                    Upgrade(Data, new UpgraderDelegate(UpdateToVersion5), Config);

                // Upgrade from version 5 to 6.
                if (DataVersion < 6)
                    Upgrade(Data, new UpgraderDelegate(UpdateToVersion6), Config);

                // Upgrade from version 6 to 7.
                if (DataVersion < 7)
                    Upgrade(Data, new UpgraderDelegate(UpdateToVersion7), Config);

                // Upgrade from version 7 to 8.
                if (DataVersion < 8)
                    Upgrade(Data, new UpgraderDelegate(UpdateToVersion8), Config);

                // Upgrade from version 8 to 9.
                if (DataVersion < 9)
                    Upgrade(Data, new UpgraderDelegate(UpdateToVersion9), Config);

                // Upgrade from version 9 to 10.
                if (DataVersion < 10)
                    Upgrade(Data, new UpgraderDelegate(UpdateToVersion10), Config);

                // Upgrade from version 10 to 11.
                if (DataVersion < 11)
                    Upgrade(Data, new UpgraderDelegate(UpdateToVersion11), Config);

                // Upgrade from version 11 to 12.
                if (DataVersion < 12)
                    Upgrade(Data, new UpgraderDelegate(UpdateToVersion12), Config);

                // Upgrade from version 12 to 13.
                if (DataVersion < 13)
                    Upgrade(Data, new UpgraderDelegate(UpdateToVersion13), Config);

                // All finished upgrading - write version number out.
                XmlHelper.SetAttribute(Data, "version", CurrentVersion.ToString());
                return (DataVersion != CurrentVersion);
                }
            else
                return false;
            }


		// ------------------------------------------------
		// Upgrade the data using the specified 'upgrader'
		// ------------------------------------------------
		private static void Upgrade(XmlNode Data, UpgraderDelegate Upgrader, Configuration Config)
			{
			foreach (XmlNode Child in XmlHelper.ChildNodes(Data, ""))
				{
                Upgrader(Child, Config);
                if (Child.Name.ToLower() == "area"
                   || Child.Name.ToLower() == "folder"
                   || Child.Name.ToLower() == "simulation"
                   || Child.Name.ToLower() == "manager"
                   || Child.Name.ToLower() == "outputfile"
                   || Child.Name.ToLower() == "graph"
                   || Child.Name.ToLower() == "data"
                   || Child.Name.ToLower() == "paddockmanager")
                    Upgrade(Child, Upgrader, Config);  // recurse
				}
			}
               

		// -----------------------------
		// Upgrade the data to version 2.
		// -----------------------------
		private static void UpdateToVersion2(XmlNode Data, Configuration Config)
			{
			if (XmlHelper.Type(Data).ToLower() == "soil")
				{
                Soil MySoil = new Soil(Data);
				double[] thickness = MySoil.Thickness;
				MySoil.UpgradeToVersion2();
				}
			else if (XmlHelper.Type(Data).ToLower() == "registrations")
				XmlHelper.SetName(Data, "global");
			else if (XmlHelper.Type(Data).ToLower() == "outputfile")
				{
				XmlNode OutputFileDescription = null;
				foreach (XmlNode Child in XmlHelper.ChildNodes(Data, ""))
					{
					if (XmlHelper.Type(Child).ToLower() == "outputfiledescription")
						OutputFileDescription = Child;
					}
                if (OutputFileDescription != null)
					{
					XmlNode Variables = XmlHelper.Find(OutputFileDescription, "variables");
					if (Variables != null)
						RemoveDataOutsidePaddock(Variables, "variable");
                    XmlNode Events = XmlHelper.Find(OutputFileDescription, "events");
					if (Events != null)
						RemoveDataOutsidePaddock(Events, "event");
					}
				}
			}	

		// -----------------------------
		// Upgrade the data to version 3.
		// -----------------------------
        private static void UpdateToVersion3(XmlNode Data, Configuration Config)
			{
			if (XmlHelper.Type(Data).ToLower() == "soil")
				{
                Soil MySoil = new Soil(Data);
				MySoil.UpgradeToVersion3();
				}
			else if (XmlHelper.Type(Data).ToLower() == "sample")
				{
                Soil MySoil = new Soil(Data.ParentNode);
				SoilSample MySample = new SoilSample(Data, MySoil);
				MySample.UpgradeToVersion3();
				}
			}	


		// ------------------------------------------
		// Remove all 'data outside paddock' from all 
		// children of specified data.
		// ------------------------------------------
		private static void RemoveDataOutsidePaddock(XmlNode Parent, string ChildType)
			{
			foreach (XmlNode Child in XmlHelper.ChildNodes(Parent, ChildType))
				{
				if (XmlHelper.Attribute(Child, "module").ToLower() == "data outside paddock")
					XmlHelper.SetAttribute(Child, "module", "global");
				if (XmlHelper.Name(Child).ToLower().IndexOf("data outside paddock.") == 0)
                    XmlHelper.SetName(Child, Child.Name.Remove(0, 21));
				}
			}

        // -----------------------------
        // Upgrade the data to version 4.
        // -----------------------------
        private static void UpdateToVersion4(XmlNode Data, Configuration Config)
            {
            if (XmlHelper.Type(Data).ToLower() == "rule")
                {
                foreach (XmlNode category in XmlHelper.ChildNodes(Data, "category"))
                    {
                    foreach (XmlNode property in XmlHelper.ChildNodes(category, "property"))
                        {
                        XmlNode NewProperty = XmlHelper.CreateNode(category.OwnerDocument, XmlHelper.Name(property), "");
                        XmlHelper.SetAttribute(NewProperty, "type", XmlHelper.Attribute(property, "type"));

                        if (XmlHelper.Attribute(property, "croppropertyname") != "")
                            XmlHelper.SetAttribute(NewProperty, "croppropertyname", XmlHelper.Attribute(property, "croppropertyname"));

                        if (XmlHelper.Attribute(property, "listvalues") != "")
                            XmlHelper.SetAttribute(NewProperty, "listvalues", XmlHelper.Attribute(property, "listvalues"));

                        XmlHelper.SetAttribute(NewProperty, "description", XmlHelper.Attribute(property, "description"));
                        NewProperty.InnerText = XmlHelper.Attribute(property, "value");
                        category.ReplaceChild(NewProperty, property);
                        }
                    }
                }
            }

        // -----------------------------
        // Upgrade the data to version 5.
        // -----------------------------
        private static void UpdateToVersion5(XmlNode Data, Configuration Config)
            {
            if (XmlHelper.Attribute(Data, "shortcut") != "" && XmlHelper.Attribute(Data, "name") == "")
                {
                XmlHelper.SetName(Data, XmlHelper.Attribute(Data, "shortcut"));
                }

            // get rid of <filename>
            if (XmlHelper.Type(Data).ToLower() == "outputfile")
                {
                XmlNode FileNameNode = XmlHelper.Find(Data, "filename");
                if (FileNameNode != null)
                    Data.RemoveChild(FileNameNode);
                }

            if (XmlHelper.Type(Data).ToLower() == "outputfiledescription")
                {
                XmlNode outputfiledescription = Data;
                if (XmlHelper.Attribute(outputfiledescription, "shortcut") == "")
                    {
                    string[] VGNames = XmlHelper.ChildNames(outputfiledescription, "variables");
                    foreach (string VGName in VGNames)
                        {
                        XmlNode VariablesGroup = XmlHelper.Find(outputfiledescription, VGName);

                        string[] VNames = XmlHelper.ChildNames(VariablesGroup, "variable");
                        foreach (string VName in VNames)
                            {
                            XmlNode Variable = XmlHelper.Find(VariablesGroup, VName);
                            if (XmlHelper.Attribute(Variable, "name") != XmlHelper.Attribute(Variable, "variablename"))
                                XmlHelper.SetName(Variable, XmlHelper.Attribute(Variable, "variablename") + " as " + XmlHelper.Attribute(Variable, "name"));
                            if (XmlHelper.Attribute(Variable, "arrayspec").Trim() != "")
                                XmlHelper.SetName(Variable, XmlHelper.Name(Variable) + XmlHelper.Attribute(Variable, "arrayspec"));
                            string ComponentName = XmlHelper.Attribute(Variable, "module");
                            if (ComponentName.ToLower() == "global")
                                ComponentName = "";
                            if (ComponentName != "" && XmlHelper.Attribute(Variable, "ModuleType") != "soil")
                                XmlHelper.SetName(Variable, ComponentName + "." + Variable.Name);
                            XmlHelper.SetAttribute(Variable, "array", "?");
                            XmlHelper.DeleteAttribute(Variable, "ModuleType");
                            XmlHelper.DeleteAttribute(Variable, "arrayspec");
                            XmlHelper.DeleteAttribute(Variable, "module");
                            XmlHelper.DeleteAttribute(Variable, "variablename");
                            }
                        XmlHelper.SetName(VariablesGroup, outputfiledescription.Name);
                        VariablesGroup.ParentNode.ParentNode.AppendChild(VariablesGroup);
                        }

                    string[] EGNames = XmlHelper.ChildNames(outputfiledescription, "events");
                    foreach (string EGName in EGNames)
                        {
                        XmlNode EventsGroup = XmlHelper.Find(outputfiledescription, EGName);
                        string[] EventNames = XmlHelper.ChildNames(EventsGroup, "event");
                        foreach (string EventName in EventNames)
                            {
                            XmlNode Event = XmlHelper.Find(EventsGroup, EventName);
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
                                ComponentName = XmlHelper.Attribute(Event, "module");
                                }

                            if (ComponentName.ToLower() == "global")
                                ComponentName = "";

                            if (ComponentName != "")
                                XmlHelper.SetName(Event, ComponentName + "." + NewEventName);
                            else
                                XmlHelper.SetName(Event, NewEventName);

                            XmlHelper.DeleteAttribute(Event, "ModuleType");
                            XmlHelper.DeleteAttribute(Event, "module");
                            XmlHelper.DeleteAttribute(Event, "eventname");
                            }
                        XmlHelper.SetName(EventsGroup, outputfiledescription.Name + " Events");
                        EventsGroup.ParentNode.ParentNode.AppendChild(EventsGroup);
                        }
                    }
                else
                    {
                    XmlNode VariablesGroup = Data.ParentNode.AppendChild(XmlHelper.CreateNode(Data.OwnerDocument, "variables", XmlHelper.Attribute(outputfiledescription, "shortcut")));
                    XmlHelper.SetAttribute(VariablesGroup, "shortcut", XmlHelper.Attribute(outputfiledescription, "shortcut"));

                    XmlNode EventsGroup = Data.ParentNode.AppendChild(XmlHelper.CreateNode(Data.OwnerDocument, "events", XmlHelper.Attribute(outputfiledescription, "shortcut") + " Events"));
                    XmlHelper.SetAttribute(EventsGroup, "shortcut", XmlHelper.Attribute(outputfiledescription, "shortcut") + " Events");
                    }
                outputfiledescription.ParentNode.RemoveChild(outputfiledescription);
                }
            }
        // -----------------------------
        // Upgrade the data to version 6.
        // -----------------------------
        private static void UpdateToVersion6(XmlNode Data, Configuration Config)
            {
            if (XmlHelper.Type(Data).ToLower() == "logic")
                {
                foreach (XmlNode script in XmlHelper.ChildNodes(Data, "script"))
                    {
                    string text = script.InnerText;
                    script.InnerText = "";
                    string eventName = XmlHelper.Name(script);
                    eventName = eventName.Replace("startofday", "start_of_day");
                    eventName = eventName.Replace("endofday", "end_of_day");
                    XmlHelper.SetValue(script, "event", eventName);
                    XmlHelper.SetValue(script, "text", text);
                    XmlHelper.DeleteAttribute(script, "name");
                    }
                }
            }

        // -----------------------------
        // Upgrade the data to version 7.
        // -----------------------------
        private static void UpdateToVersion7(XmlNode Data, Configuration Config)
            {
            if (XmlHelper.Type(Data).ToLower() == "soil")
                {
                Soil MySoil = new Soil(Data);
                MySoil.UpgradeToVersion7();
                foreach (XmlNode Child in XmlHelper.ChildNodes(Data, "soilsample"))
                    {
                    Soil ParentSoil = new Soil(Child.ParentNode);
                    SoilSample MySample = new SoilSample(Child, ParentSoil);
                    MySample.UpgradeToVersion7();
                    }
                }
            }

        // -----------------------------
        // Upgrade the data to version 8.
        // -----------------------------
        private static void UpdateToVersion8(XmlNode Data, Configuration Config)
            {
            if (XmlHelper.Type(Data).ToLower() == "soil")
                {
                Soil MySoil = new Soil(Data);
                MySoil.UpgradeToVersion8();
                }
            }

        // -----------------------------
        // Upgrade the data to version 9.
        // -----------------------------
        private static void UpdateToVersion9(XmlNode Data, Configuration Config)
            {
            if (XmlHelper.Type(Data).ToLower() == "stockherbageconverter")
                {
                string[] TypesToDelete = {"proportion_legume", "dmdValue", "p_conc_green_leaf_default",
                                          "p_conc_green_stem_default", "p_conc_senesced_leaf_default",
                                          "p_conc_senesced_stem_default", "p_conc_dead_leaf_default",
                                          "p_conc_dead_stem_default", "ash_alk_green_leaf_default",
                                          "ash_alk_green_stem_default", "ash_alk_senesced_leaf_default",
                                          "ash_alk_senesced_stem_default", "ash_alk_dead_leaf_default",
                                          "ash_alk_dead_stem_default", "ns_ratio_green_leaf_default",
                                          "ns_ratio_green_stem_default", "ns_ratio_senesced_leaf_default",
                                          "ns_ratio_senesced_stem_default", "ns_ratio_dead_leaf_default",
                                          "ns_ratio_dead_stem_default", "np_ratio_green_leaf_default",
                                          "np_ratio_green_stem_default", "np_ratio_senesced_leaf_default",
                                          "np_ratio_senesced_stem_default", "np_ratio_dead_leaf_default",
                                          "np_ratio_dead_stem_default", "dmd_green_leaf",
                                          "dmd_green_stem", "dmd_senesced_leaf",
                                          "dmd_senesced_stem", "dmd_dead_leaf",
                                          "dmd_dead_stem", "KQ5Leaf",
                                          "KQ5Stem", "KQ4",
                                          "cp_n_ratio"};

                foreach (string Type in TypesToDelete)
                    {
                    XmlNode Child = XmlHelper.Find(Data, Type);
                    if (Child != null)
                        Data.RemoveChild(Child);
                    }
                }
            }

        // -------------------------------
        // Upgrade the data to version 10.
        // -------------------------------
        private static void UpdateToVersion10(XmlNode Data, Configuration Config)
            {
            if (XmlHelper.Type(Data).ToLower() == "data")
                {
                foreach (XmlNode Child in XmlHelper.ChildNodes(Data, null))
                    foreach (XmlNode SubChild in XmlHelper.ChildNodes(Child, ""))
                        UpgradeDataComponent(Data, SubChild);
                }
            }

        private static void UpgradeDataComponent(XmlNode ParentDataNode, XmlNode DataNode)
            {
            string[] OkDataTypes = {"apsimfilereader", "xmlfilereader", "probability", "filter",
                                    "cumulative", "depth", "diff", "frequency", "kwtest",
                                    "predobs", "regression", "stats", "soi", "rems", 
                                    "excelreader", "recordfilter"};
            if (Array.IndexOf(OkDataTypes, XmlHelper.Type(DataNode).ToLower()) != -1)
                {
                // Add a source node to our data node.
                XmlNode NewNode = DataNode.AppendChild(XmlHelper.CreateNode(DataNode.OwnerDocument, "source", ""));
                XmlHelper.SetValue(NewNode, "", XmlHelper.Name(DataNode.ParentNode));

                // Move data node to parent.
                ParentDataNode.AppendChild(DataNode);
                DataNode.ParentNode.RemoveChild(DataNode);
                }
            }

        private static void UpdateToVersion11(XmlNode Data, Configuration Config)
            {
            string ShortcutPath = XmlHelper.Attribute(Data, "shortcut");

            if (ShortcutPath != "" && ShortcutPath[0] != '/')
                {
                ShortcutPath = "/" + XmlHelper.Name(Data.OwnerDocument.DocumentElement) 
                                   + "/shared/" + ShortcutPath.Replace("\\", "/");
                if (XmlHelper.FullPath(Data) != ShortcutPath)
                    XmlHelper.SetAttribute(Data, "shortcut", ShortcutPath);
                else
                    XmlHelper.DeleteAttribute(Data, "shortcut");
                XmlNode RealNode = XmlHelper.Find(Data, ShortcutPath);
                MakeNodeShortcuts(Data, RealNode, Config);
                }
            }
        private static void MakeNodeShortcuts(XmlNode ShortCutNode, XmlNode RealNode, Configuration Config)
            {
            XmlHelper.SetName(ShortCutNode, XmlHelper.Name(RealNode));
            foreach (XmlNode Child in XmlHelper.ChildNodes(RealNode, ""))
                {
                if (Config.IsComponentVisible(Child.Name) || Child.Name == "rule")
                    {
                    XmlNode NewNode = ShortCutNode.AppendChild(ShortCutNode.OwnerDocument.CreateElement(Child.Name));
                    string ShortCutPath = XmlHelper.FullPath(RealNode) + "/" + XmlHelper.Name(Child);
                    XmlNode RealChildNode = XmlHelper.Find(RealNode, ShortCutPath);
                    XmlHelper.SetAttribute(NewNode, "shortcut", ShortCutPath);
                    MakeNodeShortcuts(NewNode, Child, Config);
                    }
                }
            }
        private static void UpdateToVersion12(XmlNode Data, Configuration Config)
            {
            if (Data.Name.ToLower() == "manager")
                {
                XmlNode NewManagerNode = Data.ParentNode.AppendChild(Data.OwnerDocument.CreateElement("folder"));
                if (XmlHelper.Attribute(Data, "shortcut") != "")
                    {
                    string ShortCutPath = XmlHelper.Attribute(Data, "shortcut");
                    XmlHelper.SetAttribute(NewManagerNode, "shortcut", ShortCutPath);
                    XmlNode RealNode = XmlHelper.Find(Data, ShortCutPath);
                    XmlHelper.SetName(NewManagerNode, XmlHelper.Name(RealNode));
                    }
                else if (XmlHelper.Name(Data).ToLower() == "manager")
                    XmlHelper.SetName(NewManagerNode, "Manager folder");
                else
                    XmlHelper.SetName(NewManagerNode, XmlHelper.Name(Data));
                foreach (XmlNode Rule in XmlHelper.ChildNodes(Data, "rule"))
                    {
                    XmlNode ManagerNode = NewManagerNode.AppendChild(NewManagerNode.OwnerDocument.CreateElement("manager"));
                    XmlHelper.SetName(ManagerNode, XmlHelper.Name(Rule));
                    if (XmlHelper.Attribute(Rule, "shortcut") != "")
                        {
                        string ShortCutPath = XmlHelper.Attribute(Rule, "shortcut");
                        XmlHelper.SetAttribute(ManagerNode, "shortcut", ShortCutPath);
                        }
                    else
                        {
                        XmlNode Condition = XmlHelper.FindByType(Rule, "condition");
                        XmlHelper.SetValue(ManagerNode, "script/text", Condition.InnerText);
                        XmlHelper.SetValue(ManagerNode, "script/event", XmlHelper.Name(Condition));
                        XmlNode UI = ManagerNode.AppendChild(ManagerNode.OwnerDocument.CreateElement("ui"));
                        foreach (XmlNode Category in XmlHelper.ChildNodes(Rule, "category"))
                            {
                            XmlNode CategoryNode = UI.AppendChild(UI.OwnerDocument.CreateElement("category"));
                            XmlHelper.SetName(CategoryNode, XmlHelper.Name(Category));
                            foreach (XmlNode Prop in XmlHelper.ChildNodes(Category, ""))
                                UI.AppendChild(UI.OwnerDocument.ImportNode(Prop, true));
                            }
                        }
                    }
                Data.ParentNode.ReplaceChild(NewManagerNode, Data);
                }
            else if (Data.Name.ToLower() == "logic")
                {
                XmlNode NewManagerNode = Data.ParentNode.AppendChild(Data.OwnerDocument.CreateElement("manager"));
                XmlHelper.SetName(NewManagerNode, XmlHelper.Name(Data));
                if (XmlHelper.Attribute(Data, "shortcut") != "")
                    XmlHelper.SetAttribute(NewManagerNode, "shortcut", XmlHelper.Attribute(Data, "shortcut"));
                foreach (XmlNode Child in XmlHelper.ChildNodes(Data, ""))
                    NewManagerNode.AppendChild(NewManagerNode.OwnerDocument.ImportNode(Child, true));
                Data.ParentNode.ReplaceChild(NewManagerNode, Data);
                }
            }
        private static void UpdateToVersion13(XmlNode Variables, Configuration Config)
            {
            if (Variables.Name.ToLower() == "variables")
                {
                string[] Conversions = APSIMSettings.INIReadAllSections(APSIMSettings.ApsimDirectory() + "\\apsim\\conversions.54");
                foreach (string Conversion in Conversions)
                    {
                    string[] Bits = Conversion.Split(' ');
                    if (Bits.Length == 5 && Bits[0] == "Renamed")
                        {
                        string OldName = Bits[2].ToLower();
                        string NewName = Bits[4];
                        foreach (XmlNode Variable in XmlHelper.ChildNodes(Variables, "Variable"))
                            {
                            string VariableLine = XmlHelper.Name(Variable);

                            // Do replacement where a module name was specified.
                            int Pos = VariableLine.ToLower().IndexOf("." + OldName);
                            if (Pos != -1)
                                {
                                Pos += OldName.Length + 1;
                                if (Pos == VariableLine.Length || VariableLine[Pos] == ' ')
                                    {
                                    Pos -= OldName.Length + 1;
                                    VariableLine = VariableLine.Substring(0, Pos)
                                                  + "." + NewName
                                                  + VariableLine.Substring(Pos + OldName.Length + 1);
                                    }
                                }
                            else if (VariableLine.Length >= OldName.Length && VariableLine.ToLower().Substring(0, OldName.Length) == OldName.ToLower())
                                {
                                VariableLine = NewName;
                                if (NewName.Length < VariableLine.Length)
                                    VariableLine += VariableLine.Substring(NewName.Length);
                                }
                            XmlHelper.SetName(Variable, VariableLine);
                            }
                        }
                    else if (Bits.Length == 3 && Bits[0] == "Removed")
                        {
                        string NameToDelete = Bits[2].ToLower();
                        foreach (XmlNode Variable in XmlHelper.ChildNodes(Variables, "Variable"))
                            {
                            string VariableLine = XmlHelper.Name(Variable).ToLower();
                            int PosSpace = VariableLine.IndexOf(' ');
                            if (PosSpace == -1)
                                PosSpace = VariableLine.Length;
                            int PosPeriod = VariableLine.IndexOf('.');

                            // get the variable name
                            string VariableName;
                            if (PosPeriod != -1 && PosPeriod < PosSpace)
                                VariableName = VariableLine.Substring(PosPeriod, PosSpace - PosPeriod - 1);
                            else
                                VariableName = VariableLine.Substring(0, PosSpace);

                            // Do we want to delete this variable?
                            if (VariableName == NameToDelete)
                                Variables.RemoveChild(Variable);
                            }

                        }
                    }

                }
            }



		}
	}
