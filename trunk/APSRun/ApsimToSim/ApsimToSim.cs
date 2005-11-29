using System;
using System.IO;
using System.Collections;
using System.Collections.Specialized;
using System.Runtime.InteropServices;
using System.Reflection;
using System.Reflection.Emit;
using VBGeneral;
using CSGeneral;

namespace ApsimToSim
	{
	class ApsimToSim
		{
		private APSIMData Types;


		[STAThread]
		static void Main(string[] args)
			{
			// Main entry point into application.
			// Firstly parse all arguments.
			string ApsimFileName = null;
			StringCollection SimNames = new StringCollection();
			foreach (string arg in args)
				{
				if (ApsimFileName == null)
					ApsimFileName = arg;
				else
					SimNames.Add(arg);
				}

			if (ApsimFileName == null)
				Console.WriteLine("No .apsim file specified on the command line");

			try
				{
				ApsimToSim SimCreator = new ApsimToSim();
                SimCreator.ConvertApsimToSim(ApsimFileName, SimNames);
				}
			catch (Exception err)
				{
				Console.WriteLine(err.Message);
				}
			}

		private void ConvertApsimToSim(string ApsimFileName, StringCollection SimNames)
			{
			// convert the specified simulations in the specified apsim file name
			// into a separate .sim file for each.
			APSIMData Data = new APSIMData();
			Data.LoadFromFile(ApsimFileName);
			
			// If no simulations were specified then do all simulations in .apsim file.
			if (SimNames.Count == 0)
				SimNames = Data.ChildList("simulation");

			// we'll need the types.xml file for later.
			Types = new APSIMData();
			Types.LoadFromFile(APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), "apsimui", "typesfile"));

			// Loop through all simulations and write a sim file for each.
			foreach (string SimName in SimNames)
				{
				StringWriter Out = new StringWriter();
				WriteSimForComponent(Data.Child(SimName), Out, 0);
				Out.Close();
				
				string SortedContents = SortSim(Out.ToString());
				
				StreamWriter FileOut = new StreamWriter(SimName + ".sim");
				FileOut.Write(SortedContents);
				FileOut.Close();
				}
			}

		private void WriteSimForComponent(APSIMData Component, TextWriter Out, int Level)
			{
			APSIMData TypesNode = Types.Child(Component.Type);
			// only write sim text if node type exists in the types.xml file.
			if (TypesNode != null)
				{
				APSIMData ApsimToSim = TypesNode.Child("ApsimToSim");
				if (ApsimToSim != null)
					{
					// write module header bit i.e. <component ...
					string ModuleDLL = ApsimToSim.get_ChildValue("dll");
					string ModuleType = ApsimToSim.get_ChildValue("type");
					string ModuleINI = ApsimToSim.get_ChildValue("ini");
					if (ModuleType != "")
						{
						ModuleDLL = ModuleDLL.Replace(APSIMSettings.ApsimDirectory(), "%apsuite");
						ModuleINI = ModuleINI.Replace(APSIMSettings.ApsimDirectory(), "%apsuite");

						string ComponentHeader = "<" + ModuleType;
						if (ModuleDLL != "")
							{
							ComponentHeader += " name=\"" + Component.Name + "\"";
							ComponentHeader += " executable=\"" + ModuleDLL + "\">";
							if (ModuleType.ToLower() == "component")
								{
								ComponentHeader += "\r\n  <executable>" + ModuleDLL + "</executable>";
								ComponentHeader += "\r\n  <initdata>";
								}
							}
						else
							ComponentHeader += ">";
						Out.WriteLine(StringManip.IndentText(ComponentHeader, Level * 2));
						}

					if (ModuleINI != "")
						Out.WriteLine(StringManip.IndentText("<include>"+ModuleINI+"</include>", (Level+2)*2));

					// write module contents bit.
					APSIMData CallDllNode = ApsimToSim.Child("calldll");
					APSIMData SimNode = ApsimToSim.Child("sim");
					if (SimNode == null)
						{
						if (CallDllNode == null)
							{
							string Contents = APSIMData.FormatXML(Component.InnerXML);
							Contents = StringManip.IndentText(Contents, (Level + 2) * 2);
							Out.WriteLine(Contents);
							}
						}
					else
						{
						string Contents = APSIMData.FormatXML(SimNode.InnerXML);
						Macro macro = new Macro();
						Contents = macro.Go(Component, Contents);
						if (ModuleType.ToLower() == "component")
							Contents = StringManip.IndentText(Contents, (Level + 2) * 2);
						else
							Contents = StringManip.IndentText(Contents, (Level + 1) * 2);
						Out.WriteLine(Contents);
						}

					// write the calldll bit.
					if (CallDllNode != null)
						{
						StringWriter StringOut = new StringWriter();
						CallDll(CallDllNode, StringOut, Component);
						if (ModuleType == "")
							Out.WriteLine(StringManip.IndentText(StringOut.ToString(), Level * 2));
						else
							Out.WriteLine(StringManip.IndentText(StringOut.ToString(), (Level+2) * 2));
						}
					
					// Go through all children of the specified node and write .sim text for each node.
					if (ApsimToSim.get_ChildValue("recurse").ToLower() == "yes")
						{
						foreach (APSIMData Child in Component.get_Children(null))
							WriteSimForComponent(Child, Out, Level+1);
						}

					// write the module footer bit.
					if (ModuleType != "")
						{
						string ComponentFooter = "";
						if (ModuleType.ToLower() == "component")
			                ComponentFooter = "  </initdata>\r\n";
						ComponentFooter += 	"</" + ModuleType + ">";
						Out.WriteLine(StringManip.IndentText(ComponentFooter, Level * 2));
						}
					}
				}
			}


		private void CallDll(APSIMData CallDllNode, TextWriter Out, APSIMData Component)
			{	
			string MethodName = CallDllNode.get_ChildValue("method");
			string DllFileName = CallDllNode.get_ChildValue("dll");
			string ClassName = CallDllNode.get_ChildValue("class");
			string ConstructorArgument = CallDllNode.get_ChildValue("constructorargument").ToLower();

			// load assembly and create an instance of class.
			Assembly assembly = Assembly.LoadFrom(DllFileName);
			Type t = assembly.GetType(ClassName, true);
			object[] ConstructorArgs = new object[1];
			ConstructorArgs[0] = Component;
			if (ConstructorArgument != "this" && ConstructorArgument != "")
				{
				foreach (APSIMData Sibling in Component.Parent.get_Children(null))
					if (Sibling.Type.ToLower() == ConstructorArgument)
						ConstructorArgs[0] = Sibling;
				}
			object Obj = Activator.CreateInstance(t, ConstructorArgs);

			// call required method.
			MethodInfo Method = t.GetMethod(MethodName);
			if (Method == null)
				throw new Exception("Cannot find method '" + MethodName + "' in class '" + ClassName + "'");
            object[] Params = new object[1 + CallDllNode.ChildList("argument").Count];
			Params[0] = Out;		
			int i = 1;
			foreach (APSIMData Argument in CallDllNode.get_Children("argument"))
				{
				Params[i] = Argument.Value;
				i++;
				}
			Method.Invoke(Obj, Params);
			}



		private class ComponentSorter : IComparer  
			{
			private CaseInsensitiveComparer StringComparer = new CaseInsensitiveComparer();
			private StringCollection Components = new StringCollection();
			public ComponentSorter()
				{
				string Contents = APSIMSettings.INIReadSection(APSIMSettings.ApsimIniFile(), "component order");
				StringReader In = new StringReader(Contents);
				string Line;
				while ((Line = In.ReadLine()) != null)
					{
					if (Line.StartsWith("component"))
						{
                        int PosEquals = Line.IndexOf('=');
						if (PosEquals != -1)
							Components.Add(Line.Substring(PosEquals+1).Trim());
						}
					}

				In.Close();
				}
            int IComparer.Compare(object x, object y )  
				{
				APSIMData Data1 = (APSIMData) x;
				APSIMData Data2 = (APSIMData) y;
				string ModuleName1 = Path.GetFileNameWithoutExtension(Data1.Attribute("executable")).ToLower();
				string ModuleName2 = Path.GetFileNameWithoutExtension(Data2.Attribute("executable")).ToLower();


				if (ModuleName1 == ModuleName2)
					return StringComparer.Compare(Data1.Name, Data2.Name);
				if (Data1.Type == "title")
					return -1;
				for (int i = 0; i != Components.Count; i++)
					{
					if (StringManip.StringsAreEqual(Components[i], ModuleName1))
						return -1;
					if (StringManip.StringsAreEqual(Components[i], ModuleName2))
						return 1;
					}
				return 0; // neither are in list!!
				}
			}

		private string SortSim(string Contents)
			{
			APSIMData SortedData = SortSim(new APSIMData(Contents));
			return APSIMData.FormatXML(SortedData.XML);
			}	

		private APSIMData SortSim(APSIMData Data)
			{
			APSIMData SortedData = new APSIMData(Data.Type, Data.Name);
			string Executable = Data.Attribute("executable");
			if (Executable != "")
				SortedData.SetAttribute("executable", Executable);

            StringCollection ChildCollection = Data.ChildList(null);
			ArrayList Children = new ArrayList();
			foreach (APSIMData Child in Data.get_Children(null))
				Children.Add(Child);
			Children.Sort(new ComponentSorter());
			
			foreach (APSIMData Child in Children)
				{
				if (Child.Type.ToLower() != "system")
					SortedData.Add(Child);
				else
					SortedData.Add(SortSim(Child));
				}
			return SortedData;
			}


		}
	}
