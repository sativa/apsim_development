using System;
using System.Runtime.InteropServices;
using System.IO;
using System.Text;
using VBGeneral;
using System.Reflection;
using System.Reflection.Emit;
namespace CSGeneral
	{

	//---------------------------------------------------
	// This class encapsulates information about a 
	// component.
	// --------------------------------------------------
	public class ComponentDescription
		{
		[DllImport("apsimshared.DLL", 
		   CharSet=CharSet.Ansi,
			CallingConvention=CallingConvention.StdCall)]
		public static extern void convertIniToSim(string fileName, StringBuilder contents);
		private APSIMData Variables;
		private string ProtocolToVariablesFileName;



		// ------------
		// constructor
		// ------------
		public ComponentDescription()
			{
			APSIMSettings Settings = new APSIMSettings();
			ProtocolToVariablesFileName = Settings.GetSetting("apsimui", "ProtocolToVariablesFile");
			string VariablesFileName = Settings.GetSetting("apsimui", "variablefile");
			APSIMFile VariableFile = new APSIMFile();
			VariableFile.Open(VariablesFileName);
			Variables = VariableFile.data;
			}


		// ------------------------------------------------------------------
		// Return a list of variables (as xml) for the specified component
		// by calling into a protocol compliant DLL.
		// ------------------------------------------------------------------
		private string getDescriptionFromDLL(string moduleName, string instanceName)
			{	
			string DllFileName = APSIMSettings.ApsimDirectory() + "\\apsim\\" + moduleName + "\\lib\\" + moduleName + ".dll";

			// Dynamically create a method.
			AppDomain currentDomain = AppDomain.CurrentDomain;
			AssemblyName myAssemblyName = new AssemblyName();
			myAssemblyName.Name = "TempAssembly";
			AssemblyBuilder myAssemblyBuilder = currentDomain.DefineDynamicAssembly	(myAssemblyName, AssemblyBuilderAccess.Run);
			ModuleBuilder moduleBuilder = myAssemblyBuilder.DefineDynamicModule("TempModule");
			MethodBuilder method;
			if (moduleName == "stock")
				method = moduleBuilder.DefinePInvokeMethod("getDescription", DllFileName, 
				                                 MethodAttributes.Public | MethodAttributes.Static | MethodAttributes.PinvokeImpl,
															CallingConventions.Standard,
															typeof(void),
															new Type[] { typeof(StringBuilder) },
															CallingConvention.StdCall,
															CharSet.Ansi);
			else
				method = moduleBuilder.DefinePInvokeMethod("getDescription", DllFileName, 
				                                 MethodAttributes.Public | MethodAttributes.Static | MethodAttributes.PinvokeImpl,
															CallingConventions.Standard,
															typeof(void),
															new Type[] { typeof(string), typeof(StringBuilder) },
															CallingConvention.StdCall,
															CharSet.Ansi);
			method.SetImplementationFlags( MethodImplAttributes.PreserveSig |
			method.GetMethodImplementationFlags() );
			moduleBuilder.CreateGlobalFunctions();

			string IniFileName = APSIMSettings.ApsimDirectory() + "\\apsim\\" + instanceName + "\\" + instanceName + ".ini";
			string initScript= "<component name=\"" + instanceName + "\" executable=\"" + DllFileName + "\">\r\n   <initdata>\r\n";
			if (File.Exists(IniFileName))
				{
				StringBuilder contents = new StringBuilder(100000);
				convertIniToSim(IniFileName, contents);	
				initScript += contents.ToString();
				}
			initScript += "   </initdata>\r\n";
			initScript += "</component>";
			
			StringBuilder description = new StringBuilder(100000);
			object[] parameters;
			if (moduleName == "stock")
				parameters = new object[] {description};
			else
				parameters = new object[] {initScript, description};

			MethodInfo mi = moduleBuilder.GetMethod( "getDescription" );

			string CurrentDirectory = Directory.GetCurrentDirectory();
			Directory.SetCurrentDirectory(APSIMSettings.ApsimDirectory() + "\\bin");		
			mi.Invoke(null, parameters);
			Directory.SetCurrentDirectory(CurrentDirectory);		

			StreamReader In = new StreamReader(ProtocolToVariablesFileName);
			return CSUtility.ApplyStyleSheet(description.ToString(), In.ReadToEnd());
			//return description.ToString();
			}


		// ------------------------------------------------------------------
		// Return a list of variables (as xml) for the specified component
		// by looking in the variables file.
		// ------------------------------------------------------------------
		private string getDescriptionFromFile(string apsimuiTypeName)
			{
			APSIMData Child = Variables.Child(apsimuiTypeName);
			if (Child != null)
				return Child.XML;
			else
				return "";
			}


		// ------------------------------------------------------------------
		// Return a list of variables (as xml) for the specified components
		// eg of ApsimuiTypeName is PLANT
		// eg of ApsimuiInstanceName is WHEAT
		// ------------------------------------------------------------------
		public string getDescription(APSIMData Data)
			{
			if (Data.Type == "plant")
				{
				string PlantType = Data.Child("type").Value;
				return getDescriptionFromDLL(Data.Type, PlantType);
				}

			// The input module tries to open a met file during Init1 to register all variables in the met file.
			// We're only passing .ini file to it and not a full parameter file and so it can't find a filename
			// and so throws.
			//else if (Data.Type == "metfile")
			//	return getDescriptionFromDLL("input", "");
			else if (Data.Type == "stock")
				return getDescriptionFromDLL("stock", "");
			else
				return getDescriptionFromFile(Data.Type);
			}



		}
	}
