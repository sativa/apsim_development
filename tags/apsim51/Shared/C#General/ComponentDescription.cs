using System;
using System.Runtime.InteropServices;
using System.IO;
using System.Text;
using VBGeneral;
using System.Reflection;
using System.Reflection.Emit;
using System.Xml;
using System.Xml.Xsl;
using System.Xml.XPath;
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


		// ------------
		// constructor
		// ------------
		public ComponentDescription()
			{
			}


		// ------------------------------------------------------------------
		// Return a list of variables (as xml) for the specified component
		// by calling into a protocol compliant DLL.
		// ------------------------------------------------------------------
		static public  string getDescriptionFromDLL(string moduleName, string instanceName)
			{	
			string DllFileName = APSIMSettings.ApsimDirectory() + "\\apsim\\" + moduleName + "\\lib\\" + moduleName + ".dll";

			// Dynamically create a method.
			AppDomain currentDomain = AppDomain.CurrentDomain;
			AssemblyName myAssemblyName = new AssemblyName();
			myAssemblyName.Name = "TempAssembly";
			AssemblyBuilder myAssemblyBuilder = currentDomain.DefineDynamicAssembly	(myAssemblyName, AssemblyBuilderAccess.Run);
			ModuleBuilder moduleBuilder = myAssemblyBuilder.DefineDynamicModule("TempModule");
			MethodBuilder method;
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

			// Get the xsl transform ready.
			APSIMSettings Settings = new APSIMSettings();
			string ProtocolToVariablesXSLFileName = APSIMSettings.INIRead(APSIMSettings.ApsimIniFile(), "apsimui", "ProtocolToVariablesFile");
            System.Xml.Xsl.XslCompiledTransform xslt = new System.Xml.Xsl.XslCompiledTransform();  
			xslt.Load(ProtocolToVariablesXSLFileName);

			// Call the DLL
			StringBuilder description = new StringBuilder(100000);
			object[] parameters;
			parameters = new object[] {initScript, description};
			MethodInfo mi = moduleBuilder.GetMethod( "getDescription" );
			string CurrentDirectory = Directory.GetCurrentDirectory();
			Directory.SetCurrentDirectory(APSIMSettings.ApsimDirectory() + "\\bin");		
			mi.Invoke(null, parameters);
			Directory.SetCurrentDirectory(CurrentDirectory);		

			// Transform the xml returned from the dll with our xsl.
			StringReader ContentsReader = new StringReader(description.ToString());
			XPathDocument XmlData = new XPathDocument(ContentsReader);
			StringWriter SWriter = new StringWriter();
			XmlTextWriter Writer = new XmlTextWriter(SWriter);
			xslt.Transform(XmlData, Writer);
			Writer.Close();

			return "<?xml version=\"1.0\"?>\r\n"
				       + "<?xml-stylesheet type=\"text/xsl\" href=\"../docs/shared/Variables.xsl\"?>\r\n"
				       + SWriter.ToString();
			}


		}
	}