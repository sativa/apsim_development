using System;
using System.IO;
using System.Collections;
using System.Collections.Specialized;
using System.Runtime.InteropServices;
using System.Reflection;
using System.Reflection.Emit;
using VBGeneral;
using CSGeneral;
using System.Xml;
using ApsimFile;

namespace ApsimToSim
	{
	class ApsimToSim
		{
		private Configuration Configuration = new Configuration("ApsimUI");


		[STAThread]
		static int Main(string[] args)
			{
			// Main entry point into application.
			// Firstly parse all arguments.
			string ApsimFileName = null;
            string[] SimNames = new string[args.Length - 1];
            for (int i = 0; i != args.Length; i++ )
                {
                if (i == 0)
                    ApsimFileName = args[i];
                else
                    SimNames[i-1] = args[i];
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
                return 1;
				}
			return 0;
			}

		private void ConvertApsimToSim(string ApsimFileName, string[] SimNames)
			{
            Directory.SetCurrentDirectory(Path.GetDirectoryName(ApsimFileName));


			// convert the specified simulations in the specified apsim file name
			// into a separate .sim file for each.
			XmlDocument Doc = new XmlDocument();
			Doc.Load(ApsimFileName);
            XmlNode Data = Doc.DocumentElement;

            // Run the converter in case a conversion is needed.
            if (ApsimFile.APSIMChangeTool.Upgrade(Data))
                Doc.Save(ApsimFileName);

            findSimsAndConvert(Data, SimNames);
            }

        private void findSimsAndConvert(XmlNode Data, string[] SimNames)
            {
			// Iterate through all nested simulations and convert them to
            // .sim format if necessary.
            foreach (XmlNode child in XmlHelper.ChildNodes(Data, ""))
                {
                if (XmlHelper.Type(child).ToLower() == "simulation")
                    {
                    ApsimFile.ApsimFile Simulation = new ApsimFile.ApsimFile(Configuration);
                    Simulation.Open(child);

                    string SimName = XmlHelper.Name(child);
                    bool convertSim = (SimNames.Length == 0 || Array.IndexOf(SimNames, SimName) != -1);
                    if (convertSim)
                        {
                        try
                            {
                            XmlDocument Doc = new XmlDocument();
                            Simulation.RootComponent.WriteSim(Doc, Configuration);
                            Doc.Save(SimName + ".sim");
                            }
                        catch (Exception err)
                            {
                            throw new Exception(SimName + ": " + err.Message);
                            }
                        }
                    }
                if (XmlHelper.Type(child).ToLower() == "folder")
                    findSimsAndConvert(child, SimNames);
                }                    
			}


		}
	}
