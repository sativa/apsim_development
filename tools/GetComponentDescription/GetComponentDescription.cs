using System;
using System.IO;
using CSGeneral;

namespace GetComponentDescription
	{
	// ------------------------------------------------
	// This class simply exposes the GetDescription
	// routine in an APSIM module.dll
	// ------------------------------------------------
	class GetComponentDescription
		{
		// --------------------------------------------
		// The main entry point for the application.
		// --------------------------------------------
		[STAThread]
		static void Main(string[] args)
			{
			if (args.Length == 3)
				{
				string xml = ComponentDescription.getDescriptionFromDLL(args[0], args[1]);
				StreamWriter Out = new StreamWriter(args[2]);
				Out.Write(xml);
				Out.Close();
				}
			else
				Console.WriteLine("Usage: GetComponentDescription modulename instancename outputfilename");
			}

		}

   }
