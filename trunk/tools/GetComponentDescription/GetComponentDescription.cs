using System;
using System.IO;
//using CSGeneral;

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
			try
				{
				if (args.Length == 2)
					{
					string xml = CSGeneral.ComponentDescription.getDescriptionFromDLL(args[0], args[1]);
					Console.Write(xml);
					}
				else
					Console.WriteLine("Usage: GetComponentDescription DLLFileName instancename");
				}
			catch (Exception err)
				{
				Console.WriteLine(err.Message + " Module name: " + args[0]);
				}
			}

		}

   }
