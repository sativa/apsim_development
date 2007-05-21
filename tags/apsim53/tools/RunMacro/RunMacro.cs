using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using VBGeneral;
using CSGeneral;

namespace RunMacro
    {
    class RunMacro
        {
        static int Main(string[] args)
            {
            // -------------------------------------------------------------
            // Main entry point into program.
            // -------------------------------------------------------------
            try
                {
                if (args.Length == 2)
                    {
                    // read from stdin all contents, loop through all child nodes and create
                    // a new <type> under 'NewInterfaceFile'
                    StreamReader MacroFile = new StreamReader(args[1]);
                    string Contents = MacroFile.ReadToEnd();
                    APSIMData XMLFile = new APSIMData();
                    XMLFile.LoadFromFile(args[0]);
                    
                    // go execute macro.
                    Macro macro = new Macro();
                    macro.Go(XMLFile, Contents, "", false);
                    
                    return 0;
                    }
                else
                    throw new Exception("Usage: RunMacro MacroFileName XMLFileName");
                }
            catch (Exception err)
                {
                Console.WriteLine(err.Message);
                }
            return 1;
            }
        }
    }
