#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include <string>
#include <fstream>
#include <iostream>

#include <general/stl_functions.h>
#include <general/TreeNodeIterator.h>
#include <general/xml.h>

#include <ApsimShared/ApsimComponentData.h>
#include <ApsimShared/ApsimSystemData.h>
#include <ApsimShared/ApsimServiceData.h>
#include <ApsimShared/ApsimSimulationFile.h>

#include <ComponentInterface/interfaces.h>

#include "Simulation.h"

using namespace std;
using namespace protocol;
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
   {
   try
      {
      Simulation simulation;

      if (_argc == 3 && strcmpi(_argv[1], "/console") == 0)
         {
         AllocConsole();
         simulation.go(_argv[2]);
         }

      else if (_argc == 2)
         simulation.go(_argv[1]);

      else
         {
         ::MessageBox(NULL, "To run APSIM type : \n"
                            "   apsim [/console] sim_file_name\n\n"
                            "Where sim_file_name is the name of your simulation file (.SIM)",
                            "Error", MB_ICONSTOP | MB_OK);
         }
       Application->Initialize();
       Application->Run();
      }
   catch (const std::runtime_error& error)
      {
      ShowMessage(error.what());
      }
   catch (Exception &exception)
      {
      Application->ShowException(&exception);
      }
   catch (...)
      {
      try
         {
         throw Exception("");
         }
       catch (Exception &exception)
         {
         Application->ShowException(&exception);
         }
      }
   if (_argc == 3 && strcmpi(_argv[1], "/console") == 0)
      FreeConsole();

   return 0;
}
//---------------------------------------------------------------------------
