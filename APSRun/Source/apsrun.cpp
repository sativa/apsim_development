//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <aps\APSIMSimulationCollection.h>
#include <aps\APSIMCONSimulationCollection.h>
#include <general\string_functions.h>
#include <general\stristr.h>
#include <general\path.h>
#include <fstream>
#include <dos.h>
#include <conio.h>
#include "TAPSIMRunForm.h"

USERES("APSRun.res");
USELIB("aps32.lib");
USELIB("general.lib");
USEFORM("TAPSIMRunForm.cpp", APSIMRunForm);
//---------------------------------------------------------------------------
using namespace std;
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR,  int)
   {
// ------------------------------------------------------------------
//  Short description:
//    This application will be passed either a control file (.CON), a
//    run file (.RUN), or a .SIM file depending on what the user has
//    right clicked on.

//  Notes:

//  Changes:
//    DPH 28/10/97
//    dph 7/7/2000 added support for .sim files.
//    dph 17/1/2000 added support for /CREATESIM switch

// ------------------------------------------------------------------
   string filename;
   bool quietRun = false;
   bool createSIM = false;
   for (int argIndex = 1; argIndex < _argc; argIndex++)
      {
      if (stricmp(_argv[argIndex], "/q") == 0)
         quietRun = true;
      else if (stricmp(_argv[argIndex], "/CreateSIM") == 0)
         createSIM = true;
      else
         filename = _argv[argIndex];
      }
   if (filename != "")
      {
      if (FileExists(filename.c_str()))
         {
         APSIMSimulationCollection* simulations = NULL;

         // Does the command line contain a control file?
         if (stristr((char*) filename.c_str(), ".con") != NULL)
            {
            simulations = new APSIMSimulationCollection;

            // yes - better ask user for a configuration.
            TAPSIMRunForm* runForm = new TAPSIMRunForm(NULL);
            runForm->controlFilename = filename;
            if (runForm->ShowModal() == mrOk)
               runForm->getSelectedSimulations(*simulations);
            }
         else if (stristr((char*) filename.c_str(), ".run") != NULL)
            {
            simulations = new APSIMCONSimulationCollection(filename);
            simulations->read();
            }

         else if (stristr((char*) filename.c_str(), ".sim") != NULL)
            {
            simulations = new APSIMSimulationCollection;
            Path simPath(filename.c_str());
            simulations->addSimulation(new APSIMSimulation
               (simPath.Get_name_without_ext(), simPath.Get_path()));
            }

         try
            {
            if (createSIM)
               {
               list<string> simulationNames;
               simulations->getNames(simulationNames);
               for (list<string>::iterator i = simulationNames.begin();
                                           i != simulationNames.end();
                                           i++)
                  {
                  simulations->get(*i)->readFromFile();
                  simulations->get(*i)->writeToFile();
                  }
               }
            else
               {
               AllocConsole();
               simulations->run();
               if (!quietRun)
                  MessageBox(NULL, "APSIM has finished", "For your information", MB_ICONINFORMATION | MB_OK);
               FreeConsole();
               }
            }
         catch (string& msg)
            {
            MessageBox(NULL, msg.c_str(), "APSIM Error", MB_ICONSTOP | MB_OK);
            }
         delete simulations;
         }
      else
         {
         string msg = "Cannot find file: " + filename;
         MessageBox(NULL, msg.c_str(),
                    "APSIM Error",
                    MB_ICONSTOP | MB_OK);
         }

      }
   else
      {
      MessageBox (NULL, "Usage:  APSRun [/Q] [/CreateSIM] [.con file | .run file | "
                        ".sim file]",
                  "Error",
                  MB_ICONSTOP | MB_OK);

      }
   return 0;
   }
//---------------------------------------------------------------------------
