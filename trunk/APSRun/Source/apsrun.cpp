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
#include "TAPSIMRunForm.h"

USERES("APSRun.res");
USELIB("aps32.lib");
USELIB("general.lib");
USEFORM("TAPSIMRunForm.cpp", APSIMRunForm);
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

// ------------------------------------------------------------------
   if (_argc == 2)
      {
      APSIMSimulationCollection* simulations = NULL;

      // Does the command line contain a control file?
      if (stristr(_argv[1], ".con") != NULL)
         {
         simulations = new APSIMSimulationCollection;

         // yes - better ask user for a configuration.
         TAPSIMRunForm* runForm = new TAPSIMRunForm(NULL);
         runForm->controlFilename = _argv[1];
         if (runForm->ShowModal() == mrOk)
            runForm->getSelectedSimulations(*simulations);
         }
      else if (stristr(_argv[1], ".run") != NULL)
         {
         simulations = new APSIMCONSimulationCollection(_argv[1]);
         simulations->read();
         }

      else if (stristr(_argv[1], ".sim") != NULL)
         {
         simulations = new APSIMSimulationCollection;
         Path simPath(_argv[1]);
         simulations->addSimulation(new APSIMSimulation
            (simPath.Get_name_without_ext(), simPath.Get_path()));
         }

      try
         {
         simulations->run();
         }
      catch (string& msg)
         {
         MessageBox(NULL, msg.c_str(), "APSIM Error", MB_ICONSTOP | MB_OK);
         }
      delete simulations;
      }
   else
      {
      MessageBox (NULL, "Usage:  APSRun [.con file | .run file | "
                        ".sim file]",
                  "Error",
                  MB_ICONSTOP | MB_OK);

      }
   return 0;
   }
//---------------------------------------------------------------------------
