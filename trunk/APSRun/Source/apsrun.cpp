//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <aps\APSIMSimulationCollection.h>
#include <general\string_functions.h>
#include <general\stristr.h>
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
      APSIMSimulationCollection simulations;

      // Does the command line contain a control file?
      if (stristr(_argv[1], ".con") != NULL)
         {
         // yes - better ask user for a configuration.
         TAPSIMRunForm* runForm = new TAPSIMRunForm(NULL);
         runForm->controlFilename = _argv[1];
         if (runForm->ShowModal() == mrOk)
            runForm->getSelectedSimulations(simulations);
         }
      else if (stristr(_argv[1], ".run") != NULL)
         {
         simulations = APSIMSimulationCollection(_argv[1]);
         simulations.read();
         }

      else if (stristr(_argv[1], ".sim") != NULL)
         simulations.addSimulation(new APSIMSimulation(_argv[1]));

      try
         {
         simulations.run();
         }
      catch (string& msg)
         {
         MessageBox(NULL, msg.c_str(), "APSIM Error", MB_ICONSTOP | MB_OK);
         }
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
