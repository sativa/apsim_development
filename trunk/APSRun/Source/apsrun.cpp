//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
#include <aps\apsim_run_collection.h>
#include <general\string_functions.h>
#include <general\stristr.h>
#include <fstream>
#include <dos.h>
#include <aps\apsuite.h>
#include "TAPSIM_config_form.h"

USERES("APSRun.res");
USELIB("aps32.lib");
USELIB("general.lib");
USEFORM("TAPSIM_config_form.cpp", APSIM_config_form);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR,  int)
   {
// ------------------------------------------------------------------
//  Short description:
//    This application will be passed either a control file (.CON) or a
//    run file (.RUN) depending on what the user has right clicked on.

//  Notes:

//  Changes:
//    DPH 28/10/97

// ------------------------------------------------------------------
   if (_argc == 2)
      {
      APSIM_run_collection Run_collection;

      // Does the command line contain a control file?
      if (stristr(_argv[1], ".con") != NULL)
         {
         // yes - better ask user for a configuration.
         TAPSIM_config_form* Form = new TAPSIM_config_form(NULL);
         Form->Control_filename = _argv[1];
         if (Form->ShowModal() == mrOk)
            {
            string DefaultRunFile = APSDirectories().Get_working() + "\\apsim.run";
            Run_collection.Set_filename (DefaultRunFile.c_str());
            Run_collection.Add("Default run", Form->Get_config(), Form->Get_simulations());
            }
         }
      else if (stristr(_argv[1], ".run") != NULL)
         Run_collection.Set_filename (_argv[1]);

      // go perform run.
      if (Run_collection.Count() > 0)
         Run_collection.Run();
      }
   else
      {
      MessageBox (NULL, "Usage:  APSRun [control filename | run filename]",
                        "Error",
                        MB_ICONSTOP | MB_OK);

      }
   return 0;
   }
//---------------------------------------------------------------------------
