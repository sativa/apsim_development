
#include <condefs.h>
#pragma hdrstop

#include <fstream>
#include "APSIMSystem.h"
#include <general\path.h>
#include <aps\apsim_run_collection.h>
using std::ofstream;
using std::cout;
//---------------------------------------------------------------------------
USELIB("APSIMEngine.lib");
USELIB("..\..\..\shared\aps\aps32.lib");
USELIB("..\..\..\shared\general\general.lib");
USELIB("..\..\..\shared\odl\odl.lib");
//---------------------------------------------------------------------------
void Run_apsim (const char* RunFilename)
   {
   Path RunPath (RunFilename);
   if (RunPath.Get_directory().length() == 0)
      RunPath.Set_to_cwd();
   if (!RunPath.Exists())
      cout << "The run file : " << RunPath.Get_path() << " does not exist" << std::endl;

   else
      {
      APSIM_run_collection Run_collection (RunPath.Get_path().c_str());

      list<string> SimulationNames;
      Run_collection.Get_names(SimulationNames);
      for (list<string>::iterator sim = SimulationNames.begin();
                                  sim != SimulationNames.end();
                                  sim++)
         {
         APSIM_run Run = Run_collection.Get((*sim).c_str());

         // get name of control file.
         string ControlFilename = Run.Simulations.Get_filename();

         // delete any existing log file.
         Path LogPath(ControlFilename.c_str());
         LogPath.Set_extension(".log");
         DeleteFile(LogPath.Get_path().c_str());

         // change the current working directory to that of the
         // control file.
         Path ControlPath(ControlFilename.c_str());
         ControlPath.Change_directory();

         // get list of all sections.
         list<string> SectionNames;
         Run.Simulations.Get_names(SectionNames);

         // loop through all simulations and run each in turn.
         for (list<string>::iterator sim = SectionNames.begin();
                                     sim != SectionNames.end();
                                     sim++)
            {
            // Go RUN APSIM!!
            if (ApsimSystem().Init(*Run.Simulations.Get( (*sim).c_str()), Run.Configuration))
               ApsimSystem().Go();
            }
         }
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     MAIN program

//  Notes:

//  Changes:
//    dph 22/6/99

// ------------------------------------------------------------------
#pragma argsused
int main(int argc, char **argv)
   {
   bool Quiet_mode = false;
   if (argc == 3)
      {
      Quiet_mode = (strcmpi(argv[1], "/q") == 0);
      Run_apsim (argv[2]);
      }
   else if (argc == 2)
      Run_apsim (argv[1]);

   else
      {
      cout << "To run APSIM type : " << std::endl;
      cout << "   apsim [/Q] run_file_name" << std::endl << std::endl;
      cout << "Where run_file_name is the name of your APSIM run file (.RUN)" << std::endl;
      cout << "and the /Q switch indicates \'quiet\' mode where no \"APSIM has finished\"" << std::endl;
      cout << "is given." << std::endl;
      }
   if (!Quiet_mode)
      ShowMessage("APSIM has finished.  Press any key to continue");

   return 0;
   }

