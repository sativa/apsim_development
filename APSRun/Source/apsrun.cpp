//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
#include <aps\apsim_simulation_collection.h>
#include <general\string_functions.h>

USERES("APSRun.res");
USELIB("..\..\shared\aps\aps32.lib");
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR command_line, int)
   {
   string Command_line (command_line);
   list<string> Control_filenames;
   Split_string (Command_line, " ", Control_filenames);

   APSIM_simulation_collection Simulation_collection;
   for (list<string>::iterator i = Control_filenames.begin();
                               i != Control_filenames.end();
                               i++)
      {
      Simulation_collection.Add( (*i).c_str() );
      }
   Simulation_collection.Run();

   return 0;
   }
//---------------------------------------------------------------------------
