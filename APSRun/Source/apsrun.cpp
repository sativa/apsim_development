//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
#include <aps\apsim_simulation_collection.h>
#include <general\string_functions.h>
#include <fstream>

USERES("APSRun.res");
USELIB("..\..\shared\aps\aps32.lib");
USELIB("..\..\shared\general\general.lib");
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR command_line, int)
   {
   string Command_line (command_line);

   APSIM_simulation_collection Simulation_collection;
   list<string> Control_filenames;

   if (Command_line.length() > 0 && Command_line[0] =='@')
      {
      ifstream in (Command_line.substr(1).c_str());
      string Line;
      getline (in, Line);
      while (in && Line.length() > 0)
         {
         Replace_all (Line, "\"", "");
         Strip (Line, " ");
         Control_filenames.push_back (Line);
         getline (in, Line);
         }

      // assume that if we want to run a whole bunch of simulations then
      // we want to do so with the released apsim configuration - eg: regression tests
      Simulation_collection.Set_configuration_name ("Standard APSIM release");

      for (list<string>::iterator i = Control_filenames.begin();
                                  i != Control_filenames.end();
                                  i++)
         {
         Simulation_collection.Add( (*i).c_str() );
         }
      Simulation_collection.Run_quietly();

      }
   else
      {
      Split_string (Command_line, " ", Control_filenames);

      for (list<string>::iterator i = Control_filenames.begin();
                                  i != Control_filenames.end();
                                  i++)
         {
         Simulation_collection.Add( (*i).c_str() );
         }
      Simulation_collection.Run();
      }

   return 0;
   }
//---------------------------------------------------------------------------
