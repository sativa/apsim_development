//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include <fstream>
#include <conio.h>
#include "APSIMMain.h"
using std::ofstream;
using std::cout;
//---------------------------------------------------------------------------
USELIB("aps32.lib");
USELIB("general.lib");
USELIB("odl.lib");
USELIB("apsimengine.lib");
//---------------------------------------------------------------------------
#pragma argsused
int main(int argc, char **argv)
   {
   if (argc == 2)
      start(argv[1]);

   else
      {
      cout << "To run APSIM type : " << std::endl;
      cout << "   apsim sim_file_name" << std::endl << std::endl;
      cout << "Where sim_file_name is the name of your simulation file (.SIM)" << std::endl;
      }

   getch();
   return 0;
   }

