#pragma hdrstop
#include "Simulation.h"
#include <string>
#include <fstream>
#include <general\path.h>
#include <dir.h>
using namespace std;
using namespace protocol;
//---------------------------------------------------------------------------
void start(const char* simFilename)
   {
   ifstream in(simFilename);
   if (in)
      {
      chdir(Path(simFilename).Get_directory().c_str());
      string sdml;
      getline(in, sdml, '\0');

      Simulation simulation;
      simulation.init(sdml);
      simulation.commence();
      simulation.term();
      }
   else
      {
      cout << "Cannot find file: " << simFilename;
      }
   }
//---------------------------------------------------------------------------
int main(int argc, char* argv[])
   {
   if (argc == 3 && strcmpi(argv[1], "/q") == 0)
      start(argv[2]);

   else if (argc == 2)
      start(argv[1]);

   else
      {
      cout << "To run APSIM type : " << std::endl;
      cout << "   apsim sim_file_name" << std::endl << std::endl;
      cout << "Where sim_file_name is the name of your simulation file (.SIM)" << std::endl;
      }

   return 0;
   }
//---------------------------------------------------------------------------
 
