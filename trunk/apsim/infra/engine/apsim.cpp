#pragma hdrstop
#include "Simulation.h"
#include <string>
#include <fstream>
using namespace std;
using namespace protocol;
//---------------------------------------------------------------------------
int main(int argc, char* argv[])
   {
   Simulation simulation;

   if (argc == 3 && strcmpi(argv[1], "/q") == 0)
      simulation.go(argv[2]);

   else if (argc == 2)
      simulation.go(argv[1]);

   else
      {
      cout << "To run APSIM type : " << std::endl;
      cout << "   apsim sim_file_name" << std::endl << std::endl;
      cout << "Where sim_file_name is the name of your simulation file (.SIM)" << std::endl;
      }

   return 0;
   }
//---------------------------------------------------------------------------
 
