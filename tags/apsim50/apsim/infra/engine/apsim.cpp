#include <stdio.h>
#include <string>
#include <fstream>
#include <iostream>

#include <general/stl_functions.h>
#include <general/TreeNodeIterator.h>
#include <general/xml.h>

#include <ApsimShared/ApsimComponentData.h>
#include <ApsimShared/ApsimSystemData.h>
#include <ApsimShared/ApsimServiceData.h>
#include <ApsimShared/ApsimSimulationFile.h>

#include <ComponentInterface/interfaces.h>

#include "Simulation.h"

using namespace std;
using namespace protocol;
//---------------------------------------------------------------------------
int main(int argc, char **argv) {
   try
      {
      Simulation simulation;

      if (argc == 2)
         simulation.go(argv[1]);

      else
         {
         fprintf(stdout, "To run APSIM type : \n"
                            "   apsim sim_file_name\n\n"
                            "Where sim_file_name is the name of your simulation file (.SIM)");
         }
      }
   catch (const std::runtime_error& error)
      {
      fwrite(error.what(),strlen(error.what()), 1, stdout);
      }
   return 0;
}
//---------------------------------------------------------------------------
