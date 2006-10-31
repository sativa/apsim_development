#ifdef __WIN32__
   #include <vcl.h>    // Gets around linker error: Unresolved external 'System::__linkproc__ __fastcall HandleFinally()' referenced from C:\PROGRAM FILES\BORLAND\CBUILDER6\LIB\OBJ\SYSINIT.OBJ
   #include <dir.h>
#endif
#include <stdio.h>
#include <string>
#include <fstream>
#include <iostream>
#include <stdexcept>

#include <general/path.h>
#include <general/stl_functions.h>
#include <general/TreeNodeIterator.h>
#include <general/xml.h>

#include <ApsimShared/ApsimComponentData.h>
#include <ApsimShared/ApsimSystemData.h>
#include <ApsimShared/ApsimServiceData.h>
#include <ApsimShared/ApsimSimulationFile.h>

#include <ComponentInterface/Interfaces.h>

#include "Simulation.h"

using namespace std;
using namespace protocol;
//---------------------------------------------------------------------------
int main(int argc, char **argv) {
   try
      {
      Simulation simulation;

      if (argc == 2)
         {
         chdir(fileDirName(argv[1]).c_str());
         simulation.go(argv[1]);
         }
      else
         {
         fprintf(stdout, "To run APSIM type : \n"
                            "   apsim sim_file_name\n\n"
                            "Where sim_file_name is the name of your simulation file (.SIM)");
         return 1;
         }
      }
   catch (const exception& error)
      {
      fwrite(error.what(),strlen(error.what()), 1, stdout);
      return 1;
      }
   return 0;
}
//---------------------------------------------------------------------------
// This is needed for LINUX compatibility. ProtocolManager seems to look for
// this during link.
extern "C" unsigned get_componentID(void)
   {
   return 0;
   }
