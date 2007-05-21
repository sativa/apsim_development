#include <stdio.h>
#include <string>
#include <vector>
#include <stdexcept>

using namespace std;

#include <ApsimShared/SimCreator.h>


int main(int argc, char* argv[])
   //---------------------------------------------------------------------------
   // Main entry point.
   {
   try
      {
      string conFileName;
      vector<string> simNames;
      for (int i = 1; i < argc; i++)
         {
         if (conFileName == "")
            conFileName = argv[i];
         else
            simNames.push_back(argv[i]);
         }

      if (conFileName == "")
         throw runtime_error("No .con file specified on the command line");

      SimCreator simCreator(true);
      simCreator.ConToSim(conFileName, simNames);
      }
   catch (const exception& error)
      {
      fwrite(error.what(),strlen(error.what()), 1, stdout);
      return 1;
      }
   return 0;
   }
