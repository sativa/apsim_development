//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "PreviousRuns.h"
#include "ApsimRuns.h"
#include <ApsimShared\ApsimSettings.h>
#include <general\inifile.h>
#include <general\string_functions.h>
using namespace std;

static const char* SECTION = "Apsim Recent Runs";

// ------------------------------------------------------------------
// Constructor
// ------------------------------------------------------------------
PreviousRuns::PreviousRuns(void)
   {
   maxNumRememberedRuns = 10;
   }
// ------------------------------------------------------------------
// Get the details of a specified run if possible.  Returns
// true if found.
// ------------------------------------------------------------------
bool PreviousRuns::wasPreviouslyRun(const string& controlFilename,
                                    const string& simulationName)
   {
   ApsimSettings settings;
   vector<string> simulationNames;
   settings.read(string(SECTION) + "|" + controlFilename, simulationNames);
   return(find(simulationNames.begin(), simulationNames.end(), simulationName)
               != simulationNames.end());
   }

// ------------------------------------------------------------------
// Save the selected runs to .ini file.
// ------------------------------------------------------------------
void PreviousRuns::saveSelectedRunNames(ApsimRuns& selectedRuns)
   {
   ApsimSettings settings;
   vector<string> fileNames, simNames;
   selectedRuns.getSimulations(fileNames, simNames);

   ostringstream out;
   for (unsigned f = 0; f != fileNames.size(); f++)
      out << "   " << fileNames[f] << " = " + simNames[f] << endl;

   settings.writeSection(SECTION, out.str());
   }


