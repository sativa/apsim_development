#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ApsimSimulationFile.h"
#include <ApsimShared\ApsimDirectories.h>
#include <general\exec.h>
using namespace std;
// ------------------------------------------------------------------
// Constructor
// ------------------------------------------------------------------
ApsimSimulationFile::ApsimSimulationFile(const string& filename)
   : fileName(filename)
   {
   }
// ------------------------------------------------------------------
// Run the simulation
// ------------------------------------------------------------------
void ApsimSimulationFile::run(bool quiet)
   {
   AllocConsole();
   string commandLine = getApsimDirectory() + "\\apsim\\apsim.exe " +
                        fileName;
   Exec(commandLine.c_str(), SW_SHOW, true);
   if (!quiet)
      MessageBox(NULL, "APSIM has finished", "For your information", MB_ICONINFORMATION | MB_OK);
   FreeConsole();
   }

