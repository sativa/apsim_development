#include <vector>
#include <string>
#include <general/IniFile.h>
#include "ApsimRunFile.h"

using namespace std;

static const char* SIMULATION_FILE_KEY = "Simulation_file";
static const char* SIMULATION_KEY   = "Simulation";

// ------------------------------------------------------------------
// Constructor
// ------------------------------------------------------------------
ApsimRunFile::ApsimRunFile(const std::string& file)
   {
   ini = new IniFile(file);
   }
// ------------------------------------------------------------------
// destructor.
// ------------------------------------------------------------------
ApsimRunFile::~ApsimRunFile()
   {
   delete ini;
   }
// ------------------------------------------------------------------
// Return a list of runs to caller.
// ------------------------------------------------------------------
void ApsimRunFile::getRuns(std::vector<std::string>& runNames) const
   {
   ini->readSectionNames(runNames);
   }
// ------------------------------------------------------------------
// Return a specified run to caller.
// ------------------------------------------------------------------
void ApsimRunFile::getRun(const std::string& runName,
                          std::string& fileName,
                          std::vector<std::string>& conFileSections) const
   {
   ini->read(runName, SIMULATION_FILE_KEY, fileName);
   ini->read(runName, SIMULATION_KEY, conFileSections);
   }


