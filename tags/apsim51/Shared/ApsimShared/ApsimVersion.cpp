#include <string>
#include <vector>
#include <stdexcept>
#include <general/inifile.h>
#include "ApsimDirectories.h"
#include "ApsimVersion.h"

//---------------------------------------------------------------------------
// Return the APSIM version number
//---------------------------------------------------------------------------
std::string _export getApsimVersion(void)
   {
   IniFile ini(getApsimDirectory() + "/apsim.ini");
   std::string versionString;
   ini.read("version", "apsim", versionString);
   return versionString;
   }
