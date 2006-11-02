#include <string>
#include <vector>
#include <stdexcept>
#include <general/IniFile.h>
#include <general/platform.h>
#include "ApsimDirectories.h"
#include "ApsimVersion.h"

//---------------------------------------------------------------------------
// Return the APSIM version number
//---------------------------------------------------------------------------
std::string EXPORT getApsimVersion(void)
   {
   IniFile ini(getApsimDirectory() + "/Apsim.ini");
   std::string versionString;
   ini.read("version", "apsim", versionString);
   return versionString;
   }
