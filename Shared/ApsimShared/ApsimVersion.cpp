//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ApsimVersion.h"
#include "ApsimDirectories.h"
#include <general\inifile.h>

#pragma package(smart_init)
//---------------------------------------------------------------------------
// Return the APSIM version number
//---------------------------------------------------------------------------
std::string _export getApsimVersion(void)
   {
   IniFile ini(getApsimDirectory() + "\\apsim.ini");
   string versionString;
   ini.read("version", "apsim", versionString);
   return versionString;
   }
