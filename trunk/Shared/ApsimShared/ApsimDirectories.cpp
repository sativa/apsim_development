//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ApsimDirectories.h"
#include <general\path.h>
#include "FString.h"
#pragma package(smart_init)

using namespace std;
extern HINSTANCE hInstance;
// ------------------------------------------------------------------
// This routine provides a way for APSIM applications to get the
// home directory.  Will throw a runtime error if the current
// Application is not in the apsim directory structure.
// ------------------------------------------------------------------
string _export getApsimDirectory(void) throw(runtime_error)
   {
   char moduleFileName[MAX_PATH];
   GetModuleFileName(hInstance, moduleFileName, sizeof moduleFileName);
   Path path(moduleFileName);
   path.Set_name("version.ini");
   while (path.Back_up_directory() != "" && !path.Exists());

   if (!path.Exists())
      {
      string msg = "The application: " + path.Get_path() + " is not in the "
                   "APSIM directory structure.";
      throw runtime_error(msg);
      }
   else
      return path.Get_directory();
   }

// ------------------------------------------------------------------
// This routine provides a way for APSIM applications to get their
// home directory.  Will throw a runtime error if the current
// Application is not in the apsim directory structure.
// ------------------------------------------------------------------
std::string _export getAppHomeDirectory(void) throw(std::runtime_error)
   {
   string applicationName = Path(Application->ExeName.c_str()).Get_name_without_ext();
   try
      {
      return getApsimDirectory() + "\\" + applicationName;
      }
   catch (const exception& error)
      {
      return Path(Application->ExeName.c_str()).Get_directory();
      }
   }

// ------------------------------------------------------------------
// This routine provides a way for APSIM applications to get the
// home directory.  Will throw a runtime error if the current
// Application is not in the apsim directory structure.
// Currently called by DEMO.
// ------------------------------------------------------------------
extern "C" void _export __stdcall getApsuiteDirectory
   (const char* directory, unsigned directoryLength)
   {
   try
      {
      string dir = getApsimDirectory();
      FString(directory, directoryLength, FORString) = dir.c_str();
      }
   catch (const runtime_error& err)
      {
      ::MessageBox(NULL, err.what(), "Error", MB_ICONSTOP | MB_OK);
      }
   }

