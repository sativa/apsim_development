//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ApsimDirectories.h"
#include <general\path.h>
using namespace std;
// ------------------------------------------------------------------
// This routine provides a way for APSIM applications to get the
// home directory.  Will throw a runtime error if the current
// Application is not in the apsim directory structure.
// ------------------------------------------------------------------
string getApsimDirectory(void) throw(runtime_error)
   {
   Path path(Application->ExeName.c_str());
   path.Set_name("version.ini");
   while (path.Get_path() != "" && !path.Exists())
      path.Back_up_directory();

   if (!path.Exists())
      {
      string msg = "The application: " + path.Get_path() + " is not in the "
                   "APSIM directory structure.";
      throw runtime_error(msg);
      }
   else
      return path.Get_path();
   }


