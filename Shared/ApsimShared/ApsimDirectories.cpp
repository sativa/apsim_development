//---------------------------------------------------------------------------

#include <string>
#include <stdexcept>
#include <vector>
#include <general/TreeNodeIterator.h>
#include <general/xml.h>
#include <general/stl_functions.h>
#include <general/string_functions.h>
#include <general/path.h>
#include "FString.h"
#include "FStringExt.h"
#include "ApsimDirectories.h"
#include <general/platform.h>

using namespace std;

// ------------------------------------------------------------------
// This routine provides a way for APSIM applications to get the
// home directory.  Will throw a runtime error if the current
// Application is not in the apsim directory structure.
// ------------------------------------------------------------------

#ifdef __WIN32__

// Windows specific version:
#include <vcl.h>
extern HINSTANCE hInstance;

string EXPORT getApsimDirectory(void) throw(runtime_error)
   {

   char moduleFileName[MAX_PATH];
   GetModuleFileName(hInstance, moduleFileName, sizeof moduleFileName);
   Path path(moduleFileName);
   path.Set_name("apsim.ini");
   while (path.Back_up_directory() != "" && !path.Exists());

   if (!path.Exists())
      return Path(moduleFileName).Get_directory();
  
   return path.Get_directory();
   }
#else

// gnu libc specific version
#include <dlfcn.h>
extern "C" void EXPORT dummyFnPtr(void) {;};
string EXPORT getApsimDirectory(void) throw(runtime_error)
   {
   Dl_info dlinfo;
   if (dladdr((void *)dummyFnPtr, &dlinfo) != 0)
     {
       Path path(dlinfo.dli_fname);
       path.Set_name("apsim.ini");
       while (path.Back_up_directory() != "" && !path.Exists()) /*nothing*/;
       return path.Get_directory();
     }
   // cross fingers and hope for the best :)
   return "/usr/local/APSIM";
   }
#endif

// ------------------------------------------------------------------
// This routine provides a way for APSIM applications to get their
// home directory.  Will throw a runtime error if the current
// Application is not in the apsim directory structure.
// ------------------------------------------------------------------

std::string EXPORT getAppHomeDirectory(void) throw(std::runtime_error)
   {
   #ifdef __WIN32__
   string apsimDir = getApsimDirectory();
   string apsimIni = apsimDir + "\\apsim.ini";
   if (Path(apsimIni.c_str()).Exists())
      {
      string applicationName = Path(Application->ExeName.c_str()).Get_name_without_ext();
      return apsimDir + "\\" + applicationName;
      }
   else
      return apsimDir;
   #else
      throw runtime_error("getAppHomeDirectory not implemented");
   #endif
   }



