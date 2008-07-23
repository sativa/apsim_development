//---------------------------------------------------------------------------

#include <string>
#include <stdexcept>
#include <general/platform.h>
#include <general/string_functions.h>
#include <general/path.h>
#include "ApsimDirectories.h"

using namespace std;

// ------------------------------------------------------------------
// This routine provides a way for APSIM applications to get the
// home directory.  Will throw a runtime error if the current
// Application is not in the apsim directory structure.
// ------------------------------------------------------------------

#ifdef __WIN32__

#include <windows.h>
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
       string dll(dlinfo.dli_fname);
       while ((dll = fileDirName(dll)) != "" && !fileExists(dll + "/Apsim.ini")) /*nothing*/;
       return dll;
     }
   // cross fingers and hope for the best :)
   return "/usr/local/APSIM";
   }
#endif



