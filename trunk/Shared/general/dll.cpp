//---------------------------------------------------------------------------
#pragma hdrstop

#include "dll.h"
#ifdef __WIN32__
   #include <windows.h>
   #include <dir.h>
#else
   #include <dlfcn.h>
#endif
#include <general/path.h>

using namespace std;
void *loadDLL(const string& filename)
   {
   // ------------------------------------------------------------------
   // Loads a dll into memory and returns a handle to it.
   // ------------------------------------------------------------------
   void* result;
   string errorMessage;
   #ifdef __WIN32__
      char oldwd[MAX_PATH];
      getcwd(oldwd, MAX_PATH);
      chdir(fileDirName(filename).c_str());
      result = LoadLibrary(filename.c_str());
      chdir(oldwd);

      if (result == NULL )
         {
         // Get windows error message.
         LPVOID lpMsgBuf;
         FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
                       NULL,
                       GetLastError(),
                       MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
                       (LPTSTR) &lpMsgBuf,
                       0,
                       NULL
                       );


         errorMessage = ("Cannot load DLL: " + filename + ".\n  " + (LPTSTR) lpMsgBuf);
         LocalFree( lpMsgBuf );
         }
   #else
      result = dlopen(filename.c_str(), RTLD_NOW|RTLD_LOCAL);
      char *dllerr = dlerror();
      if (dllerr != NULL)
         errorMessage = string("Cannot load DLL: ") + filename + ".\n" + dllerr;
   #endif
   if (errorMessage != "")
      throw runtime_error(errorMessage);

   return result;
   }

void closeDLL(void* handle)
   {
   // ------------------------------------------------------------------
   // Close the specified dll handle.
   // ------------------------------------------------------------------
   #ifdef __WIN32__
      FreeLibrary(handle);
   #else
      dlclose(handle);
   #endif
   }

void* dllProcAddress(void* dllHandle, const char* name)
   {
   // ------------------------------------------------------------------
   // Return the address of a function in the dll specified by dllHandle.
   // ------------------------------------------------------------------
   #ifdef __WIN32__
      return GetProcAddress(dllHandle, name);
   #else
      return dlsym(handle, name);
   #endif
   }

