//---------------------------------------------------------------------------
#include "Loader.h"
using std::string;
#include <list>
#include <algorithm>
#include <functional>
using std::list;
// ------------------------------------------------------------------
//  Short description:
//    Load the DLL with the specified filename.  Fill the specified
//    dll structure before returning.  An exception is thrown if
//    the dll cannot be loaded.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void PROTOCOLLoader::loadComponent(const string& dllFileName,
                                   PROTOCOLDLLInfo& dllInfo)
   {
   static list<string> dllFilesLoaded;

   dllInfo.fileName = dllFileName;
   dllInfo.handle = LoadLibrary(dllFileName.c_str());
   dllFilesLoaded.push_back (dllFileName);
   dllInfo.instanceNo = 0;
   std::count_if (dllFilesLoaded.begin(),
                  dllFilesLoaded.end(),
                  std::bind1st(std::equal_to<string>(), dllFileName),
                  dllInfo.instanceNo);
   if (dllInfo.handle != NULL)
      {
      (FARPROC) dllInfo.CREATEProc = GetProcAddress(dllInfo.handle, "create");
      (FARPROC) dllInfo.INITProc = GetProcAddress(dllInfo.handle, "init");
      (FARPROC) dllInfo.TERMProc = GetProcAddress(dllInfo.handle, "term");
      (FARPROC) dllInfo.ACTIONProc = GetProcAddress(dllInfo.handle, "action");
      (FARPROC) dllInfo.INEVENTProc = GetProcAddress(dllInfo.handle, "inevent");
      if (dllInfo.CREATEProc == NULL ||
          dllInfo.INITProc == NULL ||
          dllInfo.TERMProc == NULL ||
          dllInfo.ACTIONProc == NULL ||
          dllInfo.INEVENTProc == NULL)
          {
          throw string("Not a valid APSIM DLL.  Missing 1 or more entry points.  DLL=" +
                       dllFileName);
          }
      }
   else// if (dllFileName != "")
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
      string errorMessage = ("Cannot load DLL: " + dllFileName + ".  " + (LPTSTR) lpMsgBuf);
      LocalFree( lpMsgBuf );

      throw errorMessage;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    Unload the specified dll.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void PROTOCOLLoader::unloadComponent(const PROTOCOLDLLInfo& dllInfo)
   {
   FreeLibrary(dllInfo.handle);
   }
   
