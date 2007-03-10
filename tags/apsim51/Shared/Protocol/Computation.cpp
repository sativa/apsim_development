#include <windows.h>

#include <dir.h>
#include "Computation.h"
#include "Transport.h"
#include <list>
#include <functional>
#include <general\path.h>
#include <ApsimShared\ApsimDirectories.h>
using namespace std;
using namespace protocol;

// ------------------------------------------------------------------
//  Short description:
//     Callback routine that all components call when sending a message.

//  Notes:

//  Changes:
//    dph 10/5/2001

// ------------------------------------------------------------------
void __stdcall messageCallback(const unsigned int* dummy, Message* message)
   {
   Transport::getTransport().deliverMessage(message);
   }

CallbackType* callback = &messageCallback;

// ------------------------------------------------------------------
//  Short description:
//    constructor

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
Computation::Computation(const string& name,
                         const string& fileName,
                         const string& componentInterfaceExecutable,
                         unsigned int componentId,
                         unsigned int parentId) throw (runtime_error)
   {
   // need to give the component to the transport layer.  Need a better
   // way of doing this.
   Transport::getTransport().addComponent(componentId, name, this);

   if (loadComponent(fileName, componentInterfaceExecutable))
      createInstance(fileName, componentId, parentId);
   }

// ------------------------------------------------------------------
//  Short description:
//    destructor

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
Computation::~Computation(void)
   {
   if (isOk())
      deleteInstance();
   unloadComponent();
   }

// ------------------------------------------------------------------
//  Short description:
//    call the CREATE entry point.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void Computation::createInstance(const std::string& filename,
                                 unsigned int componentId,
                                 unsigned int parentId)
   {
   static int dummy = 0;
   (*createInstanceProc) (filename.c_str(),
                          &componentId,
                          &parentId,
                          &instanceNo,
                          &dummy,
                          callback);
   }

// ------------------------------------------------------------------
//  Short description:
//    call the TERMINATE entry point

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void Computation::deleteInstance(void) const
   {
   (*deleteInstanceProc) (&instanceNo);
   }

// ------------------------------------------------------------------
//  Short description:
//    Returns the name of the wrapper filename

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void *Computation::loadDLL(const string& filename) throw (runtime_error)
   {
   void *result;
   char oldwd[MAX_PATH];

   getcwd(oldwd, MAX_PATH);
   chdir(Path(filename).Get_directory().c_str());  // XX may need to change drive too??
   result = LoadLibrary(filename.c_str());
   chdir(oldwd);

   if (result == NULL)
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
      string errorMessage = ("Cannot load DLL: " + filename + ".\n  " + (LPTSTR) lpMsgBuf);
      LocalFree( lpMsgBuf );

      throw runtime_error(errorMessage);
      }
   return result;   
   }

// ------------------------------------------------------------------
//  Short description:
//    Load the DLL and find pointers to all the entry points.
//    An exception is thrown if the dll cannot be loaded.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
bool Computation::loadComponent(const std::string& filename,
                                std::string componentInterfaceExecutable) throw (runtime_error)
   {
   executableFileName = filename;

   createInstanceProc = NULL;
   deleteInstanceProc = NULL;
   messageToLogicProc = NULL;

   string componentInterface;
   if (componentInterfaceExecutable != "")
      {
      componentInterface = componentInterfaceExecutable;
      handle = loadDLL(componentInterface.c_str());
      }
   else
      {
      handle = loadDLL(executableFileName);

      void _stdcall (*wrapperDll)(char* dllFileName);
      (FARPROC) wrapperDll = GetProcAddress(handle, "wrapperDLL");
      if (wrapperDll == NULL)
         throw runtime_error("Cannot find entry point 'wrapperDll' in dll: " + filename);

      // Go get the wrapperDll filename.
      char wrapperFileName[MAX_PATH];
      (*wrapperDll)(&wrapperFileName[0]);
      componentInterface = Path(&wrapperFileName[0]).Get_name();

      if (componentInterface != "")
         {
         char *cwd = getcwd(NULL, MAX_PATH);

         // This is a wrapped dll - it has no "entry points". Load the wrapper.
         FreeLibrary(handle);
         if (Str_i_Eq(componentInterface, "piwrapper.dll"))
            {
            Path(executableFileName).Change_directory();
            componentInterface = "C:\\Program Files\\Common Files\\AusFarm\\CMP\\piwrapper.dll";
            }
         else
            componentInterface = getApsimDirectory() + "\\bin\\" + componentInterface;
         handle = LoadLibrary(componentInterface.c_str());
         if (handle == NULL)
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
            string errorMessage = ("Cannot load DLL: " + componentInterface + ".\n  " + (LPTSTR) lpMsgBuf);
            LocalFree( lpMsgBuf );
            throw runtime_error(errorMessage);
            }
         chdir(cwd);
         free(cwd);
         }
      else
         {
         // This is not a wrapped dll - it will provide entrypoints itself
         }
      }

   (FARPROC) createInstanceProc = GetProcAddress(handle, "createInstance");
   (FARPROC) deleteInstanceProc = GetProcAddress(handle, "deleteInstance");
   (FARPROC) messageToLogicProc = GetProcAddress(handle, "messageToLogic");

   if (createInstanceProc == NULL ||
       deleteInstanceProc == NULL ||
       messageToLogicProc == NULL)
      {
      string msg = "Not a valid APSIM DLL.  Missing 1 or more entry points.  DLL="
            + filename + ", wrapper=" + componentInterface;
      throw runtime_error(msg);
      }
   return true;
   }

// ------------------------------------------------------------------
//  Short description:
//    Unload the specified dll.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
void Computation::unloadComponent(void)
   {
   if (handle != 0)
      FreeLibrary(handle);
   }
