#include <general\exec.h>
#include <general\string_functions.h>
#include <vector>
using std::vector;
#include <process.h>
#include <fstream>
// ------------------------------------------------------------------
//  Short description:
//    executes a program and optionally waits until it has finished.
//    Returns true if program was successfully executed.  NOTE:  This
//    routine is NOT THREAD SAFE.  Wrap inside a Synchronize method call
//    if being called from a thread.

//  Notes:

//  Changes:
//    DPH 17/4/1997
//    dph 20/5/98  added new algorithm based on createprocess.
//    dph 21/4/99  added better error handling code.

// ------------------------------------------------------------------
bool GENERAL_EXPORT Exec(const char* Command_line,
                         unsigned int Show_flag,
                         bool Wait_for_finish)
   {
   STARTUPINFO StartupInfo;
   PROCESS_INFORMATION ProcessInfo;

   memset(&StartupInfo, '\0', sizeof(STARTUPINFO));
   StartupInfo.cb = sizeof(StartupInfo);
   StartupInfo.dwFlags = STARTF_USESHOWWINDOW;
   StartupInfo.wShowWindow = (WORD) Show_flag;
   if (!CreateProcess( NULL,
                       (char*) Command_line,   // pointer to command line string
                       NULL,                   // pointer to process security attributes
                       NULL,                   // pointer to thread security attributes
                       false,                  // handle inheritance flag
                       CREATE_NEW_CONSOLE |    // creation flags
                       NORMAL_PRIORITY_CLASS,
                       NULL,                   // pointer to new environment block
                       NULL,                   // pointer to current directory name
                       &StartupInfo,           // pointer to STARTUPINFO
                       &ProcessInfo) )         // pointer to PROCESS_INF
      {
      LPVOID lpMsgBuf;
      FormatMessage(
          FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
          NULL,
          GetLastError(),
          MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
          (LPTSTR) &lpMsgBuf,
          0,
          NULL);

      // Display the string.
      MessageBox(NULL, (char*) lpMsgBuf, "GetLastError", MB_OK|MB_ICONINFORMATION );

      // Free the buffer.
      LocalFree( lpMsgBuf );
      }
   else
      {                              
      WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
      //GetExitCodeProcess(ProcessInfo.hProcess, Result);
      return true;
      }
   return false;
   }

