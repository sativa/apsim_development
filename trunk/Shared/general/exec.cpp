#include <general\exec.h>
#include <general\string_functions.h>
#include <general\myvector.h>
#include <process.h>

// ------------------------------------------------------------------
//  Short description:
//    executes a program and optionally waits until it has finished.
//    Returns true if program was successfully executed.

//  Notes:

//  Changes:
//    DPH 17/4/1997
//    dph 20/5/98  added new algorithm based on createprocess.

// ------------------------------------------------------------------
bool Exec(const char* Command_line,
          unsigned int Show_flag,
          bool Wait_for_finish)
   {
   STARTUPINFO StartupInfo;
   PROCESS_INFORMATION ProcessInfo;

   memset(&StartupInfo, '\0', sizeof(STARTUPINFO));
   StartupInfo.cb = sizeof(StartupInfo);
   StartupInfo.dwFlags = STARTF_USESHOWWINDOW;
   StartupInfo.wShowWindow = Show_flag;
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
      return false;
   else
      {
      WaitForSingleObject(ProcessInfo.hProcess, INFINITE);
      //GetExitCodeProcess(ProcessInfo.hProcess, Result);
      return true;
      }
   }

