#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include <general\exec.h>
#include <general\string_functions.h>
#include <vector>
#include <process.h>
#include <fstream>
using namespace std;

#define UWM_PROCESS_TERMINATED_MSG _T("UWM_PROCESS_TERMINATED-{F7113F80-6D03-11d3-9FDD-006067718D04}")
UINT UWM_PROCESS_TERMINATED = ::RegisterWindowMessage(UWM_PROCESS_TERMINATED_MSG);
class ExecThread : public TThread
   {
   public:
      __fastcall ExecThread(HWND par, HANDLE processHandle, bool createSuspended)
         : TThread(createSuspended)
            {
            parent = par;
            hProcess = processHandle;
            }

      virtual void __fastcall Execute(void)
         {
         ::WaitForSingleObject(hProcess, INFINITE);
         PostMessage(parent, UWM_PROCESS_TERMINATED, 0, (LPARAM)hProcess);
         }

   private:
      HANDLE hProcess;
      HWND parent;

   };

// ------------------------------------------------------------------
//    executes a program and optionally waits until it has finished.
//    Returns true if program was successfully executed.  NOTE:  This
//    routine is NOT THREAD SAFE.  Wrap inside a Synchronize method call
//    if being called from a thread.
// ------------------------------------------------------------------
bool Exec(const char* Command_line,
          unsigned int Show_flag,
          bool waitForFinish)
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
                       NORMAL_PRIORITY_CLASS,  // creation flags
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
   else if (waitForFinish)
      {
      HWND parent = GetForegroundWindow();
      ExecThread* waitThread = new ExecThread(parent, ProcessInfo.hProcess, false);
      MSG msg;
      while (GetMessage(&msg, 0, 0, 0) && msg.message != UWM_PROCESS_TERMINATED)
         {
         TranslateMessage(&msg);
         DispatchMessage(&msg);
         }
      CloseHandle(ProcessInfo.hProcess);
      delete waitThread;
      return true;
      }
   return false;
   }

