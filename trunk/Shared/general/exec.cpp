#include <general\exec.h>
#include <general\mystring.h>

// ------------------------------------------------------------------
//  Short description:
//    executes a program and optionally waits until it has finished.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Exec(const char* Command_line,
          int Show_flag,
          const char* Window_title,
          bool Wait_for_finish)
   {
   // Tell windows to run the command line.

   WORD Result = (WORD) WinExec(Command_line, Show_flag);

   // Display appropriate error if necessary.

   string msg;
   if (Result == 8)
      msg = "Insufficient memory or resources";

   else if (Result == ERROR_FILE_NOT_FOUND)
      {
      msg = "Cannot find program to run.  Command line = ";
      msg += Command_line;
      }
   else if (Result == ERROR_PATH_NOT_FOUND)
      msg = "Cannot find specified path";

   else if (Result == ERROR_BAD_FORMAT)
      msg = "Bad .EXE format.";

   else if (Result <= 31)
      msg = "Cannot execute program for some unknown reason.";

   if (msg.length() > 0)
      throw msg;

   else
      {
      if (Wait_for_finish)
         {

         // Wait until time to go and  yield control to windows.

         MSG msg;
         while (FindWindow (NULL, Window_title) != NULL &&
                GetMessage(&msg, 0, 0, 0))
            {
            TranslateMessage(&msg);
            DispatchMessage(&msg);
            }
         }
      }
   }

