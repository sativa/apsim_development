#include <general\exec.h>
#include <general\string_functions.h>
#include <general\myvector.h>
#include <process.h>

// ------------------------------------------------------------------
//  Short description:
//    executes a program and optionally waits until it has finished.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
void Exec(const char* Command_line,
          bool Wait_for_finish)
   {
   vector <string> words;
   string st(Command_line);
   Split_string (st, " ", words);

   char *args[20];
   for (int i = 0; i < words.size(); i++)
      args[i] = (char*) words[i].c_str();
   args[words.size()] = 0;

   if (Wait_for_finish)
      spawnv (P_WAIT, args[0], args);
   else
      spawnv (P_NOWAIT, args[0], args);

   }

