#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include <general\exec.h>
#include <general\string_functions.h>
#include <vector>
#include <process.h>
#include <fstream>
using namespace std;

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
   vector<string> arguments;
   SplitStringHonouringQuotes(Command_line, " ", arguments);
   char* argv[100];
   for (unsigned i = 0; i != arguments.size(); i++)
      {
      replaceAll(arguments[i], "\"", "");
      argv[i] = (char*) arguments[i].c_str();
      }
   argv[arguments.size()] = NULL;

   return (spawnvp(P_WAIT, arguments[0].c_str(), argv) != -1);
   }

