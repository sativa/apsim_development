//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop
#include "ApsimRuns.h"

USEFORM("TRunForm.cpp", RunForm);
//---------------------------------------------------------------------------
void processCmdLine(void)
   {
   ApsimRuns runs;

   string fileName;
   bool quietRun = false;
   bool console = false;
   bool createSIM = false;
   bool allRuns = false;
   bool run = false;
   for (int argIndex = 1; argIndex < _argc; argIndex++)
      {
      if (stricmp(_argv[argIndex], "/CreateSIM") == 0)
         {
         createSIM = true;
         allRuns = true;
         }
      else if (stricmp(_argv[argIndex], "/q") == 0)
         {
         quietRun = true;
         }
      else if (stricmp(_argv[argIndex], "/Console") == 0)
         console = true;
      else if (stricmp(_argv[argIndex], "/all") == 0)
         allRuns = true;
      else if (stricmp(_argv[argIndex], "/run") == 0)
         run =  true;
      else
         runs.addFile(_argv[argIndex], allRuns);
      }

   if (createSIM)
      runs.createSIMs();
   else
      runs.runAll(console, quietRun, run);
   }
// ------------------------------------------------------------------
// Main program entry point.
// ------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
   {
   try
      {
      Application->Initialize();
      processCmdLine();
      }
   catch (const std::runtime_error& err)
      {
      ::MessageBox(NULL, err.what(), "Error", MB_ICONSTOP | MB_OK);
      }
   catch (Exception &exception)
      {
      Application->ShowException(&exception);
      }
   catch (...)
      {
      try
         {
         throw Exception("");
         }
      catch (Exception &exception)
         {
         Application->ShowException(&exception);
         }
      }
   return 0;
   }

