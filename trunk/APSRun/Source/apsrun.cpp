//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ApsimRuns.h"
#include "TRunForm.h"

USEFORM("TRunForm.cpp", RunForm);
//---------------------------------------------------------------------------
// Process the command line.
//---------------------------------------------------------------------------
void processCmdLine(void)
   {
   ApsimRuns runs;

   string fileName;
   bool quietRun = false;
   bool console = false;
   bool createSIM = false;
   bool runImmediately = false;
   for (int argIndex = 1; argIndex < _argc; argIndex++)
      {
      if (stricmp(_argv[argIndex], "/CreateSIM") == 0)
         createSIM = true;
      else if (stricmp(_argv[argIndex], "/q") == 0)
         quietRun = true;
      else if (stricmp(_argv[argIndex], "/Console") == 0)
         console = true;
      else if (stricmp(_argv[argIndex], "/run") == 0)
         runImmediately =  true;
      else
         runs.addSimulationsFromFile(_argv[argIndex]);
      }

   if (createSIM)
      {
      runs.convertFiles();
      runs.createSims();
      }
   else if (quietRun || runImmediately)
      {
      RunForm = new TRunForm(NULL);
      RunForm->setup(runs, console);
      RunForm->Show();
      RunForm->MainPanel->Visible = false;
      runs.convertFiles();
      runs.runApsim(quietRun, console, RunForm->OnRunNotifyEvent);
      }
   else
      {
      RunForm = new TRunForm(NULL);
      RunForm->setup(runs, console);
      RunForm->ShowModal();
      }
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

