//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ApsimRuns.h"
#include "TRunForm.h"

USEFORM("TRunForm.cpp", RunForm);
//---------------------------------------------------------------------------
void processCmdLine(void)
   {
   ApsimRuns runs;

   string fileName;
   bool autoRun = false;
   for (int argIndex = 1; argIndex < _argc; argIndex++)
      {
      if (stricmp(_argv[argIndex], "/auto") == 0)
         autoRun = true;
      else
         runs.addSimulationsFromFile(_argv[argIndex]);
      }

   RunForm = new TRunForm(NULL);
   RunForm->setup(runs, autoRun);
   RunForm->Show();

   MSG msg;
   while (RunForm->Visible && GetMessage(&msg, 0, 0, 0) != 0)
      {
      TranslateMessage(&msg);
      DispatchMessage(&msg);
      }
   delete RunForm;
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
      ::MessageBox(NULL, exception.Message.c_str(), "Error", MB_ICONSTOP | MB_OK);
      }
   return 0;
   }

