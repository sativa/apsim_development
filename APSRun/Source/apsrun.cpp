//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include <ApsimShared\ApsimControlFile.h>
#include <ApsimShared\ApsimRunFile.h>
#include <ApsimShared\ApsimSimulationFile.h>
#include "TAPSIMRunForm.h"
USEFORM("TAPSIMRunForm.cpp", APSIMRunForm);
//---------------------------------------------------------------------------
using namespace std;

// ------------------------------------------------------------------
// This application will be passed either a control file (.CON), a
// run file (.RUN), or a .SIM file depending on what the user has
// right clicked on.
// ------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR,  int)
   {
   try
      {
      string fileName;
      bool quietRun = false;
      bool createSIM = false;
      for (int argIndex = 1; argIndex < _argc; argIndex++)
         {
         if (stricmp(_argv[argIndex], "/q") == 0)
            quietRun = true;
         else if (stricmp(_argv[argIndex], "/CreateSIM") == 0)
            createSIM = true;
         else
            fileName = _argv[argIndex];
         }
      if (!FileExists(fileName.c_str()))
         throw runtime_error("Cannot locate APSIM file: " + fileName);

      // Does the command line contain a control file?
      if (ExtractFileExt(fileName.c_str()).AnsiCompareIC(".con") == 0)
         {
         // yes - better ask user for a configuration.
         TAPSIMRunForm* runForm = new TAPSIMRunForm(Application);
         runForm->controlFilename = fileName;
         if (runForm->ShowModal() == mrOk)
            {
            vector<string> sections;
            runForm->getSelectedSimulations(sections);
            string configurationFile = runForm->getSelectedConfiguration();
            delete runForm;

            if (createSIM)
               {
               for (vector<string>::iterator sectionI = sections.begin();
                                             sectionI != sections.end();
                                             sectionI++)
                  {
                  ApsimControlFile controlFile(fileName, *sectionI);
                  controlFile.createSIM(configurationFile);
                  }
               }
            else
               {
               for (vector<string>::iterator sim = sections.begin();
                                             sim != sections.end();
                                             sim++)
                  {
                  ApsimControlFile simulation(fileName, *sim);
                  simulation.run(configurationFile, true);
                  }
               if (!quietRun)
                  MessageBox(NULL, "APSIM has finished", "For your information", MB_ICONINFORMATION | MB_OK);
               }
            }
         }
      else if (ExtractFileExt(fileName.c_str()).AnsiCompareIC(".run") == 0)
         {
         ApsimRunFile run(fileName);
         run.run(quietRun);
         }
      else if (ExtractFileExt(fileName.c_str()).AnsiCompareIC(".sim") == 0)
         {
         ApsimSimulationFile simulationFile(fileName);
         simulationFile.run(quietRun);
         }
      else
         throw runtime_error("Cannot run APSIM on file: " + fileName);
      }
   catch (const runtime_error& error)
      {
      MessageBox(NULL, error.what(), "Error", MB_ICONSTOP | MB_OK);
      return 1;
      }
   return 0;
   }
//---------------------------------------------------------------------------
