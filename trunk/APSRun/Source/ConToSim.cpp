//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include <ApsimShared\SimCreator.h>


int main(int argc, char* argv[])
   //---------------------------------------------------------------------------
   // Main entry point.
   {
   try
      {
      string conFileName;
      vector<string> simNames;
      for (int i = 1; i < argc; i++)
         {
         if (conFileName == "")
            conFileName = argv[i];
         else
            simNames.push_back(argv[i]);
         }

      if (conFileName == "")
         throw runtime_error("No .con file specified on the command line");

      SimCreator simCreator(true);
      simCreator.ConToSim(conFileName, simNames);
      }
   catch (const std::runtime_error& err)
      {
      ::MessageBox(NULL, err.what(), "Error", MB_ICONSTOP | MB_OK);
      }
   catch (Exception &exception)
      {
      ::MessageBox(NULL, exception.Message.c_str(), "Error", MB_ICONSTOP | MB_OK);
      Application->ShowException(&exception);
      }

   return 0;
   }

