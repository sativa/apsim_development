//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
#include <dos.h>
#include <fstream>
#include <general\stream_functions.h>
#include <aps\APSIMConfigurationCollection.h>
#include <aps\apsuite.h>
#include "TMainForm.h"

void Read_response_file (const char* Response_file_name, list<string>& Files);
void Run (list<string>& Project_files);

USERES("APSBuild.res");
USEFORM("TMainForm.cpp", MainForm);
USEUNIT("CompileThread.cpp");
USELIB("general.lib");
USELIB("aps32.lib");
USEUNIT("ComponentInterfaceGenerator.cpp");
USELIB("odl.lib");
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
   {
   static const char* DEBUG_SWITCH = "-debug";
   static const char* MAKE_SWITCH = "-make";
   static const char* BUILD_SWITCH = "-build";
   static const char* QUIET_SWITCH = "-quiet";
   static const char* COMPILETYPE_SWITCH = "-compiletype=";

   if (_argc >= 2)
      {
      bool Do_make = false;
      bool Do_build = false;
      bool Do_debug = false;
      bool Is_quiet = false;
      string compileType;
      list<string> Files;

      // loop through all command line switches.
      for (int i = 1; i < _argc; i++)
         {
         if (strcmpi(_argv[i], DEBUG_SWITCH) == 0)
            Do_debug = true;

         else if (strcmpi(_argv[i], MAKE_SWITCH) == 0)
            Do_make = true;

         else if (strcmpi(_argv[i], BUILD_SWITCH) == 0)
            Do_build = true;

         else if (strcmpi(_argv[i], QUIET_SWITCH) == 0)
            Is_quiet = true;

         else if (strncmpi(_argv[i], COMPILETYPE_SWITCH, strlen(COMPILETYPE_SWITCH)) == 0)
            {
            compileType = _argv[i];
            compileType.erase(0, strlen(COMPILETYPE_SWITCH));
            }

         else if (_argv[i][0] == '@')
            Read_response_file (&_argv[i][1], Files);

         else
            Files.push_back (_argv[i]);
         }

      if (Do_make || Do_build)
         {
   		Application->Initialize();
	   	Application->CreateForm(__classid(TMainForm), &MainForm);
       MainForm->ProjectFiles = Files;
         MainForm->Build = Do_build;
         MainForm->Debug = Do_debug;
         MainForm->Quiet = Is_quiet;
         MainForm->CompileType = compileType;
         Application->Run();
         }

      else
         {
         MessageBox(NULL, "Usage: APSBuild [-quiet] [-debug] [-make | -build] [-compiler=lf90 or lf95] APF_filename", "Error", MB_ICONSTOP | MB_OK);
         return 1;
         }
      }
   else
      {
      MessageBox(NULL, "Usage: APSBuild [-quiet] [-debug] [-make | -build] [-compiler=lf90 or lf95] APF_filename", "Error", MB_ICONSTOP | MB_OK);
      return 1;
      }
   return 0;
}
// ------------------------------------------------------------------
//  Short description:
//    read the contents of the specified response file and return.

//  Notes:

//  Changes:
//    DPH 23/4/98

// ------------------------------------------------------------------
void Read_response_file (const char* Response_file_name, list<string>& Files)
   {
   ifstream response (Response_file_name);

   string File_contents;
   Read_stream(response, File_contents);
   Split_string (File_contents, "\n", Files);
   }

