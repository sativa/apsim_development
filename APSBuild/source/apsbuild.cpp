//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
#include <dos.h>
#include <fstream>
#include <general\stream_functions.h>
#include <aps\apsim_run_collection.h>
#include <aps\apsim_configuration_collection.h>
#include <aps\apsuite.h>
#include "TMainForm.h"

void Read_response_file (const char* Response_file_name, list<string>& Files);
void Run (list<string>& Project_files);

USERES("APSBuild.res");
USEFORM("TMainForm.cpp", MainForm);
USEUNIT("CompileThread.cpp");
USELIB("..\..\shared\general\general.lib");
USELIB("..\..\shared\aps\aps32.lib");
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
   {
   static const char* DEBUG_SWITCH = "-debug";
   static const char* MAKE_SWITCH = "-make";
   static const char* BUILD_SWITCH = "-build";
   static const char* RUN_SWITCH = "-run";
   static const char* QUIET_SWITCH = "-quiet";

   if (_argc >= 2)
      {
      bool Do_make = false;
      bool Do_build = false;
      bool Do_run = false;
      bool Do_debug = false;
      bool Is_quiet = false;
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

         else if (strcmpi(_argv[i], RUN_SWITCH) == 0)
            Do_run = true;

         else if (strcmpi(_argv[i], QUIET_SWITCH) == 0)
            Is_quiet = true;

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
         Application->Run();
         }

      else if (Do_run)
         Run (Files);

      else
         {
         MessageBox(NULL, "Usage: APSBuild [-quiet] [-debug] [-make | -build] APF_filename", "Error", MB_ICONSTOP | MB_OK);
         return 1;
         }
      }
   else
      {
      MessageBox(NULL, "Usage: APSBuild [-quiet] [-debug] [-make | -build] APF_filename", "Error", MB_ICONSTOP | MB_OK);
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

// ------------------------------------------------------------------
//  Short description:
//    run APSIM for given configuration and control file.  Only ever
//    called by APSShell and APSFront.

//  Notes:

//  Changes:
//    DPH 23/4/98
//    dph 8/2/99 D229 Modified to use argv instead of command_line.

// ------------------------------------------------------------------
void Run (list<string>& Project_files)
   {
   // get a pointer to the control file name
   list<string>::iterator Control_file_name_ptr = Project_files.end();
   Control_file_name_ptr--;

   // get control file name - always last filename in Project_files
   string control_file_name = *Control_file_name_ptr;
   string config_name = "Standard APSIM Release";

   // create and setup simulation collection object.
   APSIM_simulation_collection Simulation_collection;
   Simulation_collection.Set(control_file_name.c_str());

   // are we doing an APSShell run?
   APSIM_configuration Config;
   if (Control_file_name_ptr != Project_files.begin())
      {
      // yes - don't ask user for configuration.
      Read_response_file ((*Project_files.begin()).c_str(), Project_files);

      // create an apsshell configuration
      Config = APSIM_configuration_collection::Get_apsshell_configuration();
      Config.Set_projects (Project_files);  
      }
   else
      {
      // no - must be an APSFront run - use standard release.
      Config = APSIM_configuration_collection::Get_standard_configuration();
      }

   string DefaultRunFileName = APSDirectories().Get_working() + "\\default.run";
   APSIM_run_collection Run (DefaultRunFileName.c_str());
   Run.Add ("Simulation", Config, Simulation_collection);
   Run.Run();
   }


