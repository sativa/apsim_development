#include <vcl\vcl.h>
#pragma hdrstop

//---------------------------------------------------------------------------
#include <general\path.h>
#include <general\stream_functions.h>
#include <general\exec.h>
#include <aps\apsuite.h>
#include <aps\license.h>
#include <aps\project_file.h>
#include <fstream.h>
#include <strstrea.h>
#include <dos.h>
#include <set>
#include <general\myvector.h>
#include <aps\apsim_configuration_collection.h>
#include <aps\apsim_simulation_collection.h>
USELIB("..\..\shared\aps\aps32.lib");
USELIB("..\..\shared\general\general.lib");
USEFORM("TPlease_wait_form.cpp", Please_wait_form);
USELIB("C:\Program Files\Borland\CBuilder3\Lib\memmgr.lib");
//---------------------------------------------------------------------------
#include "TPlease_wait_form.h"
//---------------------------------------------------------------------------
static const char* DEBUG_SWITCH = "-debug";
static const char* MAKE_SWITCH = "-make";
static const char* BUILD_SWITCH = "-build";
static const char* RUN_SWITCH = "-run";
static const char* COMPILE_BATCH_FILE_NAME = "compile.bat";

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
//    Compile everything.

//  Notes:

//  Changes:
//    DPH 23/4/98
//    dph 8/2/99 D229 Modified to use argv instead of command_line.

// ------------------------------------------------------------------
void Compile (list<string>& Project_files, bool Do_make_only, bool Do_debug)
   {
   // create a batch file name
   Path batch_file_name (APSDirectories().Get_working().c_str());
   batch_file_name.Set_name (COMPILE_BATCH_FILE_NAME);

   // create a compiler report file.
   Path compiler_rpt (APSDirectories().Get_working().c_str());
   compiler_rpt.Set_name ("compiler.rpt");

   // open for writing a batch file to compile everything.
   ofstream batch_file (batch_file_name.Get_path().c_str());
   batch_file << "@echo off" << endl;
   batch_file << "del " << compiler_rpt.Get_path() << endl;

   // loop through all project files and build.
   for (list<string>::iterator i = Project_files.begin();
                               i != Project_files.end();
                               i++)
      {
      Path prj_file ( (*i).c_str() );
      if (prj_file.Get_drive() == "")
         {
         prj_file.Set_to_cwd();
         prj_file.Append_path ( (*i).c_str() );
         }
      APSIM_project project( prj_file.Get_path().c_str() );
      string compiler_report_file = compiler_rpt.Get_path();
      project.Write_build_instructions (batch_file, compiler_report_file, Do_make_only);
      }
//   batch_file << "pause" << endl;
   batch_file.close();

   if (!Do_debug)
      {
      // call batch file to perform build.
      TForm* Please_wait = new TPlease_wait_form (NULL);
      Screen->Cursor = crHourGlass;
      Please_wait->Show();
      Application->ProcessMessages();
      Exec (batch_file_name.Get_path().c_str(), SW_HIDE, true);
      delete Please_wait;
      Screen->Cursor = crArrow;

      // run notepad on the compiler report file.
      if (compiler_rpt.Exists())
         {
         string command (APSDirectories().Get_home());
         command += "\\viewcmplmsg\\viewcmplmsg ";
         command += APSDirectories().Get_working() + "\\compiler.rpt";
         WinExec (command.c_str(), SW_SHOW);
         }
      else
         MessageBox(NULL, "Build complete.", "Information", MB_ICONINFORMATION | MB_OK);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    run APSIM for given configuration and control file.

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

   // create an APSShell configuration if necessary.
   if (Control_file_name_ptr != Project_files.begin())
      {
      Read_response_file ((*Project_files.begin()).c_str(), Project_files);

      // create an apsshell configuration
      APSIM_configuration_collection Configs;
      Configs.Set ("APSShell configuration", Project_files);

      // set name of apsim configuration to use.
      config_name = "APSShell configuration";
      }

   APSIM_simulation_collection Simulation_collection;
   Simulation_collection.Add(control_file_name.c_str());
   Simulation_collection.Set_configuration_name (config_name.c_str());
   Simulation_collection.Run();
   }

// ------------------------------------------------------------------
//  Short description:
//    Main program.

//  Notes:

//  Changes:
//    DPH 23/4/98
//    dph 8/2/99 D229 Modified to use argv instead of command_line.

// ------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR , int)
   {
   if (_argc >= 2)
      {
      bool Do_make = false;
      bool Do_build = false;
      bool Do_run = false;
      bool Do_debug = false;
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

         else if (_argv[i][0] == '@')
            Read_response_file (&_argv[i][1], Files);

         else
            Files.push_back (_argv[i]);
         }

      if (Do_make)
         Compile (Files, true, Do_debug);

      else if (Do_build)
         Compile (Files, false, Do_debug);

      else if (Do_run)
         Run (Files);

      else
         {
         MessageBox(NULL, "Usage: APSBuild [-debug] [-make | -build] APF_filename", "Error", MB_ICONSTOP | MB_OK);
         return 1;
         }
      }
   else
      {
      MessageBox(NULL, "Usage: APSBuild [-debug] [-make | -build] APF_filename", "Error", MB_ICONSTOP | MB_OK);
      return 1;
      }
   return 0;
   }

