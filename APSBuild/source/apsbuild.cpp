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
bool Do_debug = false;

// ------------------------------------------------------------------
//  Short description:
//    read the contents of the specified response file and return.

//  Notes:

//  Changes:
//    DPH 23/4/98

// ------------------------------------------------------------------
void Read_response_file (const char* Response_file_name, string& File_contents)
   {
   ifstream response (Response_file_name);

   Read_stream(response, File_contents);
    }

// ------------------------------------------------------------------
//  Short description:
//    Compile everything.

//  Notes:

//  Changes:
//    DPH 23/4/98

// ------------------------------------------------------------------
void Compile (string& Command_line, bool Do_make_only)
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

   // store all project files into a list.
   list<string> Project_files;
   Split_string (Command_line, ",\n", Project_files);

   // loop through all project files and build.
   for (list<string>::iterator i = Project_files.begin();
                               i != Project_files.end();
                               i++)
      {
      Path prj_file ( (*i).c_str() );
      if (prj_file.Get_directory() == "")
         prj_file.Set_to_cwd();
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

// ------------------------------------------------------------------
void Run (string& Command_line)
   {

   // separate command line arguments.
   vector<string> Command_line_args;
   Split_string (Command_line, " ", Command_line_args);

   string control_file_name, config_name;
   if (Command_line_args.size() == 1)
      {
      control_file_name = Command_line_args[0];
      config_name = "Standard APSIM Release";
      }

   else if (Command_line_args.size() == 2)
      {
      // first argument is the name of a file containing project file names.
      // read all project file names into list.
      list<string> Project_files;
      string File_contents;
      Read_response_file (Command_line_args[0].c_str(), File_contents);
      Split_string (File_contents, "\n", Project_files);

      // create an apsshell configuration
      APSIM_configuration_collection Configs;
      Configs.Set ("APSShell configuration", Project_files);

      // get name of control file which is second command line argument
      control_file_name = Command_line_args[1];
      config_name = "APSShell configuration";
      }

   if (Command_line_args.size() == 1 || Command_line_args.size() == 2)
      {
      APSIM_simulation_collection Simulation_collection;
      Simulation_collection.Add(control_file_name.c_str());
      Simulation_collection.Run();
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    Main program.

//  Notes:

//  Changes:
//    DPH 23/4/98

// ------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR Command_line, int)
   {
   istrstream in (Command_line);
   string Switch, Rest_of_line;
   in >> Switch;
   if (Switch == DEBUG_SWITCH)
      {
      Do_debug = true;
      in >> Switch;
      }
   getline(in, Rest_of_line);

   // if rest of line is a response file then open it and replace
   // the name of the response file with it's contents.
   if (Rest_of_line.length() > 0 && Rest_of_line[0] == '@')
      {
      string Contents;
      Read_response_file (Rest_of_line.substr(1).c_str(), Contents);
      Rest_of_line = Contents;
      }

   // test the switch to work out what to do.
   To_lower(Switch);
   if (Switch == MAKE_SWITCH)
      Compile (Rest_of_line, true);

   else if (Switch == BUILD_SWITCH)
      Compile (Rest_of_line, false);

   else if (Switch == RUN_SWITCH)
      Run (Rest_of_line);

   else
      {
      string Msg ("Bad command line switch : ");
      Msg += Switch;
      MessageBox(NULL, Msg.c_str(), "Error", MB_ICONSTOP | MB_OK);
      return 1;
      }
   return 0;
   }
//---------------------------------------------------------------------------
void dummy(void)
   {
   list<APSIM_project*> l;
   }

