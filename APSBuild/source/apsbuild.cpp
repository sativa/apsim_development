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
USELIB("..\..\shared\aps\aps32.lib");
USELIB("..\..\shared\general\general.lib");
USEFORM("TPlease_wait_form.cpp", Please_wait_form);
#include "TPlease_wait_form.h"
//---------------------------------------------------------------------------
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
void Read_response_file (const char* Response_file_name, string& File_contents)
   {
   ifstream response (Response_file_name);

   Read_stream(response, File_contents);
   }

// ------------------------------------------------------------------
//  Short description:
//     write a description file to the specified stream.  Return true
//     if all went ok.

//  Notes:

//  Changes:
//    DPH 15/5/98
//    dph 4/9/98 modified to use the new project file class.

// ------------------------------------------------------------------
bool Write_description_file (ostream& out_desc, list<string>& project_files)
   {
   out_desc << "[system]" << endl;
   out_desc << "name = APSIM 1.5" << endl;

   // create a set of projects - sorted by the 'order' field in the project file.
   typedef std::set<APSIM_project, std::less<APSIM_project> > Project_set;
   Project_set Projects;
   bool Ok = true;
   for (list<string>::iterator i = project_files.begin();
                               i != project_files.end() && Ok;
                               i++)
      {
      APSIM_project project((*i).c_str());
      Projects.insert(project);
      }

   // loop through all projects
   for (Project_set::iterator i = Projects.begin();
                              i != Projects.end() && Ok;
                              i++)
      {
      APSIM_project project = *i;

      // read in a list of dll names for project.
      list<string> binary_paths;
      project.Get_binary_paths (binary_paths);
      for (list<string>::iterator binary = binary_paths.begin();
                                  binary != binary_paths.end();
                                  binary++)
         {
         Path dll_path ( (*binary).c_str() );
         if (dll_path.Get_extension() == ".dll")
            {
            if (strcmpi(project.Get_name().c_str(), "clock") == 0)
               out_desc << "sequencer=";
            else
               out_desc << "component=";

            out_desc << dll_path.Get_name_without_ext() << ',';
            out_desc << dll_path.Get_path() << endl;
            }
         }
      }
   return Ok;
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

// ------------------------------------------------------------------
//  Short description:
//    get a list of all project files.  This is used by the executable
//    version of APSShell and APSFront.

//  Notes:

//  Changes:
//    DPH 24/7/98

// ------------------------------------------------------------------
void Get_project_file_list(list<string>& Project_files)
   {
   // get apsim home directory.
   string apsim_home = APSDirectories().Get_home() + "\\apsim\\";

   // get a list of all licensed modules.
   list <string> Licensed_modules =
                APSLicence().Get_modules (License_key::SOURCE_OR_LIBRARY,
                                          License_key::LICENSED,
                                          License_key::RELEASED_OR_EXPERIMENTAL);

   for (list <string> ::iterator Iter = Licensed_modules.begin();
                                 Iter != Licensed_modules.end();
                                 Iter++)
      {
      string apf_file = apsim_home + *Iter + "\\" + *Iter + ".apf";
      Path apf_path (apf_file.c_str());
      if (apf_path.Exists())
         Project_files.push_back (apf_file);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//    run APSIM given the name of a file containing APF file names and
//    the name of a control file.

//  Notes:

//  Changes:
//    DPH 23/4/98
//    dph 24/7/98 D161

// ------------------------------------------------------------------
void Run (string& Command_line)
   {
   // separate command line arguments.
   vector<string> Command_line_args;
   Split_string (Command_line, " ", Command_line_args);

  list<string> Project_files;
  string control_file_name;
   if (Command_line_args.size() == 1)
      {
      Get_project_file_list(Project_files);
      control_file_name = Command_line_args[0];
      }

   else if (Command_line_args.size() == 2)
      {
      // first argument is the name of a file containing project file names.
      // read all project file names into list.
      string File_contents;
      Read_response_file (Command_line_args[0].c_str(), File_contents);
      Split_string (File_contents, "\n", Project_files);

      // get name of control file which is second command line argument
      control_file_name = Command_line_args[1];
      }

   if (Command_line_args.size() == 1 || Command_line_args.size() == 2)
      {
      // create a description file
      Path Dsc_path (APSDirectories().Get_working().c_str());
      Dsc_path.Set_name("apsim.dsc");
      ofstream dsc_stream (Dsc_path.Get_path().c_str());
      bool Ok = Write_description_file (dsc_stream, Project_files);

      // create a top level file.
      if (Ok)
         {
         Path Top_path = Dsc_path;
         Top_path.Set_extension(".top");
         ofstream Top_stream (Top_path.Get_path().c_str());
         Top_stream << "[mappings]" << endl;
         Top_stream << "Control_file=" << control_file_name << endl;
         Top_stream << "Description_file=" << Dsc_path.Get_path() << endl;
         Top_stream.close();

         // create a batch file to compile everything.
         Path APSIM_file_path (APSDirectories().Get_home().c_str());
         APSIM_file_path.Append_path("apsim\\apsim.exe");

         string command_line (APSIM_file_path.Get_path());
         command_line += " ";
         command_line += Top_path.Get_path();
         WinExec (command_line.c_str(), SW_SHOW);
         }
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
   

