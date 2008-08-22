//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop
#include <dos.h>
#include <fstream>
#include <general\stream_functions.h>
#include "TMainForm.h"

USEFORM("TMainForm.cpp", MainForm);
//---------------------------------------------------------------------------
void Read_response_file (const char* Response_file_name, list<string>& Files);
void Run (list<string>& Project_files);

//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
   {
   static const char* DEBUG_SWITCH = "-debug";
   static const char* MAKE_SWITCH = "-make";
   static const char* BUILD_SWITCH = "-build";
   static const char* QUIET_SWITCH = "-quiet";
   static const char* COMPILERFILE_SWITCH = "-compilerfile=";
   static const char* STDOUT_SWITCH = "-stdout";
   static const char* OUT_SWITCH = "-out";

   if (_argc >= 2)
      {
      bool Do_make = false;
      bool Do_build = false;
      bool Do_debug = false;
      bool Is_quiet = false;
      bool Do_stdout = false;
      bool getOutFile = false;
      string outFile;
      string compilerFile;
      list<string> Files;

      // loop through all command line switches.
      for (int i = 1; i < _argc; i++)
         {
         if (getOutFile)
            {
            getOutFile = false;
            outFile = _argv[i];
            }

         else if (strcmpi(_argv[i], DEBUG_SWITCH) == 0)
            Do_debug = true;

         else if (strcmpi(_argv[i], MAKE_SWITCH) == 0)
            Do_make = true;

         else if (strcmpi(_argv[i], BUILD_SWITCH) == 0)
            Do_build = true;

         else if (strcmpi(_argv[i], QUIET_SWITCH) == 0)
            Is_quiet = true;

         else if (strcmpi(_argv[i], STDOUT_SWITCH) == 0)
            Do_stdout = true;

         else if (strcmpi(_argv[i], OUT_SWITCH) == 0)
            getOutFile = true;

         else if (strncmpi(_argv[i], COMPILERFILE_SWITCH, strlen(COMPILERFILE_SWITCH)) == 0)
            {
            compilerFile = _argv[i];
            compilerFile.erase(0, strlen(COMPILERFILE_SWITCH));
            }

         else if (_argv[i][0] == '@')
            Read_response_file (&_argv[i][1], Files);

         else
            Files.push_back (_argv[i]);
         }

      if (Do_make || Do_build)
         {
   		Application->Initialize();
         Application->CreateHandle();
	   	Application->CreateForm(__classid(TMainForm), &MainForm);
         MainForm->ProjectFiles = Files;
         MainForm->Build = Do_build;
         MainForm->Debug = Do_debug;
         MainForm->Quiet = Is_quiet;
         MainForm->Stdout = Do_stdout;
         MainForm->CompilerFile = compilerFile;
         MainForm->OutFile = outFile;
         Application->Run();
         }

      else
         {
         MessageBox(NULL, "Usage: APSBuild [-quiet] [-stdout] [-debug] [-out outputfile] [-make | -build] [-compilerFile=xxx.compile] APF_filename", "Error", MB_ICONSTOP | MB_OK);
         return 1;
         }
      }
   else
      {
      MessageBox(NULL, "Usage: APSBuild [-quiet] [-stdout] [-debug] [-out outputfile] [-make | -build] [-compilerFile=xxx.compile] APF_filename", "Error", MB_ICONSTOP | MB_OK);
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

