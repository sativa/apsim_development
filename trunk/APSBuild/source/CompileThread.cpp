//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "CompileThread.h"
#include <general\exec.h>
#include <aps\apsuite.h>
#include <fstream>
#include <general\io_functions.h>
#include <general\stristr.h>
#pragma package(smart_init)

static const char* COMPILER_OUTPUT_FILENAME = "compiler.rpt";

static const char* APSBUILD_SECTION = "APSBuild";
static const char* BINARY_KEY = "binary";
static const char* IMPORT_KEY = "import";
static const char* LIBRARY_KEY = "library";
static const char* INCLUDE_KEY = "include";
static const char* SWITCHES_KEY = "switches";

static const char* AUTOMAKE_FILENAME = "automake.fig";
static const char* COMPILER_RESPONSE_FILENAME = "compiler.rsp";
static const char* LINKER_RESPONSE_FILENAME = "linker.rsp";

static const char* LF90FILE_KEY = "lf90file";
static const char* LF90FILE_SECTION = "lf90files";
// ------------------------------------------------------------------
//  Short description:
//     constructor

//  Notes:

//  Changes:
//    DPH 18/3/99

// ------------------------------------------------------------------
CompileThread::CompileThread (const char* File_name)
   : TThread(true)
   {
   Compiler_output_filename = APSDirectories().Get_working() + "\\" + COMPILER_OUTPUT_FILENAME;
   InitialFilename = File_name;
   DisplayMessage = NULL;
   }

// ------------------------------------------------------------------
//  Short description:
//     go compile project.

//  Notes:

//  Changes:
//    DPH 18/3/99

// ------------------------------------------------------------------
void __fastcall CompileThread::GoDisplayMessage (void)
   {
   if (DisplayMessage != NULL)
      DisplayMessage (NULL, Message.c_str());
   }
// ------------------------------------------------------------------
//  Short description:
//     go run command line.

//  Notes:

//  Changes:
//    DPH 18/3/99

// ------------------------------------------------------------------
void __fastcall CompileThread::RunCommandLine (void)
   {
   Exec(CommandLineToExecute.c_str(), SW_HIDE, true);
   }

// ------------------------------------------------------------------
//  Short description:
//     go compile project.

//  Notes:

//  Changes:
//    DPH 18/3/99

// ------------------------------------------------------------------
void __fastcall CompileThread::Execute (void)
   {
   APSIM_project apf (InitialFilename.c_str());
   CompileProject (apf);

   // get a list of child apfs and perform build on each of them.
   list<string> Keys, Children;
   apf.Get_keys (Keys);
   for (list<string>::iterator key = Keys.begin();
                               key != Keys.end();
                               key++)
      {
      string Child;
      apf.Get((*key).c_str(), Child);
      if (Child.find(".apf") != string::npos)
         {
         InitialFilename = Child;
         Execute();
         }
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     go compile a binary file.

//  Notes:

//  Changes:
//    DPH 18/3/99

// ------------------------------------------------------------------
void CompileThread::CompileProject (APSIM_project& apf)
   {
   // get a list of binary filenames.
   list<string> BinaryFileNames;
   GetBinaryFileNames(apf, BinaryFileNames);

   for (list<string>::iterator BinaryFileNameIter = BinaryFileNames.begin();
                               BinaryFileNameIter != BinaryFileNames.end();
                               BinaryFileNameIter++)
      {
      Path BinaryFile( (*BinaryFileNameIter).c_str());

      // display message on screen.
      Message = "Compiling " + BinaryFile.Get_path();
      Synchronize(GoDisplayMessage);

      // write banner to output file.
      WriteToOutputFile ("------Compiling " + BinaryFile.Get_path() + "------");

      // make sure the binary path exists.
      CreateDirectory (BinaryFile.Get_directory().c_str(), NULL);

      // create an AUTOMAKE file for this binary.
      CreateAutoMakeFile (apf, BinaryFile);

      // create a compiler response file for this binary.
      CreateCompilerResponseFile (apf);

      // create a linker response file for this binary.
      CreateLinkerResponseFile (apf);

      // copy all import files to source directory.
      CopyImportFiles (apf);

      // if this is a build then remove all .obj files.
      if (Build)
         DeleteFiles ("*.obj");

      // run AUTOMAKE
      if (!Debug)
         {
         RunAutoMake (apf, BinaryFile);

         // cleanup after ourselves.
         Cleanup ();
         }
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     return binary filename to caller.

//  Notes:

//  Changes:
//    DPH 18/3/99

// ------------------------------------------------------------------
void CompileThread::GetBinaryFileNames (APSIM_project& apf, list<string>& BinaryFileNames)
   {
   apf.Get(BINARY_KEY, BinaryFileNames, APSBUILD_SECTION, true);
   }

// ------------------------------------------------------------------
//  Short description:
//     write a string to the output file

//  Notes:

//  Changes:
//    DPH 18/3/99

// ------------------------------------------------------------------
void CompileThread::WriteToOutputFile (string& msg)
   {
   ofstream out (Compiler_output_filename.c_str(), ios::app);
   out << msg << endl;
   }

// ------------------------------------------------------------------
//  Short description:
//     create an automake file.

//  Notes:

//  Changes:
//    DPH 18/3/99

// ------------------------------------------------------------------
void CompileThread::CreateAutoMakeFile (APSIM_project& apf, Path& BinaryFile)
   {
   // open a stream to the file we're going to write to.
   string AutomakeFilename = GetSourceDirectory(apf) + "\\" + AUTOMAKE_FILENAME;
   ofstream out (AutomakeFilename.c_str());

   // write lf90 lines to automake file.
   out << "COMPILE = @lf90 @" << COMPILER_RESPONSE_FILENAME << " -c %sf%se -i %id >> " << Compiler_output_filename << endl;
   out << "LINK = @lf90 @%rf -out %ex @" << LINKER_RESPONSE_FILENAME;
   out                            << " >> " << Compiler_output_filename << endl;

   // get an include directory string.
   list<string> IncludeDirectories;
   GetFilesForCompiler (apf, INCLUDE_KEY, IncludeDirectories);
   string IncludeString;
   Build_string (IncludeDirectories, ";", IncludeString);

   // write out include directory string.
   out << "INCLUDE=" << IncludeString << endl;

   // write out target name.
   out << "TARGET=" << BinaryFile.Get_path() << endl;

   // get a list of all source files.
   list<string> SourceFiles;
   GetSourceFileNames(apf, SourceFiles);

   // for each source file write a line to automake file.
   for (list<string>::iterator i = SourceFiles.begin();
                               i != SourceFiles.end();
                               i++)
      {
      if (i != SourceFiles.begin())
         out << "AND" << endl;
      out << "FILES=" << *i << endl;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     create a compiler response file for this binary.

//  Notes:

//  Changes:
//    DPH 18/3/99

// ------------------------------------------------------------------
void CompileThread::CreateCompilerResponseFile (APSIM_project& apf)
   {
   // open a stream to the file we're going to write to.
   string CompilerResponseFilename = GetSourceDirectory(apf) + "\\" + COMPILER_RESPONSE_FILENAME;
   ofstream out (CompilerResponseFilename.c_str());

   // copy switches to our output stream.
   CopySwitchesToStream (apf, out);
   }

// ------------------------------------------------------------------
//  Short description:
//     create a linker response file for this binary.

//  Notes:

//  Changes:
//    DPH 18/3/99

// ------------------------------------------------------------------
void CompileThread::CreateLinkerResponseFile (APSIM_project& apf)
   {
   // open a stream to the file we're going to write to.
   string LinkerResponseFilename = GetSourceDirectory(apf) + "\\" + LINKER_RESPONSE_FILENAME;
   ofstream out (LinkerResponseFilename.c_str());

   // copy switches to our output stream.
   CopySwitchesToStream (apf, out);

   // get a list of all import files for this binary.
   list<string> ImportFiles;
   GetFilesForCompiler (apf, IMPORT_KEY, ImportFiles);

   // for each import output just the filename without any directory.
   for (list<string>::iterator i = ImportFiles.begin();
                              i != ImportFiles.end();
                              i++)
      {
      Path Imp( (*i).c_str() );
      out << Imp.Get_name() << endl;
      }

   // get a list of all library files for this binary.
   list<string> LibraryFiles;
   GetFilesForCompiler (apf, LIBRARY_KEY, LibraryFiles);

   // for each library, write library name to our file.
   for (list<string>::iterator i = LibraryFiles.begin();
                               i != LibraryFiles.end();
                               i++)
      out << "-lib " << *i << endl;
   }

// ------------------------------------------------------------------
//  Short description:
//     copy all import files to source directory.

//  Notes:

//  Changes:
//    DPH 18/3/99

// ------------------------------------------------------------------
void CompileThread::CopyImportFiles (APSIM_project& apf)
   {
   // get a list of all import files for this binary.
   list<string> ImportFiles;
   GetFilesForCompiler (apf, IMPORT_KEY, ImportFiles);

   // for each file copy to source directory.
   for (list<string>::iterator i = ImportFiles.begin();
                               i != ImportFiles.end();
                               i++)
      {
      Path ExistingImportFile ((*i).c_str());
      Path NewImportFile;
      NewImportFile.Set_directory (GetSourceDirectory(apf).c_str());
      NewImportFile.Set_name (ExistingImportFile.Get_name().c_str());
      CopyFile (ExistingImportFile.Get_path().c_str(), NewImportFile.Get_path().c_str(), FALSE);
      }
   }


// ------------------------------------------------------------------
//  Short description:
//     go run AUTOMAKE.

//  Notes:

//  Changes:
//    DPH 18/3/99

// ------------------------------------------------------------------
void CompileThread::RunAutoMake (APSIM_project& apf, Path& BinaryFile)
   {
   // run automake
   SetCurrentDirectory (GetSourceDirectory(apf).c_str());
   CommandLineToExecute = string("automake fig=") + AUTOMAKE_FILENAME;
   Synchronize(RunCommandLine);

   // need to modify the amtemp.bat file to add an extra line on the
   // end that deletes the target binary file on an unsuccessful link.
   string AmtempPath = GetSourceDirectory(apf) + "\\amtemp.bat";
   ofstream amtemp (AmtempPath.c_str(), ios::app);
   amtemp << "@IF ERRORLEVEL 1 del " << BinaryFile.Get_path() << endl;
   amtemp.close();

   // run batch file that automake has created.
   CommandLineToExecute = AmtempPath.c_str();
   Synchronize(RunCommandLine);

   // delete .MAP file
   Path GeneratedMapFile(BinaryFile);
   GeneratedMapFile.Set_extension(".map");
   DeleteFile(GeneratedMapFile.Get_path().c_str());
   }

// ------------------------------------------------------------------
//  Short description:
//     cleanup all unwanted files.

//  Notes:

//  Changes:
//    DPH 18/3/99

// ------------------------------------------------------------------
void CompileThread::Cleanup (void)
   {
   DeleteFile ("amtemp.bat");
   DeleteFiles ("automake.*");
//   DeleteFiles ("*.im$");
//   DeleteFiles ("*.xp$");
   DeleteFiles ("*.imp");
   DeleteFiles ("*.rsp");
   }

// ------------------------------------------------------------------
//  Short description:
//     copy contents of switches file to specified output stream.

//  Notes:

//  Changes:
//    DPH 18/3/99

// ------------------------------------------------------------------
void CompileThread::CopySwitchesToStream (APSIM_project& apf, ostream& out)
   {
   // get a list of all compiler switch files for this binary.
   list<string> SwitchFiles;
   GetFilesForCompiler (apf, SWITCHES_KEY, SwitchFiles);

   // for each switch file, copy contents to our response file.
   for (list<string>::iterator i = SwitchFiles.begin();
                               i != SwitchFiles.end();
                               i++)
      {
      ifstream in ( (*i).c_str() );
      string SwitchLine;
      getline(in, SwitchLine);
      out << SwitchLine << endl;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     return a list of source files to compile.

//  Notes:

//  Changes:
//    DPH 18/3/99

// ------------------------------------------------------------------
void CompileThread::GetSourceFileNames (APSIM_project& apf, list<string>& SourceFiles)
   {
   // get all keys for contents of this project.
   list<string> Keys;
   apf.Get_keys (Keys);

   // for each key we want to examine the key value and if it is source code
   // then add to return list.
   for (list<string>::iterator i = Keys.begin();
                               i != Keys.end();
                               i++)
      {
      string FileName;
      apf.Get((*i).c_str(), FileName);
      Path File(FileName.c_str());
      if (File.Get_extension() == ".for" ||
          File.Get_extension() == ".f90")
         SourceFiles.push_back (FileName);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     get the source code directory.

//  Notes:

//  Changes:
//    DPH 18/3/99

// ------------------------------------------------------------------
string CompileThread::GetSourceDirectory (APSIM_project& apf)
   {
   list<string> SourceFiles;
   GetSourceFileNames(apf, SourceFiles);
   if (SourceFiles.size() > 0)
      {
      Path Source ((*SourceFiles.begin()).c_str());
      return Source.Get_directory();
      }
   return "";
   }

// ------------------------------------------------------------------
//  Short description:
//     return a list of files from the compiler section

//  Notes:

//  Changes:
//    DPH 18/3/99

// ------------------------------------------------------------------
void CompileThread::GetFilesForCompiler (APSIM_project& apf, const char* Key, list<string>& Files)
   {
   // get name of compiler file.
   string lf90file;
   apf.Get(LF90FILE_KEY, lf90file, APSBUILD_SECTION, true);

   // open compiler file and return requested information.
   APSuite_ini Ini;
   Ini.Set_file_name (lf90file.c_str());
   string Key_values[MAX_NUM_KEYS];
   int Num_keys;
   Ini.Read_list (LF90FILE_SECTION, Key, Key_values, Num_keys);
   for (int i = 0; i < Num_keys; i++)
      Files.push_back (Key_values[i]);
   }

// ------------------------------------------------------------------
//  Short description:
//     delete files matching file specification

//  Notes:

//  Changes:
//    DPH 18/3/99

// ------------------------------------------------------------------
void CompileThread::DeleteFiles (const char* Filespec)
   {
   list<string> Files;
   Get_directory_listing (".", Filespec, Files, FA_NORMAL, true);
   for (list<string>::iterator i = Files.begin();
                               i != Files.end();
                               i++)
      DeleteFile( (*i).c_str() );
   }

