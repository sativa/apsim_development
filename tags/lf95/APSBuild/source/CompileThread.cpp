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
using std::ofstream;
using std::ifstream;

static const char* COMPILER_OUTPUT_FILENAME = "compiler.rpt";

static const char* APSBUILD_SECTION = "APSBuild";
static const char* BINARY_KEY = "binary";
static const char* IMPORT_KEY = "import";
static const char* OBJECT_KEY = "object";
static const char* LIBRARY_KEY = "library";
static const char* INCLUDE_KEY = "include";
static const char* SWITCHES_KEY = "switches";
static const char* SOURCE_KEY = "source";

static const char* AUTOMAKE_FILENAME = "automake.fig";
static const char* COMPILER_RESPONSE_FILENAME = "compiler.rsp";
static const char* LINKER_RESPONSE_FILENAME = "linker.rsp";

static const char* MODULETYPE_KEY = "moduletype";
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
   // get compiler to use if necessary
   if (CompileType == "")
      APSConfig().Read("apsuite", "apsuite", "CompileType", CompileType);
   if (CompileType == "")
      CompileType = "lf90";

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
//    dph 6/2/2000 added code to delete *.xp$, *.im$ when building

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

      // if this is a build then remove all .obj files.
      if (Build)
         {
         DeleteFiles (apf, "*.obj");
         DeleteFiles (apf, "*.xp$");
         DeleteFiles (apf, "*.im$");
         DeleteFiles (apf, "*.mod");
         DeleteFiles (apf, "*.lib");
         }

      // make sure the binary path exists.
      CreateDirectory (BinaryFile.Get_directory().c_str(), NULL);

      // create an AUTOMAKE file for this binary.
      CreateAutoMakeFile (apf, BinaryFile);

      // create a compiler response file for this binary.
      CreateCompilerResponseFile (apf);

      // create a linker response file for this binary.
      CreateLinkerResponseFile (apf);

      // copy all foreign source files to current directory
      CopySourceFiles (apf);

      // run AUTOMAKE
      RunAutoMake (apf, BinaryFile);

      if (!Debug)
         {
         // cleanup after ourselves.
         Cleanup (apf);
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
   ofstream out (Compiler_output_filename.c_str(), std::ios::app);
   out << msg << std::endl;
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
   // open the automake template from.
   string AutomakeTemplate = APSDirectories().Get_home() +
                             "\\apsbuild\\automake." +
                             CompileType;
   if (!FileExists(AutomakeTemplate.c_str()))
      AutomakeTemplate = APSDirectories().Get_home() + "\\apsbuild\\automake.fig";
   ifstream in(AutomakeTemplate.c_str());

   // open a stream to the file we're going to write to.
   string AutomakeFilename = GetSourceDirectory(apf) + "\\" + AUTOMAKE_FILENAME;
   ofstream out (AutomakeFilename.c_str());

   // write template out
   string Line;
   getline (in, Line);
   while (in)
      {
      out << Line << std::endl;
      getline (in, Line);
      }

   // write out target name.
   out << "TARGET=" << BinaryFile.Get_path() << std::endl;

   // get a list of all source files.
   list<string> SourceFiles;
   GetSourceFileNames(apf, SourceFiles);

   // for each source file write a line to automake file.
   for (list<string>::iterator i = SourceFiles.begin();
                               i != SourceFiles.end();
                               i++)
      {
      Path sourcePath( (*i).c_str() );
      if (i != SourceFiles.begin())
         out << "AND" << std::endl;
      out << "FILES=" << sourcePath.Get_name() << std::endl;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     create a compiler response file for this binary.

//  Notes:

//  Changes:
//    DPH 18/3/99
//    dph 6/6/2000 added code to write include string to response file

// ------------------------------------------------------------------
void CompileThread::CreateCompilerResponseFile (APSIM_project& apf)
   {
   // open a stream to the file we're going to write to.
   string CompilerResponseFilename = GetSourceDirectory(apf) + "\\" + COMPILER_RESPONSE_FILENAME;
   ofstream out (CompilerResponseFilename.c_str());

   // copy switches to our output stream.
   CopySwitchesToStream (apf, out);

   // get an include directory string and write to response file
   list<string> IncludeDirectories;
   GetFilesForCompiler (apf, INCLUDE_KEY, IncludeDirectories);
   string IncludeString;
   Build_string (IncludeDirectories, ";", IncludeString);
   out << "-i " << IncludeString;
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

   // for each import output to linker response file
   for (list<string>::iterator i = ImportFiles.begin();
                              i != ImportFiles.end();
                              i++)
      {
      out << *i << std::endl;
      }

   // get a list of all object files for this binary.
   list<string> ObjectFiles;
   GetFilesForCompiler (apf, OBJECT_KEY, ObjectFiles);
   for (list<string>::iterator i = ObjectFiles.begin();
                              i != ObjectFiles.end();
                              i++)
      {
      out << (*i) << std::endl;
      }

   // get a list of all library files for this binary.
   list<string> LibraryFiles;
   GetFilesForCompiler (apf, LIBRARY_KEY, LibraryFiles);

   // for each library, write library name to our file.
   for (list<string>::iterator i = LibraryFiles.begin();
                               i != LibraryFiles.end();
                               i++)
      out << "-lib " << *i << std::endl;
   }

// ------------------------------------------------------------------
//  Short description:
//     copy all source files to source directory if necessary

//  Notes:

//  Changes:
//    DPH 6/6/2000

// ------------------------------------------------------------------
void CompileThread::CopySourceFiles (APSIM_project& apf)
   {
   // get a list of all import files for this binary.
   list<string> SourceFiles;
   GetFilesForCompiler (apf, SOURCE_KEY, SourceFiles);

   // for each file copy to source directory.
   for (list<string>::iterator i = SourceFiles.begin();
                               i != SourceFiles.end();
                               i++)
      {
      Path ExistingSourceFile ((*i).c_str());
      Path NewSourceFile;
      NewSourceFile.Set_directory (GetSourceDirectory(apf).c_str());
      NewSourceFile.Set_name (ExistingSourceFile.Get_name().c_str());
      CopyFile (ExistingSourceFile.Get_path().c_str(), NewSourceFile.Get_path().c_str(), FALSE);

      // make sure file is not readonly.
      SetFileAttributes( NewSourceFile.Get_path().c_str(), FILE_ATTRIBUTE_NORMAL);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     delete foreign source files

//  Notes:

//  Changes:
//    DPH 6/6/2000

// ------------------------------------------------------------------
void CompileThread::DeleteSourceFiles (APSIM_project& apf)
   {
   // get a list of all import files for this binary.
   list<string> SourceFiles;
   GetFilesForCompiler (apf, SOURCE_KEY, SourceFiles);

   // for each file copy to source directory.
   for (list<string>::iterator i = SourceFiles.begin();
                               i != SourceFiles.end();
                               i++)
      {
      Path ExistingSourceFile ((*i).c_str());
      Path NewSourceFile;
      NewSourceFile.Set_directory (GetSourceDirectory(apf).c_str());
      NewSourceFile.Set_name (ExistingSourceFile.Get_name().c_str());
      if (!Str_i_Eq(ExistingSourceFile.Get_directory(),
                   NewSourceFile.Get_directory()))
         DeleteFile (NewSourceFile.Get_name().c_str());
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

   if (!Debug)
      {
      // need to modify the amtemp.bat file to add an extra line on the
      // end that deletes the target binary file on an unsuccessful link.
      string AmtempPath = GetSourceDirectory(apf) + "\\amtemp.bat";
      ofstream amtemp (AmtempPath.c_str(), std::ios::app);
      amtemp << "@IF ERRORLEVEL 1 del " << BinaryFile.Get_path() << std::endl;
      amtemp.close();

      // run batch file that automake has created.
      CommandLineToExecute = AmtempPath + " >> " + APSDirectories().Get_working() + "\\compiler.rpt";
      Synchronize(RunCommandLine);

      // delete .MAP file
      Path GeneratedMapFile(BinaryFile);
      GeneratedMapFile.Set_extension(".map");
      DeleteFile(GeneratedMapFile.Get_path().c_str());
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     cleanup all unwanted files.

//  Notes:

//  Changes:
//    DPH 18/3/99

// ------------------------------------------------------------------
void CompileThread::Cleanup (APSIM_project& apf)
   {
   DeleteFile ("amtemp.bat");
   DeleteFiles (apf, "automake.*");
   DeleteFiles (apf, "*.rsp");
   DeleteSourceFiles (apf);
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
   string SwitchesFile = APSDirectories().Get_home() +
                             "\\apsbuild\\switches." +
                             CompileType;
   if (!FileExists(SwitchesFile.c_str()))
      SwitchesFile = APSDirectories().Get_home() +
                             "\\apsbuild\\switches.fig";

   ifstream in ( SwitchesFile.c_str() );
   string SwitchLine;
   getline(in, SwitchLine);
   out << SwitchLine << std::endl;
   }

// ------------------------------------------------------------------
//  Short description:
//     return a list of source files to compile.

//  Notes:

//  Changes:
//    DPH 18/3/99
//    dph 6/6/2000 added code to also look for source files in the
//                 compiler file.

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

   // now we need to look in the compiler file to see if there are any more.
   GetFilesForCompiler (apf, SOURCE_KEY, SourceFiles);
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
//    DPH 5/6/2000 added support for LF95

// ------------------------------------------------------------------
void CompileThread::GetFilesForCompiler (APSIM_project& apf, const char* Key, list<string>& Files)
   {
   // get name of compiler file.
   string st;
   apf.Get(MODULETYPE_KEY, st, APSBUILD_SECTION, false);
   string compilerFile = APSDirectories().Get_home() + "\\apsbuild\\" + st + "." + CompileType;
   if (!FileExists(compilerFile.c_str()))
      compilerFile = APSDirectories().Get_home() + "\\apsbuild\\" + st + ".fig";


   // open compiler file and return requested information.
   APSuite_ini Ini;
   Ini.Set_file_name (compilerFile.c_str());
   string Key_values[MAX_NUM_KEYS];
   int Num_keys;
   string compilerSection = "Instructions";
   Ini.Read_list (compilerSection.c_str(), Key, Key_values, Num_keys);
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
void CompileThread::DeleteFiles (APSIM_project& apf, const char* Filespec)
   {
   list<string> Files;
   Get_directory_listing (GetSourceDirectory(apf).c_str(), Filespec, Files, FA_NORMAL, true);
   for (list<string>::iterator i = Files.begin();
                               i != Files.end();
                               i++)
      {
      DeleteFile( (*i).c_str() );
      }
   }
