//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "CompileThread.h"
#include "CreateDataTypesF90.h"
#include <general\exec.h>
#include <fstream>
#include <general\io_functions.h>
#include <general\stristr.h>
#include <ApsimShared\ApsimDirectories.h>
#pragma package(smart_init)
using namespace std;

static const char* COMPILER_OUTPUT_FILENAME = "compiler.rpt";

static const char* APSBUILD_SECTION = "APSBuild";
static const char* BINARY_KEY = "binary";
static const char* IMPORT_KEY = "import";
static const char* OBJECT_KEY = "object";
static const char* LIBRARY_KEY = "library";
static const char* INCLUDE_KEY = "include";
static const char* MODULE_KEY = "module";
static const char* SOURCE_KEY = "source";
static const char* SWITCHES_KEY = "switches";

static const char* AUTOMAKE_FILENAME = "automake.fig";
static const char* COMPILER_RESPONSE_FILENAME = "compiler.rsp";
static const char* LINKER_RESPONSE_FILENAME = "linker.rsp";

static const char* COMPILER_FILE_KEY = "compilerFile";
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
   Compiler_output_filename = Path::getTempFolder().Get_path() + "\\" + COMPILER_OUTPUT_FILENAME;
   DeleteFile(Compiler_output_filename.c_str());

   InitialFilename = File_name;
   DisplayMessage1 = NULL;
   DisplayMessage2 = NULL;
   }

// ------------------------------------------------------------------
//  Short description:
//     destructor

//  Notes:

//  Changes:
//    DPH 18/3/99

// ------------------------------------------------------------------
__fastcall CompileThread::~CompileThread (void)
   {
   }
// ------------------------------------------------------------------
//  Short description:
//     go compile project.

//  Notes:

//  Changes:
//    DPH 18/3/99

// ------------------------------------------------------------------
void __fastcall CompileThread::goDisplayMessage1 (void)
   {
   if (DisplayMessage1 != NULL)
      DisplayMessage1 (NULL, Message.c_str());
   }
// ------------------------------------------------------------------
//  Short description:
//     go compile project.

//  Notes:

//  Changes:
//    DPH 18/3/99

// ------------------------------------------------------------------
void __fastcall CompileThread::goDisplayMessage2 (void)
   {
   if (DisplayMessage2 != NULL)
      DisplayMessage2 (NULL, Message.c_str());
   }

// ------------------------------------------------------------------
//  Short description:
//     go run command line.

//  Notes:

//  Changes:
//    DPH 18/3/99

// ------------------------------------------------------------------
void __fastcall CompileThread::runCommandLine (void)
   {
   Exec(CommandLineToExecute.c_str(), SW_HIDE, true);
   }

// ------------------------------------------------------------------
//  Short description:
//     go compile project.

//  Notes:

//  Changes:
//    DPH 18/3/99
//    dph 13/12/2000 added a try/catch block.

// ------------------------------------------------------------------
void __fastcall CompileThread::Execute (void)
   {
   try
      {
      ApsimProject apf (InitialFilename.c_str());
      compileProject (apf);

      // get a list of child apfs and perform build on each of them.
      vector<string> Keys, Children;
      apf.getKeys(Keys);
      for (vector<string>::iterator key = Keys.begin();
                                    key != Keys.end() && !Terminated;
                                    key++)
         {
         string Child;
         apf.get((*key).c_str(), Child);
         if (Child.find(".apf") != string::npos)
            {
            InitialFilename = Child;
            Execute();
            }
         }
      }
   catch (string& msg)
      {
      ShowMessage(msg.c_str());
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     go compile a binary file.

//  Notes:

//  Changes:
//    DPH 18/3/99
//    dph 6/2/2000 added code to delete *.xp$, *.im$ when building
//    dph 13/12/2000 added code to generate a componentinterface.for

// ------------------------------------------------------------------
void CompileThread::compileProject (ApsimProject& apf)
   {
   // get a list of binary filenames.
   vector<string> BinaryFileNames;
   getBinaryFileNames(apf, BinaryFileNames);

   for (vector<string>::iterator BinaryFileNameIter = BinaryFileNames.begin();
                                 BinaryFileNameIter != BinaryFileNames.end();
                                 BinaryFileNameIter++)
      {
      Path BinaryFile( (*BinaryFileNameIter).c_str());

      // display message on screen.
      Message = "<B>Compiling:</B> " + BinaryFile.Get_path();
      Synchronize(goDisplayMessage1);

      // write banner to output file.
      writeToOutputFile ("------Compiling " + BinaryFile.Get_path() + "------");

      // change the current directory to the source directory.
      SetCurrentDirectory(getSourceDirectory(apf).c_str());

      // if this is a build then remove all .obj files.
      if (Build)
         deleteDependentFiles(apf);

      // make sure the binary path exists.
      CreateDirectory (BinaryFile.Get_directory().c_str(), NULL);

      if (!Terminated)
         runExternalProgram(apf);

      // create an AUTOMAKE file for this binary.
      if (!Terminated)
         createAutoMakeFile (apf, BinaryFile);

      // create a compiler response file for this binary.
      if (!Terminated)
         createCompilerResponseFile (apf);

      // create a linker response file for this binary.
      if (!Terminated)
         createLinkerResponseFile (apf);

      // copy module files into source directory.
         copyModuleFiles(apf);

      // run AUTOMAKE
      if (!Terminated)
         runAutoMake (apf, BinaryFile);

      if (!Debug)
         {
         // cleanup after ourselves.
         cleanup (apf);

         // send contents of compiler.rpt to standard out.
         if (Stdout)
            {
            ifstream compilerRpt(Compiler_output_filename.c_str());
            string contents;
            getline(compilerRpt, contents, '\0');
            std::cout << contents;
            }
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
void CompileThread::getBinaryFileNames (ApsimProject& apf, vector<string>& BinaryFileNames)
   {
   apf.get(BINARY_KEY, BinaryFileNames, APSBUILD_SECTION, true);
   }

// ------------------------------------------------------------------
//  Short description:
//     write a string to the output file

//  Notes:

//  Changes:
//    DPH 18/3/99

// ------------------------------------------------------------------
void CompileThread::writeToOutputFile (const string& msg)
   {
   ofstream out (Compiler_output_filename.c_str(), std::ios::app);
   out << msg << std::endl;
   }

// ------------------------------------------------------------------
// create an automake file in the source directory.
// ------------------------------------------------------------------
void CompileThread::createAutoMakeFile (ApsimProject& apf, Path& BinaryFile)
   {
   // open a stream to the file we're going to write to.
   string AutomakeFilename = getSourceDirectory(apf) + "\\" + AUTOMAKE_FILENAME;
   ofstream out (AutomakeFilename.c_str());

   // write out the LINK, TARGET, INCLUDE and MODULE lines.
   out << "LINK=" << getCompilerSetting(apf, "link", ' ') << endl;
   out << "TARGET=" << BinaryFile.Get_path() << endl;
   out << "INCLUDE=" << getCompilerSetting(apf, "include") << endl;
   out << "MODULE=." << endl;

   // get a list of all source files.
   vector<string> SourceFiles;
   getSourceFileNames(apf, SourceFiles);

   // for each source file write a line to automake file.
   for (vector<string>::iterator i = SourceFiles.begin();
                               i != SourceFiles.end();
                               i++)
      {
      if (i != SourceFiles.begin())
         out << "AND" << std::endl;
      out << "COMPILE=" << getCompileLineForSourceFile(apf, *i) << endl;
      out << "FILES=" << *i << endl;
      }
   }

// ------------------------------------------------------------------
// create a compiler response file for this binary in the source directory
// ------------------------------------------------------------------
void CompileThread::createCompilerResponseFile  (ApsimProject& apf)
   {
   // open the compiler file we're going to write to.
   string filename = getSourceDirectory(apf) + "\\" + COMPILER_RESPONSE_FILENAME;
   ofstream out (filename.c_str());

   // write include and module directories
   out << "-i " << getCompilerSetting(apf, "include");
   out << " -mod ." << endl;
   }

// ------------------------------------------------------------------
// create a linker response file for this binary in the source directory.
// ------------------------------------------------------------------
void CompileThread::createLinkerResponseFile (ApsimProject& apf)
   {
   // open the compiler file we're going to write to.
   string filename = getSourceDirectory(apf) + "\\" + LINKER_RESPONSE_FILENAME;
   ofstream out (filename.c_str());

   // write out all import, object and library files
   out << getCompilerSetting(apf, "import", '\n') << endl;
   out << getCompilerSetting(apf, "object", '\n') << endl;
   out << "-lib " << getCompilerSetting(apf, "library", ',') << endl;
   out << "-libPath " << getCompilerSetting(apf, "libraryPath", ',') << endl;
   }

// ------------------------------------------------------------------
// go run AUTOMAKE.  This will invoke the compiler and linker and
// produce the target executable.
// ------------------------------------------------------------------
void CompileThread::runAutoMake (ApsimProject& apf, Path& BinaryFile)
   {
   // display message on screen.
   Message = "<B>Creating: </B>DataTypesModule.f90...";
   Synchronize(goDisplayMessage2);

   // display message on screen.
   Message = "<B>Creating: </B>DataTypesModule.f90.  Done!";
   Synchronize(goDisplayMessage2);

   if (!Terminated)
      {
      // delete the old automake.dep file.  For some reason, when this file
      // already exists, automake gets the dependency order of .f90 files
      // wrong.  Found with cropmod.
      DeleteFile("automake.dep");

      // run automake to produce the amtemp.bat file
      CommandLineToExecute = string("automake fig=") + AUTOMAKE_FILENAME;
      Synchronize(runCommandLine);
      if (!Debug)
         {
         // run batch file that automake has created.
         string AmtempPath = getSourceDirectory(apf) + "\\amtemp.bat";
         CommandLineToExecute = AmtempPath + " >> " + Compiler_output_filename;
         Synchronize(runCommandLine);

         // delete .MAP file
         Path GeneratedMapFile(BinaryFile);
         GeneratedMapFile.Set_extension(".map");
         DeleteFile(GeneratedMapFile.Get_path().c_str());
         }
      }
   }

// ------------------------------------------------------------------
// cleanup all unwanted files.
// ------------------------------------------------------------------
void CompileThread::cleanup (ApsimProject& apf)
   {
   DeleteFile ("amtemp.bat");
   deleteFiles (apf, "automake.*");
   deleteFiles (apf, "*.rsp");

   for (unsigned file = 0; file != filesToCleanup.size(); file++)
      DeleteFile(filesToCleanup[file].c_str());
   }

// ------------------------------------------------------------------
// Return a list of source files to compile.
// ------------------------------------------------------------------
void CompileThread::getSourceFileNames (ApsimProject& apf, vector<string>& sourceFiles)
   {
   // get all files listed in the contents part of this project.
   vector<string> files;
   apf.getKeys(files);

   // for each file, work out which are source files and save the the returned
   // sourceFiles list.
   for (vector<string>::iterator fileI = files.begin();
                               fileI != files.end();
                               fileI++)
      {
      string filename = *fileI;
      // remove any compile= switch from the filename.
      unsigned posSwitch = filename.find(" compile=");
      if (posSwitch != string::npos)
         filename = filename.substr(0, posSwitch);
      Path File(filename.c_str());
//      if (File.Get_extension() == ".for" ||
//          File.Get_extension() == ".f90")
         sourceFiles.push_back(filename);
      }
   }

// ------------------------------------------------------------------
// Return the the source code directory.
// ------------------------------------------------------------------
string CompileThread::getSourceDirectory (ApsimProject& apf)
   {
   Path Source (apf.getFileName().c_str());
   return Source.Get_directory();
   }

// ------------------------------------------------------------------
// Return a list of the specified compiler settings for the
// specified project file.
// ------------------------------------------------------------------
void CompileThread::getCompilerSettings(ApsimProject& apf,
                                       const string& settingName,
                                       vector<string>& settings,
                                       const string& compilerFile)
   {
   // If the caller supplied a compilerFile then use that to do the lookup.
   // Otherwise go find and use the default one for the specified project.
   string compilerFileToUse = CompilerFile;

   if (compilerFileToUse == "")
      compilerFileToUse = compilerFile;
   if (compilerFileToUse == "")
      {
      apf.get("compilerFile", compilerFileToUse, APSBUILD_SECTION, false);
      if (compilerFileToUse == "")
         compilerFileToUse = "release.compile";
      }

   // Try and open the compiler file in the apsbuild directory.  If
   // the file is opened ok, then read in the requested settings
   // If the file isn't opened ok then show an error message.
   string compilerPath = getAppHomeDirectory() + "\\" + compilerFileToUse;
   if (FileExists(compilerPath.c_str()))
      {
      // open compiler file and return requested information.
      IniFile Ini(compilerPath);
      Ini.read("Instructions", settingName, settings);
      }
   else
      {
      string msg = "Cannot find compiler file in the APSBuild directory.  File: " + compilerFileToUse;
      ShowMessage(msg.c_str());
      }
   // replace any %apsuite macros.
   for (unsigned i = 0; i < settings.size(); i++)
      Replace_all(settings[i], "%apsuite", getApsimDirectory().c_str());
   }
// ------------------------------------------------------------------
// Return a single specified compiler setting for the
// specified project file.  This routine will concatenate all
// multiple settings into a single string.
// ------------------------------------------------------------------
string CompileThread::getCompilerSetting(ApsimProject& apf,
                                         const string& settingName,
                                         const char delimiter,
                                         const string& compilerFile)
   {
   string setting;
   vector<string> settings;
   getCompilerSettings(apf, settingName, settings, compilerFile);
   for (vector<string>::iterator settingI = settings.begin();
                               settingI != settings.end();
                               settingI++)
      {
      if (setting != "")
         setting += delimiter;
      setting += *settingI;
      }
   return setting;
   }
// ------------------------------------------------------------------
// Delete files matching file specification
// ------------------------------------------------------------------
void CompileThread::deleteFiles (ApsimProject& apf, const char* filespec)
   {
   vector<string> Files;
   getDirectoryListing(getSourceDirectory(apf), filespec, Files, FA_NORMAL, true);
   for (vector<string>::iterator i = Files.begin();
                               i != Files.end();
                               i++)
      {
      DeleteFile( (*i).c_str() );
      }
   }
// ------------------------------------------------------------------
// Delete all dependant files for this module.  This will force a
// rebuild.
// ------------------------------------------------------------------
void CompileThread::deleteDependentFiles(ApsimProject& apf)
   {
   deleteFiles(apf, "*.obj");
   DeleteFile("datatypesmodule.f90");

   vector<string> sourceFilenames;
   getSourceFileNames(apf, sourceFilenames);
   for (vector<string>::iterator file = sourceFilenames.begin();
                               file != sourceFilenames.end();
                               file++)
      {
      Path sourcePath( (*file).c_str() );
      sourcePath.Set_extension(".obj");
      DeleteFile(sourcePath.Get_path().c_str());
      sourcePath.Set_extension(".xp$");
      DeleteFile(sourcePath.Get_path().c_str());
      sourcePath.Set_extension(".im$");
      DeleteFile(sourcePath.Get_path().c_str());
      sourcePath.Set_extension(".mod");
      DeleteFile(sourcePath.Get_path().c_str());
      sourcePath.Set_extension(".lib");
      DeleteFile(sourcePath.Get_path().c_str());
      }
   }
// ------------------------------------------------------------------
//  Short description:
//     create a ComponentInterface.for file for this module.

//  Notes:

//  Changes:
//    DPH 18/3/99

// ------------------------------------------------------------------
void CompileThread::CreateAutomakeRSP(ApsimProject& apf)
   {
   ofstream automakeRSP("automake.rsp");

   vector<string> sourceFilenames;
   getSourceFileNames(apf, sourceFilenames);
   for (vector<string>::iterator file = sourceFilenames.begin();
                               file != sourceFilenames.end();
                               file++)
      {
      Path sourcePath( (*file).c_str() );
      automakeRSP << sourcePath.Get_name_without_ext() << ".obj" << std::endl;
      }
   }

// ------------------------------------------------------------------
// Using the specified source file, go locate and return the compile=
// line for this source file.
// ------------------------------------------------------------------
string CompileThread::getCompileLineForSourceFile(ApsimProject& apf, const string& sourceFile)
   {
   // get all files listed in the contents part of this project.
   string file;
   apf.get(sourceFile.c_str(), file);

   // look at the returned file for a compile= switch on it.
   // If it does then use that compiler file
   // rather that the default one for looking up the compile line.
   string compilerFile;
   unsigned posSwitch = file.find(" compilerfile=");
   if (posSwitch != string::npos)
      compilerFile = file.substr(posSwitch+14);

   return getCompilerSetting(apf, "compile", ' ', compilerFile);
   }
// ------------------------------------------------------------------
// Run an external program if necessary.
// ------------------------------------------------------------------
void CompileThread::runExternalProgram(ApsimProject& apf)
   {
   CommandLineToExecute = getCompilerSetting(apf, "Program");
   if (CommandLineToExecute != "")
      {
      Replace_all(CommandLineToExecute, "%apsuite", getApsimDirectory().c_str());
      CommandLineToExecute += " " + apf.getFileName() + " " + Compiler_output_filename;
      Synchronize(runCommandLine);
      }
   }
// ------------------------------------------------------------------
// copy module files into source directory.
// ------------------------------------------------------------------
void CompileThread::copyModuleFiles(ApsimProject& apf)
   {
   string thisDirectory = Path::getCurrentFolder().Get_directory();

   string directoriesString = getCompilerSetting(apf, "module");
   vector<string> directories;
   Split_string(directoriesString, ";", directories);
   for (unsigned dir = 0; dir != directories.size(); dir++)
      {
      vector<string> files;
      getDirectoryListing(directories[dir], "*.mod", files, FA_NORMAL, true);
      for (unsigned file = 0; file != files.size(); file++)
         {
         if (!Str_i_Eq(Path(files[file]).Get_directory(), thisDirectory))
            {
            string filename = Path(files[file]).Get_name();
            CopyFile(files[file].c_str(), filename.c_str(), false);
            filesToCleanup.push_back(filename);
            }
         }
      }
   }




