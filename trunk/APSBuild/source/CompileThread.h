//---------------------------------------------------------------------------
#ifndef CompileThreadH
#define CompileThreadH
#include <general\path.h>
#include <ApsimShared\ApsimProject.h>
using std::ostream;
// ------------------------------------------------------------------
//  Short description:
//     this class is responsible for compiling an APSIM project

//  Notes:

//  Changes:
//    DPH 18/3/99

// ------------------------------------------------------------------
class CompileThread : public TThread
   {
   public:
      CompileThread (const char* Filename);
      __fastcall ~CompileThread (void);

      bool Build;
      bool Debug;
      bool Stdout;
      bool Quiet;
      string Compiler_output_filename;
      string CompilerFile;

      virtual void __fastcall Execute (void);

      typedef void __fastcall (__closure *TMessageEvent)(System::TObject* Sender, const char* Message);
      TMessageEvent DisplayMessage1;
      TMessageEvent DisplayMessage2;

   private:
      string InitialFilename;
      string Message;
      string CommandLineToExecute;
      std::vector<std::string> filesToCleanup;

      void   __fastcall goDisplayMessage1 (void);
      void   __fastcall goDisplayMessage2 (void);
      void   __fastcall runCommandLine (void);
      void   compileProject (ApsimProject& apf);

      void   getBinaryFileNames (ApsimProject& apf, vector<string>& BinaryFileNames);
      void   writeToOutputFile (const string& msg);
      void   createAutoMakeFile (ApsimProject& apf, Path& BinaryFile);
      void   createCompilerResponseFile (ApsimProject& apf);
      void   createLinkerResponseFile (ApsimProject& apf);
      void   runAutoMake (ApsimProject& apf, Path& BinaryFile);
      void   cleanup (ApsimProject& apf);
      void   getSourceFileNames (ApsimProject& apf, vector<string>& SourceFiles);
      string getSourceDirectory (ApsimProject& apf);
      void getCompilerSettings(ApsimProject& apf,
                               const std::string& settingName,
                               std::vector<std::string>& settings,
                               const std::string& compilerFile = "");
      std::string getCompilerSetting(ApsimProject& apf,
                                     const string& settingName,
                                     const char delimiter = ';',
                                     const std::string& compilerFile = "");
      void   deleteFiles (ApsimProject& apf, const char* Filespec);
      void   deleteDependentFiles(ApsimProject& apf);
      void   CreateAutomakeRSP(ApsimProject& apf);
      std::string getCompileLineForSourceFile(ApsimProject& apf,
                                              const std::string& sourceFile);
      void runExternalProgram(ApsimProject& apf);
      void copyModuleFiles(ApsimProject& apf);
   };

bool ProjectIsCompilable (const char* Filename);
#endif
