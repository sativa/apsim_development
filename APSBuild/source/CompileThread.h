//---------------------------------------------------------------------------
#ifndef CompileThreadH
#define CompileThreadH
#include <general\path.h>
#include <aps\apsim_project.h>
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
      string CompileType;
      virtual void __fastcall Execute (void);

      string Get_compiler_output_filename (const char* Filename)
         {return Compiler_output_filename;}

      typedef void __fastcall (__closure *TMessageEvent)(System::TObject* Sender, const char* Message);
      TMessageEvent DisplayMessage1;
      TMessageEvent DisplayMessage2;

   private:
      string InitialFilename;
      string Compiler_output_filename;
      string Message;
      string CommandLineToExecute;

      void   __fastcall goDisplayMessage1 (void);
      void   __fastcall goDisplayMessage2 (void);
      void   __fastcall runCommandLine (void);
      void   compileProject (APSIM_project& apf);

      void   getBinaryFileNames (APSIM_project& apf, list<string>& BinaryFileNames);
      void   writeToOutputFile (const string& msg);
      void   createAutoMakeFile (APSIM_project& apf, Path& BinaryFile);
      void   createCompilerResponseFile (APSIM_project& apf);
      void   createLinkerResponseFile (APSIM_project& apf);
      void   runAutoMake (APSIM_project& apf, Path& BinaryFile);
      void   cleanup (APSIM_project& apf);
      void   getSourceFileNames (APSIM_project& apf, list<string>& SourceFiles);
      string getSourceDirectory (APSIM_project& apf);
      void getCompilerSettings(APSIM_project& apf,
                               const std::string& settingName,
                               std::list<std::string>& settings,
                               const std::string& compilerFile = "");
      std::string getCompilerSetting(APSIM_project& apf,
                                     const string& settingName,
                                     const char delimiter = ';',
                                     const std::string& compilerFile = "");
      void   deleteFiles (APSIM_project& apf, const char* Filespec);
      void   createDataTypesModule(APSIM_project& apf);
      void   deleteDependentFiles(APSIM_project& apf);
      void   CreateAutomakeRSP(APSIM_project& apf);
      std::string getCompileLineForSourceFile(APSIM_project& apf,
                                              const std::string& sourceFile);
   };

bool ProjectIsCompilable (const char* Filename);
#endif
