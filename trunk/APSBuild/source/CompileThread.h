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
      TMessageEvent DisplayMessage;

   private:
      string InitialFilename;
      string Compiler_output_filename;
      string Message;
      string CommandLineToExecute;

      void   __fastcall GoDisplayMessage (void);
      void   __fastcall RunCommandLine (void);
      void   CompileProject (APSIM_project& apf);

      void   GetBinaryFileNames (APSIM_project& apf, list<string>& BinaryFileNames);
      void   WriteToOutputFile (string& msg);
      void   CreateAutoMakeFile (APSIM_project& apf, Path& BinaryFile);
      void   CreateCompilerResponseFile (APSIM_project& apf);
      void   CreateLinkerResponseFile (APSIM_project& apf);
      void   CopySourceFiles (APSIM_project& apf);
      void   DeleteSourceFiles (APSIM_project& apf);
      void   RunAutoMake (APSIM_project& apf, Path& BinaryFile);
      void   Cleanup (APSIM_project& apf);
      void   CopySwitchesToStream (APSIM_project& apf, ostream& out);
      void   GetSourceFileNames (APSIM_project& apf, list<string>& SourceFiles);
      string GetSourceDirectory (APSIM_project& apf);
      void   GetFilesForCompiler (APSIM_project& apf, const char* Key, list<string>& Files);
      void   DeleteFiles (APSIM_project& apf, const char* Filespec);
      void   CreateComponentInterface(APSIM_project& apf);
      void   DeleteDependentFiles(APSIM_project& apf);
      void   CreateAutomakeRSP(APSIM_project& apf);

   };

bool ProjectIsCompilable (const char* Filename);
#endif
