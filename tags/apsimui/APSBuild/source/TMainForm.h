//---------------------------------------------------------------------------
#ifndef TMainFormH
#define TMainFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "CompileThread.h"
#include <general\mystring.h>
#include <general\mylist.h>
//---------------------------------------------------------------------------
class TMainForm : public TForm
{
__published:	// IDE-managed Components
   TLabel *MessageLabel;
   void __fastcall FormShow(TObject *Sender);
private:	// User declarations
      CompileThread* Thread;
      Path CompilerReportFile;

      void Go ();
      void GetCompilableProjects (list<string>& Project_files);
      void __fastcall DisplayMessage (TObject* Object, const char* Message);
      void __fastcall ThreadTerminated (TObject* Object);

public:		// User declarations
   __fastcall TMainForm(TComponent* Owner);
   list<string> ProjectFiles;
   bool Build;
   bool Debug;
};
//---------------------------------------------------------------------------
extern PACKAGE TMainForm *MainForm;
//---------------------------------------------------------------------------
#endif