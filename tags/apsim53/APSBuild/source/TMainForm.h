//---------------------------------------------------------------------------
#ifndef TMainFormH
#define TMainFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "CompileThread.h"
#include <ExtCtrls.hpp>
#include "HTMListB.hpp"
#include <string>
#include <list>
using std::list;
using std::string;
//---------------------------------------------------------------------------
class TMainForm : public TForm
{
__published:	// IDE-managed Components
   TPanel *Panel3;
   TCheckBox *OnTopRadio;
   TButton *CloseButton;
   THTMListBox *ListBox;
   TMemo *Memo;
   void __fastcall CloseButtonClick(TObject *Sender);
   void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
   void __fastcall OnTopRadioClick(TObject *Sender);
   void __fastcall FormShow(TObject *Sender);
private:	// User declarations
      CompileThread* Thread;
      Path CompilerReportFile;

      void Go ();
      void GetCompilableProjects (list<string>& Project_files);
      void __fastcall DisplayMessage1 (TObject* Object, const char* Message);
      void __fastcall DisplayMessage2 (TObject* Object, const char* Message);
      void __fastcall ThreadTerminated (TObject* Object);
      void displayCompilerOutput(void);

public:		// User declarations
   __fastcall TMainForm(TComponent* Owner);
   __fastcall ~TMainForm();
   list<string> ProjectFiles;
   bool Build;
   bool Debug;
   bool Quiet;
   bool Stdout;
   string CompilerFile;
   string OutFile;
};
//---------------------------------------------------------------------------
extern PACKAGE TMainForm *MainForm;
//---------------------------------------------------------------------------
#endif
