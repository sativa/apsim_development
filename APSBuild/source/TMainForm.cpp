//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TMainForm.h"
#include <fstream>
#include <general\stream_functions.h>
#include <general\IniFile.h>
#include <ApsimShared\ApsimSettings.h>
#include <ApsimShared\ApsimDirectories.h>

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "HTMListB"
#pragma resource "*.dfm"
TMainForm *MainForm;
//---------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* Owner)
   : TForm(Owner)
   {
   // load the onTop property from the .ini file.
   ApsimSettings settings;
   string onTop;
   settings.read("ApsBuild Pos|on_top", onTop);
   OnTopRadio->Checked = !Str_i_Eq(onTop, "no");
   OnTopRadioClick(NULL);
   Thread = NULL;
   }
//---------------------------------------------------------------------------
__fastcall TMainForm::~TMainForm()
   {
   delete Thread;
   Thread = NULL;
   }

//---------------------------------------------------------------------------
//  Short description:
//    Compile everything.

//  Notes:

//  Changes:
//    DPH 23/4/98
//    dph 8/2/99 D229 Modified to use argv instead of command_line.

// ------------------------------------------------------------------
void TMainForm::Go ()
   {
   // go start the ball rolling and compile first project.
   Thread = NULL;
   ThreadTerminated(NULL);
   }

// ------------------------------------------------------------------
//  Short description:
//    display a message on screen.

//  Notes:

//  Changes:
//    DPH 14/4/99

// ------------------------------------------------------------------
void __fastcall TMainForm::DisplayMessage1 (TObject* Object, const char* Message)
   {
   ListBox->Items->Strings[0] = Message;
   }

// ------------------------------------------------------------------
//  Short description:
//    display a message on screen.

//  Notes:

//  Changes:
//    DPH 14/4/99

// ------------------------------------------------------------------
void __fastcall TMainForm::DisplayMessage2 (TObject* Object, const char* Message)
   {
   ListBox->Items->Strings[1] = Message;
   }

// ------------------------------------------------------------------
//  Short description:
//    Thread has terminated.  Start new one if necessary.

//  Notes:

//  Changes:
//    dph 14/4/99

// ------------------------------------------------------------------
void __fastcall TMainForm::ThreadTerminated (TObject* Object)
   {
   if (ProjectFiles.size() > 0)
      {
      string ProjectFilename = *ProjectFiles.begin();
      ProjectFiles.erase(ProjectFiles.begin());

      if (!FileExists(ProjectFilename.c_str()))
         {
         string msg = "Cannot compile: " + ProjectFilename + ".  The file doesn't exist";
         Application->MessageBox( (char*) msg.c_str(), "Error", MB_ICONSTOP | MB_OK);
         ThreadTerminated(Object);
         }
      else
         {
         delete Thread;
         Thread = new CompileThread ( ProjectFilename.c_str() );
         Thread->Build = Build;
         Thread->Debug = Debug;
         Thread->Quiet = Quiet;
         Thread->DisplayMessage1 = DisplayMessage1;
         Thread->DisplayMessage2 = DisplayMessage2;
         Thread->OnTerminate = ThreadTerminated;
         Thread->Stdout = Stdout;
         Thread->CompilerFile = CompilerFile;
         if (OutFile != "")
            Thread->Compiler_output_filename = OutFile;
         Thread->Resume();
         }
      }
   else
      displayCompilerOutput();
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::CloseButtonClick(TObject *Sender)
   {
   Thread->Terminate();
   Close();
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::FormClose(TObject *Sender, TCloseAction &Action)
   {
   // save the Left, Top and OnTop properties to the .ini file.
   string left, top, onTop;
   left = IntToStr(Left).c_str();
   top = IntToStr(Top).c_str();
   if (OnTopRadio->Checked)
      onTop = "yes";
   else
      onTop = "no";

   ApsimSettings settings;
   settings.write("ApsBuild Pos|left", left);
   settings.write("ApsBuild Pos|top", top);
   settings.write("ApsBuild Pos|on_top", onTop);
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::OnTopRadioClick(TObject *Sender)
   {
   if (OnTopRadio->Checked)
      FormStyle = fsStayOnTop;
   else
      FormStyle = fsNormal;
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::FormShow(TObject *Sender)
   {
   static bool firstTime = true;
   if (firstTime)
      {
      firstTime = false;

      // load the Left, Top properties from the .ini file.
      ApsimSettings settings;
      string left, top;
      settings.read("ApsBuild Pos|left", left);
      settings.read("ApsBuild Pos|top", top);
      if (left != "" && top != "")
         {
         Left = StrToInt(left.c_str());
         Top = StrToInt(top.c_str());
         }

      Go();
      }
   }
//---------------------------------------------------------------------------
// Display the compiler output.
//---------------------------------------------------------------------------
void TMainForm::displayCompilerOutput(void)
   {
   if (Thread != NULL && !Quiet)
      {
      ApsimSettings settings;
      string st;
      settings.read("ApsBuild Pos|height", st);
      if (st == "")
         Height = 400;
      else
         Height = StrToInt(st.c_str());
      Memo->Lines->LoadFromFile(Thread->Compiler_output_filename.c_str());
      CloseButton->Caption = "Close";
      while (ModalResult == mrNone)
         Application->ProcessMessages();
      settings.write("ApsBuild Pos|height", IntToStr(Height).c_str());
      }
   Close();
   }
