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
#pragma resource "*.dfm"
TMainForm *MainForm;
//---------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* Owner)
   : TForm(Owner)
   {
   // load the onTop property from the .ini file.
   ApsimSettings settings;
   string onTop;
   settings.read("on_top", onTop);
   OnTopRadio->Checked = !Str_i_Eq(onTop, "no");
   OnTopRadioClick(NULL);
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
   MessageLabel1->Caption = Message;
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
   MessageLabel2->Caption = Message;
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

      if (GetFileAttributes(ProjectFilename.c_str()) & FILE_ATTRIBUTE_READONLY)
         {
         string msg = "Cannot compile: " + ProjectFilename + ".  The file is readonly.  "
                      "If the file is in the APSuite directory then copy the module to "
                      "the APSWork directory, remove the readonly attribute and try "
                      "compiling again.";
         Application->MessageBox( (char*) msg.c_str(), "Error", MB_ICONSTOP | MB_OK);
         ThreadTerminated(Object);
         }
      else
         {
         Thread = new CompileThread ( ProjectFilename.c_str() );
         Thread->Build = Build;
         Thread->Debug = Debug;
         Thread->Quiet = Quiet;
         Thread->DisplayMessage1 = DisplayMessage1;
         Thread->DisplayMessage2 = DisplayMessage2;
         Thread->OnTerminate = ThreadTerminated;
         Thread->Stdout = Stdout;
         Thread->Resume();
         }
      }
   else
      Close();
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::Button1Click(TObject *Sender)
   {
   Thread->Terminate();
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
   settings.write("left", left);
   settings.write("top", top);
   settings.write("on_top", onTop);
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
      settings.read("left", left);
      settings.read("top", top);
      if (left != "" && top != "")
         {
         Left = StrToInt(left.c_str());
         Top = StrToInt(top.c_str());
         }

      Go();
      }
   }
//---------------------------------------------------------------------------

