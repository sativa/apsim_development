//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "TMainForm.h"
#include <fstream>
#include <general\stream_functions.h>
#include <general\ini_file.h>
#include <aps\apsuite.h>

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TMainForm *MainForm;
//---------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* Owner)
   : TForm(Owner)
   {
   // load the onTop property from the .ini file.
   string filename = APSDirectories().Get_working() + "\\config\\apsuite.ini";
   string onTop;
   Ini_file ini;
   ini.Set_file_name(filename.c_str());
   ini.Read("APSBuild", "on_top", onTop);
   OnTopRadio->Checked = !Str_i_Eq(onTop, "no");
   OnTopRadioClick(NULL);
   }
//---------------------------------------------------------------------------
__fastcall TMainForm::~TMainForm()
   {
   if (!Quiet)
      {
      if (CompilerReportFile.Exists())
         {
         string command (APSDirectories().Get_home());
         command += "\\viewcmplmsg\\viewcmplmsg ";
         command += APSDirectories().Get_working() + "\\compiler.rpt";
         WinExec (command.c_str(), SW_SHOW);
         }
      else
         MessageBox(NULL, "Compiler not invoked.  Probable cause: nothing to compile.", "Information", MB_ICONINFORMATION | MB_OK);
      }
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
   // create a compiler report filename
   CompilerReportFile.Set_directory(APSDirectories().Get_working().c_str());
   CompilerReportFile.Set_name ("compiler.rpt");

   // delete old compiler report file.
   DeleteFile (CompilerReportFile.Get_path().c_str());

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
         Thread->DisplayMessage1 = DisplayMessage1;
         Thread->DisplayMessage2 = DisplayMessage2;
         Thread->OnTerminate = ThreadTerminated;
         Thread->CompileType = CompileType;
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
   string filename = APSDirectories().Get_working() + "\\config\\apsuite.ini";
   string left, top, onTop;
   left = IntToStr(Left).c_str();
   top = IntToStr(Top).c_str();
   if (OnTopRadio->Checked)
      onTop = "yes";
   else
      onTop = "no";

   Ini_file ini;
   ini.Set_file_name(filename.c_str());
   ini.Write("APSBuild", "left", left.c_str());
   ini.Write("APSBuild", "top", top.c_str());
   ini.Write("APSBuild", "on_top", onTop.c_str());
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
      string filename = APSDirectories().Get_working() + "\\config\\apsuite.ini";
      string left, top;
      Ini_file ini;
      ini.Set_file_name(filename.c_str());
      ini.Read("APSBuild", "left", left);
      ini.Read("APSBuild", "top", top);
      Left = StrToInt(left.c_str());
      Top = StrToInt(top.c_str());

      Go();
      }
   }
//---------------------------------------------------------------------------

