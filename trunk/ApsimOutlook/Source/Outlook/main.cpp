//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Main.h"
#include "About.h"
#include "TDrill_down_form.h"
#include <general\vcl_functions.h>
#include <general\ini_file.h>
#include <general\path.h>
#include <dos.h>
#include <shellapi.h>
#include "TSkin.h"
#include "TSimulation_database.h"
#include <aps\apsuite.h>
#include <general\stream_functions.h>
#include <sstream>
#include <ole2.h>
using namespace std;
//---------------------------------------------------------------------
#pragma link "StrHlder"
#pragma resource "*.dfm"
TMainForm *MainForm;
extern AnsiString CommandLine;
//---------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent *Owner)
	: TForm(Owner)
   {
   CoInitialize(NULL);
   }
//---------------------------------------------------------------------
__fastcall TMainForm::~TMainForm()
   {
   CoUninitialize();
   }
//---------------------------------------------------------------------
void __fastcall TMainForm::FormCreate(TObject *Sender)
   {
   Application->OnHint = ShowHint;
   Screen->OnActiveFormChange = UpdateMenuItems;
   Application->OnMinimize = Application_minimize;
   }
//---------------------------------------------------------------------
void __fastcall TMainForm::ShowHint(TObject *Sender)
   {
	StatusBar->SimpleText = Application->Hint;
   }
//---------------------------------------------------------------------
void __fastcall TMainForm::CreateMDIChild(String Name)
   {
	TMDIChild *Child;

	//--- create a new MDI child window ----
   Child = new TMDIChild(Application);
   Child->Set_toolbar (ToolBar2);
   Child->Caption = Name;
   Child->SetPresentationFonts(FilePresentationFontsMenu->Checked);
   Child->Show();
   }
//---------------------------------------------------------------------
void __fastcall TMainForm::File_exit(TObject *Sender)
   {
	Close();
   }
//---------------------------------------------------------------------
void __fastcall TMainForm::Window_cascade(TObject *Sender)
   {
	Cascade();
   }
//---------------------------------------------------------------------
void __fastcall TMainForm::Window_tile(TObject *Sender)
   {
	Tile();
   }
//---------------------------------------------------------------------
void __fastcall TMainForm::Window_arrange_icons(TObject *Sender)
   {
	ArrangeIcons();
   }
//---------------------------------------------------------------------
void __fastcall TMainForm::Window_minimize_all(TObject *Sender)
   {
	int i;

	//---- Must be done backwards through the MDIChildren array ----
	for (i=MDIChildCount-1; i >= 0; i--)
		MDIChildren[i]->WindowState = wsMinimized;
   }
//---------------------------------------------------------------------
void __fastcall TMainForm::UpdateMenuItems(TObject *Sender)
   {
   FilePrintMenu->Enabled = MDIChildCount > 0;
   FileCloseMenu->Enabled = MDIChildCount > 0;
	Window_cascade_menu->Enabled = MDIChildCount > 0;
	Window_tile_menu->Enabled = MDIChildCount > 0;
	Window_arrange_icons_menu->Enabled = MDIChildCount > 0;
	Window_minimize_all_menu->Enabled = MDIChildCount > 0;

   ToolBar2->Visible = MDIChildCount > 0;
   Print_button->Enabled = MDIChildCount > 0;
   Copy_button->Enabled = MDIChildCount > 0;
   Copy_without_button->Enabled = MDIChildCount > 0;
   Excel_button->Enabled = MDIChildCount > 0;
   }
//---------------------------------------------------------------------
void __fastcall TMainForm::FormDestroy(TObject *Sender)
   {
	Screen->OnActiveFormChange = NULL;
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::Help_about(TObject *Sender)
   {
   AboutBox->ShowModal();
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::FormClose(TObject *Sender, TCloseAction &Action)
   {
   Close_all ();
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::Close_all ()
   {
	//---- Must be done backwards through the MDIChildren array ----
	for (int i = MDIChildCount-1; i >= 0; i--)
		delete MDIChildren[i];
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::FileCloseMenuClick(TObject *Sender)
   {
   delete ActiveMDIChild;
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::FilePrintMenuClick(TObject *Sender)
   {
   if (PrinterSetupDialog->Execute())
      {
      ActiveMDIChild->Print();
      }
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::Copy_buttonClick(TObject *Sender)
   {
   TMDIChild* Child = dynamic_cast<TMDIChild*> (ActiveMDIChild);
   Child->EditCopy(NULL);
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::Copy_without_buttonClick(TObject *Sender)
   {
   TMDIChild* Child = dynamic_cast<TMDIChild*> (ActiveMDIChild);
   Child->EditCopyWithout(NULL);
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::EXCEL_buttonClick(TObject *Sender)
   {
   TMDIChild* Child = dynamic_cast<TMDIChild*> (ActiveMDIChild);
   Child->SendDataToEXCEL(NULL);
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::Application_minimize (TObject* Sender)
   {
   Application->Minimize();
//   ShowWindow(Handle, SW_MINIMIZE);
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::FileNewMenuClick(TObject *Sender)
   {
   CreateMDIChild("Chart" + IntToStr(MDIChildCount + 1));
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::HelpContentsMenuClick(TObject *Sender)
   {
   ShellExecute (this->Handle, "open",
                 StrHolder1->Strings->Strings[1].c_str(), NULL, "", SW_SHOW);
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::FilePresentationFontsMenuClick(TObject *Sender)
   {
   FilePresentationFontsMenu->Checked = !FilePresentationFontsMenu->Checked;

   // change font on drill down form.
   Drill_down_form->SetPresentationFonts(FilePresentationFontsMenu->Checked);

   // tell all children about font change.
	for (int i = 0; i < MDIChildCount; i++)
      {
      TMDIChild* Child = dynamic_cast<TMDIChild*> (MDIChildren[i]);
      Child->SetPresentationFonts(FilePresentationFontsMenu->Checked);
      }
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::FormShow(TObject *Sender)
   {
   Skin->InitApplication();

   // change caption.
   Path p(Application->ExeName.c_str());
   if (Str_i_Eq(p.Get_name_without_ext(), "whoppercropper"))
      Caption = "Whopper Cropper";
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::Evaluate(TObject *Sender)
   {
   ShellExecute (this->Handle, "open",
                 StrHolder1->Strings->Strings[0].c_str(), NULL, "", SW_SHOW);
   }
//---------------------------------------------------------------------
void AddToOpenDialog (TOpenDialog* OpenDialog, TStringList* files)
   {
   AnsiString fileListSt = OpenDialog->FileName;

   // loop through all files
   for (int i = 0; i < files->Count; i++)
      {
      Path p;
      p.Set_path (files->Strings[i].c_str());

      fileListSt += "\"";
      fileListSt += p.Get_name().c_str();
      fileListSt += "\" ";
      OpenDialog->InitialDir = p.Get_directory().c_str();
      }

   OpenDialog->FileName = fileListSt;
   OpenDialog->Files->AddStrings(files);
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::ApsimOutlookExecuteMacro(TObject *Sender,
      TStrings *Msg)
   {
   // display an hourglass if we haven't already done so.
   if (!Timer1->Enabled)
      {
      savedCursor = Screen->Cursor;
      Screen->Cursor = crHourGlass;
      }

   // create a string stream to parse macro.
   istringstream Macro_stream (Msg->Strings[0].c_str());

   // loop through all macros in message
   string Macro_name;
   bool ok = true;
   while (!Macro_stream.eof() && ok)
      {
      ok = (Read_token (Macro_stream, " ", "(", Macro_name) == '(');
      To_lower (Macro_name);

      // read in all macro parameters.
      string Macro_parameters;
      if (ok)
         ok = (Read_token (Macro_stream, " ", ")", Macro_parameters) == ')');

      if (ok)
         {
         // interpret command.

         if (Macro_name == "open_predicted_files")
            {
            // Every time a user right clicks on an output file in explorer,
            // control will pass through here once for each output file.
            // We need to add files to the predicted file list.
            // We then need to wait a short while to make sure we got all the
            // files.
            TStringList* files = new TStringList;
            files->CommaText = Macro_parameters.c_str();
            AddToOpenDialog(OpenDialog, files);
            Timer1->Enabled = false;
            Timer1->Enabled = true;
            delete files;
            }
         else
            {
            string message;
            message = "Unknown DDE command : ";
            message += Msg->Strings[0].c_str();
            Application->MessageBox ((char*) message.c_str(), "Error", MB_ICONSTOP | MB_OK);
            }
         }
      }
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::CreateDefaultDatabase(TStrings* files)
   {
   // create a new temporary mdb file in the apswork directory to contain
   // our files.
   string sourceMDB = APSDirectories().Get_home() + "\\ApsimOutlook\\new.mdb";
   string destinationMDB = APSDirectories().Get_working() + "\\temp.mdb";
   CopyFile(sourceMDB.c_str(), destinationMDB.c_str(), false);
   SetFileAttributes(destinationMDB.c_str(), FILE_ATTRIBUTE_NORMAL);

   // create a simulation database object and get it to import our
   // APSIM output files.
   TSimulation_database* newDatabase = new TSimulation_database(this);
   newDatabase->LoginPrompt = false;
   newDatabase->File_name = destinationMDB.c_str();
   newDatabase->Import_APSIM_files (files, NULL);
   delete newDatabase;

   // need to clear the OpenDialog, otherwise in becomes inactive for the user.
   OpenDialog->FileName = destinationMDB.c_str();

   // go give the default database name to the add-in via the .ini file.
   Path iniPath(Application->ExeName.c_str());
   iniPath.Set_extension(".ini");
   Ini_file ini;
   ini.Set_file_name(iniPath.Get_path().c_str());
   string originalContents;
   ini.Read_section_contents("addins", originalContents);
   string contents = originalContents;
   To_lower(contents);
   unsigned int posAddIn = contents.find("dbaddin.dll");
   if (posAddIn != string::npos)
      {
      // add the destination MDB to the .ini file so that the DBaddin
      // can pick it up.
      contents.insert(posAddIn+11, " " + destinationMDB);
      ini.Write_section_contents("addins", contents);
      CreateMDIChild("Chart" + IntToStr(MDIChildCount + 1));

      // now remove our modification to the .ini file.
      ini.Write_section_contents("addins", originalContents);
      }
   else
      ShowMessage("Cannot find line in ini file.  Line: DBAddin.dll");

   Screen->Cursor = savedCursor;
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::Timer1Timer(TObject *Sender)
   {
   Timer1->Enabled = false;
   CreateDefaultDatabase(OpenDialog->Files);
   }
//---------------------------------------------------------------------------

