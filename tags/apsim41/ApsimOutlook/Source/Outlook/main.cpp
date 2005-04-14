//---------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "Main.h"
#include "About.h"
#include "TDrill_down_form.h"
#include <general\vcl_functions.h>
#include <general\path.h>
#include <dos.h>
#include <shellapi.h>
#include "TSkin.h"
#include "TSimulation_database.h"
#include <general\stream_functions.h>
#include <ApsimShared\ApsimDirectories.h>
#include <sstream>
#include <ole2.h>
using namespace std;
//---------------------------------------------------------------------
#pragma link "MDIWallp"
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
   }
//---------------------------------------------------------------------
void __fastcall TMainForm::CreateMDIChild(String Name)
   {
   FixMDI = false;
	TMDIChild *Child;

	//--- create a new MDI child window ----
   Child = new TMDIChild(Application);
   Child->Set_toolbar (ToolBar2);
   Child->Caption = Name;
   Child->SetPresentationFonts(FilePresentationFontsMenu->Checked);
   Child->Show();
   FixMDI = true;
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

//   if (FixMDI)
//      FixMDIChild();
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
   Skin->displayHelp();
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::FilePresentationFontsMenuClick(TObject *Sender)
   {
   FilePresentationFontsMenu->Checked = !FilePresentationFontsMenu->Checked;

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
   CalendarButton->Hint = "Day number calendar";

   // change caption.
   Path p(Application->ExeName.c_str());
   if (Str_i_Eq(p.Get_name_without_ext(), "whoppercropper"))
      Caption = "Whopper Cropper";

   // display logo if necessary.
   ApsimSettings settings;
   string fileName;
   settings.read("Outlook Skin|logo", fileName);
   if (fileName != "")
      {
      fileName = getAppHomeDirectory() + "\\" + fileName;
      LogoImage->Picture->LoadFromFile(fileName.c_str());
      }
   readCommandLine();
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::Evaluate(TObject *Sender)
   {
   Skin->displayEvaluation();
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
void TMainForm::readCommandLine(void)
   {
   if (CommandLine != "")
      {
      // display an hourglass if we haven't already done so.
      TCursor savedCursor = Screen->Cursor;
      Screen->Cursor = crHourGlass;

      TStringList* files = new TStringList;
      files->LoadFromFile(CommandLine);
      CreateDefaultDatabase(files);
      delete files;

      Screen->Cursor = savedCursor;
      }
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::CreateDefaultDatabase(TStrings* files)
   {
   // create a new temporary mdb file in the apswork directory to contain
   // our files.
   string sourceMDB = getAppHomeDirectory() + "\\new.mdb";
   string destinationMDB = Path::getTempFolder().Get_path() + "\\temp.mdb";
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
   ApsimSettings settings;
   string originalContents;
   settings.readSection("Outlook Addins", originalContents);
   string contents = originalContents;
   To_lower(contents);

   unsigned int posAddIn = findDBAddInLine(contents);

   if (posAddIn != string::npos)
      {
      // add the destination MDB to the .ini file so that the DBaddin
      // can pick it up.
      contents.insert(posAddIn+11, " " + destinationMDB);
      settings.writeSection("Outlook addins", contents);
      CreateMDIChild("Chart" + IntToStr(MDIChildCount + 1));

      // now remove our modification to the .ini file.
      settings.refresh();
      settings.writeSection("Outlook addins", originalContents);
      }
   else
      ShowMessage("Cannot find line in ini file.  Line: DBAddin.dll");
   }

//---------------------------------------------------------------------------
unsigned TMainForm::findDBAddInLine(const string& contents)
   {
   unsigned pos = 0;
   do
      {
      pos = contents.find("dbaddin.dll", pos);
      unsigned posComment = contents.find_last_of(";\n", pos);
      if (posComment == string::npos || contents[posComment] == '\n')
         return pos;
      pos++;
      }
   while (true);
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::FixMDIChild(void)
{
    if (ActiveMDIChild && ActiveMDIChild->WindowState == wsMaximized)
    {
/*        // Prevent screen updates
        SNDMSG(ClientHandle, WM_SETREDRAW, false, 0);
        try
        {
            TForm *frm = ActiveMDIChild;

            // Minimize and maximize the child to restore its icons
            SNDMSG(frm->Handle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
            SNDMSG(frm->Handle, WM_SYSCOMMAND, SC_MAXIMIZE, 0);

            // Minimize and maximize the child a second time to prevent
            //  the close button from appearing disabled
            SNDMSG(frm->Handle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
            SNDMSG(frm->Handle, WM_SYSCOMMAND, SC_MAXIMIZE, 0);

        }
        catch (...)
        {
            // Restore screen updates
            SNDMSG(ClientHandle, WM_SETREDRAW, true, 0);
        }

        // Restore screen updates
        SNDMSG(ClientHandle, WM_SETREDRAW, true, 0);
*/    }
}
//---------------------------------------------------------------------------
void __fastcall TMainForm::FormResize(TObject *Sender)
   {
   LogoImage->Left = CoolBar1->Width - LogoImage->ClientWidth - 4;
   }
//---------------------------------------------------------------------------

void __fastcall TMainForm::CalendarButtonClick(TObject *Sender)
   {
   ApsimSettings settings;
   string fileName;
   settings.read("Outlook Skin|daynumberfile", fileName, true);
   ShellExecute (MainForm->Handle, "open",
                 fileName.c_str(), NULL, "", SW_SHOW);
   }
//---------------------------------------------------------------------------

