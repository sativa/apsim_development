//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Main.h"
#include "About.h"
#include "TDirectory_select_form.h"
#include "TDrill_down_form.h"
#include <general\vcl_functions.h>
#include <general\ini_file.h>
#include <general\path.h>
#include <dos.h>
#include <shellapi.h>
//---------------------------------------------------------------------
#pragma link "TSimulations"
#pragma link "TSimulations_from_mdbs"
#pragma resource "*.dfm"
TMainForm *MainForm;
extern AnsiString CommandLine;
//---------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent *Owner)
	: TForm(Owner)
   {
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
   Child->Show();
   Child->Set_toolbar (ToolBar2);
   Child->Caption = Name;
   Child->Set_all_simulations(All_simulations);
   Child->SetPresentationFonts(FilePresentationFontsMenu->Checked);
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
   ShowWindow(Handle, SW_MINIMIZE);
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::FileOpenMenuClick(TObject *Sender)
   {
   if (OpenDialog->Execute())
      {
      Directory_select_form->SelectedMDBs->AddStrings(OpenDialog->Files);
      All_simulations->Database_file_names = Directory_select_form->SelectedMDBs;
      if (All_simulations->Count() > 0)
         CreateMDIChild("Chart" + IntToStr(MDIChildCount + 1));
      else
         {
         AnsiString msg = "No simulation data found.";
         Application->MessageBox(msg.c_str(), "Error", MB_ICONSTOP | MB_OK);
         }
      }
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::FileOpenDatasetMenuClick(TObject *Sender)
   {
   if (Directory_select_form->ShowModal() == mrOk)
      {
      All_simulations->Database_file_names = Directory_select_form->SelectedMDBs;
      if (All_simulations->Count() > 0)
         CreateMDIChild("Chart" + IntToStr(MDIChildCount + 1));
      else
         {
         AnsiString msg = "No simulation data found.";
         Application->MessageBox(msg.c_str(), "Error", MB_ICONSTOP | MB_OK);
         }
      }
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::HelpContentsMenuClick(TObject *Sender)
   {
   Path p (Application->ExeName.c_str());
   if (Str_i_Eq(p.Get_name_without_ext(), "whoppercropper"))
      {
      string manual = p.Get_directory() + "\\manual\\default.htm";
      ShellExecute (this->Handle, "open", manual.c_str(), NULL, "", SW_SHOW);
      }
   else
      {
      p.Set_extension(".hlp");

      string CommandLine = "winhlp32.exe " + p.Get_directory() + "\\docs\\" + p.Get_name();
      WinExec (CommandLine.c_str(), SW_SHOW);
      }
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
   // change caption.
   Path p(Application->ExeName.c_str());
   if (Str_i_Eq(p.Get_name_without_ext(), "whoppercropper"))
      Caption = "Whopper Cropper";

   // auto load from command line if necessary.
   if (CommandLine != "" && FileExists(CommandLine))
      {
      // user has specified a file on the command line.
      TStringList* Names = new TStringList;
      Names->Add (CommandLine);
      All_simulations->Database_file_names = Names;
      if (All_simulations->Count() > 0)
         CreateMDIChild("Chart" + IntToStr(MDIChildCount + 1));
      else
         {
         AnsiString msg = "No simulation data found in file: " + AnsiString(CommandLine);
         Application->MessageBox(msg.c_str(), "Error", MB_ICONSTOP | MB_OK);
         }
      delete Names;
      CommandLine = "";
      }
   }
//---------------------------------------------------------------------------
void __fastcall TMainForm::Evaluate(TObject *Sender)
   {
   ShellExecute (this->Handle, "open",
                 "http://www.apsru.gov.au/whopper/survey.htm", NULL, "", SW_SHOW);
   }
//---------------------------------------------------------------------------



