//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TLibraryForm.h"
#include <general\io_functions.h>
#include <general\vcl_functions.h>
#include <ApsimShared\ApsimSettings.h>

using namespace std;
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "AdvListV"
#pragma link "FlCtrlEx"
#pragma resource "*.dfm"
TLibraryForm *LibraryForm;
//---------------------------------------------------------------------------
__fastcall TLibraryForm::TLibraryForm(TComponent* Owner)
   : TForm(Owner)
   {
   }
//---------------------------------------------------------------------------
void __fastcall TLibraryForm::FormShow(TObject *Sender)
   {
   ApsimSettings settings;
   settings.read("Apsim Report|Library Path", libraryDirectory, true);

   vector<string> folders;
   getDirectoryListing(libraryDirectory, "*.*", folders, FA_DIREC);
   sort(folders.begin(), folders.end());

   // remove all 'cvs' directories.
   folders.erase(remove(folders.begin(), folders.end(), "cvs"), folders.end());

   Stl_2_tstrings(folders, TabControl->Tabs);

   // go fill the file list.
   TabControlChange(NULL);
   }
//---------------------------------------------------------------------------
void __fastcall TLibraryForm::TabControlChange(TObject *Sender)
   {
   if (TabControl->TabIndex >= 0)
      {
      string mask = libraryDirectory + "\\";
      mask += TabControl->Tabs->Strings[TabControl->TabIndex].c_str();
      mask += "\\*.report";
      FileList->Mask = mask.c_str();
      }
   }
//---------------------------------------------------------------------------
// Return the selected file to caller.
//---------------------------------------------------------------------------
AnsiString TLibraryForm::getSelectedFile(void)
   {
   AnsiString directory = libraryDirectory.c_str();
   directory += "\\";
   directory += TabControl->Tabs->Strings[TabControl->TabIndex];
   return directory + "\\" + FileList->Items->Strings[FileList->ItemIndex];
   }

