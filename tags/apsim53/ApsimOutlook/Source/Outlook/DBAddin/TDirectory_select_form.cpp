//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TDirectory_select_form.h"
#include <general\path.h>
#include <general\vcl_functions.h>
#include <general\io_functions.h>
#include <dir.h>
static const char* DATASETS_SECTION = "Outlook Datasets";
static const char* DEFAULT_DATASET_KEY = "Outlook Default Datasets|Dataset";
static const char* DEFAULT_SUBSET_KEY = "Outlook Default Datasets|Subset";

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TDirectory_select_form *Directory_select_form;
//---------------------------------------------------------------------------
__fastcall TDirectory_select_form::TDirectory_select_form(TComponent* Owner)
   : TForm(Owner)
   {
   SelectedMDBs = new TStringList;
   }
//---------------------------------------------------------------------------
__fastcall TDirectory_select_form::~TDirectory_select_form()
   {
   delete SelectedMDBs;
   }
//---------------------------------------------------------------------------
void __fastcall TDirectory_select_form::FormShow(TObject *Sender)
   {
   // read a list of dataset names from .ini file.
   vector<string> DatasetLines, DatasetNames;
   string Contents;
   settings.readSection(DATASETS_SECTION, Contents);
   Split_string (Contents, "\n", DatasetLines);

   trim_from_substring_and_copy< vector<string> > RemoveEquals("=", DatasetNames);
   std::for_each (DatasetLines.begin(), DatasetLines.end(), RemoveEquals);

   // give dataset names to dataset combo.
   Stl_2_tstrings(DatasetNames, DatasetCombo->Items);

   // select default dataset.
   string DefaultDataset;
   settings.read(DEFAULT_DATASET_KEY, DefaultDataset);
   DatasetCombo->Text = DefaultDataset.c_str();

   // if there is no item currently selected then simply select first one.
   if (DatasetCombo->Text == "")
      DatasetCombo->ItemIndex = 0;

   // go signal that the combo box has changed.
   DatasetComboChange(NULL);

   // setup the initial subset selections to match the .ini file.
   vector<string> DefaultSubsets;
   settings.read(DEFAULT_SUBSET_KEY, DefaultSubsets);
   for (vector<string>::iterator subset = DefaultSubsets.begin();
                               subset != DefaultSubsets.end();
                               subset++)
      {
      int Index = SubsetList->Items->IndexOf( (*subset).c_str() );
      if (Index >= 0)
         SubsetList->Selected[Index] = true;
      }
   SubsetListChange(Sender);
   }
//---------------------------------------------------------------------------
void __fastcall TDirectory_select_form::FormClose(TObject *Sender,
      TCloseAction &Action)
   {
   if (ModalResult == mrOk)
      {
      // write the currently selected dataset to .ini file.
      settings.write(DEFAULT_DATASET_KEY, DatasetCombo->Text.c_str());

      // write the currently selected dataset subsets to .ini file and
      // store the full path to each MDB file in the return MDB list.
      vector<string> SelectedSubsets, MDBs;
      for (int i = 0; i < SubsetList->Items->Count; i++)
         {
         if (SubsetList->Selected[i])
            {
            // add sub directory to list ready for writting to .ini file.
            SelectedSubsets.push_back (SubsetList->Items->Strings[i].c_str());

            // work out what directory user has selected.
            Path p;
            p.Set_directory (SubsetList->Directory.c_str());
            p.Append_path (SubsetList->Items->Strings[i].c_str());

            // go scan for mdb files.

            getDirectoryListing(p.Get_path(), "*.mdb", MDBs, FA_NORMAL, true);
            }
         }
      settings.write(DEFAULT_SUBSET_KEY, SelectedSubsets);
      Stl_2_tstrings(MDBs, SelectedMDBs);

      }
   }
//---------------------------------------------------------------------------
void __fastcall TDirectory_select_form::Directory_listboxChange(TObject *Sender)
   {
   Ok_button->Enabled = SubsetList->SelCount;
   }
//---------------------------------------------------------------------------
void TDirectory_select_form::Cleanup_dir_list (void)
   {
   // remove the . and .. items from the list.
   for (int i = SubsetList->Items->Count - 1; i >= 0; i--)
      {
      if (SubsetList->Items->Strings[i] == "[.]" ||
          SubsetList->Items->Strings[i] == "[..]")
         SubsetList->Items->Delete (i);
      else
         {
         // remove the square brackets from around the items.
         string item = SubsetList->Items->Strings[i].c_str();
         if (item[0] == '[')
            item = item.substr(1, item.length() - 2);
         SubsetList->Items->Strings[i] = item.c_str();
         }
      }
   }
//---------------------------------------------------------------------------
void __fastcall TDirectory_select_form::DatasetComboChange(TObject *Sender)
   {
   string Directory;
   string key = DATASETS_SECTION;
   key += "|";
   key += DatasetCombo->Text.c_str();
   settings.read(key, Directory);

   Path p = Path::getCurrentFolder();
   SubsetList->Directory = Directory.c_str();
   Cleanup_dir_list();
   // DAH: 24/8/00 - added following line to restore working directory since the
   //                SubsetList has the side effect of setting the working dir
   //                to where the datasets lie.
   p.Change_directory();
   }
//---------------------------------------------------------------------------

void __fastcall TDirectory_select_form::SubsetListChange(TObject *Sender)
{
   Ok_button->Enabled = SubsetList->SelCount;
}
//---------------------------------------------------------------------------

