//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "TDirectory_select_form.h"
#include <general\path.h>
#include <general\ini_file.h>
#include <general\vcl_functions.h>
#include <general\io_functions.h>
static const char* DATASETS_SECTION = "Datasets";
static const char* DEFAULT_SECTION = "Default Datasets";
static const char* DEFAULT_DATASET_KEY = "Dataset";
static const char* DEFAULT_SUBSET_KEY = "Subset";

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
   Path p(Application->ExeName.c_str());
   p.Set_extension(".ini");
   Ini_file Ini;
   Ini.Set_file_name(p.Get_path().c_str());
   list<string> DatasetLines, DatasetNames;
   string Contents;
   Ini.Read_section_contents (DATASETS_SECTION, Contents);
   Split_string (Contents, "\n", DatasetLines);

   remove_substring_and_copy< list<string> > RemoveEquals("=", DatasetNames);
   std::for_each (DatasetLines.begin(), DatasetLines.end(), RemoveEquals);

   // give dataset names to dataset combo.
   Stl_2_tstrings(DatasetNames, DatasetCombo->Items);

   // select default dataset.
   string DefaultDataset;
   Ini.Read (DEFAULT_SECTION, DEFAULT_DATASET_KEY, DefaultDataset);
   DatasetCombo->Text = DefaultDataset.c_str();

   // if there is no item currently selected then simply select first one.
   if (DatasetCombo->Text == "")
      DatasetCombo->ItemIndex = 0;

   // go signal that the combo box has changed.
   DatasetComboChange(NULL);

   // setup the initial subset selections to match the .ini file.
   list<string> DefaultSubsets;
   Ini.Read_list (DEFAULT_SECTION, DEFAULT_SUBSET_KEY, DefaultSubsets);
   for (list<string>::iterator subset = DefaultSubsets.begin();
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
      Path p(Application->ExeName.c_str());
      p.Set_extension(".ini");
      Ini_file Ini;
      Ini.Set_file_name(p.Get_path().c_str());
      Ini.Write (DEFAULT_SECTION, DEFAULT_DATASET_KEY, DatasetCombo->Text.c_str());

      // write the currently selected dataset subsets to .ini file and
      // store the full path to each MDB file in the return MDB list.
      list<string> SelectedSubsets, MDBs;
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

            Get_directory_listing (p.Get_path().c_str(), "*.mdb", MDBs, FA_NORMAL, true);
            }
         }
      Ini.Write_list (DEFAULT_SECTION, DEFAULT_SUBSET_KEY, SelectedSubsets);
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
   Path p(Application->ExeName.c_str());
   p.Set_extension(".ini");
   Ini_file Ini;
   Ini.Set_file_name(p.Get_path().c_str());

   string Directory;
   Ini.Read(DATASETS_SECTION, DatasetCombo->Text.c_str(), Directory);
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

