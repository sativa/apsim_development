//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "TMain_form.h"
#include "TFilespec_form.h"
#include "TBatch_import_form.h"
#include <general\io_functions.h>
#include <general\vcl_functions.h>
//---------------------------------------------------------------------
#pragma link "Grids"
#pragma resource "*.dfm"
TMain_form *Main_form;
//---------------------------------------------------------------------
__fastcall TMain_form::TMain_form(TComponent *AOwner)
	: TForm(AOwner)
   {
   Show_hide_controls();
   }
//---------------------------------------------------------------------
void __fastcall TMain_form::ShowHint(TObject *Sender)
   {
	StatusBar->SimpleText = Application->Hint;
   }
//---------------------------------------------------------------------
void __fastcall TMain_form::Exit(TObject *Sender)
   {
   Simulation_database->Connected = false;
	Close();
   }
//---------------------------------------------------------------------
void __fastcall TMain_form::Show_hide_controls(void)
   {
   Prompt_label->Visible = Simulation_database->Connected;
   Simulation_name_grid->Visible = Simulation_database->Connected;
   }
//---------------------------------------------------------------------
void __fastcall TMain_form::Open_database(TObject *Sender)
   {
	if (Database_open_dialog->Execute())
      {
      Simulation_database->File_name = Database_open_dialog->FileName;
      Index_table->Active = true;
      Show_hide_controls();
      }
   }
//---------------------------------------------------------------------
void __fastcall TMain_form::FormCreate(TObject *Sender)
   {
 	Application->OnHint = ShowHint;
   }
//---------------------------------------------------------------------
void __fastcall TMain_form::Import_simulation(TObject *Sender)
   {
   if (Simulation_database->Connected && Simulation_open_dialog->Execute())
      {
      TStringList* files = new TStringList;
      files->AddStrings(Simulation_open_dialog->Files);
      Import_files (files);
      delete files;
      }
   }
//---------------------------------------------------------------------------
void __fastcall TMain_form::Import_files (TStringList* files)
   {
   Screen->Cursor = crHourGlass;

   Index_table->Close();

   files->Sort();
   Simulation_database->Import_APSIM_files (files);

   Index_table->Open();
   Show_hide_controls();

   Screen->Cursor = crArrow;
   }
//---------------------------------------------------------------------------
void TMain_form::Import_files_using_filespec (const char* Database_file_name,
                                              const char* Directory,
                                              const char* File_spec)
   {
   if (strlen(Database_file_name) > 0 && strlen(Directory) > 0)
      {
      Simulation_database->File_name = Database_file_name;

      list<string> File_list;
      Get_directory_listing (Directory, File_spec, File_list, FA_NORMAL, true);
      TStringList* files = new TStringList;
      Stl_2_tstrings (File_list, files);
      Import_files (files);
      delete files;
      }
   }
//---------------------------------------------------------------------------
void __fastcall TMain_form::Import_simulation_using_filespec(TObject *Sender)
   {
   if (Filespec_import_form->ShowModal() == mrOk)
      {
      list<string> File_list;
      Get_directory_listing (Filespec_import_form->DirectoryEdit->Text.c_str(),
                             Filespec_import_form->FilespecEdit->Text.c_str(),
                             File_list,
                             FA_NORMAL,
                             true);
      TStringList* files = new TStringList;
      Stl_2_tstrings (File_list, files);
      Import_files (files);
      delete files;
      }
   }
//---------------------------------------------------------------------------
void __fastcall TMain_form::Batchimportusingfilespec1Click(TObject *Sender)
   {
   if (Batch_import_form->ShowModal() == mrOk)
      {
      Screen->Cursor = crHourGlass;

      // loop through all non empty rows in grid.
      for (int row = 1; row < Batch_import_form->Grid->RowCount; row++)
         {
         AnsiString Database_file_name = Batch_import_form->Grid->Cells[0][row];
         AnsiString Directory = Batch_import_form->Grid->Cells[1][row];
         AnsiString File_spec = Batch_import_form->FilespecEdit->Text;

         Import_files_using_filespec (Database_file_name.c_str(),
                                      Directory.c_str(),
                                      File_spec.c_str());
         }
      Screen->Cursor = crArrow;
      }
   }

