#include <vcl\vcl.h>
#pragma hdrstop

#include <general\vcl_functions.h>
#include <general\string_functions.h>
#include <general\path.h>
#include <general\mylist.h>
#include <vcl\dbtables.hpp>
// ------------------------------------------------------------------
//  Short description:
//     fill a grid control from a csv stream.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
void GENERAL_EXPORT Grid_input_from_csv (TStringGrid* grid, istream& csv_stream)
   {
   // loop through all lines on input stream.
   char Line[1000];
   list <string> words;
   string St;
   int Row = 0;
   while (csv_stream && !csv_stream.eof())
      {
      csv_stream.getline(Line, sizeof Line);

      St = Line;
      Split_string (St, ",", words);

      // loop through all words.
      int Col = 0;
      for (list <string>::iterator Iter = words.begin();
                                   Iter != words.end();
                                   Iter++)
         {
         if ( (*Iter).length() > 0)
            grid->Cells[Col][Row] = (*Iter).c_str();
         Col++;
         }
      Row++;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     output contents of grid to csv file.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
void GENERAL_EXPORT Grid_output_to_csv (TStringGrid* grid, ostream& csv_stream)
   {
   for (int row = 0; row < grid->RowCount; row++)
      {
      for (int col = 0; col < grid->ColCount; col++)
         {
         csv_stream << grid->Cells[col][row].c_str() << ',';
         }
      csv_stream << endl;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     clear a grid.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
void GENERAL_EXPORT Grid_clear (TStringGrid* grid)
   {
   for (int row = 0; row < grid->RowCount; row++)
      for (int col = 0; col < grid->ColCount; col++)
         grid->Cells[col][row] = "";
   }

// ------------------------------------------------------------------
//  Short description:
//      select a list of items in specified multi-select listbox.  If items
//      don't exist in listbox then they are added.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void GENERAL_EXPORT Select_items_in_listbox(TListBox* listbox, TStrings* Items_to_select)
   {
   if (listbox->MultiSelect)
      {
      for (int i = 0; i < Items_to_select->Count; i++)
         {
         int index = listbox->Items->IndexOf(Items_to_select->Strings[i]);
//         if (index < 0)
//            index = listbox->Items->Add(Items_to_select->Strings[i]);

         if (index >= 0)
            listbox->Selected[index] = true;
         }
      }
   else if (Items_to_select->Count > 0)
      listbox->ItemIndex = listbox->Items->IndexOf(Items_to_select->Strings[0]);
   }

// ------------------------------------------------------------------
//  Short description:
//      get a list of items that are selected in the specified listbox.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void GENERAL_EXPORT Get_selected_items_from_listbox(TListBox* listbox, TStrings* Selected_items)
   {
   Selected_items->Clear();
   if (listbox->MultiSelect)
      {
      for (int i = 0; i < listbox->Items->Count; i++)
         {
         if (listbox->Selected[i])
            {
            if (listbox->Items->Strings[i].Length() > 0)
               Selected_items->Add(listbox->Items->Strings[i]);
            }
         }
      }
   else if (listbox->ItemIndex >= -1)
      Selected_items->Add(listbox->Items->Strings[listbox->ItemIndex]);
   }

// ------------------------------------------------------------------
//  Short description:
//      retrieve a list of names of all active datasets in a component

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void GENERAL_EXPORT Get_active_datasets(TComponent* component, TStrings* Dataset_names)
   {
   // loop through all components in parent form.
   Dataset_names->Clear();
   for (int i = 0; i < component->ComponentCount; i++)
      {
      TDataSet* dataset = dynamic_cast<TDataSet*> (component->Components[i]);
      if (dataset != NULL)
         Dataset_names->Add(dataset->Name);
      }
   }
// ------------------------------------------------------------------
//  Short description:
//      Retrieve a list of names of all active database names in current session

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void GENERAL_EXPORT Get_active_databases(TStrings* Database_names)
   {
   Database_names->Clear();

   // loop through all components in parent form.
   for (int i = 0; i < Session->DatabaseCount; i++)
      Database_names->Add(Session->Databases[i]->DatabaseName);
   }

// ------------------------------------------------------------------
//  Short description:
//      retrieve a specific active dataset on specified component.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
TDataSet* GENERAL_EXPORT Get_active_dataset(TComponent* component, const char* Dataset_name)
   {
   // loop through all components in parent form.
   for (int i = 0; i < component->ComponentCount; i++)
      {
      TDataSet* dataset = dynamic_cast<TDataSet*> (component->Components[i]);
      if (dataset != NULL && dataset->Name == Dataset_name)
         return dataset;
      }
   return NULL;
   }

// ------------------------------------------------------------------
//  Short description:
//      return a list of tables for the specified databases.  The database
//      names passed in are database aliases.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void GENERAL_EXPORT Get_tables_from_databases (TStringList* Database_names,
                                TStringList* Table_names)
   {
   Table_names->Clear();
   for (int i = 0; i < Database_names->Count; i++)
      Session->GetTableNames(Database_names->Strings[i], "", false, false, Table_names);
   }
// ------------------------------------------------------------------
//  Short description:
//      retrieve a specified component from a parent component

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
TComponent* GENERAL_EXPORT Locate_component(TComponent* component, const char* Component_name)
   {
   // loop through all components in parent form.
   for (int i = 0; i < component->ComponentCount; i++)
      {
      if (component->Components[i]->Name == Component_name)
         return component->Components[i];
      }
   return NULL;
   }

// ------------------------------------------------------------------
//  Short description:
//      retrieve a list of all field names in a specified dataset.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void GENERAL_EXPORT Get_field_list (TDataSet* dataset, TStringList* field_names)
   {
   field_names->Clear();
   for (int i = 0; i < dataset->FieldCount; i++)
      field_names->Add (dataset->Fields[i]->FieldName);
   }

#if __BORLANDC__ == 0x530
// ------------------------------------------------------------------
//  Short description:
//      copy the structure from one dataset to another for those
//      fields specified.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void GENERAL_EXPORT Copy_dataset_structure (TDataSet* source,
                                            TDataSet* destination,
                                            TStringList* field_names)
   {
   // loop through all selected field names and create a field in
   // out memory table
   for (int j = 0; j < field_names->Count; j++)
      {
      AnsiString field_name = field_names->Strings[j];

      // only add field to our memory table for this dataset if it
      // exists in the source dataset.
      int Pos_source_field = source->FieldDefs->IndexOf(field_name);
      if (Pos_source_field >= 0)
         {
         TFieldDef* field = destination->FieldDefs->AddFieldDef();
         field->Assign (source->FieldDefs->Items[Pos_source_field]);
         }
      }
   }

// ------------------------------------------------------------------
//  Short description:
//      setup the open dialog.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void GENERAL_EXPORT Give_files_to_open_dialog (TOpenDialog* Open_dialog, TStringList* File_list)
   {
   string Initial_file_name;

   // loop through all files
   for (int i = 0; i < File_list->Count; i++)
      {
      Path p(File_list->Strings[i].c_str());

      Initial_file_name += "\"";
      Initial_file_name += p.Get_name();
      Initial_file_name += "\" ";
      Open_dialog->InitialDir = p.Get_directory().c_str();
      }

   Open_dialog->FileName = Initial_file_name.c_str();
   }

// ------------------------------------------------------------------
//  Short description:
//      setup the open dialog.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void GENERAL_EXPORT Get_files_from_open_dialog (TOpenDialog* Open_dialog, TStringList* File_list)
   {
   File_list->Clear();
   File_list->AddStrings(Open_dialog->Files);
   }
#endif
