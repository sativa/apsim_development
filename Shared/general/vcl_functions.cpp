#include <vcl\vcl.h>
#pragma hdrstop

#include <general\vcl_functions.h>
#include <general\string_functions.h>
#include <general\mylist.h>
#include <vcl\dbtables.hpp>
// ------------------------------------------------------------------
//  Short description:
//     fill a grid control from a csv stream.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
void Grid_input_from_csv (TStringGrid* grid, istream& csv_stream)
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
void Grid_output_to_csv (TStringGrid* grid, ostream& csv_stream)
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
void Grid_clear (TStringGrid* grid)
   {
   for (int row = 0; row < grid->RowCount; row++)
      for (int col = 0; col < grid->ColCount; col++)
         grid->Cells[col][row] = "";
   }

// ------------------------------------------------------------------
//  Short description:
//      retrieve a list of names of all active datasets in a component

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void Get_active_datasets(TComponent* component, TStrings* Dataset_names)
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
//      retrieve a list of names of all active dataset names in component

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void Get_active_databases(TComponent* component, TStrings* Database_names)
   {
   // loop through all components in parent form.
   Database_names->Clear();
   for (int i = 0; i < component->ComponentCount; i++)
      {
      TDatabase* database = dynamic_cast<TDatabase*> (component->Components[i]);
      if (database != NULL)
         Database_names->Add(database->Name);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//      retrieve a specific active dataset on specified component.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
TDataSet* Get_active_dataset(TComponent* component, const char* Dataset_name)
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
void Get_tables_from_databases (TStringList* Database_names,
                                TStringList* Table_names)
   {
   for (int i = 0; i < Database_names->Count; i++)
      {
      Session->GetTableNames(Database_names->Strings[i], "", true, false, Table_names);
      }
   }
// ------------------------------------------------------------------
//  Short description:
//      retrieve a specified component from a parent component

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
TComponent* Locate_component(TComponent* component, const char* Component_name)
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
void Get_field_list (TDataSet* dataset, TStringList* field_names)
   {
   for (int i = 0; i < dataset->FieldCount; i++)
      field_names->Add (dataset->Fields[i]->FieldName);
   }

// ------------------------------------------------------------------
//  Short description:
//      copy the structure from one dataset to another for those
//      fields specified.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void Copy_dataset_structure (TDataSet* source,
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

