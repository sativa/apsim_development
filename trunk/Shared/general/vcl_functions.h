#ifndef vclfunctionsH
#define vclfunctionsH

#include "Grids.hpp"
#include <fstream.h>
#include <vcl\db.hpp>

// ------------------------------------------------------------------
//  Short description:
//     Copy contents from a VCL TStrings object to a STL container.

//  Notes:

//  Changes:
//    DPH 17/4/1997

// ------------------------------------------------------------------
template <class STL_container>
void TStrings_2_stl (TStrings* VCL_list, STL_container& ls)
   {
   ls.erase (ls.begin(), ls.end());

   // loop through all field names.
   for (int Iter = 0; Iter < VCL_list->Count; Iter++)
      ls.push_back (VCL_list->Strings[Iter].c_str());
   }

// ------------------------------------------------------------------
//  Short description:
//     Copy contents from a STLcontainer to a TStrings object

//  Notes:

//  Changes:
//    DPH 17/4/1997
//    dph 17/3/98 removed reference to list - defect.

// ------------------------------------------------------------------
template <class STL_container>
void Stl_2_tstrings (STL_container& ls, TStrings* VCL_list)
   {
   VCL_list->Clear();

   // loop through all field names.
   for (STL_container::iterator Iter = ls.begin();
                                Iter != ls.end();
                                Iter++)
      VCL_list->Add ((*Iter).c_str());
   }

// ------------------------------------------------------------------
//  Short description:
//     fill a grid control from a csv stream.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
void Grid_input_from_csv (TStringGrid* grid, istream& csv_stream);

// ------------------------------------------------------------------
//  Short description:
//     output contents of grid to csv stream.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
void Grid_output_to_csv (TStringGrid* grid, ostream& csv_stream);

// ------------------------------------------------------------------
//  Short description:
//     clear a grid.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
void Grid_clear (TStringGrid* grid);

// ------------------------------------------------------------------
//  Short description:
//      retrieve a list of names of all active datasets in a component

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void Get_active_datasets(TComponent* component, TStrings* Dataset_names);

// ------------------------------------------------------------------
//  Short description:
//      retrieve a list of names of all active dataset names in component

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void Get_active_databases(TComponent* component, TStrings* Database_names);

// ------------------------------------------------------------------
//  Short description:
//      retrieve a specific active dataset on specified component.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
TDataSet* Get_active_dataset(TComponent* component, const char* Dataset_name);

// ------------------------------------------------------------------
//  Short description:
//      return a list of tables for the specified databases.  The database
//      names passed in are database aliases.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void Get_tables_from_databases (TStringList* Database_names,
                                TStringList* Table_names);

// ------------------------------------------------------------------
//  Short description:
//      retrieve a specified component from a parent component

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
TComponent* Locate_component(TComponent* component, const char* Component_name);


// ------------------------------------------------------------------
//  Short description:
//      retrieve a list of all field names in a specified dataset.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void Get_field_list (TDataSet* dataset, TStringList* field_names);

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
                             TStringList* field_names);

#endif
