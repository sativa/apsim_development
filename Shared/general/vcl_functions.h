#ifndef vclfunctionsH
#define vclfunctionsH

#include <general\general.h>

#include "Grids.hpp"
#include <fstream>
#include <vcl\db.hpp>
#include <oleauto.h>
#include <vector>
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
void GENERAL_EXPORT Grid_input_from_csv (TStringGrid* grid, std::istream& csv_stream);

// ------------------------------------------------------------------
//  Short description:
//     output contents of grid to csv stream.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
void GENERAL_EXPORT Grid_output_to_csv (TStringGrid* grid, std::ostream& csv_stream);

// ------------------------------------------------------------------
//  Short description:
//     clear a grid.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
void GENERAL_EXPORT Grid_clear (TStringGrid* grid);

// ------------------------------------------------------------------
//  Short description:
//      select a list of items in specified multi-select listbox.  If items
//      don't exist in listbox then they are added.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void GENERAL_EXPORT Select_items_in_listbox(TListBox* listbox, TStrings* Items_to_select);

// ------------------------------------------------------------------
//  Short description:
//      get a list of items that are selected in the specified listbox.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void GENERAL_EXPORT Get_selected_items_from_listbox(TListBox* listbox, TStrings* Selected_items);

// ------------------------------------------------------------------
//  Short description:
//      retrieve a list of names of all active datasets in a component

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void GENERAL_EXPORT Get_active_datasets(TComponent* component, TStrings* Dataset_names);

// ------------------------------------------------------------------
//  Short description:
//      retrieve a list of names of all active dataset names in component

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void GENERAL_EXPORT Get_active_databases(TStrings* Database_names);

// ------------------------------------------------------------------
//  Short description:
//      retrieve a specific active dataset on specified component.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
TDataSet* GENERAL_EXPORT Get_active_dataset(TComponent* component, const char* Dataset_name);

// ------------------------------------------------------------------
//  Short description:
//      return a list of tables for the specified databases.  The database
//      names passed in are database aliases.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void GENERAL_EXPORT Get_tables_from_databases (TStringList* Database_names,
                                               TStringList* Table_names);

// ------------------------------------------------------------------
//  Short description:
//      retrieve a specified component from a parent component

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
TComponent* GENERAL_EXPORT Locate_component(TComponent* component, const char* Component_name);


// ------------------------------------------------------------------
//  Short description:
//      retrieve a list of all field names in a specified dataset.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void GENERAL_EXPORT Get_field_list (TDataSet* dataset, TStringList* field_names);

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
                                            TStringList* field_names);

// ------------------------------------------------------------------
//  Short description:
//      get the files from a multi select open dialog box.  These file
//      names will be fully qualified file names with full path info.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void GENERAL_EXPORT Get_files_from_open_dialog (TOpenDialog* Open_dialog, TStringList* File_list);

// ------------------------------------------------------------------
//  Short description:
//      Give the specified list of files to the specified open dialog box.
//      This routine takes care of file paths etc.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void GENERAL_EXPORT Give_files_to_open_dialog (TOpenDialog* Open_dialog, TStringList* File_list);

// ------------------------------------------------------------------
//  Short description:
//      convert a colour string to a TColor

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
TColor GENERAL_EXPORT ColorStringToTColor (const char* ColourString);

// ------------------------------------------------------------------
//  Short description:
//      convert a font style string to a TFontStyle

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
TFontStyles GENERAL_EXPORT FontStringToTFontStyles (const char* StyleString);

// ------------------------------------------------------------------
//  Short description:
//      this routine sets up a Olevariant array with the
//      specified bounds.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void GENERAL_EXPORT OleVariantInit (VARIANT& OleVariant, int NumElements, VARTYPE DataType);

// ------------------------------------------------------------------
//  Short description:
//      this routine converts a vector of numbers into an OLE
//      variant array that can be passed to an ACTIVEX object.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void GENERAL_EXPORT Doubles_to_olevariant (std::vector<double>& StlArray, VARIANT& OleVariant);

// ------------------------------------------------------------------
//  Short description:
//      this routine converts an ACTIVEX VARIANT into a vector
//      of numbers

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void GENERAL_EXPORT Olevariant_to_doubles (VARIANT& OleVariant, std::vector<double>& StlArray);

// ------------------------------------------------------------------
//  Short description:
//      this routine converts a vector of strings into an OLE
//      variant array that can be passed to an ACTIVEX object.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void GENERAL_EXPORT Strings_to_olevariant (std::vector<std::string>& StlArray, VARIANT& OleVariant);

#endif
