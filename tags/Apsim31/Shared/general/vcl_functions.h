#ifndef vclfunctionsH
#define vclfunctionsH

#include <Grids.hpp>
#include <db.hpp>
#include <fstream>
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
void Grid_input_from_csv (TStringGrid* grid, std::istream& csv_stream);

// ------------------------------------------------------------------
//  Short description:
//     output contents of grid to csv stream.

//  Notes:

//  Changes:
//    DPH 18/4/1997

// ------------------------------------------------------------------
void Grid_output_to_csv (TStringGrid* grid, std::ostream& csv_stream);

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
//      select a list of items in specified multi-select listbox.  If items
//      don't exist in listbox then they are added.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void Select_items_in_listbox(TListBox* listbox, TStrings* Items_to_select);

// ------------------------------------------------------------------
//  Short description:
//      get a list of items that are selected in the specified listbox.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void Get_selected_items_from_listbox(TListBox* listbox, TStrings* Selected_items);

// ------------------------------------------------------------------
//  Short description:
//      retrieve a specified component from a parent component

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
TComponent* Locate_component(TComponent* component, const char* Component_name);

// ------------------------------------------------------------------
// Retrieve a component of type T, from the specified owner component
// Does not recursively search children of the specified owner.
// to use: TDataSet* dataset = getComponent<TDataSet>(owner, "mydataset");
// ------------------------------------------------------------------
template <class T>
T* getComponent(TComponent* owner, const AnsiString& componentName)
   {
   // loop through all components in parent form.
   for (int componentI = 0; componentI < owner->ComponentCount; componentI++)
      {
      if (owner->Components[componentI]->Name.AnsiCompareIC(componentName) == 0)
         return dynamic_cast<T*> (owner->Components[componentI]);
      }
   return NULL;
   }

// ------------------------------------------------------------------
// Loop through all components owned by the specified component and
// retrieve a list of component names that match T.
// Does not recursively search children of the specified owner.
// to use: getComponentNames<TDataSet>(owner, datasetNames);
// ------------------------------------------------------------------
template <class T>
void getComponentNames(TComponent* owner, TStrings* componentNames)
   {
   // loop through all components in parent form.
   for (int componentI = 0; componentI < owner->ComponentCount; componentI++)
      {
      TComponent* component = dynamic_cast<T*>(owner->Components[componentI]);
      if (component != NULL)
         componentNames->Add(component->Name);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//      get the files from a multi select open dialog box.  These file
//      names will be fully qualified file names with full path info.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void Get_files_from_open_dialog (TOpenDialog* Open_dialog, TStringList* File_list);

// ------------------------------------------------------------------
//  Short description:
//      Give the specified list of files to the specified open dialog box.
//      This routine takes care of file paths etc.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void Give_files_to_open_dialog (TOpenDialog* Open_dialog, TStringList* File_list);

// ------------------------------------------------------------------
//  Short description:
//      convert a colour string to a TColor

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
TColor ColorStringToTColor (const char* ColourString);

// ------------------------------------------------------------------
//  Short description:
//      convert a font style string to a TFontStyle

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
TFontStyles FontStringToTFontStyles (const char* StyleString);

// ------------------------------------------------------------------
//  Short description:
//      this routine sets up a Olevariant array with the
//      specified bounds.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void OleVariantInit (VARIANT& OleVariant, int NumElements, VARTYPE DataType);

// ------------------------------------------------------------------
//  Short description:
//      this routine converts a vector of numbers into an OLE
//      variant array that can be passed to an ACTIVEX object.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void Doubles_to_olevariant (std::vector<double>& StlArray, VARIANT& OleVariant);

// ------------------------------------------------------------------
//  Short description:
//      this routine converts an ACTIVEX VARIANT into a vector
//      of numbers

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void Olevariant_to_doubles (VARIANT& OleVariant, std::vector<double>& StlArray);

// ------------------------------------------------------------------
//  Short description:
//      this routine converts a vector of strings into an OLE
//      variant array that can be passed to an ACTIVEX object.

//  Notes:

//  Changes:
//    DPH 5/2/98

// ------------------------------------------------------------------
void Strings_to_olevariant (std::vector<std::string>& StlArray, VARIANT& OleVariant);

// ------------------------------------------------------------------
// Load a component from a stream.
// ------------------------------------------------------------------
void loadComponent(AnsiString filename, TComponent*& component);

// ------------------------------------------------------------------
// Save a component to a stream.  Works best saving an entire form.
// ------------------------------------------------------------------
void saveComponent(AnsiString filename, TComponent* component);

//---------------------------------------------------------------------------
// Resolve the componentName.propertyName passed in with the value
// of the specified property.  Only component owned by the specified owner
// will be found.
//---------------------------------------------------------------------------
AnsiString resolveComponentPropertyMacro(TComponent* owner, AnsiString text);

#endif
