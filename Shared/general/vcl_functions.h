#ifndef vclfunctionsH
#define vclfunctionsH

#include "Grids.hpp"

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

// ------------------------------------------------------------------
template <class STL_container>
void Stl_2_tstrings (STL_container& ls, TStrings* VCL_list)
   {
   VCL_list->Clear();

   // loop through all field names.
   for (list<string>::iterator Iter = ls.begin();
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

#endif
