#include <vcl\vcl.h>
#pragma hdrstop

#include <general\vcl_functions.h>
#include <general\string_functions.h>
#include <general\mylist.h>
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
   while (!csv_stream.eof())
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
         csv_stream << grid->Cells[col][row] << ',';
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


