#include <graph\global.h>
#pragma hdrstop

#include <consts.h>
#include <cl\chartwin.h>
#include <graph\printout.h>

// *******************************************************************
       Chart_printout::Chart_printout
          (const char *Title, int Max_num_charts)
      	: TPrintout(Title)  {
// *******************************************************************

//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 9/1/96

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   Num_charts = 0;
   Max_charts = Max_num_charts;
   Chart_objects = new TWindow*[Max_charts];
   Header_strings = NULL;
	}

// *******************************************************************
       Chart_printout::~Chart_printout (void)  {
// *******************************************************************

//  Short description:
//    destructor

//  Notes:

//  Changes:
//    DPH 9/1/96

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

	}

// *******************************************************************
       void Chart_printout::GetDialogInfo(int& minPage, int& maxPage,
                                          int& selFromPage, int& selToPage)  {
// *******************************************************************

//  Short description:
//    Return page info to caller.

//  Notes:

//  Changes:
//    DPH 9/1/96

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   minPage = 1;
   maxPage = Num_pages();
   selFromPage = minPage;
   selToPage = maxPage;
   }

// *******************************************************************
       void Chart_printout::Adjust_print_area(TRect& Print_area,
                                              int Chart_position)  {
// *******************************************************************

//  Short description:
//    Adjust the print area for the specified chart position (starts
//    at position 1)

//  Notes:

//  Changes:
//    DPH 9/1/96

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   // Adjust the x coordinates.

   if (Charts_per_page > 1)
      {
      int Half_width = Print_area.right / 2;
      if (Chart_position == 1 || Chart_position == 3 ||
          Chart_position == 5 || Chart_position == 7)
         {
         Print_area.right = Half_width;
         }
      else
         {
         Print_area.left = Half_width;
         }
      }

   // Adjust the y coordinates.

   if (Charts_per_page >= 4)
      {
      int Row_height = Print_area.bottom / (Charts_per_page / 2);
      int Row_number = (Chart_position - 1) / 2 + 1;

      if (Row_number == 1)
         // Top row

         Print_area.bottom = Row_height;

      else if (Row_number == Charts_per_page / 2)
         // Bottom row

         Print_area.top = Print_area.bottom - Row_height;

      else
         {
         // Middle rows.

         Print_area.top = Print_area.top + (Row_number - 1) * Row_height;
         Print_area.bottom = Print_area.top + Row_height;
         }
      }
   }

// *******************************************************************
       void Chart_printout::PrintPage(int Page, TRect& , unsigned )  {
// *******************************************************************

//  Short description:
//    Print a page of charts on the printer

//  Notes:

//  Changes:
//    DPH 9/1/96

//  Calls:

//  Internal variables
      TSize OldVExt, OldWExt;
      int Chart_number;                // Current chart number

// -------------------- Executable code section ----------------------

   int Start_chart = First_chart + (Page - 1) * Charts_per_page - 1;
   int End_chart = min(Start_chart + Charts_per_page, Num_charts) - 1;

   for (Chart_number = Start_chart;
        Chart_number <= End_chart;
        Chart_number++)
      {

      // Print the headers.

      int Text_height = DC->GetTextExtent("X", 1).cy;
      int Num_headers = 0;
      if (Header_strings != NULL)
         {
         Print_headers(Text_height);
         Num_headers = Header_strings->GetItemsInContainer();
         }

      // Adjust the printing area for the current chart.

      TRect R;
      R.left = 1;
      R.top = 1 + Num_headers * Text_height;
      R.right = PageSize.cx;
      R.bottom = PageSize.cy - 10;        // Take 10 pixels off bottom so it will

      Adjust_print_area(R, Chart_number - Start_chart + 1);

      // Call the chart to print itself.

      Chart_window* Chart_window_ptr = TYPESAFE_DOWNCAST (Chart_objects[Chart_number],
                                                          Chart_window);
      if (Chart_window_ptr != NULL)
         Chart_window_ptr->Chart.Set_dirty();

      Chart_objects[Chart_number]->Paint(*DC, false, R);

      if (Chart_window_ptr != NULL)
         Chart_window_ptr->Chart.Set_dirty();
      }
	}

// *******************************************************************
       void Chart_printout::Print_headers(int Text_height)  {
// *******************************************************************

//  Short description:
//    Print header strings.

//  Notes:

//  Changes:
//    DPH 9/1/96

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   for (int String_number = 0;
        String_number < Header_strings->GetItemsInContainer();
        String_number++)
      {
      DC->TextOut (1, 1 + String_number * Text_height,
                   (*Header_strings)[String_number].c_str());
      }
   }

// *******************************************************************
       void Chart_printout::Add_chart(TWindow *Chart_window_ptr)  {
// *******************************************************************

//  Short description:
//    Add specified chart to chart window list.

//  Notes:

//  Changes:
//    DPH 9/1/96

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   if (Num_charts == Max_charts)
      {
      ::MessageBox(*Chart_window_ptr,
                   "Too many charts to print.",
                   "Error",
                   MB_ICONSTOP | MB_OK);
      }
   else
      {
      Chart_objects[Num_charts] = Chart_window_ptr;
      Num_charts++;
      }
	}

