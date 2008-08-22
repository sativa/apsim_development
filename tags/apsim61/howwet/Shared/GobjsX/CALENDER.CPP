#include <owl\owlpch.h>
#pragma hdrstop

#include <consts.h>
#include <gobjsx\calender.h>
#include <stdlib.h>
#include <stdio.h>

const char *Month_str[12] =
   {"Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};
TPoint Points[4];

// *******************************************************************
       Calender::Calender(void)  {
// *******************************************************************

//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 13/3/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   strcpy(Font_name, "");
   Font_height = 10;
   Have_totals = FALSE;
   Have_averages = FALSE;
   Column1_width_char = 5;
   }

// *******************************************************************
       Calender::~Calender(void)  {
// *******************************************************************

//  Short description:
//    destructor.

//  Notes:

//  Changes:
//    DPH 13/3/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   }

// *******************************************************************
       void Calender::Setup(GDate& Start_d,
                            GDate& End_d,
                            String_array* Data_array,
                            bool Have_tots,
                            bool Have_avgs)  {
// *******************************************************************

//  Short description:
//    Setup the calender object

//  Notes:

//  Changes:
//    DPH 13/3/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   Start_date = Start_d;
   End_date = End_d;
   Data = Data_array;
   Have_totals = Have_tots;
   Have_averages = Have_avgs;
   }

// *******************************************************************
       TRect Get_cell_position (TRect& Draw_rectangle,
                                int Column_number,
                                int Row_number,
                                int Column_width,
                                int Row_height,
                                int Column1_width)  {
// *******************************************************************

//  Short description:
//       Calculate where in the calender the current date is.

//  Notes:

//  Changes:
//    DPH 14/3/95

//  Calls:

//  Internal variables
      TRect Return_rect;               // return rectangle

// -------------------- Executable code section ----------------------

   if (Column_number == 1)
      {
      Return_rect.left = 1;
      Return_rect.right = Return_rect.left + Column1_width - 1;
      }
   else
      {
      Return_rect.left = (Column_number - 2) * (Column_width + 1) + 1 + Column1_width;
      Return_rect.right = Return_rect.left + Column_width - 1;
      }

   // Calculate row position and store.

   Return_rect.top = (Row_number - 1) * (Row_height + 1) + 1;
   Return_rect.bottom = Return_rect.top + Row_height - 1;

   // Add Draw_rectangle to return_rectangle

   Return_rect.left += Draw_rectangle.left;
   Return_rect.right += Draw_rectangle.left;
   Return_rect.top += Draw_rectangle.top;
   Return_rect.bottom += Draw_rectangle.top;

   return Return_rect;
   }

// *******************************************************************
       TRect Calender::Get_cell_position (TRect& Draw_rectangle,
                                          GDate& Start_date,
                                          int Column_width,
                                          int Row_height,
                                          GDate& Current_date)  {
// *******************************************************************

//  Short description:
//       Calculate where in the calender the current date is.

//  Notes:

//  Changes:
//    DPH 14/3/95

//  Calls:

//  Internal variables
      TRect Return_rect;               // return rectangle

// -------------------- Executable code section ----------------------

   // Calculate column and row number

   int Month_diff = Num_months_between(Start_date, Current_date) + 1;
   int Day_diff = Current_date.Get_day();

   return ::Get_cell_position(Draw_rectangle, Month_diff + 1, Day_diff + 1,
                              Column_width, Row_height, Column1_width);
   }

// *******************************************************************
       void Draw_text_in_cell(TDC& dc,
                              TRect& Cell_rectangle,
                              const char *Text)  {
// *******************************************************************

//  Short description:
//    Draw the specified text into the specified cell.

//  Notes:

//  Changes:
//    DPH 20/3/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   // Get the size of the text we're dealing with.

   TSize Text_size = dc.GetTextExtent("X", 1);

   // Get the cell size

   int Cell_height = Cell_rectangle.bottom - Cell_rectangle.top;

  // Set the text alignment to bottom and right

   dc.SetTextAlign(TA_RIGHT | TA_BASELINE);

   // if the text size is bigger than the cell then simply draw the text
   // on the bottom of the cell boundary.  Otherwise try and center the text
   // in the cell.

   int x = Cell_rectangle.right - Text_size.cx / 2;
   int y = Cell_rectangle.bottom;
   if (Cell_height > Text_size.cy)
      y -= (Cell_height - Text_size.cy) / 2;

   dc.TextOut(x, y, Text);
   }

// *******************************************************************
       void Draw_borders(TDC& dc,
                         TRect& Draw_rectangle,
                         int /*Column_width*/,
                         int Row_height,
                         int Column1_width)  {
// *******************************************************************

//  Short description:
//    Shade the borders of the calender.

//  Notes:

//  Changes:
//    DPH 20/3/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   // Select a white brush

   dc.SelectObject((HBRUSH)GetStockObject(WHITE_BRUSH));

   // Fill in the calender in white.

   Points[0].x = Draw_rectangle.left;
   Points[0].y = Draw_rectangle.top;
   Points[1].x = Draw_rectangle.right;
   Points[1].y = Draw_rectangle.top;
   Points[2].x = Draw_rectangle.right;
   Points[2].y = Draw_rectangle.bottom;
   Points[3].x = Draw_rectangle.left;
   Points[3].y = Draw_rectangle.bottom;
   dc.Polygon(Points, 4);

   // Select a grey brush

   TBrush Gray_brush(RGB(192, 192, 192));
   dc.SelectObject(Gray_brush);

   // shade the column headings.

   Points[0].x = Draw_rectangle.left;
   Points[0].y = Draw_rectangle.top;
   Points[1].x = Draw_rectangle.right;
   Points[1].y = Draw_rectangle.top;
   Points[2].x = Draw_rectangle.right;
   Points[2].y = Draw_rectangle.top + Row_height;
   Points[3].x = Draw_rectangle.left;
   Points[3].y = Draw_rectangle.top + Row_height;
   dc.Polygon(Points, 4);

   // shade the row headings.

   Points[0].x = Draw_rectangle.left;
   Points[0].y = Draw_rectangle.top;
   Points[1].x = Draw_rectangle.left + Column1_width;
   Points[1].y = Draw_rectangle.top;
   Points[2].x = Draw_rectangle.left + Column1_width;
   Points[2].y = Draw_rectangle.bottom;
   Points[3].x = Draw_rectangle.left;
   Points[3].y = Draw_rectangle.bottom;
   dc.Polygon(Points, 4);

   // Select a white brush

   dc.SelectObject((HBRUSH)GetStockObject(WHITE_BRUSH));
   }

// *******************************************************************
       void Calender::Set_font(char *Font_n,
                               int Font_h)  {
// *******************************************************************

//  Short description:
//    Setup the font name and height (points)

//  Notes:

//  Changes:
//    DPH 21/3/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   strcpy(Font_name, Font_n);
   Font_height = Font_h;
   }

// *******************************************************************
       int Points_to_pixels(TDC& dc, int Points)  {
// *******************************************************************

//  Short description:
//    Return the number of pixels for the given number of points

//  Notes:

//  Changes:
//    DPH 21/3/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   int cxInch = dc.GetDeviceCaps(LOGPIXELSX);
   return (cxInch / 72.0 * Points) * 1.0 + 1;
   }

// *******************************************************************
       void Calender::Draw(TDC &dc, TRect Draw_rectangle)  {
// *******************************************************************

//  Short description:
//    Draw the calender

//  Notes:

//  Changes:
//    DPH 13/3/95

//  Calls:

//  Internal variables
      int Column_width;                // Width of a column (minus lines)
      TRect Cell_rectangle;            // Cell rectangle
      int Indx;                        // Loop index.
      int Num_columns;                 // Number of columns in calender
      int Num_rows;                    // Number of rows in calender
      TPoint Point1, Point2;           // point objects for drawing lines
      int Row_height;                  // Height of a row (minus lines)
      GString Row_heading;             // Heading for row number
      LOGFONT Font_struct;             // Font structure

// -------------------- Executable code section ----------------------

   // Setup the font to use

   memset(&Font_struct, 0, sizeof(Font_struct));
   Font_struct.lfHeight = Points_to_pixels(dc, Font_height);
   strcpy(Font_struct.lfFaceName, Font_name);
   TFont *Our_font = new TFont(&Font_struct);
   dc.SelectObject(*Our_font);
   dc.SetBkMode(TRANSPARENT);
   dc.SetBkColor(RGB(255, 255, 255));

   // Reduce drawing region to allow a margin around the calender.

   TSize Text_size = dc.GetTextExtent("X", 1);
   Draw_rectangle.left += Text_size.cx;
   Draw_rectangle.top += Text_size.cx;
   Draw_rectangle.right -= Text_size.cx;
   Draw_rectangle.bottom -= Text_size.cx;

   Column1_width = Text_size.cx * Column1_width_char;

   // Calculate number of columns and number of rows in calender.

   Num_columns = Num_months_between(Start_date, End_date) + 2;
   Num_rows = 31 + 1;
   if (Have_totals)
      Num_rows++;
   if (Have_averages)
      Num_rows++;
   if (Have_totals || Have_averages)
      Num_rows++;

   // Calculate column and row width.

   Column_width = (Draw_rectangle.right - Draw_rectangle.left -
                   Num_columns - 1 - Column1_width) * 1.0
                  / (Num_columns - 1);
   Row_height = (Draw_rectangle.bottom - Draw_rectangle.top - Num_rows - 1) * 1.0
                / Num_rows;

   // Draw borders around calender.

   Draw_borders(dc, Draw_rectangle, Column_width, Row_height, Column1_width);

   // Draw all vertical lines.

   Point1.x = Draw_rectangle.left + Column1_width;
   Point1.y = Draw_rectangle.top;
   Point2.x = Draw_rectangle.left + Column1_width;
   Point2.y = Draw_rectangle.bottom;
   for (Indx = 2;
        Indx <= Num_columns;
        Indx++)
      {
      Point1.Offset(Column_width + 1, 0);
      Point2.Offset(Column_width + 1, 0);
      dc.MoveTo(Point1);
      dc.LineTo(Point2);
      }

   // Draw all horizontal lines.

   Point1.x = Draw_rectangle.left;
   Point1.y = Draw_rectangle.top;
   Point2.x = Draw_rectangle.right;
   Point2.y = Draw_rectangle.top;
   for (Indx = 1;
        Indx <= Num_rows;
        Indx++)
      {
      Point1.Offset(0, Row_height + 1);
      Point2.Offset(0, Row_height + 1);
      dc.MoveTo(Point1);
      dc.LineTo(Point2);
      }

   dc.SelectObject((HBRUSH) GetStockObject(WHITE_BRUSH));

   // Draw in all data values

   GDate Current_date = Start_date;
   Indx = 0;
   while (Current_date < End_date)
      {
      // Calculate where in the calender the current date is.

      Cell_rectangle = Get_cell_position(Draw_rectangle, Start_date, Column_width, Row_height, Current_date);

      // Output the text to the current cell

      Draw_text_in_cell(dc, Cell_rectangle, (*Data)[Indx].c_str());

      // Goto next date

      Indx++;
      Current_date += 1;
      }

   // Draw the column headings in.

   int Month = Start_date.Get_month();
   for (Indx = 1;
        Indx < Num_columns;
        Indx++)
      {
      Cell_rectangle = ::Get_cell_position (Draw_rectangle, Indx + 1, 1, Column_width, Row_height, Column1_width);
      Draw_text_in_cell(dc, Cell_rectangle, Month_str[Month - 1]);
      Month++;
      if (Month > 12)
         Month = 1;
      }

   // Draw the row headings in.

   for (Indx = 1;
        Indx <= 31;
        Indx++)
      {
      Cell_rectangle = ::Get_cell_position (Draw_rectangle, 1, Indx + 1, Column_width, Row_height, Column1_width);
      Row_heading.Set_integer(Indx);
      Draw_text_in_cell(dc, Cell_rectangle, Row_heading.c_str());
      }

   Draw_totals_and_averages (dc, Draw_rectangle, Num_columns, Column_width, Row_height);
   }

// *******************************************************************
       void Calender::Draw_totals_and_averages(TDC& dc,
                                               TRect Draw_rectangle,
                                               int Num_columns,
                                               int Column_width,
                                               int Row_height)  {
// *******************************************************************

//  Short description:
//    Draw totals and averages into calender.

//  Notes:

//  Changes:
//    DPH 14/3/95

//  Calls:

//  Internal variables
      char St[20];                     // character string.
      float Total;                     // Total in month
      int Num_days;                    // num days in month.
      TRect Cell_rectangle;            // Cell rectangle

// -------------------- Executable code section ----------------------

   if (Have_totals || Have_averages)
      {
      if (Have_totals)
         {
         Cell_rectangle = ::Get_cell_position (Draw_rectangle, 1, 34, Column_width, Row_height, Column1_width);
         Draw_text_in_cell(dc, Cell_rectangle, "Tot:");
         }
      if (Have_averages)
         {
         Cell_rectangle = ::Get_cell_position (Draw_rectangle, 1, 35, Column_width, Row_height, Column1_width);
         Draw_text_in_cell(dc, Cell_rectangle, "Avg:");
         }

      // Loop through all months on calender.

      GDate Current_date = Start_date;
      for (int Column_number = 2;
           Column_number <= Num_columns;
           Column_number++)
         {
         // Calculate total for this month.

         Get_total_for_month(Current_date, Total, Num_days);

         if (Have_totals)
            {
            Cell_rectangle = ::Get_cell_position (Draw_rectangle, Column_number, 34, Column_width, Row_height, Column1_width);
            sprintf(St, "%5.0f", Total);
            Draw_text_in_cell(dc, Cell_rectangle, St);
            }
         if (Have_averages)
            {
            Cell_rectangle = ::Get_cell_position (Draw_rectangle, Column_number, 35, Column_width, Row_height, Column1_width);
            sprintf(St, "%5.0f", Averages[Current_date.Get_month()-1]);
            Draw_text_in_cell(dc, Cell_rectangle, St);
            }
         }
      }
   }

// *******************************************************************
       void Calender::Get_total_for_month(GDate& Current_date,
                                          float& Total,
                                          int& Num_days)  {
// *******************************************************************

//  Short description:
//    Return the total for month + number of days in month.
//    update index to point to start of next month.

//  Notes:

//  Changes:
//    DPH 14/3/95

//  Calls:

//  Internal variables
      int Indx;                        // Indx into data array

// -------------------- Executable code section ----------------------

   int This_month = Current_date.Get_month();
   Total = 0.0;
   Num_days = 0;

   while (Current_date.Get_month() == This_month &&
          Current_date <= End_date)
      {
      Indx = (int) (Current_date - Start_date);
      Total += (*Data)[Indx].Get_real();
      Current_date++;
      Num_days++;
      }
   }

// *******************************************************************
       int Calender::Num_months_between(GDate &Date1, GDate &Date2)  {
// *******************************************************************

//  Short description:
//    Calculate number of months between the 2 specified dates.

//  Notes:

//  Changes:
//    DPH 14/3/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   int Month1 = Date1.Get_month();
   int Month2 = Date2.Get_month();
   int Year1 = Date1.Get_year();
   int Year2 = Date2.Get_year();

   if (Year1 != Year2)
      return (Year2 - Year1 - 1) * 12 + (12 - Month1) + Month2;

   return (Month2 - Month1);
   }


