#include <graph\global.h>
#pragma hdrstop
#include <graph\dobject.h>

IMPLEMENT_CASTABLE1 (DLine, Drawable_object);
IMPLEMENT_STREAMABLE1 (DLine, Drawable_object);

// *******************************************************************
      void DLine::Streamer::Write(opstream& os) const {
// *******************************************************************

//  Short description:
//    Writes an instance of DLine to the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   WriteBaseObject( (Drawable_object*) GetObject(), os);

   os << GetObject()->Point1;
   os << GetObject()->Point2;
   }

// *******************************************************************
      void *DLine::Streamer::Read(ipstream& is, uint32 /*version*/) const {
// *******************************************************************

//  Short description:
//    Reads an instance of DLine from the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   ReadBaseObject ( (Drawable_object*) GetObject(), is);

   is >> GetObject()->Point1;
   is >> GetObject()->Point2;

   return GetObject();
   }

// *******************************************************************
       void DLine::Paint_object(GDC& DC)  {
// *******************************************************************

//  Short description:
//    Draw the line on the specified device context.

//  Notes:

//  Changes:
//    DPH 10/4/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   DC.Line(Point1, Point2);
   }

IMPLEMENT_CASTABLE1 (DRectangle, Drawable_object);
IMPLEMENT_STREAMABLE1 (DRectangle, Drawable_object);

// *******************************************************************
      void DRectangle::Streamer::Write(opstream& os) const {
// *******************************************************************

//  Short description:
//    Writes an instance of DRectangle to the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   WriteBaseObject( (Drawable_object*) GetObject(), os);

   os << GetObject()->Point1;
   os << GetObject()->Point2;
   }

// *******************************************************************
      void *DRectangle::Streamer::Read(ipstream& is, uint32 /*version*/) const {
// *******************************************************************

//  Short description:
//    Reads an instance of DRectangle from the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   ReadBaseObject ( (Drawable_object*) GetObject(), is);

   is >> GetObject()->Point1;
   is >> GetObject()->Point2;

   return GetObject();
   }

// *******************************************************************
       DRectangle::DRectangle(void)  {
// *******************************************************************

//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   Is_movable = TRUE;
   Is_sizable = TRUE;
   }

// *******************************************************************
       void DRectangle::Paint_object(GDC& DC)  {
// *******************************************************************

//  Short description:
//    Draw the rectangle on the specified device context.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   DC.Rectangle(Point1, Point2);
   }
#ifdef INTERACTIVE

// *******************************************************************
       void DRectangle::Get_bounding_rectangle(GDC& DC, TRect& R)  {
// *******************************************************************

//  Short description:
//    return bounding rectangle to caller.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   DC.Set_coord(Coord);
   Point1.Get_pixel(DC, R.left, R.top);
   Point2.Get_pixel(DC, R.right, R.bottom);
   }

// *******************************************************************
       void DRectangle::Set_bounding_rectangle(GDC& DC, TRect& R)  {
// *******************************************************************

//  Short description:
//    set bounding rectangle

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   DC.Set_coord(Coord);

   Point1.Set_pixel(R.left, R.top);
   Point2.Set_pixel(R.right, R.bottom);

   DC.Pixel_to_world(Point1);
   DC.Pixel_to_world(Point2);
   }
#endif
IMPLEMENT_CASTABLE1 (DText, Drawable_object);
IMPLEMENT_STREAMABLE1 (DText, Drawable_object);

// *******************************************************************
      void DText::Streamer::Write(opstream& os) const {
// *******************************************************************

//  Short description:
//    Writes an instance of DText to the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   WriteBaseObject( (Drawable_object*) GetObject(), os);

   os << GetObject()->Draw_box;
   os << GetObject()->Column_width;
   os << GetObject()->Row_height;
   os << GetObject()->Num_of_rows;
   os << GetObject()->Num_of_columns;
   os << GetObject()->Start_point;
   os << GetObject()->End_point;
   os << GetObject()->Num_lines;
   os << GetObject()->Line_height;
   os << GetObject()->X_alignment;
   os << GetObject()->Y_alignment;
   os << GetObject()->X_margin;
   os << GetObject()->Num_rows;
   os << GetObject()->Num_columns;
   os << GetObject()->Horiz_pos;
   os << GetObject()->Vert_pos;
   os << GetObject()->Tie_point;
   os << GetObject()->Tie_x_align;
   os << GetObject()->Tie_y_align;
   os << GetObject()->Top_left;
   os << GetObject()->Bottom_right;
   os << GetObject()->Text;
   os << GetObject()->Font;
   }

// *******************************************************************
      void *DText::Streamer::Read(ipstream& is, uint32 /*version*/) const {
// *******************************************************************

//  Short description:
//    Reads an instance of DText from the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   ReadBaseObject ( (Drawable_object*) GetObject(), is);

   is >> GetObject()->Draw_box;
   is >> GetObject()->Column_width;
   is >> GetObject()->Row_height;
   is >> GetObject()->Num_of_rows;
   is >> GetObject()->Num_of_columns;
   is >> GetObject()->Start_point;
   is >> GetObject()->End_point;
   is >> GetObject()->Num_lines;
   is >> GetObject()->Line_height;
   is >> GetObject()->X_alignment;
   is >> GetObject()->Y_alignment;
   is >> GetObject()->X_margin;
   is >> GetObject()->Num_rows;
   is >> GetObject()->Num_columns;
   is >> (int) GetObject()->Horiz_pos;
   is >> (int) GetObject()->Vert_pos;
   is >> GetObject()->Tie_point;
   is >> GetObject()->Tie_x_align;
   is >> GetObject()->Tie_y_align;
   is >> GetObject()->Top_left;
   is >> GetObject()->Bottom_right;
   is >> GetObject()->Text;
   is >> GetObject()->Font;

   return GetObject();
   }

// *******************************************************************
       DText::DText(void)  {
// *******************************************************************

//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Is_movable = TRUE;
   Is_sizable = TRUE;

   X_alignment = TA_LEFT;
   Y_alignment = TA_TOP;
   Vert_pos = top;
   Horiz_pos = right;
   Num_rows = 0;
   Num_columns = 0;
   X_margin = 0;
   Line_height = 1.0;
   Draw_box = FALSE;
   }

// *******************************************************************
       void DText::Calc_tie_point(GPoint& Tie_point,
                                  int& Tie_x_align,
                                  int& Tie_y_align)  {
// *******************************************************************

//  Short description:
//    Calculate the tie point and tie alignment based on horiz_pos and
//    vert_pos.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      int x1, y1;                      // x and y coordinates for tie point

// -------------------- Executable code section ----------------------

   switch (Horiz_pos)
      {
      case left   : x1 = 0;
                    Tie_x_align = TA_LEFT;
                    break;
      case middle : x1 = 5000;
                    Tie_x_align = TA_CENTER;
                    break;
      case right  : x1 = 10000;
                    Tie_x_align = TA_RIGHT;
                    break;
      }

   switch (Vert_pos)
      {
      case top    : y1 = 0;
                    Tie_y_align = TA_TOP;
                    break;
      case middle : y1 = 5000;
                    Tie_y_align = TA_CENTER;
                    break;
      case bottom : y1 = 10000;
                    Tie_y_align = TA_BOTTOM;
                    break;
      }
   Tie_point.Set_world(x1, y1);
   }

// *******************************************************************
       void DText::Find_line_stats(GDC& DC,
                                   GString& Text,
                                   int& Num_lines,
                                   int& Num_columns,
                                   int& Longest_line)  {
// *******************************************************************

//  Short description:
//    Calculate the box coordinates from the tie point and alignment.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      size_t Start_pos = 0;            // starting position of current line.
      size_t End_pos;                  // ending position of current line.
      GString Line;                    // current line.

// -------------------- Executable code section ----------------------

   Num_lines = 0;
   Num_columns = 0;
   Longest_line = 0;

   // Firstly we need to work out our extents for our box.
   // Go through each line of text and look for longest line (in pixels)

   do
      {
      // get the current string to write.

      End_pos = Text.find_first_of("\n\t", Start_pos);
      if (End_pos == NPOS)
         End_pos = Text.length();
      Line = Text.substr(Start_pos, End_pos - Start_pos);

      // Get line length

      TSize Size = DC.GetTextExtent(Line.c_str(), Line.length());
      Longest_line = max(Longest_line, (int)Size.cx);

      // interpret the delimiter as either a \n or \c

      if (End_pos < Text.length())
         {
         if (Text[End_pos] == '\n')
            Num_lines++;

         else
            Num_columns++;
         }
      Start_pos = End_pos + 1;
      }
   while (End_pos != Text.length());

   if (Line.length() > 0)
      Num_lines++;

   Num_columns++;
   }

// *******************************************************************
       void DText::Add_pixel(GPoint& Point,
                             int x,
                             int y)  {
// *******************************************************************

//  Short description:
//    Add pixel amounts to the specified point in the x and y directions.
//    This routine takes into account the orientation of the text.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   int Angle = Font.Get_angle();

   switch (Angle)
      {
      case   0 : Point.Add_pixel(x, y);   break;
      case  90 : Point.Add_pixel(y, -x);  break;
      case 180 : Point.Add_pixel(-x ,-y); break;
      case 270 : Point.Add_pixel(-y, x);  break;
      }
   }

// *******************************************************************
       void DText::Add_mm(GPoint& Point,
                          int x,
                          int y)  {
// *******************************************************************

//  Short description:
//    Add mm amounts to the specified point in the x and y directions.
//    This routine takes into account the orientation of the text.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   int Angle = Font.Get_angle();

   switch (Angle)
      {
      case   0 : Point.Add_mm(x, y);   break;
      case  90 : Point.Add_mm(y, -x);  break;
      case 180 : Point.Add_mm(-x ,-y); break;
      case 270 : Point.Add_mm(-y, x);  break;
      }
   }

// *******************************************************************
       void DText::Calc_box_coords(GDC& DC,
                                   GPoint& Tie_point,
                                   int Tie_x_align,
                                   int Tie_y_align,
                                   GString& Text)  {
// *******************************************************************

//  Short description:
//    Calculate the box coordinates from the tie point and alignment.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      int Longest_line = 0;            // Longest line length (pixels)
      long x1,y1;                       // World coordinates of top left corner of box.

// -------------------- Executable code section ----------------------

   Find_line_stats(DC,
                   Text,
                   Num_of_rows,
                   Num_of_columns,
                   Longest_line);
   Num_lines = Num_of_rows;

   // Calculate number of rows and columns to use based on what user has
   // asked for and what is in the text string.

   if (Num_rows > 0 && Num_columns > 0)
      {
      // user has specified num rows and columns.
      Num_of_rows = Num_rows;
      Num_of_columns = Num_columns;
      }
   else if (Num_rows == 0 && Num_columns == 0)
      {
      // User has specified nothing.
      // Check to see if we are tied to middle.  If so then have only 5 rows.
      // by as many columns as we need.

      if (Horiz_pos == middle)
         {
         Num_of_columns = (Num_of_rows-1) / 5 + 1;
         Num_of_rows = min(Num_of_rows, 5);
         }
      else
         {
         Num_of_columns = 1;
         Num_of_rows = Num_of_rows;
         }
      }
   else if (Num_rows == 0)
      {
      // User has specified number of columns

      Num_of_columns = Num_columns;
      Num_of_rows = Num_of_rows - Num_of_rows / Num_of_columns;
      }
   else if (Num_columns == 0)
      {
      // User has specified number of rows.

      Num_of_columns = (Num_of_rows-1) / Num_rows + 1;
      Num_of_rows = Num_rows;
      }

   // We need the dimensions of a text character.

   TSize Size = DC.GetTextExtent("X", 1);

   // Calculate the width of a column

   GPoint Margin;
   Margin.Set_mm(X_margin, 0);
   Margin.Get_pixel(DC, x1, y1);
   Column_width = (2 * Size.cx) + x1 + Longest_line;

   // Calculate height of a row

   Row_height = Size.cy * Line_height;

   // We need to calculate a total size of box.

   int cx = Column_width * Num_of_columns;
   int cy = Row_height * Num_of_rows;

   // Get the starting and ending x coordinate depending on value of Horiz_pos.

   Start_point = Tie_point;
   switch (Tie_x_align)
      {
      case TA_LEFT   : break;
      case TA_CENTER : Add_pixel(Start_point, -(cx / 2), 0);  break;
      case TA_RIGHT  : Add_pixel(Start_point, -cx, 0);        break;
      }

   switch (Tie_y_align)
      {
      case TA_TOP    : break;
      case TA_CENTER : Add_pixel(Start_point, 0, -(cy/2));    break;
      case TA_BOTTOM : Add_pixel(Start_point, 0, -cy);        break;
      }

   // Ok we now have the top left corner of box.  Simply return the 2 point
   // structures for top left and bottom right corner of box.

   End_point = Start_point;
   Add_pixel(End_point, cx, cy);
   }

// *******************************************************************
       void DText::Modify_text(GString& Text)  {
// *******************************************************************

//  Short description:
//    Modify the text string passed in.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      size_t Start_pos = 0;            // starting position of current line.
      size_t End_pos;                  // ending position of current line.
      int Num_lines = 1;               // number of lines so far.
//      int Num_columns = 1;             // number of columns so far.

// -------------------- Executable code section ----------------------

   do
      {
      // get the current string to write.

      End_pos = Text.find_first_of("\n\t", Start_pos);
      if (End_pos == NPOS)
         End_pos = Text.length();

      // interpret the delimiter as either a \n or \t

      if (End_pos < Text.length())
         {
         if (Text[End_pos] == '\n')
            Num_lines++;

         else
            {
//            Num_columns++;
            Num_lines = 1;
            }
         }

      // If the number of lines > Num_of_rows then we need to insert a '\c'
      // (new column) character.

      if (Num_of_rows > 0 && Num_lines > Num_of_rows)
         {
         if (End_pos < Text.length())
            {
            if (Text[End_pos] == '\n')
               {
               Text[End_pos] = '\t';
               Num_lines = 1;
               }
            }
         }
      Start_pos = End_pos + 1;
      }
   while (End_pos != Text.length());
   }

// *******************************************************************
       void DText::Goto_row_col(GDC& DC, int Row, int Col, GPoint& Point)  {
// *******************************************************************

//  Short description:
//    Goto specified row and column

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      int X_align;                     // Current x alignment
      int Y_align;                     // Current y alignment

// -------------------- Executable code section ----------------------

   // We need the dimensions of a text character.

   TSize Size = DC.GetTextExtent("X", 1);

   Point = Start_point;
   Add_pixel(Point, (Col-1) * Column_width, 0);
   Add_pixel(Point, 0, (Row-1) * Row_height);

   DC.Get_text_align(X_align, Y_align);
   switch (X_align)
      {
      case TA_LEFT   : Add_mm(Point, X_margin, 0);
                       Add_pixel(Point, Size.cx, 0);          break;
      case TA_CENTER : Add_pixel(Point, Column_width / 2, 0); break;
      case TA_RIGHT  : Add_pixel(Point, Column_width, 0);     break;
      }
   switch (Y_align)
      {
      case TA_CENTER : Add_pixel(Point, 0, Row_height / 2);   break;
      case TA_BOTTOM : Add_pixel(Point, 0, Row_height);       break;
      }
   }

// *******************************************************************
       void DText::Paint_object(GDC& DC)  {
// *******************************************************************

//  Short description:
//    Draw the text on the specified device context.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      GString Modified_text = Text;    // Our modified version of text.
      GPoint Current_point;            // current drawing point
      GString Current_line;            // Current line we're outputting
      size_t Start_pos = 0;            // starting position of current line.
      size_t End_pos;                  // ending position of current line.
      int Current_row = 1;             // current row for output
      int Current_col = 1;             // current column for output

// -------------------- Executable code section ----------------------

   // Select the current font.

   DC.Select_object(Font);

   // calculate the coordinates for our box if the user has specified
   // a logical position.

   if (Horiz_pos != none)
      {
      Calc_tie_point(Tie_point, Tie_x_align, Tie_y_align);
      Calc_box_coords(DC, Tie_point, Tie_x_align, Tie_y_align,
                      Modified_text);
      }
   else if (Tie_x_align != -1)
      {
      Calc_box_coords(DC, Tie_point, Tie_x_align, Tie_y_align,
                      Modified_text);
      }
   else
      {
      Start_point = Top_left;
      End_point = Bottom_right;
      }

   // Go modify the text string to fit in our box.

   Modify_text(Modified_text);

   // Draw rectangle around our box.

   if (Draw_box)
      DC.Rectangle(Start_point, End_point);

   // set the column text alignment

   DC.Set_text_align(X_alignment, Y_alignment);

   // Write out the text string honouring the carriage returns

   do
      {
      // Goto current row and column

      Goto_row_col(DC, Current_row, Current_col, Current_point);

      // get the current string to write.

      End_pos = Modified_text.find_first_of("\n\t", Start_pos);
      if (End_pos == NPOS)
         End_pos = Modified_text.length();
      Current_line = Modified_text.substr(Start_pos, End_pos - Start_pos);

      // output line to device context.

      if (Current_line.length() > 0)
         DC.Text_out(Current_point, Current_line);

      // interpret the delimiter as either a \n or \c

      if (End_pos < Modified_text.length())
         {
         if (Modified_text[End_pos] == '\n')
            Current_row++;

         else
            {
            Current_col++;
            Current_row = 1;
            }
         }
      Start_pos = End_pos + 1;
      }
   while (End_pos != Modified_text.length());
   }

#ifdef INTERACTIVE
// *******************************************************************
       void DText::Get_bounding_rectangle(GDC& DC, TRect& R)  {
// *******************************************************************

//  Short description:
//    return bounding rectangle to caller.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   DC.Set_coord(Coord);
   Start_point.Get_pixel(DC, R.left, R.top);
   End_point.Get_pixel(DC, R.right, R.bottom);
   }

// *******************************************************************
       void DText::Set_bounding_rectangle(GDC& DC, TRect& R)  {
// *******************************************************************

//  Short description:
//    set bounding rectangle

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      GPoint Point1, Point2;           // new points

// -------------------- Executable code section ----------------------

   DC.Set_coord(Coord);
   Point1.Set_pixel((R.left + R.right) / 2, (R.top + R.bottom) / 2);

   DC.Pixel_to_world(Point1);
   Set_position(Point1, TA_CENTER, TA_CENTER);
   }
#endif
