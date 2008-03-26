// *******************************************************************
//     file ggdi.cpp
// *******************************************************************

//  Short description:
//      This module adds new functionality to the standard GDI classes
//      that comes with OWL 2.0.  It can also be used with the DOS
//      implementation of the TDC class.

//  Notes:

//  Changes:
//      DPH 23/5/94

// ----------------------- Declaration section -----------------------

//  Include files
#include <graph\global.h>
#pragma hdrstop
      #include <graph\gpoint.h>
      #include <math.h>          // floor

//  Constants
const int MAX_WORLD_COORD = 10000;

// *******************************************************************
      GDC::GDC(HDC Handle) : TDC(Handle) {
// *******************************************************************

//  Short description:
//      constructor

//  Notes:

//  Changes:
//      DPH 2/6/94

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   Old_pen = NULL;
   Old_brush = NULL;
   Old_font = NULL;
   Logical_minx1 = 0;
   Logical_maxx1 = 0;
   Logical_miny1 = 0;
   Logical_maxy1 = 0;
   Logical_minx2 = 0;
   Logical_maxx2 = 0;
   Logical_miny2 = 0;
   Logical_maxy2 = 0;
   Chart_area.left = 0;                  // Physical coords at full display area
   Chart_area.top = 0;
   Chart_area.right = 800;
   Chart_area.bottom = 600;
   Plot_area.left = 0.20 * 10000;
   Plot_area.top = 0.20 * 10000;
   Plot_area.right = 10000 - 0.20 * 10000;
   Plot_area.bottom = 10000 - 0.20 * 10000;
   Current_x = 0;
   Current_y = 0;
   Justify_x = 0;
   Justify_y = 0;
   }

// *******************************************************************
       GDC::~GDC(void)  {
// *******************************************************************

//  Short description:
//      Destructor

//  Notes:

//  Changes:
//      DPH 2/6/94

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   // Delete all old objects

   if (Old_pen != NULL)
      delete Old_pen;

   if (Old_brush != NULL)
      delete Old_brush;

   if (Old_font != NULL)
      delete Old_font;
   }

// *******************************************************************
      void GDC::Set_coord (Coordinate_system& Coord)  {
// *******************************************************************

//  Short description:
//    Set the physical coordinates for our chart.

//  Notes:

//  Changes:
//      DPH 2/8/95

//  Internal variables

// -------------------- Executable code section ----------------------

   if (Coord.Pixel_dimensions.left >= 0)
      {
      Chart_area = Coord.Pixel_dimensions;

      int x1 = World_X(Coord.Chart_area.left);
      int y1 = World_Y(Coord.Chart_area.top);
      int x2 = World_X(Coord.Chart_area.right);
      int y2 = World_Y(Coord.Chart_area.bottom);

      Chart_area.left = x1;
      Chart_area.top = y1;
      Chart_area.right = x2;
      Chart_area.bottom = y2;

      Plot_area.left = World_X(Coord.Plot_area.left);
      Plot_area.top = World_Y(Coord.Plot_area.top);
      Plot_area.right = World_X(Coord.Plot_area.right);
      Plot_area.bottom = World_Y(Coord.Plot_area.bottom);

//      Chart_area = Coord.Pixel_dimensions;

      // set logical coordinate system

      Logical_minx1 = Coord.Minx1;
      Logical_maxx1 = Coord.Maxx1;
      Logical_miny1 = Coord.Miny1;
      Logical_maxy1 = Coord.Maxy1;
      Logical_minx2 = Coord.Minx2;
      Logical_maxx2 = Coord.Maxx2;
      Logical_miny2 = Coord.Miny2;
      Logical_maxy2 = Coord.Maxy2;
      }
   };

// *******************************************************************
      bool GDC::Text_out
         (GPoint& Point, GString& Text)  {
// *******************************************************************

//  Short description:
//    Draw text at the specified point

//  Notes:

//  Changes:
//      DPH 23/5/94

//  Internal variables
      TSize Text_size;                  // Size of current font.
      long x,y;                           // coordinates of point

// -------------------- Executable code section ----------------------

   // Extract x and y out of point

   Point.Get_pixel(*this, x, y);

   // Get the current text width and height.

   GetTextExtent("X", 1, Text_size);

   if (Justify_y == TA_CENTER)
      {
      y -= Text_size.cy / 2;
      SetTextAlign(Justify_x | TA_TOP);
      }
   else
      {
      SetTextAlign(Justify_x | Justify_y);
//      SetTextAlign(TA_LEFT | TA_BOTTOM);
      }

   return TextOut(x, y, Text.c_str(), -1);
   };

// *******************************************************************
      bool GDC::Draw_text
         (TRect& r, GString& Text)  {
// *******************************************************************

//  Short description:
//    Draw text in the specified rectangle.

//  Notes:

//  Changes:
//      DPH 31/7/95

//  Internal variables

// -------------------- Executable code section ----------------------

   uint16 Flags = DT_WORDBREAK;
   if (Justify_x == TA_CENTER)
      Flags = Flags | (uint16)DT_CENTER;

   else if (Justify_x == TA_LEFT)
      Flags = Flags | (uint16)DT_LEFT;

   else
      Flags = Flags | (uint16)DT_RIGHT;

   SetTextAlign(TA_LEFT | TA_TOP);         // Needed for drawtext to work properly

   Flags = Flags | (uint16)DT_NOCLIP;
   return DrawText(Text.c_str(), -1, r, Flags);
   };

// *******************************************************************
       void GDC::Get_text_align(int& X_align, int& Y_align)  {
// *******************************************************************

//  Short description:
//      Return the x and y text alignment to caller.

//  Notes:

//  Changes:
//      DPH 25/5/94

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   X_align = Justify_x;
   Y_align = Justify_y;
   }

// *******************************************************************
       void GDC::Set_text_align(int X_align, int Y_align)  {
// *******************************************************************

//  Short description:
//      Set the text justification.

//  Notes:

//  Changes:
//      DPH 25/5/94

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   Justify_x = X_align;
   Justify_y = Y_align;

   // If caller has specified TA_CENTER for the y justification then
   // modify it to be TA_BOTTOM.

   if (Y_align == TA_CENTER)
      Y_align = TA_BOTTOM;

   SetTextAlign(X_align | Y_align);
   }

// *******************************************************************
       void GDC::Select_object(GPen& Pen_object)  {
// *******************************************************************

//  Short description:
//      Select the specified pen into the dc

//  Notes:

//  Changes:
//      DPH 26/5/94

//  Internal variables

// -------------------- Executable code section ----------------------

   // Create new pen

   LOGPEN Log_pen;
   Pen_object.Fill_LOGPEN(Log_pen);

   Current_pen_width = Log_pen.lopnWidth.x;
//   Log_pen.lopnWidth = 1;

   TPen *New_pen = new TPen(&Log_pen);

   // Select the new pen into the device context

   SelectObject(*New_pen);

   // Delete the old pen

   if (Old_pen != NULL)
      delete Old_pen;

   // Make the old pen point to the current pen.

   Old_pen = New_pen;

   // Change the text colour option to the current pen colour

   SetTextColor(Log_pen.lopnColor);
   SetBkMode(TRANSPARENT);
   }

// *******************************************************************
       void GDC::Select_object(GBrush& Brush_object)  {
// *******************************************************************

//  Short description:
//      Select the specified brush into the dc

//  Notes:

//  Changes:
//      DPH 26/5/94

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   // Create new brush

   LOGBRUSH Log_brush;
   Brush_object.Fill_LOGBRUSH(Log_brush);
   TBrush *New_brush = new TBrush(&Log_brush);

   // Select the new brush into the device context

   SelectObject(*New_brush);

   // Delete the old brush

   if (Old_brush != NULL)
      delete Old_brush;

   // Make the old brush point to the current brush

   Old_brush = New_brush;
   }

// *******************************************************************
       void GDC::Select_object(GFont& Font_object)  {
// *******************************************************************

//  Short description:
//      Select the specified font into the dc

//  Notes:

//  Changes:
//      DPH 26/5/94

//  Internal variables
      LOGFONT F;                       // LOGBRUSH to use.

// -------------------- Executable code section ----------------------

   // Convert the font size from points to pixels.

   F = Font_object.Font_info;
   F.lfHeight = -Point_X(F.lfHeight);
   F.lfWidth = 0;

   // Create new font

   TFont *New_font = new TFont(&F);

   // Select the new font into the device context

   SelectObject(*New_font);

   // Delete the old font

   if (Old_font != NULL)
      delete Old_font;

   // Make the old font point to the current font

   Old_font = New_font;
   }

// *******************************************************************
       int GDC::Point_X(int Point_size)  {
// *******************************************************************

//  Short description:
//    Convert a point x value into a physical pixel number.

//  Notes:

//  Changes:
//    DPH 11/4/95

// ---------------------- Declaration section ------------------------

//  Internal constants
//    none

//  Routine arguments
//    none

//  Internal variables

// -------------------- Executable code section ----------------------

   int Logpixelsx = GetDeviceCaps(LOGPIXELSX);

   return Point_size * Logpixelsx / 72.0;
   }

// *******************************************************************
       int GDC::Log_X1(float Logical_x)  {
// *******************************************************************

//  Short description:
//    Convert a logical x1 value into a physical pixel number.

//  Notes:

//  Changes:
//    DPH 11/4/95

// ---------------------- Declaration section ------------------------

//  Internal constants
//    none

//  Routine arguments
//    none

//  Internal variables
      float Pixels_per_unit;           // Pixels per logical unit

// -------------------- Executable code section ----------------------

   // Check for MINFLOAT and MAXFLOAT

   if (Logical_x < -1e10)
      return Plot_area.left;

   else if (Logical_x > 1e10)
      return Plot_area.right;

   else if (Logical_maxx1 - Logical_minx1 == 0)
      return 0;

   else
      {
      Pixels_per_unit = (Plot_area.right - Plot_area.left) /
                        (Logical_maxx1 - Logical_minx1);

      return(Plot_area.left + (Logical_x - Logical_minx1) * Pixels_per_unit + 0.5);
      }
   }

// *******************************************************************
      int GDC::Log_Y1(float Logical_y)  {
// *******************************************************************

//  Short description:
//    Convert a logical y1 value into a physical pixel number.

//  Notes:
//    Be aware that the Phs_bottom is greater than Phs_top and
//                      Log_top    is greater than Log_bottom.
//    This is because physical pixels are zero at the top of the screen and
//    increase down the screen whereas logical coordinates are highest at the
//    top of the screen and get smaller down the screen.

//  Changes:
//    DPH 11/4/95

//  Calls:
//    none

// ---------------------- Declaration section ------------------------

//  Internal constants
//    none

//  Routine arguments
//    none

//  Internal variables
      float Pixels_per_unit;           // Pixels per logical unit

// -------------------- Executable code section ----------------------

   // Check for MINFLOAT and MAXFLOAT

   if (Logical_y < -1e10)
      return Plot_area.bottom;

   else if (Logical_y > 1e10)
      return Plot_area.top;

   else if (Logical_maxy1 - Logical_miny1 == 0)
      return 0;

   else
      {
      Pixels_per_unit = (Plot_area.top - Plot_area.bottom) /
                        (Logical_maxy1 - Logical_miny1);
      return (Plot_area.top - (Logical_maxy1 - Logical_y) * Pixels_per_unit + 0.5);
      }
   }

// *******************************************************************
       int GDC::Log_X2(float Logical_x)  {
// *******************************************************************

//  Short description:
//    Convert a logical x2 value into a physical pixel number.

//  Notes:

//  Changes:
//    DPH 11/4/95

// ---------------------- Declaration section ------------------------

//  Internal constants
//    none

//  Routine arguments
//    none

//  Internal variables
      float Pixels_per_unit;           // Pixels per logical unit

// -------------------- Executable code section ----------------------

   // Check for MINFLOAT and MAXFLOAT

   if (Logical_x < -1e10)
      return Plot_area.left;

   else if (Logical_x > 1e10)
      return Plot_area.right;

   else if (Logical_maxx2 - Logical_minx2 == 0)
      return 0;

   else
      {
      Pixels_per_unit = (Plot_area.right - Plot_area.left) /
                        (Logical_maxx2 - Logical_minx2);

      return(Plot_area.left + (Logical_x - Logical_minx2) * Pixels_per_unit + 0.5);
      }
   }

// *******************************************************************
      int GDC::Log_Y2(float Logical_y)  {
// *******************************************************************

//  Short description:
//    Convert a logical y2 value into a physical pixel number.

//  Notes:
//    Be aware that the Phs_bottom is greater than Phs_top and
//                      Log_top    is greater than Log_bottom.
//    This is because physical pixels are zero at the top of the screen and
//    increase down the screen whereas logical coordinates are highest at the
//    top of the screen and get smaller down the screen.

//  Changes:
//    DPH 11/4/95

//  Calls:
//    none

// ---------------------- Declaration section ------------------------

//  Internal constants
//    none

//  Routine arguments
//    none

//  Internal variables
      float Pixels_per_unit;           // Pixels per logical unit

// -------------------- Executable code section ----------------------

   // Check for MINFLOAT and MAXFLOAT

   if (Logical_y < -1e10)
      return Plot_area.bottom;

   else if (Logical_y > 1e10)
      return Plot_area.top;

   else if (Logical_maxy2 - Logical_miny2 == 0)
      return 0;

   else
      {
      Pixels_per_unit = (Plot_area.top - Plot_area.bottom) /
                        (Logical_maxy2 - Logical_miny2);
      return (Plot_area.top - (Logical_maxy2 - Logical_y) * Pixels_per_unit + 0.5);
      }
   }

// *******************************************************************
       int GDC::World_X(int World_coord)  {
// *******************************************************************

//  Short description:
//     Convert a world coordinate to a physical pixel in x direction.

//  Notes:

//  Changes:
//    DPH 11/4/95

//  Calls:

// ---------------------- Declaration section ------------------------

//  Internal constants
//    none

//  Routine arguments
//    none

//  Internal variables
      float Coord;

// -------------------- Executable code section ----------------------

   Coord = World_coord;

   return Chart_area.left + (Chart_area.right - Chart_area.left)* (Coord / MAX_WORLD_COORD);
   }

// *******************************************************************
       int GDC::World_Y(int World_coord)  {
// *******************************************************************

//  Short description:
//     Convert a world coordinate to a physical pixel in y direction.

//  Notes:

//  Changes:
//    DPH 11/4/95

//  Calls:

// ---------------------- Declaration section ------------------------

//  Internal constants
//    none

//  Routine arguments
//    none

//  Internal variables
      float Coord;

// -------------------- Executable code section ----------------------

   Coord = World_coord;

   return Chart_area.top + (Chart_area.bottom - Chart_area.top)* (Coord / MAX_WORLD_COORD);
   }

// *******************************************************************
       void GDC::Pixel_to_world(GPoint& Point)  {
// *******************************************************************

//  Short description:
//     Convert a pixel point into a world point.

//  Notes:

//  Changes:
//    DPH 11/4/95

//  Calls:

// ---------------------- Declaration section ------------------------

//  Internal constants
//    none

//  Routine arguments
//    none

//  Internal variables
      long x,y;                         // coordinates.

// -------------------- Executable code section ----------------------

   Point.Get_pixel(*this, x, y);
   x = x * 1.0 / (Chart_area.right - Chart_area.left) * MAX_WORLD_COORD;
   y = y * 1.0 / (Chart_area.bottom - Chart_area.top) * MAX_WORLD_COORD;
   Point.Set_world(x, y);
   }

// *******************************************************************
       int GDC::mm_X(int mm)  {
// *******************************************************************

//  Short description:
//    Converts millimeters to horizontal pixels.

//  Notes:

//  Changes:
//    DPH 11/4/95

//  Calls:

// ---------------------- Declaration section ------------------------

//  Internal constants
//    none

//  Routine arguments
//    none

//  Internal variables

// -------------------- Executable code section ----------------------

   return mm *  (GetDeviceCaps(HORZRES) / GetDeviceCaps(HORZSIZE));
   }

// *******************************************************************
       int GDC::mm_Y(int mm)  {
// *******************************************************************

//  Short description:
//    Converts millimeters to vertical pixels.

//  Notes:

//  Changes:
//    DPH 11/4/95

//  Calls:

// ---------------------- Declaration section ------------------------

//  Internal constants
//    none

//  Routine arguments
//    none

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   return mm * (GetDeviceCaps(VERTRES) / GetDeviceCaps(VERTSIZE));
   }

// *******************************************************************
       void GDC::Line(GPoint Point1, GPoint Point2)  {
// *******************************************************************

//  Short description:
//    Draw the specified line on the current device.

//  Notes:

//  Changes:
//    DPH 20/7/95

//  Calls:

// ---------------------- Declaration section ------------------------

//  Internal constants
//    none

//  Routine arguments
//    none

//  Internal variables

// -------------------- Executable code section ----------------------

   MoveTo(Point1);
   LineTo(Point2);
   }

// *******************************************************************
       void GDC::Rectangle(GPoint Point1, GPoint Point2)  {
// *******************************************************************

//  Short description:
//    Draw the specified rectangle on the current device.

//  Notes:

//  Changes:
//    DPH 20/7/95

//  Calls:

// ---------------------- Declaration section ------------------------

//  Internal constants
//    none

//  Routine arguments
//    none

//  Internal variables
      long x1, y1;                      // coordinates of point 1.
      long x2, y2;                      // coordinates of point 2.

// -------------------- Executable code section ----------------------

   Point1.Get_pixel(*this, x1, y1);
   Point2.Get_pixel(*this, x2, y2);
   TDC::Rectangle(x1, y1, x2, y2);

/*   for (int Width = 2;
        Width <= Current_pen_width;
        Width++)
      {
      x1++;
      x2++;
      y1++;
      y2++;
      TDC::Rectangle(x1, y1, x2, y2);
      }
*/   }

// *******************************************************************
       void GDC::Ellipse(GPoint Point1, GPoint Point2)  {
// *******************************************************************

//  Short description:
//    Draw the specified ellipse on the current device.

//  Notes:

//  Changes:
//    DPH 20/7/95

//  Calls:

// ---------------------- Declaration section ------------------------

//  Internal constants
//    none

//  Routine arguments
//    none

//  Internal variables
      long x1, y1;                      // coordinates of point 1.
      long x2, y2;                      // coordinates of point 2.

// -------------------- Executable code section ----------------------

   Point1.Get_pixel(*this, x1, y1);
   Point2.Get_pixel(*this, x2, y2);
   TDC::Ellipse(x1, y1, x2, y2);
   }

// *******************************************************************
       void Increment_point_array(TPoint* p, int Num_points)  {
// *******************************************************************

//  Short description:
//    Increment all coordinates in points array

//  Notes:

//  Changes:
//    DPH 20/7/95

//  Calls:

// ---------------------- Declaration section ------------------------

//  Internal constants
//    none

//  Routine arguments
//    none

//  Internal variables

// -------------------- Executable code section ----------------------

   for (int Point_number = 0;
        Point_number < Num_points;
        Point_number++)
      {
      p[Point_number].x++;
      p[Point_number].y++;
      }
   }

// *******************************************************************
       void GDC::Polygon(GPoint Points[], int Num_points)  {
// *******************************************************************

//  Short description:
//    Draw the specified polygon on the current device.

//  Notes:

//  Changes:
//    DPH 20/7/95

//  Calls:

// ---------------------- Declaration section ------------------------

//  Internal constants
//    none

//  Routine arguments
//    none

//  Internal variables
      TPoint *p;                        // points to pass to API polygon

// -------------------- Executable code section ----------------------

   p = new TPoint[Num_points];

   for (int Point = 0;
        Point < Num_points;
        Point++)
      {
      Points[Point].Get_pixel(*this, p[Point].x, p[Point].y);
      }

   TDC::Polygon(p, Num_points);

/*   for (int Width = 2;
        Width <= Current_pen_width;
        Width++)
      {
      Increment_point_array(p, Num_points);
      TDC::Polygon(p, Num_points);
      }
*/
   delete [] p;
   }

// *******************************************************************
       void GDC::Polyline(GPoint Points[], int Num_points)  {
// *******************************************************************

//  Short description:
//    Draw the specified polyline on the current device.

//  Notes:

//  Changes:
//    DPH 20/7/95

//  Calls:

// ---------------------- Declaration section ------------------------

//  Internal constants
//    none

//  Routine arguments
//    none

//  Internal variables
      TPoint *p;                        // points to pass to API polygon

// -------------------- Executable code section ----------------------

   p = new TPoint[Num_points];

   for (int Point = 0;
        Point < Num_points;
        Point++)
      {
      Points[Point].Get_pixel(*this, p[Point].x, p[Point].y);
      }

   TDC::Polyline(p, Num_points);

   delete [] p;
   }

// *******************************************************************
       void GDC::MoveTo(GPoint Point)  {
// *******************************************************************

//  Short description:
//    Move the current pointer to the specified point

//  Notes:

//  Changes:
//    DPH 20/7/95

//  Calls:

// ---------------------- Declaration section ------------------------

//  Internal constants
//    none

//  Routine arguments
//    none

//  Internal variables
      long x1, y1;                      // coordinates of point 1.

// -------------------- Executable code section ----------------------

   Point.Get_pixel(*this, x1, y1);
   Current_x = x1;
   Current_y = y1;

   TDC::MoveTo(x1, y1);

   }

// *******************************************************************
       void GDC::LineTo(GPoint Point)  {
// *******************************************************************

//  Short description:
//    Draw a line from the current pointer to the specified point

//  Notes:

//  Changes:
//    DPH 20/7/95

//  Calls:

// ---------------------- Declaration section ------------------------

//  Internal constants
//    none

//  Routine arguments
//    none

//  Internal variables
      long x1, y1;                      // coordinates of point 1.

// -------------------- Executable code section ----------------------

   Point.Get_pixel(*this, x1, y1);
   Current_x = x1;
   Current_y = y1;

   TDC::LineTo(x1, y1);
   }

IMPLEMENT_CASTABLE (GColour);
IMPLEMENT_STREAMABLE (GColour);

// *******************************************************************
      void GColour::Streamer::Write(opstream& os) const {
// *******************************************************************

//  Short description:
//    Writes an instance of GColour to the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   os << GetObject()->GetValue();
   }

// *******************************************************************
      void *GColour::Streamer::Read(ipstream& is, uint32 /*version*/) const {
// *******************************************************************

//  Short description:
//    Reads an instance of GColour from the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   is >> GetObject()->GetValue();

   return GetObject();
   }


// *********************************************************
// PENS, BRUSHS and FONTS
// *********************************************************

IMPLEMENT_CASTABLE1 (GPen, Interactive_object);
IMPLEMENT_STREAMABLE1 (GPen, Interactive_object);

// *******************************************************************
      void GPen::Streamer::Write(opstream& os) const {
// *******************************************************************

//  Short description:
//    Writes an instance of GPen to the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   WriteBaseObject( (Interactive_object*) GetObject(), os);

   os <<       GetObject()->Colour;
   os << (int) GetObject()->Style;
   os <<       GetObject()->Width;
   }

// *******************************************************************
      void *GPen::Streamer::Read(ipstream& is, uint32 /*version*/) const {
// *******************************************************************

//  Short description:
//    Reads an instance of GPen from the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   ReadBaseObject ( (Interactive_object*) GetObject(), is);

   is >>        GetObject()->Colour;
   is >> (int)  GetObject()->Style;
   is >>        GetObject()->Width;

   return GetObject();
   }

// *******************************************************************
       GPen::GPen(void)  {
// *******************************************************************

//  Short description:
//      Constructor

//  Notes:

//  Changes:
//      DPH 26/5/94

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   Colour = TColor(TColor::Black);
   Style = PS_Solid;
   Width = 0;
   }

// *******************************************************************
       void GPen::Fill_LOGPEN(LOGPEN& Log_pen)  {
// *******************************************************************

//  Short description:
//      Fill the logical pen structure.

//  Notes:

//  Changes:
//      DPH 26/5/94

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   switch (Style)
      {
      case PS_Solid : Log_pen.lopnStyle = PS_SOLID;
                      break;
      case PS_Null : Log_pen.lopnStyle = PS_NULL;
                     break;
      case PS_Dash : Log_pen.lopnStyle = PS_DASH;
                     break;
      case PS_Dot  : Log_pen.lopnStyle = PS_DOT;
                     break;
      case PS_Dash_dot : Log_pen.lopnStyle = PS_DASHDOT;
                        break;
      case PS_Dash_dot_dot : Log_pen.lopnStyle = PS_DASHDOTDOT;
                           break;
      }

   Log_pen.lopnWidth.x = Width;
   Log_pen.lopnColor = Colour;
   }

IMPLEMENT_CASTABLE1 (GBrush, Interactive_object);
IMPLEMENT_STREAMABLE1 (GBrush, Interactive_object);

// *******************************************************************
      void GBrush::Streamer::Write(opstream& os) const {
// *******************************************************************

//  Short description:
//    Writes an instance of GBrush to the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   WriteBaseObject( (Interactive_object*) GetObject(), os);

   os <<       GetObject()->Colour;
   os << (int) GetObject()->Style;
   }

// *******************************************************************
      void *GBrush::Streamer::Read(ipstream& is, uint32 /*version*/) const {
// *******************************************************************

//  Short description:
//    Reads an instance of GBrush from the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   ReadBaseObject ( (Interactive_object*) GetObject(), is);

   is >>        GetObject()->Colour;
   is >> (int)  GetObject()->Style;

   return GetObject();
   }

// *******************************************************************
       GBrush::GBrush(void)  {
// *******************************************************************

//  Short description:
//      Constructor

//  Notes:

//  Changes:
//      DPH 26/5/94

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   Colour = TColor(TColor::White);
   Style = BS_Null;
   }

// *******************************************************************
       void GBrush::Fill_LOGBRUSH(LOGBRUSH& Log_brush)  {
// *******************************************************************

//  Short description:
//      Fill the logical brush structure.

//  Notes:

//  Changes:
//      DPH 26/5/94

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   Log_brush.lbColor = Colour;
   Log_brush.lbStyle = BS_HATCHED;
   Log_brush.lbHatch = 0;
   switch (Style)
      {
      case BS_Solid :      Log_brush.lbStyle = BS_SOLID;
                        break;
      case BS_Null  :      Log_brush.lbStyle = BS_NULL;
                        break;
      case BS_BDiagonal :  Log_brush.lbHatch = HS_BDIAGONAL;
                        break;
      case BS_Cross :      Log_brush.lbHatch = HS_CROSS;
                        break;
      case BS_Diagcross  : Log_brush.lbHatch = HS_DIAGCROSS;
                        break;
      case BS_FDiagonal  : Log_brush.lbHatch = HS_FDIAGONAL;
                        break;
      case BS_Horizontal : Log_brush.lbHatch = HS_HORIZONTAL;
                        break;
      case BS_Vertical   : Log_brush.lbHatch = HS_VERTICAL;
                        break;
      }
   }

IMPLEMENT_CASTABLE1 (GFont, Interactive_object);
IMPLEMENT_STREAMABLE1 (GFont, Interactive_object);

// *******************************************************************
      void GFont::Streamer::Write(opstream& os) const {
// *******************************************************************

//  Short description:
//    Writes an instance of GFont to the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   WriteBaseObject( (Interactive_object*) GetObject(), os);

   os.writeBytes(&GetObject()->Font_info, sizeof(LOGFONT));
   }

// *******************************************************************
      void *GFont::Streamer::Read(ipstream& is, uint32 /*version*/) const {
// *******************************************************************

//  Short description:
//    Reads an instance of GFont from the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   ReadBaseObject ( (Interactive_object*) GetObject(), is);

   is.readBytes(&GetObject()->Font_info, sizeof(LOGFONT));

   return GetObject();
   }

// *******************************************************************
       GFont::GFont(void)  {
// *******************************************************************

//  Short description:
//      Constructor

//  Notes:

//  Changes:
//      DPH 26/5/94

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   memset(&Font_info, 0, sizeof(Font_info));

   // Select a 10 point arial font as default.

   Font_info.lfHeight = 10;
   Font_info.lfWeight = FW_NORMAL;
   Font_info.lfOutPrecision = OUT_STROKE_PRECIS;
   Font_info.lfClipPrecision = CLIP_STROKE_PRECIS;
   Font_info.lfQuality = DRAFT_QUALITY;
   Font_info.lfPitchAndFamily = FF_SWISS | VARIABLE_PITCH;
   strcpy(Font_info.lfFaceName, "Arial");
   }

