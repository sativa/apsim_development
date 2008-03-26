#include <graph\global.h>
#pragma hdrstop
#include <graph\dseries.h>
#include <assert.h>

#include <values.h>
#include <stdio.h>
#include <graph\daxis.h>
#include <gobjs\grealcol.h>
#include <gobjs\gcharcol.h>
#define MAX_COLUMNS     5

IMPLEMENT_CASTABLE1 (DMarker, Drawable_object);
IMPLEMENT_STREAMABLE1 (DMarker, Drawable_object);

// *******************************************************************
      void DMarker::Streamer::Write(opstream& os) const {
// *******************************************************************

//  Short description:
//    Writes an instance of DMarker to the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   WriteBaseObject( (Drawable_object*) GetObject(), os);

   os << GetObject()->Marker_type;
   os << GetObject()->Marker_size;
   }

// *******************************************************************
      void *DMarker::Streamer::Read(ipstream& is, uint32 /*version*/) const {
// *******************************************************************

//  Short description:
//    Reads an instance of DMarker from the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   ReadBaseObject ( (Drawable_object*) GetObject(), is);

   is >> (int) GetObject()->Marker_type;
   is >> GetObject()->Marker_size;

   return GetObject();
   }

// *******************************************************************
       DMarker::DMarker(void)  {
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

   Marker_type = M_none;
   Marker_size = 3;        // 3 mm
   }

// *******************************************************************
       void DMarker::Paint_object(GDC& DC)  {
// *******************************************************************

//  Short description:
//    paint the marker at the specified coordinates.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      GPoint Point[4];                 // points for marker

// -------------------- Executable code section ----------------------

   int Half_marker_size = Marker_size / 2;

   Point[0] = XYPoint;
   Point[1] = Point[0];
   Point[2] = Point[0];
   Point[3] = Point[0];
   Point[0].Add_mm(-Half_marker_size, -Half_marker_size);
   Point[1].Add_mm(Half_marker_size, -Half_marker_size);
   Point[2].Add_mm(Half_marker_size, Half_marker_size);
   Point[3].Add_mm(-Half_marker_size, Half_marker_size);

   switch (Marker_type)
      {
      case M_square : DC.Rectangle(Point[0], Point[2]);
                    break;
      case M_circle : DC.Ellipse(Point[0], Point[2]);
                    break;
      case M_up_triangle : Point[1].Add_mm(-Half_marker_size, 0);
                         DC.Polygon(&Point[1], 3);
                         break;
      case M_down_triangle : Point[2].Add_mm(-Half_marker_size, 0);
                           DC.Polygon(&Point[0], 3);
                           break;
      case M_left_triangle : Point[3].Add_mm(0, -Half_marker_size);
                           DC.Polygon(&Point[1], 3);
                           break;
      case M_right_triangle : Point[1].Add_mm(0, Half_marker_size);
                            Point[2] = Point[3];
                            DC.Polygon(&Point[0], 3);
                            break;
      case M_diamond : Point[0].Add_mm(Half_marker_size, 0);
                     Point[1].Add_mm(0, Half_marker_size);
                     Point[2].Add_mm(-Half_marker_size, 0);
                     Point[3].Add_mm(0, -Half_marker_size);
                     DC.Polygon(&Point[0], 4);
                     break;
      }
   }

IMPLEMENT_CASTABLE1 (DSeries, Drawable_object);
IMPLEMENT_STREAMABLE1 (DSeries, Drawable_object);

// *******************************************************************
      void DSeries::Streamer::Write(opstream& os) const {
// *******************************************************************

//  Short description:
//    Writes an instance of DSeries to the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   WriteBaseObject( (Drawable_object*) GetObject(), os);

   os << GetObject()->x_axis_link;
   os << GetObject()->y_axis_link;
   os << GetObject()->Include_data_labels;
   os << GetObject()->Data_label_x_delta;
   os << GetObject()->Data_label_y_delta;
   os << GetObject()->Data_label_dec_places;
   os << GetObject()->Allow_duplicate_labels;
   os << GetObject()->Data_label_x_middle;
   os << GetObject()->Data_label_y_middle;
   os << GetObject()->Data;
   os << GetObject()->Name;
   os << GetObject()->Data_label_font;
   }

// *******************************************************************
      void *DSeries::Streamer::Read(ipstream& is, uint32 /*version*/) const {
// *******************************************************************

//  Short description:
//    Reads an instance of DSeries from the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   ReadBaseObject ( (Drawable_object*) GetObject(), is);

   is >> (int) GetObject()->x_axis_link;
   is >> (int) GetObject()->y_axis_link;
   is >> GetObject()->Include_data_labels;
   is >> GetObject()->Data_label_x_delta;
   is >> GetObject()->Data_label_y_delta;
   is >> GetObject()->Data_label_dec_places;
   is >> GetObject()->Allow_duplicate_labels;
   is >> GetObject()->Data_label_x_middle;
   is >> GetObject()->Data_label_y_middle;
   is >> GetObject()->Data;
   is >> GetObject()->Name;
   is >> GetObject()->Data_label_font;

   return GetObject();
   }

// *******************************************************************
       DSeries::DSeries(void)  {
// *******************************************************************

//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Data.Init (MAX_COLUMNS);
   Name = "Series name";
   x_axis_link = x1_axis;
   y_axis_link = y1_axis;
   Data_label_x_delta = 0;
   Data_label_y_delta = 5;             // 5 mm above the data point.
   Data_label_font.Set_size(8);        // 8 point data labels.
   Data_label_dec_places = -1;         // auto decimal places.
   Include_data_labels = FALSE;
   Allow_duplicate_labels = TRUE;
   Data_label_x_middle = FALSE;
   Data_label_y_middle = FALSE;
   }

// *******************************************************************
       float DSeries::Get_data_label (int Data_point)  {
// *******************************************************************

//  Short description:
//    return the data label to caller.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   // Try and get the label column.

   GColumn* y_column = Get_data_column("y1");

   assert (y_column != NULL);
   return y_column->Get_real(Data_point);
   }

// *******************************************************************
       void DSeries::Paint_object (GDC& DC)  {
// *******************************************************************

//  Short description:
//    paint the specified data labels if necessary.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      GString Format;                  // Format statement for printf.
      GString Dec_places_st;           // Decimal places as a string.
      char Tick_label[50];             // Tick label to return to caller.
      GPoint Point;                    // point to display data label at.

// -------------------- Executable code section ----------------------

   DC.Select_object(Data_label_font);
   DC.Set_text_align(TA_CENTER, TA_CENTER);

   if (Include_data_labels)
      {
      GColumn* x_column = Get_data_column("x1");
      GColumn* y_column = Get_data_column("y1");
      GColumn* Label_column = Get_data_column("label");
      float x, y;

      for (int Data_point = 0;
           Data_point < (*Data[0]).Get_num_elements();
           Data_point++)
         {
         x = x_column->Get_real(Data_point);
         y = y_column->Get_real(Data_point);
         if (x < MISSING_VALUE && y < MISSING_VALUE)
            {

            // get and format label if necessary.

            GString Label;
            if (Label_column == NULL)
               {
               float Label_value = Get_data_label (Data_point);

               // Create a printf format statement.

               if (Data_label_dec_places < 0)
                  Dec_places_st.Set_integer(Calc_dec_places(Label_value));
               else
                  Dec_places_st.Set_integer(Data_label_dec_places);
               Format = "%15." + Dec_places_st + "f";

               // Convert coordinate to a string using format.

               sprintf(Tick_label, Format.c_str(), Label_value);

               // Strip off any spaces.

               Label = Tick_label;
               Label.Strip(string::Both);
               }
            else
               Label = Label_column->Get_char(Data_point);

            // go paint label if necessary.

            if (Allow_duplicate_labels ||
                Previous_label != Label)
               {
               if (Data_label_x_middle)
                  {
                  if (x_axis_link == x1_axis)
                     x = (x + Coord.Minx1) / 2;
                  else
                     x = (x + Coord.Minx2) / 2;
                  }
               if (Data_label_y_middle)
                  {
                  if (y_axis_link == y1_axis)
                     y = (y + Coord.Miny1) / 2;
                  else
                     y = (y + Coord.Miny2) / 2;
                  }

               Point.Set_logical(x, y, x_axis_link, y_axis_link);
               Point.Add_mm(Data_label_x_delta, Data_label_y_delta);
               DC.Text_out(Point, Label);
               }

            Previous_label = Label;
            }
         }
      }
   }

IMPLEMENT_CASTABLE1 (DXY_scatter_series, DSeries);
IMPLEMENT_STREAMABLE1 (DXY_scatter_series, DSeries);

// *******************************************************************
      void DXY_scatter_series::Streamer::Write(opstream& os) const {
// *******************************************************************

//  Short description:
//    Writes an instance of DXY_scatter_series to the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   WriteBaseObject( (DSeries*) GetObject(), os);
   os << GetObject()->Marker;

   }

// *******************************************************************
      void *DXY_scatter_series::Streamer::Read(ipstream& is, uint32 /*version*/) const {
// *******************************************************************

//  Short description:
//    Reads an instance of DXY_scatter_series from the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   ReadBaseObject ( (DSeries*) GetObject(), is);

   is >> GetObject()->Marker;
   return GetObject();
   }

// *******************************************************************
       DXY_scatter_series::DXY_scatter_series(void)  {
// *******************************************************************

//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   }

// *******************************************************************
       DXY_scatter_series::~DXY_scatter_series(void)  {
// *******************************************************************

//  Short description:
//    destructor

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   }

// *******************************************************************
       GColumn* DXY_scatter_series::Get_data_column(const char *Spec)  {
// *******************************************************************

//  Short description:
//    return a data column to caller.
//    spec should equal x1, y1, label

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   assert (Data.Get_num_columns() >= 2);

   if (strcmpi(Spec, "x1") == 0)
      return Data[0];

   else if (strcmpi(Spec, "y1") == 0)
      return Data[1];

   else if (strcmpi(Spec, "label") == 0 &&
            Data.Get_num_columns() == 3)
      return Data[2];

   else
      return NULL;
   }

// *******************************************************************
       void DXY_scatter_series::Paint_points(GDC& DC,
                                             int Start_point,
                                             int End_point)  {
// *******************************************************************

//  Short description:
//       Paint all points from start point to end point.  Used for animated charts.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      GPoint Point1, Point2;           // Points for drawing line.
      float x, y;
      GPoint *Points;                  // Points to plot on line.

// -------------------- Executable code section ----------------------

   if (Start_point >= 0 && Start_point < Data[0]->Get_num_elements() &&
       End_point >= Start_point && End_point < Data[0]->Get_num_elements())
      {

      // Create space in the points array.

      int Num_points = 0;
      Points = new GPoint[End_point - Start_point + 2];


      // Loop through each data point and plot on chart.

      GColumn& x_column = *Get_data_column("x1");
      GColumn& y_column = *Get_data_column("y1");
      for (int Data_point = Start_point;
           Data_point <= End_point;
           Data_point++)
         {
         x = x_column.Get_real(Data_point);
         y = y_column.Get_real(Data_point);
         if (x < MISSING_VALUE && y < MISSING_VALUE)
            {

            // Draw markers.

            Marker.XYPoint.Set_logical(x, y, x_axis_link, y_axis_link);
            Marker.Paint(DC);

            // Draw line between markers..

            Points[Data_point - Start_point + 1].Set_logical(x, y, x_axis_link, y_axis_link);
            Num_points++;
            }
         }

      // Draw line on graph.

      DC.Select_object(Pen);

      // If start_point <> 0 then make sure line conects to previous point.

      if (Start_point > 0)
         {
         x = x_column.Get_real(Start_point - 1);
         y = y_column.Get_real(Start_point - 1);

         Points[0].Set_logical(x, y, x_axis_link, y_axis_link);
         DC.Polyline(Points, Num_points + 1);
         }
      else
         DC.Polyline(&Points[1], Num_points);

      // Delete the points array


      delete [] Points;
      }
   }

// *******************************************************************
       void DXY_scatter_series::Paint_object (GDC& DC)  {
// *******************************************************************

//  Short description:
//    paint data series on chart.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   if (Data.Get_num_columns() == 2)
      {
      Paint_points(DC, 0, Data[0]->Get_num_elements() - 1);
      }

   DSeries::Paint_object(DC);
   }

// *******************************************************************
       void DXY_scatter_series::Draw_series_sample(GDC& DC, TRect& r)  {
// *******************************************************************

//  Short description:
//    Draw a sample of the series in the specified rectangle.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      int Middle_y = (r.top + r.bottom) / 2;
                                       // Middle x pos of rectangle.
      int Middle_x = (r.left + r.right) / 2;
                                       // Middle y pos of rectangle.

// -------------------- Executable code section ----------------------

   // Draw line.

   DC.Select_object(Pen);
   ((TDC*) &DC)->MoveTo(r.left, Middle_y);
   ((TDC*) &DC)->LineTo(r.right, Middle_y);

   // Draw marker

   Marker.XYPoint.Set_pixel(Middle_x, Middle_y);
   Marker.Paint(DC);
   }

IMPLEMENT_CASTABLE1 (DColumn_series, DSeries);
IMPLEMENT_STREAMABLE1 (DColumn_series, DSeries);

// *******************************************************************
      void DColumn_series::Streamer::Write(opstream& os) const {
// *******************************************************************

//  Short description:
//    Writes an instance of DColumn_series to the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   WriteBaseObject( (DSeries*) GetObject(), os);

   os << GetObject()->Number_column_series;
   os << GetObject()->Series_number;
   os << GetObject()->Bar_width;

   }

// *******************************************************************
      void *DColumn_series::Streamer::Read(ipstream& is, uint32 /*version*/) const {
// *******************************************************************

//  Short description:
//    Reads an instance of DColumn_series from the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   ReadBaseObject ( (DSeries*) GetObject(), is);

   is >> GetObject()->Number_column_series;
   is >> GetObject()->Series_number;
   is >> GetObject()->Bar_width;

   return GetObject();
   }

// *******************************************************************
       DColumn_series::DColumn_series(void)  {
// *******************************************************************

//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Series_number = 1;               // This is series 1 of 1.
   Number_column_series = 1;
   Bar_width = -1;                  // auto.
   Bar_gap = 5;                     // 5 mm
   }

// *******************************************************************
       DColumn_series::~DColumn_series(void)  {
// *******************************************************************

//  Short description:
//    destructor

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   }

// *******************************************************************
       GColumn* DColumn_series::Get_data_column(const char *Spec)  {
// *******************************************************************

//  Short description:
//    return a data column to caller.
//    spec should equal x1, y1, y2, label

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   assert (Data.Get_num_columns() >= 2);

   if (strcmpi(Spec, "x1") == 0)
      return Data[0];

   else if (strcmpi(Spec, "y1") == 0)
      return Data[1];

   else
      {
      // do we have a y2 column ?

      bool Have_y2_column = FALSE;
      if (Data.Get_num_columns() >= 3)
         Have_y2_column = (TYPESAFE_DOWNCAST(Data[2], GReal_column) != NULL);

      if (strcmpi(Spec, "y2") == 0 && Have_y2_column)
         return Data[2];

      else if (strcmpi(Spec, "label") == 0)
         {
         if (Have_y2_column && Data.Get_num_columns() == 4)
            return Data[3];

         else if (!Have_y2_column && Data.Get_num_columns() == 3)
            return Data[2];

         else
            return NULL;
         }
      else
         return NULL;
      }
   }

// *******************************************************************
       bool DColumn_series::Get_rect (GDC& DC,
                                      int Data_point,
                                      DRectangle& Rect)  {
// *******************************************************************

//  Short description:
//    return the rectangle for the specified point.  Return TRUE if
//    all went ok.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   GColumn* x1_column = Get_data_column("x1");
   GColumn* y1_column = Get_data_column("y1");
   GColumn* y2_column = Get_data_column("y2");

   // calculate the width of a bar in pixels.

   int Bar_width_pixels;
   if (Bar_width < 0)
      {

      // Need to get the interval in pixels on the x axis.

      long x1, x2, Dummy;
      GPoint Point0;
      Point0.Set_logical (x1_column->Get_real(Data_point), 0, x_axis_link, y_axis_link);
      GPoint Point1;
      Point1.Set_logical (x1_column->Get_real(Data_point)+1, 0, x_axis_link, y_axis_link);
      Point0.Get_pixel(DC, x1, Dummy);
      Point1.Get_pixel(DC, x2, Dummy);
      int Interval = abs(x2 - x1);

      // calculate the bar width in pixels.

      Bar_width_pixels = (Interval - DC.mm_X(Bar_gap)) / Number_column_series;
      Bar_width_pixels = max(Bar_width_pixels, DC.mm_X(1));
      }
   else
      Bar_width_pixels = DC.mm_X(Bar_width);

   float x1;
   float y1;
   float y2 = MINIMUM_LOGICAL;

   x1 = x1_column->Get_real(Data_point);
   y1 = y1_column->Get_real(Data_point);
   if (y2_column != NULL)
      y2 = y2_column->Get_real(Data_point);

   if (x1 < MISSING_VALUE && y1 < MISSING_VALUE)
      {
      Rect.Point1.Set_logical(x1, y1, x_axis_link, y_axis_link);
      int Pixel_to_add = -Bar_width_pixels * (Number_column_series * 0.5) +
                     (Series_number - 1) * Bar_width_pixels;
      Rect.Point1.Add_pixel(Pixel_to_add, 0);
      Rect.Point2.Set_logical(x1, y2, x_axis_link, y_axis_link);
      Rect.Point2.Add_pixel(Pixel_to_add + Bar_width_pixels, 0);
      return TRUE;
      }
   else
      return FALSE;
   }

// *******************************************************************
       void DColumn_series::Paint_points (GDC& DC,
                                          int Start_point,
                                          int End_point)  {
// *******************************************************************

//  Short description:
//    paint data points on chart.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      DRectangle Rect;                 // rectangle

// -------------------- Executable code section ----------------------

   Rect.Pen = Pen;
   Rect.Brush = Brush;
   Rect.Set_coord(Coord);

   if (Start_point >= 0 && Start_point < Data[0]->Get_num_elements() &&
       End_point >= Start_point && End_point < Data[0]->Get_num_elements())
      {

      for (int Data_point = Start_point;
           Data_point <= End_point;
           Data_point++)
         {
         if (Get_rect(DC, Data_point, Rect))
            Rect.Paint(DC);
         }
      }
   }

// *******************************************************************
       void DColumn_series::Paint_object (GDC& DC)  {
// *******************************************************************

//  Short description:
//    paint data series on chart.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   if (Data.Get_num_columns() >= 2)
      {
      Paint_points(DC, 0, Data[0]->Get_num_elements() - 1);
      }
   DSeries::Paint_object(DC);
   }

// *******************************************************************
       void DColumn_series::Draw_series_sample(GDC& DC, TRect& r)  {
// *******************************************************************

//  Short description:
//    Draw a sample of the series in the specified rectangle.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   DC.Select_object(Pen);
   DC.Select_object(Brush);

   // Draw bar

   ((TDC*) &DC)->Rectangle(r);
   }

IMPLEMENT_CASTABLE1 (DBar_series, DColumn_series);
IMPLEMENT_STREAMABLE_FROM_BASE (DBar_series, DColumn_series);

// *******************************************************************
       DBar_series::DBar_series(void)  {
// *******************************************************************

//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   }

// *******************************************************************
       DBar_series::~DBar_series(void)  {
// *******************************************************************

//  Short description:
//    destructor

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   }

// *******************************************************************
       float DBar_series::Get_data_label (int Data_point)  {
// *******************************************************************

//  Short description:
//    return the data label to caller.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   // Try and get the label column.

   GColumn* x_column = Get_data_column("x2");
   if (x_column == NULL)
      x_column = Get_data_column("x1");

   return x_column->Get_real(Data_point);
   }

// *******************************************************************
       GColumn* DBar_series::Get_data_column(const char *Spec)  {
// *******************************************************************

//  Short description:
//    return a data column to caller.
//    spec should equal x1, x2, y1, label

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   assert (Data.Get_num_columns() >= 2);

   if (strcmpi(Spec, "x1") == 0)
      return Data[0];

   else
      {
      // do we have a label column or an x2 column ?

      bool Have_label_column =
         (TYPESAFE_DOWNCAST(Data[Data.Get_num_columns()-1], GChar_column) != NULL);
      bool Have_x2_column;
      if (Have_label_column)
         Have_x2_column = (Data.Get_num_columns() == 4);
      else
         Have_x2_column = (Data.Get_num_columns() == 3);

      if (strcmpi(Spec, "x2") == 0 && Have_x2_column)
         return Data[1];

      else if (strcmpi(Spec, "y1") == 0)
         {
         if (Have_x2_column)
            return Data[2];
         else
            return Data[1];
         }
      else if (strcmpi(Spec, "label") == 0 && Have_label_column)
         {
         if (Have_x2_column)
            return Data[3];
         else
            return Data[2];
         }
      else
         return NULL;
      }
   }

// *******************************************************************
       bool DBar_series::Get_rect (GDC& DC,
                                   int Data_point,
                                   DRectangle& Rect)  {
// *******************************************************************

//  Short description:
//    return the rectangle for the specified point.  Return TRUE if
//    all went ok.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   GColumn* x1_column = Get_data_column("x1");
   GColumn* x2_column = Get_data_column("x2");
   GColumn* y1_column = Get_data_column("y1");

   // calculate the width of a bar in pixels.

   int Bar_width_pixels;
   if (Bar_width < 0)
      {

      // Need to get the interval in pixels on the y axis.

      long y1, y2, Dummy;
      GPoint Point0;
      Point0.Set_logical (0, y1_column->Get_real(Data_point), x_axis_link, y_axis_link);
      GPoint Point1;
      Point1.Set_logical (0, y1_column->Get_real(Data_point)+1, x_axis_link, y_axis_link);
      Point0.Get_pixel(DC, Dummy, y1);
      Point1.Get_pixel(DC, Dummy, y2);
      int Interval = abs(y2 - y1);

      // calculate the bar width in pixels.

      Bar_width_pixels = (Interval - DC.mm_Y(Bar_gap)) / Number_column_series;
      Bar_width_pixels = max(Bar_width_pixels, DC.mm_Y(1));
      }
   else
      {
      Bar_width_pixels = DC.mm_Y(Bar_width);
      }

   float x1;
   float x2 = MINIMUM_LOGICAL;
   float y1;

   x1 = x1_column->Get_real(Data_point);
   y1 = y1_column->Get_real(Data_point);
   if (x2_column != NULL)
      x2 = x2_column->Get_real(Data_point);

   if (x1 < MISSING_VALUE && y1 < MISSING_VALUE)
      {
      Rect.Point1.Set_logical(x1, y1, x_axis_link, y_axis_link);
      int Pixel_to_add = -Bar_width_pixels * (Number_column_series * 0.5) +
                   (Series_number - 1) * Bar_width_pixels;
      Rect.Point1.Add_pixel(0, Pixel_to_add);
      Rect.Point2.Set_logical(x2, y1, x_axis_link, y_axis_link);
      Rect.Point2.Add_pixel(0, Pixel_to_add + Bar_width_pixels);
      return TRUE;
      }
   else
      return FALSE;
   }

// *******************************************************************
       void DBar_series::Paint_points (GDC& DC,
                                       int Start_point,
                                       int End_point)  {
// *******************************************************************

//  Short description:
//    paint data points on chart.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      DRectangle Rect;                 // rectangle

// -------------------- Executable code section ----------------------

   Rect.Pen = Pen;
   Rect.Brush = Brush;

   if (Start_point >= 0 && Start_point < Data[0]->Get_num_elements() &&
       End_point >= Start_point && End_point < Data[0]->Get_num_elements())
      {

      for (int Data_point = Start_point;
           Data_point <= End_point;
           Data_point++)
         {
         if (Get_rect(DC, Data_point, Rect))
            Rect.Paint(DC);
         }
      }
   }


