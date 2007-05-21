#include <graph\global.h>
#pragma hdrstop

#include <graph\dchart.h>
#include <values.h>
#include <gobjs\grealcol.h>
#include <gobjs\gdatecol.h>
#include <graph\sersetup.h>
#include <values.h>

       void Replace_macros
                  (GString& Macro,
                   const char *X_heading,
                   const char *X_unit,
                   const char *Y_heading,
                   const char *Y_unit,
                   const char *Title);

       void Check_axis_scale_object(DAxis *Axis_ptr,
                                    GColumn& Data);

       void Conv_stacked_data_to_absolute
         (GColumns& Data);

// *******************************************************************
      Series_attributes::Series_attributes(void)
         : Columns_array(10, 0, 10)  {
// *******************************************************************

//  Short description:
//    Writes an instance of DChart to the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   Columns_array.OwnsElements(FALSE);
   Title = "%y vs %x";
   Series_name = "%y";
   X_heading = "%x %xu";
   Y_heading = "%y %yu";
   Series_setup_ptr = NULL;
   Series_setup_start_number = 1;
   }

// *******************************************************************
      void Series_attributes::Set_columns_array(GColumns_array& Columns)  {
// *******************************************************************

//  Short description:
//    set the columns_array field.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   for (int Index = 0;
        Index < Columns.GetItemsInContainer();
        Index++)
      {
      Columns_array.Add(Columns[Index]);
      }
   }

IMPLEMENT_CASTABLE1 (DChart, Interactive_object);
IMPLEMENT_OUR_STREAMABLE (DChart);
DChart::DChart (StreamableInit Init)
   : Interactive_object(Init)
    {};


// *******************************************************************
      void DChart::Streamer::Write(opstream& os) const {
// *******************************************************************

//  Short description:
//    Writes an instance of DChart to the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   WriteBaseObject( (Interactive_object*) GetObject(), os);

   os << GetObject()->Min_x1;
   os << GetObject()->Max_x1;
   os << GetObject()->Min_x2;
   os << GetObject()->Max_x2;
   os << GetObject()->Min_y1;
   os << GetObject()->Max_y1;
   os << GetObject()->Min_y2;
   os << GetObject()->Max_y2;
   os << GetObject()->Invert_x1_axis;
   os << GetObject()->Invert_x2_axis;
   os << GetObject()->Invert_y1_axis;
   os << GetObject()->Invert_y2_axis;
   os << GetObject()->Objects;
   }

// *******************************************************************
      void *DChart::Streamer::Read(ipstream& is, uint32 /*version*/) const {
// *******************************************************************

//  Short description:
//    Reads an instance of DChart from the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   ReadBaseObject ( (Interactive_object*) GetObject(), is);

   is >> GetObject()->Min_x1;
   is >> GetObject()->Max_x1;
   is >> GetObject()->Min_x2;
   is >> GetObject()->Max_x2;
   is >> GetObject()->Min_y1;
   is >> GetObject()->Max_y1;
   is >> GetObject()->Min_y2;
   is >> GetObject()->Max_y2;
   is >> GetObject()->Invert_x1_axis;
   is >> GetObject()->Invert_x2_axis;
   is >> GetObject()->Invert_y1_axis;
   is >> GetObject()->Invert_y2_axis;
   is >> GetObject()->Objects;

   return GetObject();
   }


// *******************************************************************
       DChart::DChart(void)  {
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

   Invert_x1_axis = FALSE;
   Invert_x2_axis = FALSE;
   Invert_y1_axis = FALSE;
   Invert_y2_axis = FALSE;

   Min_x1 = MAXFLOAT;
   Max_x1 = -MAXFLOAT;
   Min_x2 = MAXFLOAT;
   Max_x2 = -MAXFLOAT;
   Min_y1 = MAXFLOAT;
   Max_y1 = -MAXFLOAT;
   Min_y2 = MAXFLOAT;
   Max_y2 = -MAXFLOAT;

   // Create all default child objects.

   Objects.Add(new DRectangle(), Front, "chartarea");
   Objects.Add(new DRectangle(), Front, "plotarea");
   Objects.Add(new DText(),      Front, "title");
   Objects.Add(new DLegend(),    Front, "legend");

   // setup chart and plot areas.

   Chart_area()->Point1.Set_world(0, 0);
   Chart_area()->Point2.Set_world(10000, 10000);
   Plot_area()->Point1.Set_world(2000, 2000);
   Plot_area()->Point2.Set_world(8000, 7000);
   Plot_area()->Pen.Set_style(PS_Null);

   Need_for_setup = TRUE;

   // setup title of chart to bold.

   DText *Title_ptr = Title();
   Title_ptr->Font.Set_weight(FW_BOLD);
   Title_ptr->Font.Set_size(10);
   Title_ptr->Set_position(DText::top, DText::middle);
   Title_ptr->Set_column_align(TA_CENTER, TA_CENTER);
   }

// *******************************************************************
       DChart::~DChart(void)  {
// *******************************************************************

//  Short description:
//    destructor

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   }

// *******************************************************************
       int DChart::Get_num_series(void)  {
// *******************************************************************

//  Short description:
//    return number of series to caller.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   // need to know how many series we're dealing with.

   int Num_series = 0;
   while (Series(Num_series + 1) != NULL)
      {
      Num_series++;
      }
   return Num_series;
   }

// *******************************************************************
       void DChart::Add_series (DSeries* Series_ptr,
                                Axis_type_enum x_type,
                                Axis_type_enum y_type)  {
// *******************************************************************

//  Short description:
//    Add a series to the chart.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   char St[20];
   sprintf (St, "series%i", Get_num_series() + 1);

   // Add object to our object list.

   Objects.Add(Series_ptr, Back, St);

   // set the axis links.

   Series_ptr->Set_axis_links(x_type, y_type);

   // update our minimum and maximum axis values.

   Calc_min_max_1_series(Series_ptr);

   // Go check axis to see if it exists and that it has a scale object and
   // that the scale object is up to date.

   Setup_axes(*Series_ptr->Get_data_column("x1"),
              *Series_ptr->Get_data_column("y1"),
              x_type, y_type);

   }

// *******************************************************************
       DXY_scatter_series *DChart::Add_xy_scatter_series
                                      (GColumns_array& Data,
                                       int Table_number,
                                       int X1,
                                       int Y1,
                                       int Labels,
                                       Axis_type_enum x_type,
                                       Axis_type_enum y_type)  {
// *******************************************************************

//  Short description:
//    Add an x/y pair to the chart.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      DXY_scatter_series *Series_ptr;  // new series to return to caller.

// -------------------- Executable code section ----------------------

   // Create new xy line scatter serires object.

   Series_ptr = new DXY_scatter_series();

   // create data object.

   GColumns& Arr = *Data[Table_number];
   Series_ptr->Data.Set_own_all_data(FALSE);
   Series_ptr->Data.Add(Arr[X1], Table_number, X1);
   Series_ptr->Data.Add(Arr[Y1], Table_number, Y1);
   if (Labels != -1)
      Series_ptr->Data.Add(Arr[Labels], Table_number, Labels);

   // go process series.

   Add_series(Series_ptr,
              x_type,
              y_type);

   return Series_ptr;
   }

// *******************************************************************
       DColumn_series *DChart::Add_column_series
                                      (GColumns_array& Data,
                                       int Table_number,
                                       int X1,
                                       int Y1,
                                       int Y2,
                                       int Labels,
                                       Axis_type_enum x_type,
                                       Axis_type_enum y_type)  {
// *******************************************************************

//  Short description:
//    Add an x/y pair to the chart.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      DColumn_series *Series_ptr;      // new series to return to caller.

// -------------------- Executable code section ----------------------

   // Create new xy line scatter serires object.

   Series_ptr = new DColumn_series();

   // create data object.

   GColumns& Arr = *Data[Table_number];
   Series_ptr->Data.Set_own_all_data(FALSE);
   Series_ptr->Data.Add(Arr[X1], Table_number, X1);
   Series_ptr->Data.Add(Arr[Y1], Table_number, Y1);
   if (Y2 != -1)
      Series_ptr->Data.Add(Arr[Y2], Table_number, Y2);
   if (Labels != -1)
      Series_ptr->Data.Add(Arr[Labels], Table_number, Labels);

   // go process series.

   Add_series(Series_ptr,
              x_type,
              y_type);

   return Series_ptr;
   }

// *******************************************************************
       DColumn_series *DChart::Add_bar_series
                                      (GColumns_array& Data,
                                       int Table_number,
                                       int X1,
                                       int X2,
                                       int Y1,
                                       int Labels,
                                       Axis_type_enum x_type,
                                       Axis_type_enum y_type)  {
// *******************************************************************

//  Short description:
//    Add an x/y pair to the chart.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      DBar_series *Series_ptr;      // new series to return to caller.

// -------------------- Executable code section ----------------------

   // Create new xy line scatter serires object.

   Series_ptr = new DBar_series();

   // create data object.

   GColumns& Arr = *Data[Table_number];
   Series_ptr->Data.Set_own_all_data(FALSE);
   Series_ptr->Data.Add(Arr[X1], Table_number, X1);
   if (X2 != -1)
      Series_ptr->Data.Add(Arr[X2], Table_number, X2);
   Series_ptr->Data.Add(Arr[Y1], Table_number, Y1);
   if (Labels != -1)
      Series_ptr->Data.Add(Arr[Labels], Table_number, Labels);

   // go process series.

   Add_series(Series_ptr,
              x_type,
              y_type);

   return Series_ptr;
   }

// *******************************************************************
       void DChart::Setup_axes(GColumn& X, GColumn& Y,
                               Axis_type_enum x_type,
                               Axis_type_enum y_type)  {
// *******************************************************************

//  Short description:
//    Check to make sure axes are setup for this data.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      DAxis* Axis_ptr;

// -------------------- Executable code section ----------------------

   if (x_type == x1_axis)
      {
      // check to see if x1 axis has been created.

      Axis_ptr = X1_axis();
      if (Axis_ptr == NULL)
         {
         // Create new bottom axis.

         Axis_ptr = new DBottom_axis();
         Objects.Add(Axis_ptr, Front, "x1axis");
         }

      // Check axis scaling.

      Check_axis_scale_object(Axis_ptr, X);
      Axis_ptr->Axis_scale().Set_data_min_max(Min_x1, Max_x1);
      }
   else
      {
      // check to see if x2 axis has been created.

      Axis_ptr = X2_axis();
      if (Axis_ptr == NULL)
         {
         // Create new bottom axis.

         Axis_ptr = new DTop_axis();
         Objects.Add(Axis_ptr, Front, "x2axis");
         }

      // Check axis scaling.

      Check_axis_scale_object(Axis_ptr, X);
      Axis_ptr->Axis_scale().Set_data_min_max(Min_x2, Max_x2);
      }

   if (y_type == y1_axis)
      {
      // check to see if y1 axis has been created.

      Axis_ptr = Y1_axis();
      if (Axis_ptr == NULL)
         {
         // Create new bottom axis.

         Axis_ptr = new DLeft_axis();
         Objects.Add(Axis_ptr, Front, "y1axis");
         }

      // Check axis scaling.

      Check_axis_scale_object(Axis_ptr, Y);
      Axis_ptr->Axis_scale().Set_data_min_max(Min_y1, Max_y1);
      }
   else
      {
      // check to see if y2 axis has been created.

      Axis_ptr = Y2_axis();
      if (Axis_ptr == NULL)
         {
         // Create new bottom axis.

         Axis_ptr = new DRight_axis();
         Objects.Add(Axis_ptr, Front, "y2axis");
         }

      // Check axis scaling.

      Check_axis_scale_object(Axis_ptr, Y);
      Axis_ptr->Axis_scale().Set_data_min_max(Min_y2, Max_y2);
      }

   Need_for_setup = TRUE;
   }


// *******************************************************************
       void DChart::Get_min_max_of_axis(DAxis& Axis,
                                        Axis_type_enum Axis_type,
                                        float& Minimum,
                                        float& Maximum)  {
// *******************************************************************

//  Short description:
//    return the minimum and maximum scales for the specified axis.
//    This routine also inverts min and max if required.

//  Notes:

//  Changes:
//    DPH 19/1/95

//  Calls:

//  Internal variables
      float Int;                       // Dummy variable not used.
      float Swap;                      // Swap variable
      bool Invert = FALSE;             // Invert min and max?

// -------------------- Executable code section ----------------------

   Axis.Axis_scale().Get_min_max_int(Minimum, Maximum, Int);

   switch (Axis_type)
      {
      case x1_axis : Invert = Invert_x1_axis;
                     break;
      case x2_axis : Invert = Invert_x2_axis;
                     break;
      case y1_axis : Invert = Invert_y1_axis;
                     break;
      case y2_axis : Invert = Invert_y2_axis;
                     break;
      }

   if (Invert)
      {
      Swap = Minimum;
      Minimum = Maximum;
      Maximum = Swap;
      }
   }

// *******************************************************************
       void DChart::Calc_min_max_1_series(DSeries* Series_ptr)  {
// *******************************************************************

//  Short description:
//    setup the min/max variables for specified series pointer.

//  Notes:

//  Changes:                                   
//    DPH 14/3/96

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   // get axis links.

   Axis_type_enum x_type;
   Axis_type_enum y_type;
   Series_ptr->Get_axis_links (x_type, y_type);

   GColumn* x_column = Series_ptr->Get_data_column("x1");
   GColumn* y_column = Series_ptr->Get_data_column("y1");

   if (x_type == x1_axis)
      {
      Min_x1 = min(Min_x1, x_column->Get_minimum());
      Max_x1 = max(Max_x1, x_column->Get_maximum());
      }
   else
      {
      Min_x2 = min(Min_x2, x_column->Get_minimum());
      Max_x2 = max(Max_x2, x_column->Get_maximum());
      }
   if (y_type == y1_axis)
      {
      Min_y1 = min(Min_y1, y_column->Get_minimum());
      Max_y1 = max(Max_y1, y_column->Get_maximum());
      }
   else
      {
      Min_y2 = min(Min_y2, y_column->Get_minimum());
      Max_y2 = max(Max_y2, y_column->Get_maximum());
      }
   }

// *******************************************************************
       void DChart::Calc_min_max_all_series(void)  {
// *******************************************************************

//  Short description:
//    setup all the min/max variables for all axes.

//  Notes:

//  Changes:
//    DPH 14/3/96

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   // loop through all series

   int Series_number = 1;
   DSeries* Series_ptr = Series(Series_number);

   while (Series_ptr != NULL)
      {
      // go calculate min max for this series.

      Calc_min_max_1_series (Series_ptr);

      // get next series.

      Series_number++;
      Series_ptr = Series(Series_number);
      }
   }

// *******************************************************************
       void DChart::Setup_coords(GDC& DC, TRect& DC_dimensions)  {
// *******************************************************************

//  Short description:
//    setup all coordinate systems for all objects.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      int x, y;                        // world coordinates of chart
      Coordinate_system Coord;         // coordinate system

// -------------------- Executable code section ----------------------

   // Setup the physical coordinate system.

   Coord.Pixel_dimensions = DC_dimensions;

   Chart_area()->Point1.Get_world(x, y);
   if (x == 0 && y == 0)
      {
      long pixelX, pixelY;
      Chart_area()->Point1.Get_pixel(DC, pixelX, pixelY);
      x = pixelX * 1.0 / DC_dimensions.right * 10000;
      y = pixelY * 1.0 / DC_dimensions.bottom * 10000;
      }
   Coord.Chart_area.left = x;
   Coord.Chart_area.top = y;

   Chart_area()->Point2.Get_world(x, y);
   if (x == 0 && y == 0)
      {
      long pixelX, pixelY;
      Chart_area()->Point2.Get_pixel(DC, pixelX, pixelY);
      x = pixelX * 1.0 / DC_dimensions.right * 10000;
      y = pixelY * 1.0 / DC_dimensions.bottom * 10000;
      }
   Coord.Chart_area.right = x;
   Coord.Chart_area.bottom = y;

   Plot_area()->Point1.Get_world(x, y);
   if (x == 0 && y == 0)
      {
      long pixelX, pixelY;
      Plot_area()->Point1.Get_pixel(DC, pixelX, pixelY);
      x = pixelX * 1.0 / DC_dimensions.right * 10000;
      y = pixelY * 1.0 / DC_dimensions.bottom * 10000;
      }
   Coord.Plot_area.left = x;
   Coord.Plot_area.top = y;

   Plot_area()->Point2.Get_world(x, y);
   if (x == 0 && y == 0)
      {
      long pixelX, pixelY;
      Plot_area()->Point2.Get_pixel(DC, pixelX, pixelY);
      x = pixelX * 1.0 / DC_dimensions.right * 10000;
      y = pixelY * 1.0 / DC_dimensions.bottom * 10000;
      }
   Coord.Plot_area.right = x;
   Coord.Plot_area.bottom = y;

   // Get the logical coordinates for all axes.

   if (Y1_axis() != NULL)
      Get_min_max_of_axis(*Y1_axis(), y1_axis, Coord.Miny1, Coord.Maxy1);
   if (Y2_axis() != NULL)
      Get_min_max_of_axis(*Y2_axis(), y2_axis, Coord.Miny2, Coord.Maxy2);
   if (X1_axis() != NULL)
      Get_min_max_of_axis(*X1_axis(), x1_axis, Coord.Minx1, Coord.Maxx1);
   if (X2_axis() != NULL)
      Get_min_max_of_axis(*X2_axis(), x2_axis, Coord.Minx2, Coord.Maxx2);

   // Tell the object list to set the coordinate for all objects.

   Objects.Set_coord (Coord);

   // give all series to legend.

   DLegend* Legend_ptr = Legend();
   if (Legend_ptr != NULL)
      {

      int Series_number = 1;
      Legend_ptr->Series_list->Flush();
      DSeries* Series_ptr = Series(Series_number);
      while (Series_ptr != NULL)
         {
         // give this series to legend.

         Legend_ptr->Series_list->Add(Series_ptr);

         // get next series.

         Series_number++;
         Series_ptr = Series(Series_number);
         }
      }

   Need_for_setup = FALSE;
   }

// *******************************************************************
       void DChart::Paint(GDC& DC, TRect& DC_dimensions)  {
// *******************************************************************

//  Short description:
//    paint the chart object.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   // Setup the device context and the chart.

   if (Need_for_setup)
      Setup_coords(DC, DC_dimensions);

   Objects.Paint(DC);
   }


// *******************************************************************
       int DChart::Create_series (Series_attributes& Series_attr)  {
// *******************************************************************

//  Short description:
//    create series objects for the data passed in.  Return number of
//    series created.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      GString Name;                    // name used for axis headings & title.
      DSeries *New_series;             // pointer to newly created series.
      int Series_start_number;         // start number for series setup.

// -------------------- Executable code section ----------------------

   Series_start_number = Series_attr.Series_setup_start_number;

   // Need to count the total number of column series.

   int Total_column_series = 0;
   for (int Data_index = Series_attr.Data_start_index;
        Data_index <= Series_attr.Data_end_index;
        Data_index++)
      {
      GColumns& New_data = *(Series_attr.Columns_array[Data_index]);
      for (int Column_number = 0;
           Column_number < New_data.Get_num_columns();
           Column_number++)
         {
         if (Series_attr.Column_matchups[Column_number] >= 0)
            {
            if (Series_attr.Chart_type[Column_number] == column)
               Total_column_series++;
            }
         }
      }

   // Loop through each new data object.

   int Num_column_series_so_far = 0;
   for (int Data_index = Series_attr.Data_start_index;
        Data_index <= Series_attr.Data_end_index;
        Data_index++)
      {
      GColumns& New_data = *(Series_attr.Columns_array[Data_index]);

      // Loop through each column of data in the current data object
      // and create a series object for the column, set it up and add
      // it to the series collection for the chart.

      int Previous_x_column = -1;
      int Previous_y_column = -1;
      for (int Column_number = 0;
           Column_number < New_data.Get_num_columns();
           Column_number++)
         {
         // Look for a positive column matchup.

         int X_column;
         int Y_column;
         if (Series_attr.Column_matchups[Column_number] >= 0)
            {
            if (Series_attr.Column_types[Column_number] == x1_axis ||
                Series_attr.Column_types[Column_number] == x2_axis)
               {
               X_column = Column_number;
               Y_column = Series_attr.Column_matchups[Column_number];
               }
            else
               {
               Y_column = Column_number;
               X_column = Series_attr.Column_matchups[Column_number];
               }

            // Make sure we have some data to plot.

            if (New_data[X_column]->Get_num_elements() > 0 &&
                New_data[Y_column]->Get_num_elements() > 0)
               {
               Chart_type_enum This_chart_type = Series_attr.Chart_type[Column_number];
               if (This_chart_type == stacked_absolute_column && Previous_y_column < 0)
                  This_chart_type = column;
               if (This_chart_type == stacked_relative_column && Previous_y_column < 0)
                  This_chart_type = column;
               if (This_chart_type == stacked_relative_bar && Previous_x_column < 0)
                  This_chart_type = bar;

               // Create a new chart series.

               if (This_chart_type == xy_scatter)
                  {
                  New_series = Add_xy_scatter_series
                     (Series_attr.Columns_array,
                      Data_index,                  // table number
                      X_column,                    // X1 data
                      Y_column,                    // Y1 data
                      -1,                          // no labels.
                      Series_attr.Column_types[X_column],
                      Series_attr.Column_types[Y_column]);
                  }

               else if (This_chart_type == column)
                  {
                  New_series = Add_column_series
                     (Series_attr.Columns_array,
                      Data_index,                  // table number
                      X_column,                    // X1 data
                      Y_column,                    // Y1 data
                      -1,                          // no Y2 data
                      -1,                          // no labels
                      Series_attr.Column_types[X_column],
                      Series_attr.Column_types[Y_column]);
                  }
               else if (This_chart_type == bar)
                  {
                  New_series = Add_bar_series
                     (Series_attr.Columns_array,
                      Data_index,                  // table number
                      X_column,                    // X1 data
                      -1,                          // no X2 data
                      Y_column,                    // Y1 data
                      -1,                          // no labels
                      Series_attr.Column_types[X_column],
                      Series_attr.Column_types[Y_column]);
                  }
               else if (This_chart_type == stacked_absolute_column ||
                        This_chart_type == stacked_relative_column)
                  {
                  if (This_chart_type == stacked_relative_column)
                     Conv_stacked_data_to_absolute(*Series_attr.Columns_array[Data_index]);
                  New_series = Add_column_series
                     (Series_attr.Columns_array,
                      Data_index,                  // table number
                      X_column,                    // X1 data
                      Previous_y_column,           // Y1 data
                      Y_column,                    // Y2 data
                      -1,                          // no labels
                      Series_attr.Column_types[X_column],
                      Series_attr.Column_types[Y_column]);
                  }
               else if (This_chart_type == stacked_absolute_bar ||
                        This_chart_type == stacked_relative_bar)
                  {
                  if (This_chart_type == stacked_relative_bar)
                     Conv_stacked_data_to_absolute(*Series_attr.Columns_array[Data_index]);
                  New_series = Add_bar_series
                     (Series_attr.Columns_array,
                      Data_index,                  // table number
                      Previous_x_column,           // X1 data
                      X_column,                    // X2 data
                      Y_column,                    // Y1 data
                      -1,                          // no labels
                      Series_attr.Column_types[X_column],
                      Series_attr.Column_types[Y_column]);
                  }
               // if this is a column series then setup the series numbering.

               if (Series_attr.Chart_type[Column_number] == column)
                  {
                  DColumn_series *Column_series_ptr =
                     TYPESAFE_DOWNCAST (New_series, DColumn_series);
                  Num_column_series_so_far++;
                  Column_series_ptr->Set_series_number (Num_column_series_so_far);
                  Column_series_ptr->Set_num_column_series (Total_column_series);
                  }

               const char *X_head = New_data[X_column]->Get_heading();
               const char *X_unit = New_data[X_column]->Get_unit();
               const char *Y_head = New_data[Y_column]->Get_heading();
               const char *Y_unit = New_data[Y_column]->Get_unit();
               const char *Tit = New_data.Get_title();

               // Create x axis heading.

               Name = Series_attr.X_heading;
               Replace_macros(Name,
                              X_head,
                              X_unit,
                              Y_head,
                              Y_unit,
                              Tit);

               if (Series_attr.Column_types[X_column] == x1_axis)
                  {
                  if (X1_axis()->Title.Text.find(Name) == NPOS)
                     {
                     if (X1_axis()->Title.Text != "")
                        X1_axis()->Title.Text += " & ";
                     X1_axis()->Title.Text += Name;
                     }
                  }
               else if (Series_attr.Column_types[X_column] == x2_axis)
                  {
                  if (X2_axis()->Title.Text.find(Name) == NPOS)
                     {
                     if (X2_axis()->Title.Text != "")
                        X2_axis()->Title.Text += " & ";
                     X2_axis()->Title.Text += Name;
                     }
                  }

               // Create y axis heading.

               Name = Series_attr.Y_heading;
               Replace_macros(Name,
                              X_head,
                              X_unit,
                              Y_head,
                              Y_unit,
                              Tit);
               if (Series_attr.Column_types[Y_column] == y1_axis)
                  {
                  if (Y1_axis()->Title.Text.find(Name) == NPOS)
                     {
                     if (Y1_axis()->Title.Text != "")
                        Y1_axis()->Title.Text += " & ";
                     Y1_axis()->Title.Text += Name;
                     }
                  }
               else if (Series_attr.Column_types[Y_column] == y2_axis)
                  {
                  if (Y2_axis()->Title.Text.find(Name) == NPOS)
                     {
                     if (Y2_axis()->Title.Text != "")
                        Y2_axis()->Title.Text += " & ";
                     Y2_axis()->Title.Text += Name;
                     }
                  }

               // Setup the name of the series.

               Name = Series_attr.Series_name;
               Replace_macros(Name,
                              X_head,
                              X_unit,
                              Y_head,
                              Y_unit,
                              Tit);

               New_series->Name = Name;

               // Setup colours etc of new series.

               if (Series_attr.Series_setup_ptr != NULL)
                  {
                  Series_attr.Series_setup_ptr->Setup_series
                           (New_series,
                            Series_start_number);
                  Series_start_number++;
                  }

               // set the previous x and y column numbers.

               Previous_x_column = X_column;
               Previous_y_column = Y_column;
               }
            }
         }
      }

   // build up title depending on what axes we have.

   GString Y_head;
   if (Y1_axis() != NULL)
      Y_head = Y1_axis()->Title.Text;
   if (Y2_axis() != NULL)
      {
      if (Y_head != "")
         Y_head += " & ";
      Y_head += Y2_axis()->Title.Text;
      }
   GString X_head;
   if (X1_axis() != NULL)
      X_head = X1_axis()->Title.Text;
   if (X2_axis() != NULL)
      {
      if (X_head != "")
         X_head += " & ";
      X_head += X2_axis()->Title.Text;
      }
   GString Tit;
   if (Series_attr.Columns_array.GetItemsInContainer() > 0)
      Tit = Series_attr.Columns_array[0]->Get_title();

   // Add a title to the chart if necessary.

   if (X_head != NULL)
      {
      Name = Series_attr.Title;
      Replace_macros(Name,
                     X_head.c_str(),
                     "",
                     Y_head.c_str(),
                     "",
                     Tit.c_str());
      Title()->Text = Name;

      Need_for_setup = TRUE;
      }
   return Series_start_number - Series_attr.Series_setup_start_number;
   }


