#if !defined(__DCHART_H)
#define __DCHART_H

#include <graph\daxis.h>
#include <graph\dseries.h>
#include <graph\dlegend.h>
#include <graph\objlist.h>
#include <stdio.h>
enum Chart_type_enum
   {xy_scatter, column, stacked_absolute_column, stacked_relative_column,
   bar, stacked_absolute_bar, stacked_relative_bar};

#define MAX_SERIES   100
class GRAPH_CLASS_EXPORT Series_attributes
   {
   public:
      GColumns_array    Columns_array;
      int               Data_start_index;
      int               Data_end_index;
      Axis_type_enum    Column_types[MAX_SERIES];
      int               Column_matchups[MAX_SERIES];
      Chart_type_enum   Chart_type[MAX_SERIES];
      const char *      Title;                        // default = %y vs %x
      const char *      Series_name;                  // default = %y
      const char *      X_heading;                    // default = %x %xu
      const char *      Y_heading;                    // default = %y %yu
      Series_setup*     Series_setup_ptr;             // default = NULL
      int               Series_setup_start_number;    // default = 1

      void Set_columns_array(GColumns_array& Columns);
                                       // set the columns_array field.
      Series_attributes(void);         // constructor.
   };


// *******************************************************************
       class GRAPH_CLASS_EXPORT DChart : public Interactive_object  {
// *******************************************************************

//  Short description:
//    Object for encapsulating a chart including axes, data series, titles etc.

//  Notes:
//    If ANY object is added to this chart then it will be deleted in the
//    destructor of this chart.

//  Changes:
//    DPH 21/7/95

// ------------------------- Private section -------------------------

   private :

//   Private data
//      none

//   Private member functions
//      none

// ------------------------- Protected section -----------------------

   protected :

//   Protected data
      float Min_x1, Max_x1;            // minimum and maximum of x1 data.
      float Min_x2, Max_x2;            // minimum and maximum of x2 data.
      float Min_y1, Max_y1;            // minimum and maximum of y1 data.
      float Min_y2, Max_y2;            // minimum and maximum of y2 data.

      bool Need_for_setup;             // Is there a need to setup all objects?
      bool Invert_x1_axis;             // invert the x1 axis?
      bool Invert_x2_axis;             // invert the x2 axis?
      bool Invert_y1_axis;             // invert the y1 axis?
      bool Invert_y2_axis;             // invert the y2 axis?

//   Protected member functions
       void Calc_min_max_1_series(DSeries* DSeries_ptr);
                                       // setup the min/max variables for specified
                                       // series pointer.
       void Calc_min_max_all_series(void);
                                       // setup all the min/max variables for all axes.

      void Setup_coords(GDC& DC, TRect& DC_dimensions);
                                       // setup all coordinate systems for all objects.
       void Setup_axes(GColumn& X, GColumn& Y,
                       Axis_type_enum x_type,
                       Axis_type_enum y_type);
                                       // Check to make sure axes are setup for this data.
       void Get_min_max_of_axis(DAxis& Axis,
                                Axis_type_enum Axis_type,
                                float& Minimum,
                                float& Maximum);
                                       // return the minimum and maximum scales
                                       // for the specified axis. This routine also
                                       // inverts min and max if required.

// -------------------------- Public section -------------------------

   public :

//   Public data
      Object_list Objects;             // List of all objects.

//   Public member functions
      DChart(void);                    // constructor
      int operator==(const DChart& )
         {
         return FALSE;
         }

      virtual ~DChart(void);           // destructor
      void Paint(GDC& DC, TRect& DC_dimensions);
                                       // paint the chart on the specified
                                       // device context.

      // ****** GET Routines ******

      DText* Title(void)
         {
         return TYPESAFE_DOWNCAST(Objects.Get_object("title"), DText);
         };                            // return Title of chart

      DAxis* X1_axis(void)
         {
         return TYPESAFE_DOWNCAST(Objects.Get_object("x1axis"), DAxis);
         };                            // return X1 axis of chart
      DAxis* X2_axis(void)
         {
         return TYPESAFE_DOWNCAST(Objects.Get_object("x2axis"), DAxis);
         };                            // return X2 axis of chart
      DAxis* Y1_axis(void)
         {
         return TYPESAFE_DOWNCAST(Objects.Get_object("y1axis"), DAxis);
         };                            // return Y1 axis of chart
      DAxis* Y2_axis(void)
         {
         return TYPESAFE_DOWNCAST(Objects.Get_object("y2axis"), DAxis);
         };                            // return Y2 axis of chart
      DRectangle* Plot_area(void)
         {
         return TYPESAFE_DOWNCAST(Objects.Get_object("plotarea"), DRectangle);
         };                            // return rectangle defining axis plot area (world)
      DRectangle* Chart_area(void)
         {
         return TYPESAFE_DOWNCAST(Objects.Get_object("chartarea"), DRectangle);
         };                            // return rectangle defining chart area (world)
      DLegend* Legend(void)
         {
         return TYPESAFE_DOWNCAST(Objects.Get_object("legend"), DLegend);
         };                            // return legend object.
      DSeries* Series(int Series_number)
         {
         char St[20];
         sprintf (St, "series%i", Series_number);
         return TYPESAFE_DOWNCAST(Objects.Get_object(St), DSeries);
         };                            // return series.

      // ****** SERIES routines ******

      DXY_scatter_series *Add_xy_scatter_series
                            (GColumns_array& Arr,
                             int Table_number,
                             int X1_column_number,
                             int Y1_column_number,
                             int Labels_column_number,
                             Axis_type_enum x_type,
                             Axis_type_enum y_type);
                                       // Add an x/y scatter series to the chart.
      DColumn_series *Add_column_series
                            (GColumns_array& Arr,
                             int Table_number,
                             int X1_column_number,
                             int Y1_column_number,
                             int Y2_column_number,
                             int Labels_column_number,
                             Axis_type_enum x_type,
                             Axis_type_enum y_type);
                                       // Add a column series to the chart.
      DColumn_series *Add_bar_series
                            (GColumns_array& Arr,
                             int Table_number,
                             int X1_column_number,
                             int X2_column_number,
                             int Y1_column_number,
                             int Labels_column_number,
                             Axis_type_enum x_type,
                             Axis_type_enum y_type);
                                       // Add a bar series to the chart.
       void Add_series (DSeries* Series_ptr,
                        Axis_type_enum x_type,
                        Axis_type_enum y_type);
                                       // Generic routine that can be called instead
                                       // of the specific routines above.
       int Get_num_series(void);       // return number of series to caller.

      int Create_series (Series_attributes& Series_attr);
                                       // create series from the structure.  Return
                                       // number of series created.

      // ****** Axis invert routines ******

      void Set_invert_x1_axis(bool Invert = TRUE)
         {
         Invert_x1_axis = Invert;
         };                            // invert the x1 axis
      void Set_invert_x2_axis(bool Invert = TRUE)
         {
         Invert_x2_axis = Invert;
         };                            // invert the x2 axis
      void Set_invert_y1_axis(bool Invert = TRUE)
         {
         Invert_y1_axis = Invert;
         };                            // invert the y1 axis
      void Set_invert_y2_axis(bool Invert = TRUE)
         {
         Invert_y2_axis = Invert;
         };                            // invert the y2 axis
      void Set_dirty(void)
         {
         Need_for_setup = TRUE;
         };                            // set the need_for_setup flag to TRUE

      #ifdef INTERACTIVE
      public:
         void Left_button_down(TWindow& Parent, TPoint& Point)
            {
            Objects.Left_button_down(Parent, Point);
            };                         // Left button was pressed - process event.
         void Left_button_up(TPoint& Point)
            {
            Objects.Left_button_up(Point);
            Set_dirty();
            };                         // Left button was released - process event.
         void Right_button_down(TWindow& Parent, TPoint& Point)
            {
            Objects.Right_button_down(Parent, Point);
            };                         // Right button was pressed - process event.
         void Mouse_move(TPoint& Point)
            {
            Objects.Mouse_move(Point);
            };                         // Mouse was moved - process event.
         void Command(TWindow& Parent, int id)
            {
            Objects.Command(Parent, id);
            Set_dirty();
            };                         // The user clicked on a popup menu.  Go
                                       // perform required action.
      #endif

   DECLARE_STREAMABLE (GRAPH_CLASS_EXPORT, DChart, 1);
   };
typedef TIArrayAsVector <DChart> Chart_array;

#endif
