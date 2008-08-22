#if !defined(__DSERIES_H)
#define __DSERIES_H

#include <gobjs\gcolumns.h>
#include <graph\dobject.h>
#include <classlib\arrays.h>

class GRAPH_CLASS_EXPORT Series_setup;             // forward declaration

#define MISSING_VALUE     1.0E+10

// *******************************************************************
       class GRAPH_CLASS_EXPORT DMarker : public Drawable_object  {
// *******************************************************************

//  Short description:
//    Class for displaying a marker.

//  Notes:

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
      Marker_type_enum Marker_type;    // Type of marker to draw (index)
      int Marker_size;                 // Size of marker (mm)

//   Protected member functions

// -------------------------- Public section -------------------------

   public :

//   Public data
      GPoint XYPoint;                  // logical coordinates for marker center

//   Public member functions
      DMarker(void);                   // constructor
      void Paint_object(GDC& DC);      // paint object.

      // ****** SET Routines ******

      void Set_marker_type(Marker_type_enum Type) {Marker_type = Type;};
                                       // set the type of marker.
      void Set_marker_size(int Size) {Marker_size = Size;};
                                       // set size of marker (mm)

      // ****** GET Routines ******

      Marker_type_enum Get_marker_type(void) {return Marker_type;};
                                       // return marker type to caller.

      #ifdef INTERACTIVE
         void Create_popup_menu(TPopupMenu& Popup_menu, int& Menu_id);
                                       // Create the popup menu for this object.
         bool Do_menu_item (TWindow& Parent, int& Menu_ID, int User_selected_id);
                                       // If the user_selected_id belongs to us
                                       // then return TRUE and perform appropriate
                                       // actions.
         void Properties(TWindow& Parent);
                                       // Display properties box and allow user
                                       // to change settings.
         bool Hit_test(GDC &DC, TPoint& Point);
                                       // Return TRUE if user has clicked on
                                       // this object.
      #endif

   DECLARE_STREAMABLE (GRAPH_CLASS_EXPORT, DMarker, 1);
   };

// *******************************************************************
       class GRAPH_CLASS_EXPORT DSeries : public Drawable_object  {
// *******************************************************************

//  Short description:
//    Base object for encapsulating a graph series.  This is a pure
//    virtual object.

//  Notes:

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
      Axis_type_enum x_axis_link;      // x axis link
      Axis_type_enum y_axis_link;      // y axis link
      bool Include_data_labels;        // include data labels for each point?
      int Data_label_x_delta;          // position of data label in x direction.
      int Data_label_y_delta;          // position of data label in y direction.
      int Data_label_dec_places;       // data label decimal places.
      GString Previous_label;          // previous label.
      bool Allow_duplicate_labels;     // allow duplicate labels?
      bool Data_label_x_middle;        // display data label half way between x
                                       // coordinate and x axis.
      bool Data_label_y_middle;        // display data label half way between y
                                       // coordinate and y axis.

//   Protected member functions
      void Paint_object(GDC& DC);      // paint the data label.
      virtual float Get_data_label(int Data_point);
                                       // return data label to caller.

// -------------------------- Public section -------------------------

   public :

//   Public data
      GColumns Data;                   // Data for series.
      GString Name;                    // Name of series.
      GFont Data_label_font;

//   Public member functions
      DSeries(void);                   // constructor
      int operator ==(const DSeries& DSeries_obj)
         { return (this == &DSeries_obj);};
                                       // equals operator

      virtual void Paint_points(GDC& DC,
                                int Start_point,
                                int End_point) {};
                                       // Paint all points from start point
                                       // to end point.  Used for animated charts.
      virtual void Set_marker_type(Marker_type_enum Marker_type)
         {};                           // Set the marker type of the series.
      virtual void Set_line_type(Pen_style_enum  Line_index)
         {};                           // set the line type of the series.
      virtual void Set_fill_type(Brush_style_enum Fill_index)
         {};                           // set the fill type of the series.
      virtual void Set_colour_type(GColour& Colour)
         {};                           // set the colour of the series.
      virtual void Draw_series_sample(GDC& DC, TRect& r)
         {};                           // Draw a sample of the series in the
                                       // specified rectangle.
      void Get_axis_links(Axis_type_enum& x_type, Axis_type_enum& y_type)
         {
         x_type = x_axis_link;
         y_type = y_axis_link;
         };
      void Set_axis_links(Axis_type_enum x_type, Axis_type_enum y_type)
         {
         x_axis_link = x_type;
         y_axis_link = y_type;
         };
      void Set_paint_data_labels(bool Paint_data_labels = TRUE)
         {
         Include_data_labels = Paint_data_labels;
         };                            // set the paint data labels flag.
      bool Get_paint_data_labels(void)
         {
         return Include_data_labels;
         };                            // return the paint data labels flag.
      void Set_data_label_x_delta (int X_delta)
         {
         Data_label_x_delta = X_delta;
         };                            // set the data label x flag. (mm)
      void Set_data_label_y_delta (int Y_delta)
         {
         Data_label_y_delta = Y_delta; // set the data label y flag.  (mm)
         };
      int Get_data_label_x_delta (void)
         {
         return Data_label_x_delta;    // get the data label x flag.  (mm)
         };
      int Get_data_label_y_delta (void)
         {
         return Data_label_y_delta;    // get the data label y flag.  (mm)
         };
      void Set_data_label_dec_places(int Num_places)
         {
         Data_label_dec_places = Num_places;
         };                            // Set number of decimal places in data label.
                                       // -1 sets it to auto.
      int Get_data_label_dec_places(void)
         {
         return Data_label_dec_places;
         };                            // Return number of decimal places in data label.
      void Set_allow_duplicate_labels(bool Allow)
         {
         Allow_duplicate_labels = Allow;
         };                            // Set allow duplicate labels flag.
      bool Get_allow_duplicate_labels(void)
         {
         return Allow_duplicate_labels;
         };                            // Return allow duplicate labels flag.
      void Set_data_label_x_middle(bool Allow)
         {
         Data_label_x_middle = Allow;
         };                            // Set data_label_x_middle
      bool Get_data_label_x_middle(void)
         {
         return Data_label_x_middle;
         };                            // Return data_label_x_middle.
      void Set_data_label_y_middle(bool Allow)
         {
         Data_label_y_middle = Allow;
         };                            // Set data_label_y_middle
      bool Get_data_label_y_middle(void)
         {
         return Data_label_y_middle;
         };                            // Return data_label_y_middle.
      virtual GColumn* Get_data_column(const char *Spec)
         {
         return NULL;
         };                            // return a column of data.
                                       // eg. Spec = x1 or y1 or x2 or y2 etc.

   DECLARE_STREAMABLE (GRAPH_CLASS_EXPORT, DSeries, 1);
   };
typedef TIArrayAsVector <DSeries> DSeries_array;

// *******************************************************************
       class GRAPH_CLASS_EXPORT DXY_scatter_series : public DSeries {
// *******************************************************************

//  Short description:
//    Object for an x/y scatter series.

//  Notes:
//    Format of Data structure :-   x1, y1,  [label]

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
      float Old_x, Old_y;              // Previous x and y logical coords used
                                       // in drawing lines between markers.

//   Protected member functions
     void Paint_object(GDC& DC);     // Paint all series on chart

// -------------------------- Public section -------------------------

   public :

//   Public data
      DMarker Marker;                  // marker object

//   Public member functions
      DXY_scatter_series(void);        // constructor
      ~DXY_scatter_series(void);       // destructor

      void Set_coord(Coordinate_system& C)
         {
         DSeries::Set_coord(C);
         Marker.Set_coord(C);
         };

      void Paint_points(GDC& DC,
                        int Start_point,
                        int End_point);
                                       // Paint all points from start point
                                       // to end point.  Used for animated charts.

      // ****** GET Routines ******

      void Set_marker_type(Marker_type_enum Marker_type)
         {
         Marker.Set_marker_type(Marker_type);
         };                           // Set the marker type of the series.
      virtual void Set_line_type(Pen_style_enum Line_index)
         {
         Pen.Set_style(Line_index);
         };                           // set the line type of the series.
      virtual void Set_colour_type(GColour& Colour)
         {
         Marker.Pen.Set_colour(Colour);
         Pen.Set_colour(Colour);
         };                           // set the colour of the series.
      void Draw_series_sample(GDC& DC, TRect& r);
                                       // Draw a sample of the series in the
                                       // specified rectangle.
      GColumn* Get_data_column(const char *Spec);
                                       // return a column of data.
                                       // eg. Spec = x1 or y1 or x2 or y2 etc.

      #ifdef INTERACTIVE
         void Create_popup_menu(TPopupMenu& Popup_menu, int& Menu_id);
                                       // Create the popup menu for this object.
         bool Do_menu_item (TWindow& Parent, int& Menu_ID, int User_selected_id);
                                       // If the user_selected_id belongs to us
                                       // then return TRUE and perform appropriate
                                       // actions.
         bool Hit_test(GDC &DC, TPoint& Point);
                                       // Return TRUE if user has clicked on
                                       // this object.
         void Draw_selection(GDC& DC);
                                       // This object is selected.  Draw handles
                                       // around object.
      #endif

   DECLARE_STREAMABLE (GRAPH_CLASS_EXPORT, DXY_scatter_series, 1);
   };

// *******************************************************************
       class GRAPH_CLASS_EXPORT DColumn_series : public DSeries {
// *******************************************************************

//  Short description:
//    Object for a vertical column series.

//  Notes:
//    Format of data structure :- either   x1, y1, [label]
//                                    or   x1, y1, y2, [label]

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
      int Number_column_series;        // Number of column series on graph.
      int Series_number;               // The number of this series.
      int Bar_width;                   // Width of bar (mm)  default : -1
      int Bar_gap;                     // Gap between bars (mm) default : 5mm

//   Protected member functions
      void Paint_object(GDC& DC);      // Paint all series on chart
      virtual bool Get_rect (GDC& DC,
                             int Data_point,
                             DRectangle& Rect);
                                       // return the rectangle for the specified
                                       // point.

// -------------------------- Public section -------------------------

   public :

//   Public data

//   Public member functions
      DColumn_series(void);            // constructor
      ~DColumn_series(void);           // destructor
      void Paint_points(GDC& DC,
                        int Start_point,
                        int End_point);
                                       // Paint all points from start point
                                       // to end point.  Used for animated charts.

      void Set_line_type(Pen_style_enum Line_index)
         {
         Pen.Set_style(Line_index);
         };                           // set the line type of the series.
      void Set_fill_type(Brush_style_enum Fill_type)
         {
         Brush.Set_style(Fill_type);
         };                           // set the fill type of the series.
      void Set_colour_type(GColour& Colour)
         {
         Pen.Set_colour(Colour);
         Brush.Set_colour(Colour);
         };                           // set the colour of the series.


      // ****** SET Routines ******

      void Set_num_column_series(int Num_series) {Number_column_series = Num_series;};
                                       // Number of column series.
      void Set_series_number(int Series_num) {Series_number = Series_num;};
                                       // Set the series number for this series
      void Set_bar_width(int Width)
         {
         Bar_width = Width;
         Bar_gap = -1;
         };
                                       // Set the width of bars.
      void Set_bar_gap(int Gap)
         {
         Bar_width = -1;
         Bar_gap = Gap;
         };
                                       // Set the gap between the bars.
      void Draw_series_sample(GDC& DC, TRect& r);
                                       // Draw a sample of the series in the
                                       // specified rectangle.
      GColumn* Get_data_column(const char *Spec);
                                       // return a column of data.
                                       // eg. Spec = x1 or y1 or x2 or y2 etc.

      #ifdef INTERACTIVE
         void Create_popup_menu(TPopupMenu& Popup_menu, int& Menu_id);
                                       // Create the popup menu for this object.
         bool Do_menu_item (TWindow& Parent, int& Menu_ID, int User_selected_id);
                                       // If the user_selected_id belongs to us
                                       // then return TRUE and perform appropriate
                                       // actions.
         bool Hit_test(GDC &DC, TPoint& Point);
                                       // Return TRUE if user has clicked on
                                       // this object.
         void Draw_selection(GDC& DC);
                                       // This object is selected.  Draw handles
                                       // around object.
         void Properties(TWindow& Parent);
                                       // Display properties box and allow user
                                       // to change settings.
      #endif

   DECLARE_STREAMABLE (GRAPH_CLASS_EXPORT, DColumn_series, 1);
   };

// *******************************************************************
       class GRAPH_CLASS_EXPORT DBar_series : public DColumn_series {
// *******************************************************************

//  Short description:
//    Object for a horizontal bar series.

//  Notes:
//    Format of data structure :- either   x1, y1, [label]
//                                    or   x1, x2, y1, [label]

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

//   Protected member functions
      bool Get_rect (GDC& DC,
                     int Data_point,
                     DRectangle& Rect);
                                       // return the rectangle for the specified
                                       // point.
      float Get_data_label(int Data_point);
                                       // return data label to caller.

// -------------------------- Public section -------------------------

   public :

//   Public data

//   Public member functions
      DBar_series(void);               // constructor
      ~DBar_series(void);              // destructor
      void Paint_points(GDC& DC,
                        int Start_point,
                        int End_point);
                                       // Paint all points from start point
                                       // to end point.  Used for animated charts.
      GColumn* Get_data_column(const char *Spec);
                                       // return a column of data.
                                       // eg. Spec = x1 or y1 or x2 or y2 etc.

   DECLARE_STREAMABLE_FROM_BASE (GRAPH_CLASS_EXPORT, DBar_series, DColumn_series);
   };

#endif
