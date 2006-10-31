#if !defined(__DAXIS_H)
#define __DAXIS_H


#include <graph\dobject.h>
#include <gobjs\gdate.h>

// *******************************************************************
       int Calc_dec_places(float Value);
// *******************************************************************

//  Short description:
//    return the number of decimal places required to represent the
//    value passed in.


// *******************************************************************
       class GRAPH_CLASS_EXPORT GScale : public Interactive_object  {
// *******************************************************************

//  Short description:
//    Object for storing and calculating axis scaling options.  This is a
//    pure virtual base class built for other derived classes.

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
//    none

//   Protected member functions

// -------------------------- Public section -------------------------

   public :

//   Public data

//   Public member functions
      GScale(void) {};                 // constructor
      virtual int Get_num_ticks(void) {return 0;};
                                       // Return number of tick marks to draw to caller
      virtual float Get_tick_coord(int Tick_mark) {return 0;};
                                       // Return the specified tick coordinate in logical coordinates.
      virtual void Get_tick_label(int Tick_number, GString& Label_string) {};
                                       // return the label for the specified tick number
      virtual void Get_min_max_int(float& Minimum, float& Maximum, float& Interval) {};
                                       // return the minimum and maximum logical coordinates.
      virtual void Set_data_min_max(float Min, float Max) {};
                                       // called by dchart to set the minimum,
                                       // maximum values of data.
   DECLARE_STREAMABLE (GRAPH_CLASS_EXPORT, GScale, 1);
   };

// *******************************************************************
       class GRAPH_CLASS_EXPORT GMin_max_scale : public GScale {
// *******************************************************************

//  Short description:
//    Object for encapsulating a normal axis scaling system of
//    minimum, maximum and interval scaling options.

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
      float Minimum;                   // Minimum value on axis.
      float Maximum;                   // Maximum value on axis.
      float Interval;                  // Interval on axis.
      bool Minimum_fixed;              // Minimum fixed ?
      bool Maximum_fixed;              // Maximum fixed ?
      bool Interval_fixed;             // Interval fixed ?
      bool User_set_data_min_max;      // Did the user set the data min/max?
      int Label_dec_places;            // Label decimal places.

//   Protected member functions

// -------------------------- Public section -------------------------

   public :

//   Public data

//   Public member functions
      GMin_max_scale(void);            // constructor

      // ****** GET Routines ******

      int Get_num_ticks(void);
                                       // Return number of tick marks to draw to caller
      float Get_tick_coord(int Tick_mark);
                                       // Return the specified tick coordinate in logical coordinates.
      void Get_tick_label(int Tick_number, GString& Label_string);
                                       // return the label for the specified tick number
      void Get_min_max_int(float& Min_coord, float& Max_coord, float& Int);
                                       // return the minimum, maximum and interval on axis.

      // ****** SET Routines ******

      void Set_fixed_min_max_int(float Min, float Max, float Int)
         {
         Minimum = Min;
         Maximum = Max;
         Interval = Int;
         Minimum_fixed = TRUE;
         Maximum_fixed = TRUE;
         Interval_fixed = TRUE;
         };                            // set the minimum, maximum and interval.
      void Set_auto_min_max(float Min, float Max)
         {
         Minimum = Min;
         Maximum = Max;
         Minimum_fixed = FALSE;
         Maximum_fixed = FALSE;
         Interval_fixed = FALSE;
         User_set_data_min_max = TRUE;
         };                            // set the minimum, maximum and interval.
      void Set_data_min_max(float Min, float Max)
         {
         if (!User_set_data_min_max)
            {
            Minimum = Min;
            Maximum = Max;
            Minimum_fixed = FALSE;
            Maximum_fixed = FALSE;
            Interval_fixed = FALSE;
            }
         };                            // set the minimum, maximum and interval.

      void Set_dec_places(int Dec_places)
         {
         Label_dec_places = Dec_places;
         };                            // set the number of decimal places to use
                                       // for axis labels.
                                       // return the minimum and maximum logical coordinates.
      void Set_minimum(float Min, bool Fixed = TRUE)
         {
         Minimum = Min;
         Minimum_fixed = Fixed;
         };                            // Set fixed minimum
      void Set_maximum(float Max, bool Fixed = TRUE)
         {
         Maximum = Max;
         Maximum_fixed = Fixed;
         };                            // Set fixed maximum
      void Set_interval(float Int, bool Fixed = TRUE)
         {
         Interval = Int;
         Interval_fixed = Fixed;
         };                            // Set fixed interval

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
      #endif

   DECLARE_STREAMABLE (GRAPH_CLASS_EXPORT, GMin_max_scale, 1);
   };

// *******************************************************************
       class GRAPH_CLASS_EXPORT GLabel_scale : public GScale  {
// *******************************************************************

//  Short description:
//    Object for storing and calculating axis scaling options.

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
      String_array Labels;             // Labels to display.
      bool Unique_labels;              // Do we only add unique labels to labels array?
      int Num_unique;                  // Number of unique labels so far.
      int Label_interval;              // Interval for labels.

//   Protected member functions

// -------------------------- Public section -------------------------

   public :

//   Public data

//   Public member functions
      GLabel_scale(void);              // constructor
      int Get_num_ticks(void)
         {
         return Num_unique;
         };                            // Return number of tick marks to draw to caller
      float Get_tick_coord(int Tick_mark);
                                       // Return the specified tick coordinate in logical coordinates.
      void Get_tick_label(int Tick_number, GString& Label_string);
                                       // return the label for the specified tick number
      void Get_min_max_int(float& Minimum, float& Maximum, float& Interval)
         {
         Minimum = 0.0;
         Maximum = Labels.GetItemsInContainer();
         Interval = 1.0;
         };                            // return the minimum and maximum logical coordinates.
      int Get_label_interval(void)
         {
         return Label_interval;
         };                            // return the label interval to caller
      void Set_data_min_max(float Min, float Max)
         {};                           // called by dchart to set the minimum,
                                       // maximum values of data.
      void Set_unique_labels(int Unique = TRUE)
         {
         Unique_labels = Unique;
         };                            // Set the unique labels flag.
      void Set_label_interval(int Interval = 1)
         {
         Label_interval = Interval;
         };                            // set the label interval to use.
      void Add_label(GString& Label);  // Add label to system.

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
      #endif

   DECLARE_STREAMABLE (GRAPH_CLASS_EXPORT, GLabel_scale, 1);
   };

// *******************************************************************
       class GRAPH_CLASS_EXPORT GDate_scale : public GScale {
// *******************************************************************

//  Short description:
//    Object for encapsulating a normal axis scaling system of
//    minimum, maximum and interval scaling options.

//  Notes:

//  Changes:
//    DPH 5/5/96

// ------------------------- Private section -------------------------

   private :

//   Private data
//      none

//   Private member functions
//      none

// ------------------------- Protected section -----------------------

   public:
      enum Interval_enum {daily, monthly, yearly};
                                       // different types of interval supported.
   protected :

//   Protected data
      GDate Minimum;                   // Minimum value on axis.
      GDate Maximum;                   // Maximum value on axis.
      Interval_enum Interval;          // Interval on axis.
      bool Minimum_fixed;              // Minimum fixed ?
      bool Maximum_fixed;              // Maximum fixed ?
      bool Interval_fixed;             // Interval fixed ?
      bool User_set_data_min_max;      // Did the user set the data min/max?
      GDate::Write_format Date_format; // format to write dates.
      bool User_set_date_format;       // did user set the date format?

//   Protected member functions

// -------------------------- Public section -------------------------

   public :

//   Public data

//   Public member functions
      GDate_scale(void);               // constructor

      // ****** GET Routines ******

      int Get_num_ticks(void);
                                       // Return number of tick marks to draw to caller
      float Get_tick_coord(int Tick_mark);
                                       // Return the specified tick coordinate in logical coordinates.
      void Get_tick_label(int Tick_number, GString& Label_string);
                                       // return the label for the specified tick number
      void Get_min_max_int(float& Min_coord, float& Max_coord, float& Int);
                                       // return the minimum, maximum and interval on axis.
      void Get_min_max_int(GDate& Min,
                           GDate& Max,
                           Interval_enum& Int);
                                       // return minimum, maximum and interval for
                                       // axis.  Caller is usually this object.

      // ****** SET Routines ******

      void Set_fixed_min_max_int(GDate& Min, GDate& Max, Interval_enum Int)
         {
         Minimum = Min;
         Maximum = Max;
         Interval = Int;
         Minimum_fixed = TRUE;
         Maximum_fixed = TRUE;
         Interval_fixed = TRUE;
         };                            // set the minimum, maximum and interval.
      void Set_auto_min_max(GDate& Min, GDate& Max)
         {
         Minimum = Min;
         Maximum = Max;
         Minimum_fixed = FALSE;
         Maximum_fixed = FALSE;
         Interval_fixed = FALSE;
         User_set_data_min_max = TRUE;
         };                            // set the minimum, maximum and interval.
      void Set_data_min_max(float Min, float Max)
         {
         if (!User_set_data_min_max)
            {
            Minimum.Set(Min);
            Maximum.Set(Max);
            Minimum_fixed = FALSE;
            Maximum_fixed = FALSE;
            Interval_fixed = FALSE;
            }
         };                            // set the minimum, maximum and interval.
                                       // called by dchart to set the minimum,
                                       // maximum values of data.
      void Set_data_min_max(GDate& Min, GDate& Max)
         {
         if (!User_set_data_min_max)
            {
            Minimum = Min;
            Maximum = Max;
            Minimum_fixed = FALSE;
            Maximum_fixed = FALSE;
            Interval_fixed = FALSE;
            }
         };                            // set the minimum, maximum and interval.

      void Set_minimum(GDate& Min, bool Fixed = TRUE)
         {
         Minimum = Min;
         Minimum_fixed = Fixed;
         };                            // Set fixed minimum
      void Set_maximum(GDate& Max, bool Fixed = TRUE)
         {
         Maximum = Max;
         Maximum_fixed = Fixed;
         };                            // Set fixed maximum
      void Set_interval(Interval_enum Int, bool Fixed = TRUE)
         {
         Interval = Int;
         Interval_fixed = Fixed;
         };                            // Set fixed interval
      void Set_write_format(GDate::Write_format Format)
         {
         Date_format = Format;
         User_set_date_format = TRUE;
         };

   DECLARE_STREAMABLE (GRAPH_CLASS_EXPORT, GDate_scale, 1);
   };

// *******************************************************************
       class GRAPH_CLASS_EXPORT DAxis : public Drawable_object  {
// *******************************************************************

//  Short description:
//    Object for drawing an axis.  This class is a pure virtual object
//    intending for other axis objects to be derived from it.

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

   public:
      enum Tick_style_enum {in, out, cross};

   protected :

//   Protected data
      Tick_style_enum Tick_style;      // Style of tick marks.
      int Tick_size;                   // Size of tick marks (mm)
      int Title_distance;              // Distance of title from axis line (mm)
      GScale* Axis_scale_ptr;          // axis scale object pointer

      GPoint Start_point;              // Starting point for axis line.
      GPoint End_point;                // Ending point for axis line.

//   Protected member functions
      void Draw_tick_marks(GDC& DC);   // Draw tick marks and tick labels on axis.
      virtual void Get_tick_position
          (GDC& DC, float Tick_coord, GPoint& Tick_start, GPoint& Tick_end,
           GPoint& Label_start) {};    // setup the start and end positions of
                                       // the ticks and their labels.
      virtual void Get_grid_line_end
          (GDC& DC, float Tick_coord, GPoint& Grid_line_end)  {};
                                       // setup the end positions of
                                       // the grid line
      virtual void Draw_axis_title(GDC& DC) {};
                                       // Draw the axis title on the specified
                                       // device context.
      virtual void Calc_axis_line_points(void) {};
                                       // Calculate the axis line start and end point
      void Paint_object(GDC& Device_context);
                                       // Paint the axis

// -------------------------- Public section -------------------------

   public :

//   Public data
      GPen Grid_line_pen;              // grid line pen.
      DText  Title;                    // Title of axis.
      GFont  Tick_font;                // Font to use for all ticks

//   Public member functions
      DAxis(void);                     // constructor
      virtual ~DAxis(void);            // destructor

      // ****** SET ROUTINES ******

      void Set_tick_size(int size) {Tick_size = size;};
                                       // set the tick size (mm)
      void Set_title_distance(int Distance) {Title_distance = Distance;};
                                       // set the title distance (mm)
      void Set_tick_style(Tick_style_enum Style) {Tick_style = Style;};
                                       // set the tick style (in, out, cross)
      void Set_axis_scale(GScale *Axis_scale)  {Axis_scale_ptr = Axis_scale;};
                                       // set the axis scale object.  DAxis will
                                       // delete axis scale object in destructor.

      // ****** GET ROUTINES ******

      GScale& Axis_scale(void)  {return *Axis_scale_ptr;};
                                       // Return axis scale object
      bool Has_axis_scale(void)  {return Axis_scale_ptr != NULL;};
                                       // return TRUE if we have an axis scale object

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
      void Properties(TWindow& Parent);
                                       // Display properties box and allow user
                                       // to change settings.
      void Draw_selection(GDC& DC);
                                       // The object is selected.  Draw handles
                                       // to indicate selection.
      #endif

   DECLARE_STREAMABLE (GRAPH_CLASS_EXPORT, DAxis, 1);
   };

// *******************************************************************
       class GRAPH_CLASS_EXPORT DBottom_axis : public DAxis  {
// *******************************************************************

//  Short description:
//    Object for drawing a bottom or x axis.

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
//    none

//   Protected member functions
      void Get_tick_position
          (GDC& DC, float Tick_coord, GPoint& Tick_start, GPoint& Tick_end,
           GPoint& Label_start);       // setup the start and end positions of
                                       // the ticks and their labels.
      void Draw_axis_title(GDC& DC);
                                       // Draw the axis title on the specified
                                       // device context.
      void Calc_axis_line_points(void);
                                       // Calculate the axis line start and end point
      void Get_grid_line_end
          (GDC& DC, float Tick_coord, GPoint& Grid_line_end);
                                       // setup the end positions of
                                       // the grid line

// -------------------------- Public section -------------------------

   public :

//   Public data
//    none

//   Public member functions
      DBottom_axis(void);              // constructor
      DECLARE_STREAMABLE_FROM_BASE (GRAPH_CLASS_EXPORT, DBottom_axis, DAxis);
   };

// *******************************************************************
       class GRAPH_CLASS_EXPORT DTop_axis : public DAxis  {
// *******************************************************************

//  Short description:
//    Object for drawing a top horizontal axis.

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
//    none

//   Protected member functions
      void Get_tick_position
          (GDC& DC, float Tick_coord, GPoint& Tick_start, GPoint& Tick_end,
           GPoint& Label_start);       // setup the start and end positions of
                                       // the ticks and their labels.
      void Draw_axis_title(GDC& DC);
                                       // Draw the axis title on the specified
                                       // device context.
      void Calc_axis_line_points(void);
                                       // Calculate the axis line start and end point
      void Get_grid_line_end
          (GDC& DC, float Tick_coord, GPoint& Grid_line_end);
                                       // setup the end positions of
                                       // the grid line

// -------------------------- Public section -------------------------

   public :

//   Public data
//    none

//   Public member functions
      DTop_axis(void);                 // constructor
      DECLARE_STREAMABLE_FROM_BASE (GRAPH_CLASS_EXPORT, DTop_axis, DAxis);
   };

// *******************************************************************
       class GRAPH_CLASS_EXPORT DLeft_axis : public DAxis  {
// *******************************************************************

//  Short description:
//    Object for drawing a left vertical axis.

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
//    none

//   Protected member functions
      void Get_tick_position
          (GDC& DC, float Tick_coord, GPoint& Tick_start, GPoint& Tick_end,
           GPoint& Label_start);       // setup the start and end positions of
                                       // the ticks and their labels.
      void Draw_axis_title(GDC& DC);
                                       // Draw the axis title on the specified
                                       // device context.
      void Calc_axis_line_points(void);
                                       // Calculate the axis line start and end point
      void Get_grid_line_end
          (GDC& DC, float Tick_coord, GPoint& Grid_line_end);
                                       // setup the end positions of
                                       // the grid line

// -------------------------- Public section -------------------------

   public :

//   Public data
//    none

//   Public member functions
      DLeft_axis(void);                // constructor
      DECLARE_STREAMABLE_FROM_BASE (GRAPH_CLASS_EXPORT, DLeft_axis, DAxis);
   };

// *******************************************************************
       class GRAPH_CLASS_EXPORT DRight_axis : public DAxis  {
// *******************************************************************

//  Short description:
//    Object for drawing a right hand vertical axis.

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
//    none

//   Protected member functions
      void Get_tick_position
          (GDC& DC, float Tick_coord, GPoint& Tick_start, GPoint& Tick_end,
           GPoint& Label_start);       // setup the start and end positions of
                                       // the ticks and their labels.
      void Draw_axis_title(GDC& DC);
                                       // Draw the axis title on the specified
                                       // device context.
      void Calc_axis_line_points(void);
                                       // Calculate the axis line start and end point
      void Get_grid_line_end
          (GDC& DC, float Tick_coord, GPoint& Grid_line_end);
                                       // setup the end positions of
                                       // the grid line

// -------------------------- Public section -------------------------

   public :

//   Public data
//    none

//   Public member functions
      DRight_axis(void);               // constructor
      DECLARE_STREAMABLE_FROM_BASE (GRAPH_CLASS_EXPORT, DRight_axis, DAxis);
   };

#endif
