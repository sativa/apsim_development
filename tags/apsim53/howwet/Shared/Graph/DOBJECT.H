#if !defined(__DOBJECT_H)
#define __DOBJECT_H

// *******************************************************************
       class GRAPH_CLASS_EXPORT DLine : public Drawable_object  {
// *******************************************************************

//  Short description:
//    Object for drawing a line.

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
      void Paint_object(GDC& Device_context);
                                       // Draw the object.

// -------------------------- Public section -------------------------

   public :

//   Public data
      GPoint Point1;                   // Starting point of line
      GPoint Point2;                   // Ending point of line

//   Public member functions
      DLine(void)  {};                 // constructor
      #ifdef INTERACTIVE
         bool Hit_test(GDC& DC, TPoint& point);
                                       // Return TRUE if user clicked on this
                                       // object.
      void Draw_selection(GDC& DC);
                                       // The object is selected.  Draw handles
                                       // to indicate selection.
      #endif

   DECLARE_STREAMABLE (GRAPH_CLASS_EXPORT, DLine, 1);
   };

// *******************************************************************
       class GRAPH_CLASS_EXPORT DRectangle : public Drawable_object  {
// *******************************************************************

//  Short description:
//    Object for drawing a rectangle.

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
      void Paint_object(GDC& Device_context);
                                       // Draw the object.

// -------------------------- Public section -------------------------

   public :

//   Public data
      GPoint Point1;                   // Starting point of line
      GPoint Point2;                   // Ending point of line

//   Public member functions
      DRectangle(void);                // constructor
      #ifdef INTERACTIVE
         bool Hit_test(GDC& DC, TPoint& point);
                                       // Return TRUE if user clicked on this
                                       // object.
         void Get_bounding_rectangle(GDC& DC, TRect& R);
                                       // Return bounding rectangle to caller.
         void Set_bounding_rectangle(GDC& DC, TRect& R);
                                       // Set bounding rectangle.
      #endif

   DECLARE_STREAMABLE (GRAPH_CLASS_EXPORT, DRectangle, 1);
   };

// *******************************************************************
       class GRAPH_CLASS_EXPORT DText : public Drawable_object  {
// *******************************************************************

//  Short description:
//    Object for drawing some text inside a rectangle.  The text will
//    wrap around to multiple lines if necessary.

//  Notes:

//  Changes:
//    DPH 21/7/95

   public:
      enum Position_enum  {none, top, middle, bottom, left, right};

// ------------------------- Protected section -----------------------

   protected :

//   Protected data

      // Calculated every paint message.

      bool Draw_box;                   // draw box around text?
      int Column_width;                // Width of a column.
      int Row_height;                  // Height of a row.
      int Num_of_rows;                 // Number of rows of text
      int Num_of_columns;              // Number of columns of text in box.
      GPoint Start_point;              // starting point of box.
      GPoint End_point;                // ending point of box.
      int Num_lines;                   // Number of lines of text.

      // user specified line height

      float Line_height;               // Height of a line of text (char. height)

      // user specified text alignment within a column

      int X_alignment;                 // X Alignment of text
      int Y_alignment;                 // Y Alignment of text

      // user specified x margin

      int X_margin;                    // X Margin (mm)

      // user specified rows and columns.

      int Num_rows;                    // Number of rows of text.
      int Num_columns;                 // Number of columns of text

      // Logical position of box

      Position_enum Horiz_pos;         // Horizontal position of box.
      Position_enum Vert_pos;          // Vertical position of box.

      // Tie system.

      GPoint Tie_point;                // Point to tie box to.
      int Tie_x_align;                 // x alignment of box to tie point
      int Tie_y_align;                 // y alignment of box to tie point

      // Absolute coordinates.

      GPoint Top_left;                 // top left coordinate of box
      GPoint Bottom_right;             // bottom right coordinate of box.

//   Protected member functions
      void Add_mm(GPoint& Point, int x, int y);
                                       // Add mm amounts to the specified point
                                       // in the x and y directions.  This routine
                                       // takes into account the orientation of the text.
      void Add_pixel(GPoint& Point, int x, int y);
                                       // Add pixel amounts to the specified point
                                       // in the x and y directions.  This routine
                                       // takes into account the orientation of the text.
      void Paint_object(GDC& Device_context);
                                       // Draw the object.
      void Calc_tie_point(GPoint& Tie_point,
                          int& Tie_x_align,
                          int& Tie_y_align);
                                       // Calculate the tie point and tie alignment
                                       // based on horiz_pos and vert_pos.
      void Find_line_stats(GDC& DC,
                           GString& Text,
                           int& Num_lines,
                           int& Num_columns,
                           int& Longest_line);
                                       // Calculate the box coordinates from the
                                       // tie point and alignment.

      void Calc_box_coords(GDC& DC,
                           GPoint& Tie_point,
                           int Tie_x_align,
                           int Tie_y_align,
                           GString& Text);
                                       // Calculate the box coordinates from the
                                       // tie point and alignment.
      void Modify_text(GString& Text);
                                       // Modify the text string passed in.
      void Goto_row_col(GDC& DC, int Row, int Col, GPoint& Point);
                                       // Goto specified row and column

// -------------------------- Public section -------------------------

   public :

//   Public data
      GString Text;                    // Text string to display
      GFont Font;                      // font

//   Public member functions
      DText(void);                     // constructor

      // ****** SET Routines ******

      void Set_line_height(float Height)
         {
         Line_height = Height;
         };                            // set the line height as proportion of
                                       // a character e.g. 1.5 = 1.5 * normal text height.
                                       // (default = 1.0)
      void Set_draw_box(bool Draw = TRUE)
         {
         Draw_box = Draw;
         };                            // Set the draw_box flag.

      void Set_position(Position_enum Vert_p,
                        Position_enum Horiz_p)
         {
         Horiz_pos = Horiz_p;
         Vert_pos = Vert_p;
         };                            // set the position of box using
                                       // logical coords.  (default = top right)
      void Set_position(GPoint& Point,
                        int X_align,
                        int Y_align)
         {
         Horiz_pos = none;
         Tie_point = Point;
         Tie_x_align = X_align;
         Tie_y_align = Y_align;
         };                            // set the position of box using a point
                                       // and alignment flags.
      void Set_position(GPoint& Top_l,
                        GPoint& Bottom_r)
         {
         Horiz_pos = none;
         Tie_x_align = -1;
         Top_left = Top_l;
         Bottom_right = Bottom_r;
         };                            // set the position of top left and
                                       // bottom right coords.  Box will not
                                       // resize.

      void Set_num_rows_columns(int Rows = 0, int Columns = 0)
         {
         Num_rows = Rows;
         Num_columns = Columns;
         };                            // set the number of rows & columns of text.
                                       // 0 = automatic calculation (default)

      void Set_column_align(int XAlign, int YAlign)
         {
         X_alignment = XAlign;
         Y_alignment = YAlign;
         };                            // Set column alignment of text
                                       // (default = TA_LEFT, TA_BASELINE)
      void Set_x_margin(Margin)
         {
         X_margin = Margin;
         };                            // Set X margin (mm) (default = 0)

      #ifdef INTERACTIVE
         bool Hit_test(GDC& DC, TPoint& point);
                                       // Return TRUE if user clicked on this
                                       // object.
         void Properties(TWindow& Parent);
                                       // Display properties dialog box.
         void Get_bounding_rectangle(GDC& DC, TRect& r);
                                       // return bounding rectangle to caller.
         void Set_bounding_rectangle(GDC& DC, TRect& r);
                                       // return bounding rectangle to caller.
         void Create_popup_menu(TPopupMenu& Popup_menu, int& Menu_id);
                                       // Create the popup menu for this object.
         bool Do_menu_item (TWindow& Parent, int& Menu_ID, int User_selected_id);
                                       // If the user_selected_id belongs to us
                                       // then return TRUE and perform appropriate
                                       // actions.
      #endif

   DECLARE_STREAMABLE (GRAPH_CLASS_EXPORT, DText, 1);
   };

#endif

