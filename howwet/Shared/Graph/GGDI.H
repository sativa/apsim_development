#if !defined(__GGDI_H)
#define __GGDI_H

#include <gobjs\gstring.h>
#include <graph\interact.h>
// *******************************************************************
       class GRAPH_CLASS_EXPORT GColour : public TColor, public TStreamableBase  {
// *******************************************************************

//  Short description:
//      Colour class

//  Notes:

//  Changes:
//      DPH 25/5/94

// ------------------------- Declaration section -----------------------

   public :

//   Protected data
   GColour(int R, int G, int B) : TColor(RGB(R, G, B))
      {}                               // constructor
   GColour(void) : TColor(RGB(0, 0, 0))
      {}                               // constructor
   GColour(COLORREF value) : TColor(value)
      {}                               // constructor
   GColour(TColor& col) : TColor(col)
      {}                               // constructor
   DECLARE_STREAMABLE (GRAPH_CLASS_EXPORT, GColour, 1);
   };

// *********************************************************
// PENS, BRUSHS and FONTS
// *********************************************************

// *******************************************************************
       class GRAPH_CLASS_EXPORT GPen : public Interactive_object  {
// *******************************************************************

//  Short description:
//      Pen class

//  Notes:

//  Changes:
//      DPH 25/5/94

// ------------------------- Declaration section -----------------------

   protected :

//   Protected data
      GColour Colour;
      Pen_style_enum Style;
      int Width;

//   Protected member functions

// -------------------------- Public section -------------------------

   public :

//   Public data
//      none

//   Public member functions
      GPen(void);                      // constructor
      void Set_colour(GColour& Col)
         {
         Colour = Col;
         };                            // Set colour of pen.
      void Set_style(Pen_style_enum Style_indx)
         {
         Style = Style_indx;
         };                            // set style of pen.
      void Set_width(int Width_indx)
         {
         Width = Width_indx;
         };                            // set width of pen.
      void Get_colour(GColour& Col)
         {
         Col = Colour;
         };                            // Get colour of pen.
      void Get_style(Pen_style_enum& Sty)
         {
         Sty = Style;
         };                            // Get style of pen.
      void Get_width(int& Width_indx)
         {
         Width_indx = Width;
         };                            // Get width of pen.
      void Fill_LOGPEN(LOGPEN& Log_pen);
                                       // Fill a log pen structure.

      #ifdef INTERACTIVE
          void Properties(TWindow& Parent);
                                       // Display properties box.
      #endif

   friend GDC;

   DECLARE_STREAMABLE (GRAPH_CLASS_EXPORT, GPen, 1);
   };

// *******************************************************************
       class GRAPH_CLASS_EXPORT GBrush : public Interactive_object {
// *******************************************************************

//  Short description:
//      Brush class

//  Notes:

//  Changes:
//      DPH 25/5/94

// ------------------------- Protected section -----------------------

   protected :

//   Protected data
      GColour Colour;
      Brush_style_enum Style;

//   Protected member functions

// -------------------------- Public section -------------------------

   public :

//   Public data
//      none

//   Public member functions
      GBrush(void);                    // constructor
      void Set_colour(GColour& Col)
         {
         Colour = Col;
         };                            // Set colour of brush.
      void Set_style(Brush_style_enum Style_indx)
         {
         Style = Style_indx;
         };                            // set style of brush.
      void Get_colour(GColour& Col)
         {
         Col = Colour;
         };                            // Get colour of brush.
      void Get_style(Brush_style_enum& Style_indx)
         {
         Style_indx = Style;
         };                            // Get style of brush.
      void Fill_LOGBRUSH(LOGBRUSH& Log_brush);
                                       // Fill a log brush structure.

   #ifdef INTERACTIVE
      void Properties(TWindow& Parent);
                                       // Display properties box.
   #endif

   friend GDC;
   DECLARE_STREAMABLE (GRAPH_CLASS_EXPORT, GBrush, 1);
   };

// *******************************************************************
       class GRAPH_CLASS_EXPORT GFont : public Interactive_object  {
// *******************************************************************

//  Short description:
//      Font class

//  Notes:

//  Changes:
//      DPH 25/5/94

// ------------------------- Private section -------------------------

   private :

//   Private data
//      none

//   Private member functions

// ------------------------- Protected section -----------------------

   protected :

//   Protected data
      LOGFONT Font_info;               // Information about font

//   Protected member functions
//      none

// -------------------------- Public section -------------------------

   public :

//   Public data
      void Set_angle(int Angle)
         {
         Font_info.lfEscapement = Angle * 10;
         Font_info.lfOrientation = Angle * 10;
         };                            // set angle to display text at (degrees)
      int Get_angle(void)
         {
         return Font_info.lfEscapement / 10;
         };                            // return angle of display text (degrees)
      void Set_weight(int Weight)
         {
         Font_info.lfWeight = Weight;
         };                            // set weight of text.
      void Set_char_set(BYTE Char_set)
         {
         Font_info.lfCharSet = Char_set;
         };                            // set character set to use.
      void Set_size(int Size)
         {
         Font_info.lfHeight = Size;
         };                            // set character set to use. (points)
      void Set_italics(bool Italics)
         {
         Font_info.lfItalic = Italics;
         };                            // set text as italics?
      void Set_underline(bool Underline)
         {
         Font_info.lfUnderline = Underline;
         };                            // set underline text?
      void Set_strikeout(bool Strike_out)
         {
         Font_info.lfStrikeOut = Strike_out;
         };                            // set strike out text?
      void Set_family(BYTE Family)
         {
         Font_info.lfPitchAndFamily = Family;
         };                            // set pitch and family of font.
      void Set_face_name(char *Face_name)
         {
         strcpy(Font_info.lfFaceName, Face_name);
         };                            // set name of type face to use.

//   Public member functions
      GFont(void);                     // constructor

      #ifdef INTERACTIVE
          void Properties(TWindow& Parent);
                                       // Display properties box.
      #endif

   friend GDC;
   DECLARE_STREAMABLE (GRAPH_CLASS_EXPORT, GFont, 1);
   };

class Coordinate_system : public TStreamableBase
   {
   public:
      Coordinate_system(void)
         {
         Pixel_dimensions.left = -1;
         };                         // constructor
      bool Is_set(void)
         {
         return (Pixel_dimensions.left >= 0);
         };                         // return TRUE if coordinate system
                                    // has been set.
      float Minx1;
      float Maxx1;
      float Miny1;
      float Maxy1;
      float Minx2;
      float Maxx2;
      float Miny2;
      float Maxy2;
      TRect Pixel_dimensions;      // 1)  Chart_area coordinates (world)
      TRect Chart_area;            //     relative to pixel dimensions
      TRect Plot_area;             // 2)  Plot_area coordinates (world) are
                                   //     relative to pixel dimensions
   DECLARE_STREAMABLE (GRAPH_CLASS_EXPORT, Coordinate_system, 1);
   };

#define MINIMUM_LOGICAL -1e20
#define MAXIMUM_LOGICAL  1e20
// *******************************************************************
       class GRAPH_CLASS_EXPORT GDC : public TDC {
// *******************************************************************

//  Short description:
//     Enhancement of TDC class with some new functions mainly to
//       do with text output.

//  Notes:

//  Changes:
//     DPH - 23/5/94

// ------------------------- Declaration section -----------------------

   protected :

//   Protected data
      int Current_x, Current_y;        // Current graphics coordinates
      int Current_pen_width;           // current width of pen
      int Justify_x;                   // X text justification
      int Justify_y;                   // Y text justification
      TPen *Old_pen;                   // Pointer to old pen object
      TBrush *Old_brush;               // Pointer to old brush object
      TFont *Old_font;                 // Pointer to old font object

      TRect Chart_area;                // Chart area (pixels)
      TRect Plot_area;                 // Plot area (pixels)
      float Logical_minx1;             // Minimum x1 axis value
      float Logical_maxx1;             // Maximum x1 axis value
      float Logical_miny1;             // Minimum y1 axis value
      float Logical_maxy1;             // Maximum y1 axis value
      float Logical_minx2;             // Minimum x2 axis value
      float Logical_maxx2;             // Maximum x2 axis value
      float Logical_miny2;             // Minimum y2 axis value
      float Logical_maxy2;             // Maximum y2 axis value

//   Protected member functions
       void Increment_point_array(TPoint* p, int Num_points);
                                       // Increment all coordinates in points array

// -------------------------- Public section -------------------------

   public :

//   Public data
//      none

//   Public member functions
      GDC(HDC Handle);                 // constructor
      ~GDC(void);                      // destructor
       int Log_X1(float Logical_x);    // Convert a logical x1 value into a
                                       // physical pixel number.
       int Log_Y1(float Logical_y);    // Convert a logical y1 value into a
                                       // physical pixel number.
       int Log_X2(float Logical_x);    // Convert a logical x2 value into a
                                       // physical pixel number.
       int Log_Y2(float Logical_y);    // Convert a logical y2 value into a
                                       // physical pixel number.
       int World_X(int World_coord);   // Convert a world x coordinate to a
                                       // physical pixel number.
       int World_Y(int World_coord);   // Convert a world y coordinate to a
                                       // physical pixel number.
       int mm_X(int mm);               // Convert a mm x coordinate to a
                                       // physical pixel number.
       int mm_Y(int mm);               // Convert a mm y coordinate to a
                                       // physical pixel number.
       int Point_X(int Point_size);    // Convert a point measurement into a
                                       // pixel measurement.
       void Pixel_to_world(GPoint& Point);
                                       // Convert a pixel point into a world point.

      bool Text_out(GPoint& P, GString& Text);
                                       // Draw text at the specified point
      bool Draw_text (TRect& r, GString& Text);
                                       // Draw text in the specified rectange.
      void Get_text_align(int& X_align, int& Y_align);
                                       // Get the current text justification
      void Set_text_align(int X_align, int Y_align);
      void Select_object(GPen& Pen_object);
                                       // Select the specified pen into the dc
      void Select_object(GBrush& Brush_object);
                                       // Select the specified brush into the dc
      void Select_object(GFont& Font_object);
                                       // Select the specified font into the dc
      void Set_coord(Coordinate_system& Coord);
                                       // Set the physical and logical coordinate system.
      void Get_physical(TRect& Chart_a, TRect& Plot_a)
         {
         Chart_a = Chart_area;
         Plot_a = Plot_area;
         };                            // Get the physical coordinate system.
/*      void Get_logical(float& min_x1, float& min_y1, float& max_x1, float& max_y1,
                       float& min_x2, float& min_y2, float& max_x2, float& max_y2)
         {
         Logical_minx = minx;
         Logical_maxx = maxx;
         Logical_miny = miny;
         Logical_maxy = maxy;
         };                            // Set the logical coordinate system.
*/
      void Line(GPoint Point1, GPoint Point2);
                                       // Draw the specified line
      void Rectangle(GPoint Point1, GPoint Point2);
                                       // Draw the specified rectangle
      void Ellipse(GPoint Point1, GPoint Point2);
                                       // Draw the specified ellipse
      void Polygon(GPoint Points[], int Num_points);
                                       // Draw the specified polygon on the current device.
                                       // Auto closes the polygon.
      void Polyline(GPoint Points[], int Num_points);
                                       // Draw the specified line on the current device.
                                       // No auto closing polyline.
      void MoveTo(GPoint Point);       // Move current pointer to the specified point
      void LineTo(GPoint Point);       // Draw a line from the current pointer to
                                       // specified point.

   };

#endif

