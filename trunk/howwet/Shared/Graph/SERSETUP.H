#if !defined(__SERSETUP_H)
#define __SERSETUP_H

// *******************************************************************
       class GRAPH_CLASS_EXPORT Series_setup : public TStreamableBase {
// *******************************************************************

//  Short description:
//    Object for setting up all series objects.

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
      int Priority_list[4];            // priorities for markers, pen styles,
                                       // brush styles and colours respectively.
      Marker_type_enum Marker_list[MAX_MARKERS];
                                       // markers to use.
      int Num_markers;                 // number of markers in array.
      Pen_style_enum Pen_style_list[MAX_PEN_STYLES];
                                       // pen styles to use.
      int Num_pen_styles;              // number of pen styles in array
      Brush_style_enum Brush_style_list[MAX_BRUSH_STYLES];
                                       // brush styles to use.
      int Num_brush_styles;            // number of brush styles in array.
      GColour Colour_list[MAX_COLOURS];
                                       // colours to use.
      int Num_colours;                 // number of colours in array.

//   Protected member functions
      int Get_attribute_to_use (int Attribute_type,
                                int Series_number);
                                       // return the index into the attribute
                                       // list to use for this series number.

// -------------------------- Public section -------------------------

   public :

//   Public data

//   Public member functions
      Series_setup(void);
                                       // constructor
      void Setup_series(DSeries *Series_ptr, int Series_number);
                                       // setup the specified series.
      int Get_num_markers (void)
         {return Num_markers;};        // return number of markers in list.
      int Get_num_pen_styles (void)
         {return Num_pen_styles;};     // return number of pen styles in list.
      int Get_num_brush_styles (void)
         {return Num_brush_styles;};   // return number of brush styles in list.
      int Get_num_colours (void)
         {return Num_colours;};        // return number of colours in list.

      void Set_num_markers (int Num)
         {Num_markers = Num;};        // set number of markers in list.
      void Set_num_pen_styles (int Num)
         {Num_pen_styles = Num;};     // set number of pen styles in list.
      void Set_num_brush_styles (int Num)
         {Num_brush_styles = Num;};   // set number of brush styles in list.
      void Set_num_colours (int Num)
         {Num_colours = Num;};        // set number of colours in list.

      Marker_type_enum* Get_marker_list (void)
         {return Marker_list;};        // return markers list.
      Pen_style_enum* Get_pen_style_list (void)
         {return Pen_style_list;};     // return pen styles list.
      Brush_style_enum* Get_brush_style_list (void)
         {return Brush_style_list;};   // return brush styles list.
      GColour* Get_colour_list (void)
         {return Colour_list;};        // return colours list.

//      int *Get_marker_priorities(void)
//         {
//         return (int*) &Marker_priority_list;
//         };                            // return the marker priorities.
//      int *Get_pen_style_priorities(void)
//         {
//         return (int*) &Pen_style_priority_list;
//         };                            // return the pen style priorities.
//      int *Get_brush_style_priorities(void)
//         {
//         return (int*) &Brush_style_priority_list;
//         };                            // return the brush style priorities.
//      int *Get_colour_priorities(void)
//         {
//         return (int*) &Colour_priority_list;
//         };                            // return the colour priorities.
      int *Get_priorities(void)
         {
         return (int*) &Priority_list;
         };                            // return priority list to caller.
      void Set_priorities(int Priorities[4])
         {
         Priority_list[0] = Priorities[0];
         Priority_list[1] = Priorities[1];
         Priority_list[2] = Priorities[2];
         Priority_list[3] = Priorities[3];
         }

      #ifdef INTERACTIVE
         void Properties(TWindow& Parent);
      #endif

   DECLARE_STREAMABLE(_OWLCLASS, Series_setup, 1);
   };

#endif
