#include <graph\global.h>
#pragma hdrstop

#include <graph\dseries.h>
#include <graph\sersetup.h>
#include <gobjs\intarray.h>
#include <math.h>

IMPLEMENT_CASTABLE(Series_setup);
IMPLEMENT_STREAMABLE(Series_setup);

// *******************************************************************
      void *Series_setup::Streamer::Read(ipstream& is, uint32 /*version*/) const {
// *******************************************************************

//  Short description:
//    Reads an instance of Series_setup from the passed ipstream.

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables


// -------------------- Executable code section ----------------------

   for (int Priority = 0;
        Priority < 4;
        Priority++)
      {
      is >> GetObject()->Priority_list[Priority];
      }

   is >> GetObject()->Num_markers;
   for (int Priority = 0;
        Priority < GetObject()->Num_markers;
        Priority++)
      {
      is >> (int) GetObject()->Marker_list[Priority];
      }

   is >> GetObject()->Num_pen_styles;
   for (int Priority = 0;
        Priority < GetObject()->Num_pen_styles;
        Priority++)
      {
      is >> (int) GetObject()->Pen_style_list[Priority];
      }

   is >> GetObject()->Num_brush_styles;
   for (int Priority = 0;
        Priority < GetObject()->Num_brush_styles;
        Priority++)
      {
      is >> (int) GetObject()->Brush_style_list[Priority];
      }

   is >> GetObject()->Num_colours;
   for (int Priority = 0;
        Priority < GetObject()->Num_colours;
        Priority++)
      {
      is >> GetObject()->Colour_list[Priority];

      }

   return GetObject();
   }

// *******************************************************************
      void Series_setup::Streamer::Write(opstream& os) const {
// *******************************************************************

//  Short description:
//    Writes an instance of Series_setup to the passed opstream.

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables


// -------------------- Executable code section ----------------------

   for (int Priority = 0;
        Priority < 4;
        Priority++)
      {
      os << GetObject()->Priority_list[Priority];
      }

   os << GetObject()->Num_markers;
   for (int Priority = 0;
        Priority < GetObject()->Num_markers;
        Priority++)
      {
      os << (int) GetObject()->Marker_list[Priority];
      }

   os << GetObject()->Num_pen_styles;
   for (int Priority = 0;
        Priority < GetObject()->Num_pen_styles;
        Priority++)
      {
      os << (int) GetObject()->Pen_style_list[Priority];
      }

   os << GetObject()->Num_brush_styles;
   for (int Priority = 0;
        Priority < GetObject()->Num_brush_styles;
        Priority++)
      {
      os << (int) GetObject()->Brush_style_list[Priority];
      }

   os << GetObject()->Num_colours;
   for (int Priority = 0;
        Priority < GetObject()->Num_colours;
        Priority++)
      {
      os << GetObject()->Colour_list[Priority];
      }
   }

// *******************************************************************
      Series_setup::Series_setup(void)  {
// *******************************************************************

//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables
      int Index;

// -------------------- Executable code section ----------------------

   for (Index = 0;
        Index < 4;
        Index++)
      Priority_list[Index] = Index + 1;

   Num_markers = MAX_MARKERS;
   for (Index = 0;
        Index < Num_markers;
        Index++)
      Marker_list[Index] = All_markers_list[Index];

   Num_pen_styles = MAX_PEN_STYLES - 1;
   for (Index = 1;
        Index < MAX_PEN_STYLES;
        Index++)
      Pen_style_list[Index-1] = All_pen_styles_list[Index];

   Num_brush_styles = MAX_BRUSH_STYLES - 1;
   for (Index = 1;
        Index < MAX_BRUSH_STYLES;
        Index++)
      Brush_style_list[Index-1] = All_brush_styles_list[Index];

   Num_colours = MAX_COLOURS;
   for (Index = 0;
        Index < Num_colours;
        Index++)
      Colour_list[Index] = All_colours_list[Index];

   };

// *******************************************************************
      int Series_setup::Get_attribute_to_use
                                      (int Attribute_type,
                                       int Series_number)  {
// *******************************************************************

//  Short description:
//    return the index into the attribute list to use for this series number.

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables
      int Num[4];                      // number of attributes.
      int Index;                       // Index to return to caller

// -------------------- Executable code section ----------------------

   Num[0] = Num_markers;
   Num[1] = Num_pen_styles;
   Num[2] = Num_brush_styles;
   Num[3] = Num_colours;

   int Total_combinations = 1;
   for (int Priority = 1;
        Priority <= Priority_list[Attribute_type];
        Priority++)
      {
      Total_combinations *= Num[Find_int_in_array(Priority, Priority_list, 4)];
      }

   int Round_up = (Series_number + Total_combinations - 1)
                  / Total_combinations * Total_combinations;
   Index = abs(Round_up - Series_number - Total_combinations);

   if (Priority_list[Attribute_type] == 1)
      Index = Index / (Total_combinations / Num[Attribute_type]);
   else
      Index = (Index-1) / (Total_combinations / Num[Attribute_type]) + 1;

   return Index - 1;
   }

// *******************************************************************
      void Series_setup::Setup_series(DSeries *Series_ptr, int Series_number)  {
// *******************************************************************

//  Short description:
//    setup the specified series.

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables
      #define MARKER_TYPE  0
      #define PEN_TYPE     1
      #define BRUSH_TYPE   2
      #define COLOUR_TYPE  3

      int Index_to_use;
// -------------------- Executable code section ----------------------

   // set all series bits to off.

   Series_ptr->Set_marker_type(M_none);
   Series_ptr->Set_line_type(PS_Null);
   Series_ptr->Set_fill_type(BS_Solid);
   Series_ptr->Set_colour_type(GColour(TColor::Black));

   if (Priority_list[MARKER_TYPE] > 0)
      {
      Index_to_use = Get_attribute_to_use(MARKER_TYPE,
                                          Series_number);
      Series_ptr->Set_marker_type(Marker_list[Index_to_use]);
      }

   if (Priority_list[PEN_TYPE] > 0)
      {
      Index_to_use = Get_attribute_to_use(PEN_TYPE,
                                          Series_number);
      Series_ptr->Set_line_type(Pen_style_list[Index_to_use]);
      }

   if (Priority_list[BRUSH_TYPE] > 0)
      {
      Index_to_use = Get_attribute_to_use(BRUSH_TYPE,
                                          Series_number);
      Series_ptr->Set_fill_type(Brush_style_list[Index_to_use]);
      }

   if (Priority_list[COLOUR_TYPE] > 0)
      {
      Index_to_use = Get_attribute_to_use(COLOUR_TYPE,
                                          Series_number);
      Series_ptr->Set_colour_type(Colour_list[Index_to_use]);
      }
   }

