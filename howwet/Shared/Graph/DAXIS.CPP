#include <graph\global.h>
#pragma hdrstop
#include <graph\daxis.h>

#include <stdio.h>      // sprintf
#include <math.h>       // floor, ceil

IMPLEMENT_CASTABLE1 (GScale, Interactive_object);
IMPLEMENT_STREAMABLE1 (GScale, Interactive_object);

// *******************************************************************
      void GScale::Streamer::Write(opstream& os) const {
// *******************************************************************

//  Short description:
//    Writes an instance of GScale to the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   WriteBaseObject( (Interactive_object*) GetObject(), os);

   }

// *******************************************************************
      void *GScale::Streamer::Read(ipstream& is, uint32 /*version*/) const {
// *******************************************************************

//  Short description:
//    Reads an instance of GScale from the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   ReadBaseObject ( (Interactive_object*) GetObject(), is);

   return GetObject();
   }

// *******************************************************************
      int Exponent(double Number)   {
// *******************************************************************

//  Short description:
//                                                   E
//    Returns the exponent 'E' in the equation 1 x 10  for a
//    given number.

//  Notes:
//    If Number = 45.8 then the number will be converted to scientific
//    notation (4.58E1) and the exponent returned (1)

//  Changes:
//    DPH - 24/03/93

//  Calls:
//    none

// ---------------------- Declaration section ------------------------

//  Internal constants
//    none

//  Routine arguments
//    float Number                     // Number to calculate exponent

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   if (Number != 0)
      return(floor(log10(fabs(Number))));
   else
      return 0;
   }

// *******************************************************************
       int Calc_dec_places(float Value)  {
// *******************************************************************

//  Short description:
//    return the number of decimal places required to represent the
//    value passed in.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   if (Value > 0 && Value < 1)
      return -Exponent(Value);
   else
      return 0;
   }

// *******************************************************************
       float Round_special(float Number) {
// *******************************************************************

//  Short description:
//     This function rounds the input value upto the nearest integer
//     power of 10 times 1, 2, or 5, in the case of positive numbers.
//     Negative numbers are rounded down in a similar manner.
//     This routine is useful for axis scaling.

//  Notes:
//    e.g.  if Number = 0.17  round to 0.2
//    e.g.  if Number = 23.6  round to 50.0

//  Changes:
//    DPH - 26/03/93
//    DPH - 28/05/93 Fixed problem with losing sign of Number.
//                   Numbers a multiple of 10 also weren't working.

//  Calls:
//    Exponent

// ---------------------- Declaration section ------------------------

//  Internal constants
//    none

//  Routine arguments
//    float Number                     // Number to round off

//  Internal variables
      float Magnitude;                 // Magnitude of Number
      int Sign;                        // Sign of Number

// -------------------- Executable code section ----------------------

   // Calculate the sign of Number and convert Number to positive

   if (Number < 0)
      Sign = -1;

   else
      Sign = 1;

   Number = fabs(Number);

   // Calculate a magnitude based on Number
   // e.g. if number = 23.6 magnitude = 10
   // e.g. if number = 0.02 magnitude = 0.01

   Magnitude = pow(10,Exponent(Number));

   if (Number <= 1 * Magnitude)
      return 1 * Magnitude * Sign;

   else if (Number <= 2 * Magnitude)
      return 2 * Magnitude * Sign;

   else if (Number <= 5 * Magnitude)
      return 5 * Magnitude * Sign;

   else
      return 10 * Magnitude * Sign;
   }

// *******************************************************************
       float Round(float Number, float Round_off, int Round_up)  {
// *******************************************************************

//  Short description:
//    Rounds Number to the nearest Round_off number either up or down
//    depending on variable Round_up.

//  Notes:
//    e.g. if number = 23.6, round_off = 5 and Round_up = FALSE
//         then return 20.0

//  Changes:
//    DPH - 26/03/93
//    DPH - 28/05/93 Fixed problem with Round_off = 0
//    DPH - 11/08/93 Put in a tolerance factor because situations arose
//                   when Round_off was not precisely zero.

//  Calls:
//    none

// ---------------------- Declaration section ------------------------

//  Internal constants
      const float Tolerance = 1e-5;    // Error tolerance

//  Routine arguments
//    float Number                     // Number to round off
//    float Round_off                  // Round off to this number
//    int Round_up                     // Round up ?

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   if (Round_off > -Tolerance && Round_off < Tolerance)
      Number = 0;

   else
      {
      if (Round_up)
         Number = ceil(Number / Round_off) * Round_off;
      else
         Number = floor(Number / Round_off) * Round_off;
      }

   return (Number);
   }

IMPLEMENT_CASTABLE1 (GMin_max_scale, GScale);
IMPLEMENT_STREAMABLE1 (GMin_max_scale, GScale);

// *******************************************************************
      void GMin_max_scale::Streamer::Write(opstream& os) const {
// *******************************************************************

//  Short description:
//    Writes an instance of GMin_max_scale to the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   WriteBaseObject( (GScale*) GetObject(), os);

   os << GetObject()->Minimum;
   os << GetObject()->Maximum;
   os << GetObject()->Interval;
   os << GetObject()->Minimum_fixed;
   os << GetObject()->Maximum_fixed;
   os << GetObject()->Interval_fixed;
   os << GetObject()->Label_dec_places;
   }

// *******************************************************************
      void *GMin_max_scale::Streamer::Read(ipstream& is, uint32 /*version*/) const {
// *******************************************************************

//  Short description:
//    Reads an instance of GMin_max_scale from the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   ReadBaseObject ( (GScale*) GetObject(), is);

   is >> GetObject()->Minimum;
   is >> GetObject()->Maximum;
   is >> GetObject()->Interval;
   is >> GetObject()->Minimum_fixed;
   is >> GetObject()->Maximum_fixed;
   is >> GetObject()->Interval_fixed;
   is >> GetObject()->Label_dec_places;

   return GetObject();
   }

// *******************************************************************
       GMin_max_scale::GMin_max_scale(void)  {
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

   Minimum = 0.0;
   Maximum = 0.0;
   Interval = 0.0;
   Minimum_fixed = FALSE;
   Maximum_fixed = FALSE;
   Interval_fixed = FALSE;
   Label_dec_places = -1;
   User_set_data_min_max = FALSE;
   }

// *******************************************************************
       void GMin_max_scale::Get_min_max_int(float& Min, float& Max, float& Int)  {
// *******************************************************************

//  Short description:
//    return number of ticks to caller

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      static const bool Round_up = TRUE;
      static const bool Round_down = FALSE;

// -------------------- Executable code section ----------------------

   if (Minimum == 0 && Maximum == 0)
      {
      Min = 0.0;
      Max = 0.0;
      Int = 0.0;
      }
   else
      {
      if (Interval_fixed)
         Int = Interval;

      else
         Int = Round_special((Maximum - Minimum) / 5);

      if (Minimum_fixed)
         Min = Minimum;
      else
         Min = Round(Minimum, Int, Round_down);

      if (Maximum_fixed)
         Max = Maximum;
      else
         Max = Round(Maximum, Int, Round_up);
      }
   }

// *******************************************************************
       int GMin_max_scale::Get_num_ticks(void)  {
// *******************************************************************

//  Short description:
//    return number of ticks to caller

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      float Min, Max, Int;             // Axis scaling.

// -------------------- Executable code section ----------------------

   Get_min_max_int(Min, Max, Int);

   if (Int != 0)
      return (Max - Min) / Int + 1;

   else
      return 0;
   }

// *******************************************************************
       float GMin_max_scale::Get_tick_coord(int Tick_number)  {
// *******************************************************************

//  Short description:
//    return the logical tick coordinate for the specified tick number

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      float Min, Max, Int;             // Axis scaling.

// -------------------- Executable code section ----------------------

   Get_min_max_int(Min, Max, Int);

   if (Int != 0)
      return Min + (Int * (Tick_number - 1));

   else
      return 0.0;
   }

// *******************************************************************
       void GMin_max_scale::Get_tick_label(int Tick_number, GString& Label)  {
// *******************************************************************

//  Short description:
//    return the label for the specified tick number

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      GString Format;                  // Format statement for printf.
      GString Dec_places_st;           // Decimal places as a string.
      char Tick_label[50];             // Tick label to return to caller.

// -------------------- Executable code section ----------------------

   float Tick_coord = Get_tick_coord(Tick_number);

   // Create a printf format statement.

   if (Label_dec_places < 0)
      {
      float Min, Max, Int;             // Axis scaling.

      Get_min_max_int(Min, Max, Int);
      Dec_places_st.Set_integer(Calc_dec_places(Int));
      }
   else
      Dec_places_st.Set_integer(Label_dec_places);
   Format = "%15." + Dec_places_st + "f";

   // Convert coordinate to a string using format.

   sprintf(Tick_label, Format.c_str(), Tick_coord);

   // Strip off any spaces.

   Label = Tick_label;
   Label.Strip(string::Both);

   }

IMPLEMENT_CASTABLE1 (GLabel_scale, GScale);
IMPLEMENT_OUR_STREAMABLE (GLabel_scale);
GLabel_scale::GLabel_scale(StreamableInit Init)
   : GScale(Init), Labels(50, 0, 50)
   {}


// *******************************************************************
      void GLabel_scale::Streamer::Write(opstream& os) const {
// *******************************************************************

//  Short description:
//    Writes an instance of GLabel_scale to the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   WriteBaseObject( (GScale*) GetObject(), os);

   os << GetObject()->Labels;
   os << GetObject()->Unique_labels;
   os << GetObject()->Num_unique;
   os << GetObject()->Label_interval;
   }

// *******************************************************************
      void *GLabel_scale::Streamer::Read(ipstream& is, uint32 /*version*/) const {
// *******************************************************************

//  Short description:
//    Reads an instance of GLabel_scale from the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   ReadBaseObject ( (GScale*) GetObject(), is);

   is >> GetObject()->Labels;
   is >> GetObject()->Unique_labels;
   is >> GetObject()->Num_unique;
   is >> GetObject()->Label_interval;

   return GetObject();
   }

// *******************************************************************
       GLabel_scale::GLabel_scale(void)
          : Labels(50, 0, 50)  {
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

   Unique_labels = TRUE;
   Num_unique = 0;
   Label_interval = 1;
   }

// *******************************************************************
       float GLabel_scale::Get_tick_coord(int Tick_number)  {
// *******************************************************************

//  Short description:
//    return the logical tick coordinate for the specified tick number

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      int Num_so_far = 0;              // Num labels so far.

// -------------------- Executable code section ----------------------

   // Loop through all labels in list looking only at non blank
   // strings.  Stop looking when Tick_number labels have been found.

   int Indx;
   for (Indx = 0;
        Indx < Labels.GetItemsInContainer() && Num_so_far < Tick_number;
        Indx++)
      {
      if (Labels[Indx].length() > 0)
         Num_so_far++;
      }

   return Indx;
   }

// *******************************************************************
       void GLabel_scale::Get_tick_label(int Tick_number, GString& Label)  {
// *******************************************************************

//  Short description:
//    return the label for the specified tick number

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      float Real_tick = Tick_number;
                                       // Real version of tick number

// -------------------- Executable code section ----------------------

   // Check to see if we print a label for this tick number.

   bool Display_label = ((Tick_number-1) / Label_interval ==
                    (Real_tick-1) / Label_interval);

   if (Display_label)
      Label = Labels[Get_tick_coord(Tick_number)-1];
   else
      Label = "";
   }

// *******************************************************************
       void GLabel_scale::Add_label(GString& Label)  {
// *******************************************************************

//  Short description:
//    add string to label list.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   if (Unique_labels)
      {
      // get last label in list.

      GString Last_label;

      // find last non blank label.

      bool Found = FALSE;
      int Label_index;
      for (Label_index = Labels.GetItemsInContainer() - 1;
           Label_index >= 0 && !Found;
           Label_index--)
         {
         Found = (Labels[Label_index].length() > 0);
         }
      if (Found)
         Last_label = Labels[Label_index + 1];

      if (Last_label != Label)
         {
         Labels.Add(Label);
         Num_unique++;
         }
      else
         Labels.Add("");
      }
   else
      {
      Labels.Add(Label);
      Num_unique++;
      }
   }

IMPLEMENT_CASTABLE1 (GDate_scale, GScale);
IMPLEMENT_STREAMABLE1 (GDate_scale, GScale);

// *******************************************************************
      void GDate_scale::Streamer::Write(opstream& os) const {
// *******************************************************************

//  Short description:
//    Writes an instance of GDate_scale to the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   WriteBaseObject( (GScale*) GetObject(), os);

   os << GetObject()->Minimum;
   os << GetObject()->Maximum;
   os << GetObject()->Interval;
   os << GetObject()->Minimum_fixed;
   os << GetObject()->Maximum_fixed;
   os << GetObject()->Interval_fixed;
   os << GetObject()->Date_format;
   os << GetObject()->User_set_date_format;
   }

// *******************************************************************
      void *GDate_scale::Streamer::Read(ipstream& is, uint32 /*version*/) const {
// *******************************************************************

//  Short description:
//    Reads an instance of GDate_scale from the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   ReadBaseObject ( (GScale*) GetObject(), is);

   is >> GetObject()->Minimum;
   is >> GetObject()->Maximum;
   is >> (int) GetObject()->Interval;
   is >> GetObject()->Minimum_fixed;
   is >> GetObject()->Maximum_fixed;
   is >> GetObject()->Interval_fixed;
   is >> (int) GetObject()->Date_format;
   is >> GetObject()->User_set_date_format;

   return GetObject();
   }

// *******************************************************************
       GDate_scale::GDate_scale(void)  {
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

   Interval = GDate_scale::monthly;
   Minimum_fixed = FALSE;
   Maximum_fixed = FALSE;
   Interval_fixed = FALSE;
   User_set_data_min_max = FALSE;
   User_set_date_format = FALSE;
   }

// *******************************************************************
       void GDate_scale::Get_min_max_int(GDate& Min,
                                         GDate& Max,
                                         Interval_enum& Int)  {
// *******************************************************************

//  Short description:
//    return minimum, maximum and interval for axis.  Caller is usually this object.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   if (!Minimum.Is_valid() || !Maximum.Is_valid())
      {
      Min = Minimum;
      Max = Maximum;
      Int = Interval;
      }
   else
      {
      if (Interval_fixed)
         Int = Interval;

      else
         {
         if (Maximum - Minimum < 15)
            Int = GDate_scale::daily;

         else if (Maximum - Minimum < 366)
            Int = GDate_scale::monthly;

         else
            Int = GDate_scale::yearly;
         }

      // calculate a minimum

      Min = Minimum;
      if (!Minimum_fixed)
         {
         switch (Int)
            {
            case GDate_scale::monthly :
               Min.Set(1, Min.Get_month(), Min.Get_year());
               break;
            case GDate_scale::yearly :
               Min.Set(1, 1, Min.Get_year());
               break;
            }
         }

      // calculate a maximum

      Max = Maximum;
      if (!Maximum_fixed)
         {
         switch (Int)
            {
            case GDate_scale::monthly :
               Max.Add_months(1);
               break;
            case GDate_scale::yearly :
               Max.Set(1, 1, Max.Get_year() + 1);
               break;
            }
         }
      }
   }

// *******************************************************************
       void GDate_scale::Get_min_max_int(float& Min, float& Max, float& Int)  {
// *******************************************************************

//  Short description:
//    return minimum, maximum and interval for axis.  Caller is usually DChart

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
   GDate Our_min;
   GDate Our_max;
   Interval_enum Our_int;

// -------------------- Executable code section ----------------------

   Get_min_max_int(Our_min, Our_max, Our_int);
   Min = Our_min.Get_jday();
   Max = Our_max.Get_jday();
   Int = (Max - Min) / 5;
   }

// *******************************************************************
       int GDate_scale::Get_num_ticks(void)  {
// *******************************************************************

//  Short description:
//    return number of ticks to caller

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
   GDate Min_date;
   GDate Max_date;
   Interval_enum Int;

// -------------------- Executable code section ----------------------

   Get_min_max_int(Min_date, Max_date, Int);

   switch (Int)
      {
      case GDate_scale::daily :
           return (int)Max_date.Get_jday() - Min_date.Get_jday() + 1;
      case GDate_scale::monthly :
           {
           int Num_ticks = 0;
           for (GDate d = Min_date;
                d <= Max_date;
                d.Add_months(1))
               Num_ticks++;
           return Num_ticks;
           }
      case GDate_scale::yearly :
           return min(Max_date.Get_year() - Min_date.Get_year() + 1, (unsigned) 10);
      }
   return 0;
   }

// *******************************************************************
       float GDate_scale::Get_tick_coord(int Tick_number)  {
// *******************************************************************

//  Short description:
//    return the logical tick coordinate for the specified tick number

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
   GDate Min_date;
   GDate Max_date;
   Interval_enum Int;

// -------------------- Executable code section ----------------------

   Get_min_max_int(Min_date, Max_date, Int);

   switch (Int)
      {
      case GDate_scale::daily :
           return Min_date.Get_jday() + (Tick_number - 1);
      case GDate_scale::monthly :
           {
           for (int Ticks = 0;
                Ticks < Tick_number - 1;
                Ticks++)
              Min_date.Add_months(1);
           return Min_date.Get_jday();
           }
      case GDate_scale::yearly :
           {
           int Num_ticks = Get_num_ticks();
           int Interval = (Max_date.Get_year() - Min_date.Get_year() + 1) / Num_ticks;
           Min_date.Set(1, 1, Min_date.Get_year() + (Tick_number - 1) * Interval);
           return Min_date.Get_jday();
           }
      }

   return Tick_number;
   }

// *******************************************************************
       void GDate_scale::Get_tick_label(int Tick_number, GString& Label)  {
// *******************************************************************

//  Short description:
//    return the label for the specified tick number

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
      GDate Our_min;
      GDate Our_max;
      Interval_enum Our_int;
      char Tick_label[50];             // Tick label to return to caller.

// -------------------- Executable code section ----------------------

   Get_min_max_int(Our_min, Our_max, Our_int);

   switch (Our_int)
      {
      case GDate_scale::daily : Our_min += Tick_number - 1;
                                if (!User_set_date_format)
                                   Date_format = GDate::DDMMYY;
                                break;
      case GDate_scale::monthly : Our_min.Add_months(Tick_number - 1);
                                  if (!User_set_date_format)
                                     Date_format = GDate::MMM;
                                  break;
      case GDate_scale::yearly :
                                 int Num_ticks = Get_num_ticks();
                                 int Interval = (Our_max.Get_year() - Our_min.Get_year() + 1) / Num_ticks;
                                 Our_min.Set(Our_min.Get_day(),
                                             Our_min.Get_month(),
                                             Our_min.Get_year() + (Tick_number - 1) * Interval);
                                 if (!User_set_date_format)
                                    Date_format = GDate::YYYY;
                                 break;
      }

   // Get label from our_min

   Our_min.Set_write_option(Date_format);
   Our_min.Write(Tick_label);
   Label = Tick_label;
   }

IMPLEMENT_CASTABLE1 (DAxis, Drawable_object);
IMPLEMENT_STREAMABLE1 (DAxis, Drawable_object);

// *******************************************************************
      void DAxis::Streamer::Write(opstream& os) const {
// *******************************************************************

//  Short description:
//    Writes an instance of DAxis to the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables
//    none

// -------------------- Executable code section ----------------------

   WriteBaseObject( (Drawable_object*) GetObject(), os);

   os << GetObject()->Tick_style;
   os << GetObject()->Tick_size;
   os << GetObject()->Title_distance;
   bool Has_axis = (GetObject()->Axis_scale_ptr != NULL);
   os << Has_axis;
   if (Has_axis)
      os << GetObject()->Axis_scale_ptr;

   os << GetObject()->Start_point;
   os << GetObject()->End_point;
   os << GetObject()->Grid_line_pen;
   os << GetObject()->Title;
   os << GetObject()->Tick_font;
   }

// *******************************************************************
      void *DAxis::Streamer::Read(ipstream& is, uint32 /*version*/) const {
// *******************************************************************

//  Short description:
//    Reads an instance of DAxis from the passed ipstream.

//  Notes:

//  Changes:
//      DPH 23/6/94

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   ReadBaseObject ( (Drawable_object*) GetObject(), is);

   is >> (int) GetObject()->Tick_style;
   is >> GetObject()->Tick_size;
   is >> GetObject()->Title_distance;
   bool Has_axis;
   is >> Has_axis;
   if (Has_axis)
      is >> GetObject()->Axis_scale_ptr;

   is >> GetObject()->Start_point;
   is >> GetObject()->End_point;
   is >> GetObject()->Grid_line_pen;
   is >> GetObject()->Title;
   is >> GetObject()->Tick_font;

   return GetObject();
   }

// *******************************************************************
       DAxis::DAxis(void)  {
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

   Tick_style = in;
   Tick_size = 3;
   Title_distance = 15;
   Axis_scale_ptr = NULL;
   Grid_line_pen.Set_style(PS_Null);

   Title.Set_draw_box(FALSE);
   }

// *******************************************************************
       DAxis::~DAxis(void)  {
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

   if (Axis_scale_ptr != NULL)
      delete Axis_scale_ptr;
   }

// *******************************************************************
       void DAxis::Paint_object(GDC& DC)  {
// *******************************************************************

//  Short description:
//    Draw the axis on the specified device context.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   // Draw the axis line

   Calc_axis_line_points();
   DC.Line(Start_point, End_point);

   // Draw the tick marks on axis.

   if (Axis_scale_ptr != NULL)
      {
      // Select the tick font.

      DC.Select_object(Tick_font);
      Draw_tick_marks(DC);
      }

   // Draw in axis title.

   Draw_axis_title (DC);
   }

// *******************************************************************
       void DAxis::Draw_tick_marks(GDC& DC)  {
// *******************************************************************

//  Short description:
//    Draw tick marks and tick labels on axis.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
   float Tick_coord;                   // Tick logical coordinate
   GPoint Tick_start;                  // point object used for tick start
   GPoint Tick_end;                    // point object used for tick end
   GPoint Label_start;                 // start of tick label
   GPoint Grid_line_end;               // grid line end point
   GString Label;                      // Tick label to draw.

// -------------------- Executable code section ----------------------

   for (int Tick_mark = 1;
        Tick_mark <= Axis_scale_ptr->Get_num_ticks();
        Tick_mark++)
      {
      Tick_coord = Axis_scale_ptr->Get_tick_coord(Tick_mark);

      Get_tick_position(DC, Tick_coord, Tick_start, Tick_end,
                        Label_start);

      DC.MoveTo(Tick_start);
      DC.LineTo(Tick_end);

      // draw in gridlines.

      DC.Select_object(Grid_line_pen);
      Get_grid_line_end(DC, Tick_coord, Grid_line_end);
      DC.LineTo(Grid_line_end);
      DC.Select_object(Pen);

      // Draw in tick label.

      Axis_scale_ptr->Get_tick_label(Tick_mark, Label);
      DC.Text_out(Label_start, Label);
      }
   }

IMPLEMENT_CASTABLE1( DBottom_axis, DAxis );
IMPLEMENT_STREAMABLE_FROM_BASE ( DBottom_axis, DAxis );

// *******************************************************************
       DBottom_axis::DBottom_axis (void)  {
// *******************************************************************

//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Title.Font.Set_angle(0);
   }

IMPLEMENT_CASTABLE1( DTop_axis, DAxis );
IMPLEMENT_STREAMABLE_FROM_BASE ( DTop_axis, DAxis );

// *******************************************************************
       DTop_axis::DTop_axis (void)  {
// *******************************************************************

//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Title.Font.Set_angle(0);
   }

IMPLEMENT_CASTABLE1( DLeft_axis, DAxis );
IMPLEMENT_STREAMABLE_FROM_BASE ( DLeft_axis, DAxis );

// *******************************************************************
       DLeft_axis::DLeft_axis (void)  {
// *******************************************************************

//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Title.Font.Set_angle(90);
   }

IMPLEMENT_CASTABLE1( DRight_axis, DAxis );
IMPLEMENT_STREAMABLE_FROM_BASE ( DRight_axis, DAxis );

// *******************************************************************
       DRight_axis::DRight_axis (void)  {
// *******************************************************************

//  Short description:
//    constructor

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Title.Font.Set_angle(270);
   }

// *******************************************************************
       void DBottom_axis::Get_tick_position
          (GDC& DC, float Tick_coord, GPoint& Tick_start, GPoint& Tick_end,
           GPoint& Label_start)  {
// *******************************************************************

//  Short description:
//    setup the start and end positions of the ticks and their labels.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Tick_start.Set_logical(Tick_coord, MINIMUM_LOGICAL, x1_axis, y1_axis);
   Tick_end = Tick_start;
   Label_start = Tick_start;
   Label_start.Add_mm(0, Tick_size);
   DC.Set_text_align(TA_CENTER, TA_TOP);

   switch (Tick_style)
      {
      case in    : Tick_end.Add_mm(0, -Tick_size);
                   break;
      case out   : Tick_end.Add_mm(0, Tick_size);
                   break;
      case cross : Tick_start.Add_mm(0, -Tick_size);
                   Tick_end.Add_mm(0, Tick_size);
                   break;
      }
   }

// *******************************************************************
       void DTop_axis::Get_tick_position
          (GDC& DC, float Tick_coord, GPoint& Tick_start, GPoint& Tick_end,
           GPoint& Label_start)  {
// *******************************************************************

//  Short description:
//    setup the start and end positions of the ticks and their labels.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Tick_start.Set_logical(Tick_coord, MAXIMUM_LOGICAL, x2_axis, y1_axis);
   Tick_end = Tick_start;
   Label_start = Tick_start;
   Label_start.Add_mm(0, -Tick_size);
   DC.Set_text_align(TA_CENTER, TA_BOTTOM);
   switch (Tick_style)
      {
      case in    : Tick_end.Add_mm(0, Tick_size);
                   break;
      case out   : Tick_end.Add_mm(0, -Tick_size);
                   break;
      case cross : Tick_start.Add_mm(0, -Tick_size);
                   Tick_end.Add_mm(0, Tick_size);
                   break;
      }
   }

// *******************************************************************
       void DLeft_axis::Get_tick_position
          (GDC& DC, float Tick_coord, GPoint& Tick_start, GPoint& Tick_end,
           GPoint& Label_start)  {
// *******************************************************************

//  Short description:
//    setup the start and end positions of the ticks and their labels.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Tick_start.Set_logical(MINIMUM_LOGICAL, Tick_coord, x1_axis, y1_axis);
   Tick_end = Tick_start;
   Label_start = Tick_start;
   Label_start.Add_mm(-Tick_size, 0);
   DC.Set_text_align(TA_RIGHT, TA_CENTER);
   switch (Tick_style)
      {
      case in    : Tick_end.Add_mm(Tick_size, 0);
                   break;
      case out   : Tick_end.Add_mm(-Tick_size, 0);
                   break;
      case cross : Tick_start.Add_mm(-Tick_size, 0);
                   Tick_end.Add_mm(Tick_size, 0);
                   break;
      }
   }

// *******************************************************************
       void DRight_axis::Get_tick_position
          (GDC& DC, float Tick_coord, GPoint& Tick_start, GPoint& Tick_end,
           GPoint& Label_start)  {
// *******************************************************************

//  Short description:
//    setup the start and end positions of the ticks and their labels.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Tick_start.Set_logical(MAXIMUM_LOGICAL, Tick_coord, x1_axis, y2_axis);
   Tick_end = Tick_start;
   Label_start = Tick_start;
   Label_start.Add_mm(Tick_size, 0);
   DC.Set_text_align(TA_LEFT, TA_CENTER);
   switch (Tick_style)
      {
      case in    : Tick_end.Add_mm(-Tick_size, 0);
                   break;
      case out   : Tick_end.Add_mm(Tick_size, 0);
                   break;
      case cross : Tick_start.Add_mm(-Tick_size, 0);
                   Tick_end.Add_mm(Tick_size, 0);
                   break;
      }
   }

// *******************************************************************
       void DBottom_axis::Get_grid_line_end
          (GDC& , float Tick_coord, GPoint& Grid_line_end)  {
// *******************************************************************

//  Short description:
//    return the gridline end point.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Grid_line_end.Set_logical(Tick_coord, MAXIMUM_LOGICAL, x1_axis, y1_axis);
   }

// *******************************************************************
       void DLeft_axis::Get_grid_line_end
          (GDC& , float Tick_coord, GPoint& Grid_line_end)  {
// *******************************************************************

//  Short description:
//    return the gridline end point.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Grid_line_end.Set_logical(MAXIMUM_LOGICAL, Tick_coord, x1_axis, y1_axis);
   }

// *******************************************************************
       void DTop_axis::Get_grid_line_end
          (GDC& , float Tick_coord, GPoint& Grid_line_end)  {
// *******************************************************************

//  Short description:
//    return the gridline end point.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Grid_line_end.Set_logical(Tick_coord, MINIMUM_LOGICAL, x2_axis, y1_axis);
   }

// *******************************************************************
       void DRight_axis::Get_grid_line_end
          (GDC& , float Tick_coord, GPoint& Grid_line_end)  {
// *******************************************************************

//  Short description:
//    return the gridline end point.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Grid_line_end.Set_logical(MINIMUM_LOGICAL, Tick_coord, x1_axis, y2_axis);
   }

// *******************************************************************
       void DBottom_axis::Calc_axis_line_points(void)  {
// *******************************************************************

//  Short description:
//    Draw the axis line on the specified device context.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Start_point.Set_logical(MINIMUM_LOGICAL, MINIMUM_LOGICAL, x1_axis, y1_axis);
   End_point.Set_logical(MAXIMUM_LOGICAL, MINIMUM_LOGICAL, x1_axis, y1_axis);
   }

// *******************************************************************
       void DLeft_axis::Calc_axis_line_points(void)  {
// *******************************************************************

//  Short description:
//    Draw the axis line on the specified device context.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Start_point.Set_logical(MINIMUM_LOGICAL, MINIMUM_LOGICAL, x1_axis, y1_axis);
   End_point.Set_logical(MINIMUM_LOGICAL, MAXIMUM_LOGICAL, x1_axis, y1_axis);
   }

// *******************************************************************
       void DTop_axis::Calc_axis_line_points(void)  {
// *******************************************************************

//  Short description:
//    Draw the axis line on the specified device context.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Start_point.Set_logical(MINIMUM_LOGICAL, MAXIMUM_LOGICAL, x2_axis, y1_axis);
   End_point.Set_logical(MAXIMUM_LOGICAL, MAXIMUM_LOGICAL, x2_axis, y1_axis);
   }

// *******************************************************************
       void DRight_axis::Calc_axis_line_points(void)  {
// *******************************************************************

//  Short description:
//    Draw the axis line on the specified device context.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables

// -------------------- Executable code section ----------------------

   Start_point.Set_logical(MAXIMUM_LOGICAL, MINIMUM_LOGICAL, x1_axis, y1_axis);
   End_point.Set_logical(MAXIMUM_LOGICAL, MAXIMUM_LOGICAL, x1_axis, y1_axis);
   }

// *******************************************************************
       void DBottom_axis::Draw_axis_title(GDC& DC)  {
// *******************************************************************

//  Short description:
//    Draw the axis title on the specified device context.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
   GPoint Start_point;                 // Starting point for axis.
   long x1,y1,x2,y2;                    // x and y coordinates of axis

// -------------------- Executable code section ----------------------

   Start_point.Set_logical(MINIMUM_LOGICAL, MINIMUM_LOGICAL, x1_axis, y1_axis);
   Start_point.Get_pixel(DC, x1, y1);
   Start_point.Set_logical(MAXIMUM_LOGICAL, MINIMUM_LOGICAL, x1_axis, y1_axis);
   Start_point.Get_pixel(DC, x2, y2);
   Start_point.Set_pixel( (x1+x2)/2, y1);
   Start_point.Add_mm(0, Title_distance);

   Title.Set_position(Start_point, TA_CENTER, TA_CENTER);
   Title.Paint(DC);
   }

// *******************************************************************
       void DLeft_axis::Draw_axis_title(GDC& DC)  {
// *******************************************************************

//  Short description:
//    Draw the axis title on the specified device context.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
   GPoint Start_point;                 // Starting point for axis.
   long x1,y1,x2,y2;                    // x and y coordinates of axis

// -------------------- Executable code section ----------------------

   Start_point.Set_logical(MINIMUM_LOGICAL, MINIMUM_LOGICAL, x1_axis, y1_axis);
   Start_point.Get_pixel(DC, x1, y1);
   Start_point.Set_logical(MINIMUM_LOGICAL, MAXIMUM_LOGICAL, x1_axis, y1_axis);
   Start_point.Get_pixel(DC, x2, y2);
   Start_point.Set_pixel(x1, (y1+y2)/2);
   Start_point.Add_mm(-Title_distance, 0);

   Title.Set_position(Start_point, TA_CENTER, TA_CENTER);
   Title.Paint(DC);
   }

// *******************************************************************
       void DTop_axis::Draw_axis_title(GDC& DC)  {
// *******************************************************************

//  Short description:
//    Draw the axis title on the specified device context.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
   GPoint Start_point;                 // Starting point for axis.
   long x1,y1,x2,y2;                    // x and y coordinates of axis

// -------------------- Executable code section ----------------------

   Start_point.Set_logical(MINIMUM_LOGICAL, MAXIMUM_LOGICAL, x1_axis, y1_axis);
   Start_point.Get_pixel(DC, x1, y1);
   Start_point.Set_logical(MAXIMUM_LOGICAL, MAXIMUM_LOGICAL, x1_axis, y1_axis);
   Start_point.Get_pixel(DC, x2, y2);
   Start_point.Set_pixel( (x1+x2)/2, y1);
   Start_point.Add_mm(0, -Title_distance);

   Title.Set_position(Start_point, TA_CENTER, TA_CENTER);
   Title.Paint(DC);
   }

// *******************************************************************
       void DRight_axis::Draw_axis_title(GDC& DC)  {
// *******************************************************************

//  Short description:
//    Draw the axis title on the specified device context.

//  Notes:

//  Changes:
//    DPH 21/7/95

//  Calls:

//  Internal variables
   GPoint Start_point;                 // Starting point for axis.
   long x1,y1,x2,y2;                    // x and y coordinates of axis

// -------------------- Executable code section ----------------------

   Start_point.Set_logical(MAXIMUM_LOGICAL, MINIMUM_LOGICAL, x1_axis, y1_axis);
   Start_point.Get_pixel(DC, x1, y1);
   Start_point.Set_logical(MAXIMUM_LOGICAL, MAXIMUM_LOGICAL, x1_axis, y1_axis);
   Start_point.Get_pixel(DC, x2, y2);
   Start_point.Set_pixel(x1, (y1+y2)/2);
   Start_point.Add_mm(Title_distance, 0);

   Title.Set_position(Start_point, TA_CENTER, TA_CENTER);
   Title.Paint(DC);
   }

