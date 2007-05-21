#include <gobjs\gdate.h>
#include <gobjs\gstring.h>

// *******************************************************************
       class Calender  {
// *******************************************************************

//  Short description:
//    Object for displaying a calender on a device context.

//  Notes:

//  Changes:
//    DPH 13/3/95

// ------------------------- Private section -------------------------

   private :

//   Private data
//      none

//   Private member functions
//      none

// ------------------------- Protected section -----------------------

   protected :

//   Protected data
      GDate Start_date;                // Starting date
      GDate End_date;                  // Ending date
      String_array *Data;              // pointer to data list
      char Font_name[50];              // name of font to use
      int Font_height;                 // Size of font (points) to use
      bool Have_totals;                // put monthly totals on calender?
      bool Have_averages;              // put monthly averages on calender?
      int Column1_width_char;          // column 1 width (char pos's).
      int Column1_width;               // column 1 width (pixels)

//   Protected member functions
       int Num_months_between(GDate &Date1, GDate &Date2);
                                       // Calculate number of months between the
                                       // 2 specified dates.
       TRect Get_cell_position (TRect& Draw_rectangle,
                                GDate& Start_date,
                                int Column_width,
                                int Row_height,
                                GDate& Current_date);
                                       // Calculate where in the calender the
                                       // current date is.
       void Draw_totals_and_averages(TDC& dc,
                                     TRect Draw_rectangle,
                                     int Num_columns,
                                     int Column_width,
                                     int Row_height);
                                       // Draw totals and averages into calender.
       void Get_total_for_month(GDate& Current_date,
                                float& Total,
                                int& Num_days);
                                       // Return the total for month + number of
                                       // days in month. Update current_date to point to
                                       // start of next month.

// -------------------------- Public section -------------------------

   public :

//   Public data
      float Averages[12];              // Monthly averages - used when
                                       // plotting averages on calender

//   Public member functions
      Calender(void);                  // constructor
      ~Calender(void);                 // destructor
      void Draw(TDC& dc, TRect Draw_rectangle);
                                       // Draw calender object
      void Setup(GDate& Start_date,
                 GDate& End_date,
                 String_array* Data,
                 bool Have_totals,
                 bool Have_averages);  // Setup the calender object
      void Set_font(char *Font_name, int Font_height);
                                       // Setup the font name and height (points)
      void Set_column1_width(int Width)
         {
         Column1_width_char = Width;
         };                            // set the column 1 width (char posns)
   };

