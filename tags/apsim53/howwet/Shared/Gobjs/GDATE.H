#if !defined (__gdate_h)
#define __gdate_h

#include <gobjs\gobjs.h>

#include <strstrea.h>

#define NO_YEAR  2100

#include <classlib\objstrm.h>
// *******************************************************************
       class GOBJS_CLASS_EXPORT GDate : public TStreamableBase {
// *******************************************************************

//  Short description:
//    Class for encapsulating dates

//  Notes:

//  Changes:
//    DPH 9/1/95

// ------------------------- Private section -------------------------

//   Public typedefs
   public:
      enum Write_format {MMM, MMMMMM, DDMMYY, DDMM, DD_MMM, DD_MMM_YYYY, DD_MMMMMM, DD_MMMMMM_YYYY, MMM_YYYY, YYYY};
      // MMM                   :- Jan
      // MMMMMM                :- January
      // DDMMYY                :- 1/1/95
      // DDMM                  :- 1/1
      // DD_MMM format         :- 1 Jan
      // DD_MMM_YYYY format    :- 1 Jan 1995
      // DD_MMMMMM_YYYY format :- 1 January 1995

// ------------------------- Private section -------------------------

   private :

//   Private data
//      none

//   Private member functions
//      none

// ------------------------- Protected section -----------------------

   protected :

//   Protected data
      Write_format Current_write_format;// Current write format to use.
      unsigned long Julian_day;         // Julian day
      bool Must_have_year;              // Must we have a year when checking dates?

//   Protected member functions
      bool Less_than(const GDate& D) const;
                                       // Return TRUE if this object is less
                                       // than the D object passed in.
      bool Dmy_is_valid(unsigned int Day, unsigned int Month, unsigned int Year);
                                       // Check that the day, month and year are valid.
                                       // Return TRUE if valid

// -------------------------- Public section -------------------------

   public :

//   Public member functions
      GDate(void);                     // constructor
      GDate(unsigned int Days, unsigned int Months, unsigned int Years);
                                       // constructor
      void Set(unsigned int D, unsigned int M, unsigned int Y);
                                       // Set the julian day of this object from
                                       // day, month and year
      void Set(unsigned int Day_of_year, unsigned int Year);
                                       // Set the julian day of this object from
                                       // day of year and year
      void Set(unsigned long Jday)
         {Julian_day = Jday;};         // set date to specified julian day
      void Set_to_current(void);       // Set object to current date.
      void Set_must_have_year(bool Must_have = true)
         {Must_have_year = Must_have;};// Set the Must_have_year flag.
      void Read(const char *Str);      // Read a date (in normal format) from string
      void Read(istream& In_stream);   // Read a date (in normal format) from in stream
      void Write(char *Str);           // Write the date to string.
      void Write(ostream& Out_stream); // Write the date to the output stream
      void Set_write_option(Write_format Current_format)
         {Current_write_format = Current_format;};
                                       // Set the format of the next print.
      bool Is_valid(void)  {return (Julian_day != 0);};
                                       // Return TRUE if date is valid.
      bool Is_leap_year(int Year = -1);// Return TRUE if year is a leap year
      unsigned int Get_day(void);      // Return day
      unsigned int Get_month(void);    // Return month
      unsigned int Get_year(void);     // Return year
      void Get_dmy(unsigned int& D, unsigned int& M, unsigned int& Y);
                                       // Return day, month, and year.
      unsigned long Get_jday(void) const {return Julian_day;};
                                       // Return the julian day number
      unsigned int Get_days_in_year(void)  {return Is_leap_year() ? 366 : 365;};
      unsigned int Get_day_of_year(void);
                                       // Return day number
      int Get_days_in_month(unsigned int Month, unsigned int Year);
                                       // Return number of days in month.
      void Add_months(int Num_months = 1);
                                       // Add set number of months to date.
                                       // Day is set to 1st of month.

      // operators

      int operator == (const GDate& D)
         {
         return (Julian_day == D.Get_jday());
         }                             // == operator
      int operator != (const GDate& D)
         {
         return (Julian_day != D.Get_jday());
         }                             // != operator
      int operator < (const GDate& D)
         {
         return (Julian_day < D.Get_jday());
         }                             // < operator
      int operator <= (const GDate& D)
         {
         return (Julian_day <= D.Get_jday());
         }                             // <= operator
      int operator > (const GDate& D)
         {
         return (Julian_day > D.Get_jday());
         }                             // > operator
      int operator >= (const GDate& D)
         {
         return (Julian_day >= D.Get_jday());
         }                             // >= operator
      GDate& operator += (const int Num_to_add)
         {
         Julian_day += Num_to_add;
         return *this;
         }                             // += operator
      GDate& operator -= (const int Num_to_subtract)
         {
         Julian_day -= Num_to_subtract;
         return *this;
         }                             // -= operator
      GDate& operator + (const int Num_to_add)
         {
         Julian_day += Num_to_add;
         return *this;
         }                             // + operator
      GDate& operator ++ (void)
         {
         Julian_day ++;
         return *this;
         }                             // ++ operator
      GDate& operator -- (void)
         {
         Julian_day --;
         return *this;
         }                             // -- operator
      GDate& operator - (const int Num_to_add)
         {
         Julian_day -= Num_to_add;
         return *this;
         }                             // - operator
      long operator - (const GDate& Date)
         {
         return Julian_day - Date.Get_jday();
         }                             // - operator
      friend ostream& operator << (ostream& Out_stream, GDate& Date)
         {
         Date.Write(Out_stream);
         return Out_stream;
         }
      friend istream& operator >> (istream& In_stream, GDate& Date)
         {
         Date.Read(In_stream);
         return In_stream;
         }

   DECLARE_STREAMABLE(GOBJS_CLASS_EXPORT, GDate, 1);
   };

#include <classlib\arrays.h>

typedef TArrayAsVector <GDate> Date_array;
#endif


