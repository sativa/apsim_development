#include <stdio.h>
#include <math.h>
#include <vector>
#include <string>
#include "Plantlibrary.h"

//==========================================================================
int offset_day_of_year (int iyr,     //(INPUT) day of year number
                        int doy,     //(INPUT) year
                        int ndays)   //(INPUT) number of days to adjust by
//==========================================================================

/* Purpose
*       adds or subtracts specified days to/from day of year number
*
*  Definition
*     Returns the day of year for the day "ndays" after the day
*     specified by the day of year, "doy", in the year, "iyr".
*     "ndays" may well be negative.
*
*  Mission Statement
*      %1 offset by %3 days
*
* Changes
*       21/5/2003 ad converted to BC++
*       4-mar-91 programmed jngh
*       051191  jngh changed algorithm to use julian days - cr166, cr168
*                    added variable descriptions - cr167
*       100393  jngh changed date_to_jday arguments to integer. Impacted
*                    jday_to_date arguments.
*       010494  jngh put year in argument
*       21/05/2003 ad converted to BC++
*/
   {
   double days;                     // julian day number of new date
   int day;                         //day of month
   int month;                       // month number of year
   int year;                        // year
   int check_day = 1;
   int check_month =1;

   // Implementation Section ----------------------------------
   days = date_to_jday(check_day, check_month, iyr) -
                double(1.0) + double(doy) + double(ndays);
   jday_to_date (&day, &month, &year, days);

   // now get the day of year number
   return (int(days - date_to_jday(check_day, check_month, year) +
         double(1.0) + double(0.5)));
   }
//==========================================================================
bool leap_year(int *year)
//==========================================================================

/*Purpose
 *   Returns TRUE if year YEAR is a leap year
 *Parameters
 *   year: year
 *Calls
 */
   {
   //Local Variables
   bool y4;       //Leap year - 4th year
   bool y100;     //Not leap year - century
   bool y400;     //Leap year - 4th century
   bool leapY;    // the result of this function

   //Implementation

   //Divisible by 4 ?
   y4 = (*year % 4 == 0);

   //divisible by 100 - centesimal year?
   y100 = (*year % 100 == 0);

   //divisible by 400 ?
   y400 = (*year % 400 == 0);

   //is the year is divisible by 4 but not by 100;
   //or is the year is divisible by 400.
   leapY = (y4 && (!y100)) || y400;

   return leapY;
   }

//==========================================================================
double date_to_jday (int day, int month, int year)
//==========================================================================

/*Purpose
 *   return a date as a Julian day number
 *Assumptions
 *   Assumes the date is after 1583. if not the function returns 0.0.
 *Parameters
 *   day:   day
 *   month: month
 *   year:  year
 *Calls
 *   check_date
 */
   {
   double jday = 0.0;

   //Implementation
   if (year > 1582 && check_date(day, month, year))
      {
      // Fliegel calculations	
      double quotnt = int ((month - 14.0)/12.0);
      jday = day - 32075.0
         + int(1461.0* (year + 4800.0 + quotnt) /4.0)
         + int(367.0* (month - 2.0 - quotnt*12.0) /12.0)
         - int(3.0 * int((year + 4900.0 + quotnt) /100.0) /4.0);
      }
   return jday;
   }

//==========================================================================
void jday_to_date (int *day, int *month, int *year, double jday)
//==========================================================================

/*Purpose
 *   return a date from a julian day
 *Parameters
 *   day:   day
 *   month: month
 *   year:  year
 *Calls
 */

   {
   //Local variables
   double dayd;         //day, long double type
   double monthd;       //month, long double type
   double yeard;        //year, long double type
   double mm;           //temp. veriable
   double yy;           //temp. veriable
   double work;         //temp. veriable
   double work0;        //temp. veriable

   //Implementation

   //Check julian date and option are legal
   if (jday > 0.0)
      {
      //fliegel and van flanden algorithm
      work = jday + 68569.0;
      work0 = int(4.0 * work / 146097.0);
      work = work - int((146097.0 * work0 + 3.0) / 4.0);
      yy = int(4000.0 * (work + 1.0) / 1461001.0);

      work = work - int(1461.0 * yy / 4.0) + 31.0;
      mm = int(80.0 * work / 2447.0);
      dayd = work - int(2447.0 * mm / 80.0);

      work = int(mm / 11.0);
      monthd = mm + 2.0 - 12.0 * work;
      yeard = 100.0 * (work0 - 49.0) + yy + work;

      *day = int(dayd + 0.5);
      *month = int(monthd + 0.5);
      *year = int(yeard + 0.5);
      }
   else
      {
      *day = 0;
      *month = 0;
      *year = 0;
      }
   }

//     ===========================================================
float day_length (int dyoyr,      // (INPUT) day of year number
                  float lat,      // (INPUT) latitude of site (deg)
		  float sun_angle) // (INPUT) angle to measure time between
			   // such as twilight (deg).
			   // angular distance between 90 deg
			   // and end of twilight - altitude
			   // of sun. +ve up, -ve down.
 {

   //+ Purpose
   //      return the time elasped in hours between the specified sun angle
   //      from 90 deg in am and pm. +ve above the horizon, -ve below the horizon.

   //+ Notes
   //                    there is a small err in cos (90), thus a special
   //                    case is made for this.

   //+  Mission Statement
   //      day length for %1 and %2

   //+ Changes
   //       020392 jngh specified and programmed
   //       130592 jngh limited altitude for twilight to increase range_of
   //                   of latitudes. - cr324
   //                   limited cos of the hourangle between -1 and 1 - cr324
   //       190592 jngh renamed day_length routine - cr323
   //       290592 jngh set cos hourangle to +/-1 when latitude is +/- 90 - cr350
   //                   corrected descriptions - cr352
   //       230792 jngh corrected coshra to take account of latitude sign - cr401
   //       200893 jngh corrected problem with precision which occurred when
   //                   latitude is very close to tropic line and declination
   //                   is also very close, abs(slsd-clcd) may go slightly above
   //                   1.0, which asin doesn't like.
   //       071293 jngh added sun (twilight) angle to arguments
   //       270295 jngh put in function to test for equal reals.


   //+ Constant Values
   const double  aeqnox = 82.25 ;//  average day number of autumnal equinox

   const double  pi = 3.14159265359 ;

   const double  dg2rdn =  (2.0*pi) /360.0 ; // convert degrees to radians

   const double  decsol = 23.45116 * dg2rdn ; // amplitude of declination of sun
                                            //   - declination of sun at solstices.
   // cm says here that the maximum
   // declination is 23.45116 or 23 degrees
   // 27 minutes.
   // I have seen else_where that it should
   // be 23 degrees 26 minutes 30 seconds -
   // 23.44167
   const double  dy2rdn =  (2.0*pi) /365.25 ; // convert days to radians
   const double  rdn2hr = 24.0/(2.0*pi)  ; // convert radians to hours

   //+ Local Variables
   double alt;// twilight altitude limited to max/min
   //   sun altitudes end of twilight
   //   - altitude of sun. (radians)
   double altmn;// altitude of sun at midnight
   double altmx;// altitude of sun at midday
   double clcd;// cos of latitude * cos of declination
   double coshra;// cos of hour angle - angle between the
   //   sun and the meridian.
   double dec;// declination of sun in radians - this
   //   is the angular distance at solar
   //   noon between the sun and the equator.
   double hrangl;// hour angle - angle between the sun
   //   and the meridian (radians).
   double hrlt;// day_length in hours
   double latrn;// latitude in radians
   double slsd;// sin of latitude * sin of declination
   double sun_alt;// angular distance between
 // sunset and end of twilight - altitude
 // of sun. (radians)
 // Twilight is defined as the interval
 // between sunrise or sunset and the
 // time when the true centre of the sun
 // is 6 degrees below the horizon.
 // Sunrise or sunset is defined as when
 // the true centre of the sun is 50'
 // below the horizon.

 //- Implementation Section ----------------------------------

   sun_alt = sun_angle * dg2rdn;

   // calculate daylangth in hours by getting the
   // solar declination (radians) from the day of year, then using
   // the sin and cos of the latitude.

   // declination ranges from -.41 to .41 (summer and winter solstices)

   dec = decsol*sin (dy2rdn* ((double)dyoyr - aeqnox));

   // get the max and min altitude of sun for today and limit
   // the twilight altitude between these.

   if (reals_are_equal(fabs(lat), 90.0)) {
     //coshra = sign (1.0, -dec) * sign (1.0, lat); XXsign???
   } else {
     latrn = lat*dg2rdn;
     slsd = sin(latrn)*sin(dec);
     clcd = cos(latrn)*cos(dec);

     altmn = asin(min(max(slsd - clcd, -1.0), 1.0));
     altmx = asin(min(max(slsd + clcd, -1.0), 1.0));
     alt = min(max(sun_alt, altmn), altmx);

     // get cos of the hour angle
     coshra = (sin (alt) - slsd) /clcd;
     coshra = min(max(coshra, -1.0), 1.0);
   }

   // now get the hour angle and the hours of light
   hrangl = acos (coshra);
   hrlt = hrangl*rdn2hr*2.0;
   return hrlt;
}

//     ===========================================================
void jday_to_day_of_year (double *julday,// (INPUT) day, month, year
                          int   *dyoyr,  // (OUTPUT) day of year
                          int   *year)   // (OUTPUT) year
//+ Purpose
//     Convert the julian day number to a day of year + year

//+  Mission Statement
//

//+ Changes
//     DPH 11/4/96 programmed
//     dph 11/7/96 added +1 to equation - bug fix
//     dph 26/11/96 added call to jday_to_date - fixes bug with year being undefined.

//+ Local Variables
   {
   int   day;
   int   month;

   // need to get year from julday
   jday_to_date (&day, &month, year, *julday);
   *dyoyr = (*julday) - date_to_jday(1, 1, *year) + 1;
}
// ====================================================================
bool  check_date (int   day,  // (INPUT) Day
                  int   month,// (INPUT) Month
                  int   year) // (INPUT) Year
{
//+ Purpose
//      Checks validity of of a Gregorian date D/M/Y.  Returns true if
//      date is Ok.  False otherwise

//+ Assumptions
//     Valid date range_of :- 1583 to 4200

//+  Mission Statement
//

//+ Changes
//      21/1/91   Specified and programmed JNGH
//      30/6/92   DPH - turned routine into function - removed call to
//      report_date_error
//      11/08/93  DPH - Put comment in assumption section

//+ Constant Values
const int  year_high=4200 ;// Max. valid year
//
const int  year_low=1583 ;// Min valid year

//+ Local Variables
;// Length of each month - days
int   months[13] =  {0, 31, 28,31,30,31,30,31,31,30,31,30,31};

//- Implementation Section ----------------------------------

// Check validity of Year

   if (year<year_low || year>year_high) {
        return(0);
   } else {
        // Set February to 28 or 29
         if (leap_year(&year)) {
            months[2] = 29;
        } else {
            months[2] = 28;
        }

        // Check validity of Month
         if (month<1 || month>12) {
            return(0);
         // Check validity of Day
         } else if (day<1 || day>months[month]) {
            return(0);
         } else {
            //OK.
         }
   }
   return(1);
}

#if 0
ATTIC
//     ===========================================================
extern "C" void _stdcall _export jday_to_date (int   *dayz,// (OUTPUT) day
                   int   *monthz,// (OUTPUT) month
                   int   *yearz,// (OUTPUT) year
                   double *julday)// (INPUT) julian day number
{
//+ Purpose
//       return a date from a julian day

//+ Notes
//            based on the algorithm by fliegel and van flandern in c.acm
//            vol.11 (oct 1968) p.657

//+  Mission Statement
//

//+ Changes
//       100393 jngh changed day, month, year arguments to integer

//+ Calls

//+ Local Variables
double day             ;// day
double mm              ;// temp. variable
double month           ;// month
double work            ;// temp. variable
double work0           ;// temp. variable
double year            ;// year
double yy              ;// temp `variable

//- Implementation Section ----------------------------------

// check julian date and option are legal

      if (*julday>0.0) {

// fliegel and van flanden algorithm:

         work = *julday + 68569.0;
         work0 = floor((4.0*work/146097.0));
         work = work - floor(((146097.0*work0 + 3.0) /4.0));
         yy = floor((4000.0* (work + 1.0) /1461001.0));

         work = work - floor((1461.0*yy/4.0) + 31.0);
         mm = floor((80.0*work/2447.0));
         day = work - floor((2447.0*mm/80.0));

         work = floor((mm/11.0));
         month = mm + 2.0 - 12.0*work;
         year = 100.0* (work0 - 49.0) + yy + work;

         *dayz = floor(day + 0.5);
         *monthz = floor(month+0.5);
         *yearz = floor(year+0.5);

    } else {
         *dayz = 0;
         *monthz = 0;
         *yearz = 0;
    }
}


// ====================================================================
extern "C" double _stdcall _export  date_to_jday (int   *dayz,// (INPUT) Day
                     int   *monthz,// (INPUT) Month
                     int   *yearz)// (INPUT) Year
{
//+ Purpose
//      Return a date as a Julian day number

//+ Assumptions
//      Assumes the date is after 1583. If not the function returns 0.0.

//+ Notes
//      This implementation is only valid for dates in the Gregorian
//      calender (after 15 October 1582).
//      THIS IS BASED ON THE ALGORITHM BY FLIEGEL AND VAN FLANDERN IN
//      C.ACM VOL.11 (OCT,1968) P.657

//+  Mission Statement
//

//+ Changes
//      Original coding by JNGH for AUSSIM - Modified DPH 30/6/92
//       100393  jngh changed date_to_jday arguments to integer.
//       270295  jngh changed real to dble intrinsic function.

//+ Local Variables
double day            ;// Day
double month          ;// Month
double quotnt         ;// Quotient used in Fliegel calculations
double year           ;// Year

//- Implementation Section ----------------------------------

    if (*yearz>1582 && check_date (dayz, monthz, yearz)) {

         day = (double) *dayz;
         month = (double) *monthz;
         year = (double) *yearz;

         quotnt = floor(((month - 14.0)/12.0));

         return (double) (
           day - 32075.0 
         + floor(1461.0* (year + 4800.0 + quotnt) /4.0)
         + floor(367.0* (month - 2.0 - quotnt*12.0) /12.0)
         - floor(3.0*floor((year + 4900.0 + quotnt) /100.0) /4.0)  );

     } else {
         return( 0.0);
     }
/* notreached*/
}
/ATTIC
#endif
