#include <stdlib.h>
#include "date.h"

/*
***********
* Date utilities - derived from NF's grasp code
**************
*/

// A set of routines for doing things with dates. 
// Tells if a year is a leap year. Returns true if it is, false otherwise.
// year is a four digit A.D. year number.
// According to the Gregorian calender (i.e. the one we follow) a year is
// a leap year if it is divisible by four and not by 100, except for those
// which are divisible by 400, which are also leap years.
int leapYear(int year) {

  if (year % 4 != 0) {
    return(0);
  } else {
    if ((year%100 == 0) && (year%400 != 0)) {
      return(0);
    } else {
      return(1);
    }
  }
  /* notreached */
}


// Routine for telling the number of days in a given month, in a particular
// year. Accounts for leap years according to Gregorian calendar 
// (i.e. properly).
// month is the month number (1-12), year is the four digit year number A.D.
//
int Date::daysInMonth(int month, int year) const {

  static int  numDays[12] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

  if (month != 2) {
    return(numDays[month-1]);
  } else {
    if (leapYear(year)) {
      return(numDays[month-1] + 1);
    } else {
      return(numDays[month-1]);
    }
  }
  /* notreached */
}

// Tells if a year is a leap year. Returns true if it is, false otherwise.
// year is a four digit A.D. year number.
// According to the Gregorian calender (i.e. the one we follow) a year is
// a leap year if it is divisible by four and not by 100, except for those
// which are divisible by 400, which are also leap years.
int Date::leapYear(int year) const{

  if (year % 4 != 0) {
    return(0);
  } else {
    if ((year%100 == 0) && (year%400 != 0)) {
      return(0);
    } else {
      return(1);
    }
  }
  /* notreached */
}



// Routine to convert a year, month into a single day number. The number is
// guaranteed to be properly ordered i.e. daynumbers are in the same order 
// as the dates to which they correspond. They are also guaranteed to be
// contiguous i.e. there are no 'holes' in the sequence of daynumbers which
// don't correspond to any date_ The day number for a given year, month
// is the day number previous day of the last month.
//
int Date::toDayNum(int year, int month) const{
  int i, daynum;
  int daysin400, daysin100, daysinyear = 365;

  daysin100 = 100*daysinyear+99/4;
  daysin400 = 4*daysin100+1;
  int yr;

  // Calculate number of days up to end of last year.
  yr = year-1;
  daynum = 0;

  daynum = daynum + (yr/400)*daysin400;
  yr = yr - 400*(yr/400);

  daynum = daynum + (yr/100)*daysin100;
  yr = yr - 100*(yr/100);

  daynum = daynum + yr*daysinyear + yr/4;

  // Add all the days so far this year.
  for (i = 1; i < month; i++) {
    daynum = daynum + daysInMonth(i, year);
  }

  return(daynum);
}


// Reverse of the above routine. Converts from a daynumber to the corresponding
// year and month. 
void Date::toYearMonth(int dayNum, int *year, int *month) const{
  int dn;
  int daysin400, daysin100, daysinyear = 365, daysin4;

  daysin100 = 100*daysinyear+99/4;
  daysin400 = 4*daysin100+1;
  daysin4 = 4*daysinyear+1;

  dn = dayNum;
  *year = 1;
  *year = *year + (dn/daysin400)*400;
  dn = dn - daysin400*(dn/daysin400);

  if ((dn+1) == daysin400) {
    *year = *year + ((dn-1)/daysin100)*100;
    dn = dn - daysin100*((dn-1)/daysin100);
  } else {
    *year = *year + (dn/daysin100)*100;
    dn = dn - daysin100*(dn/daysin100);
  }

  *year = *year + (dn/daysin4)*4;
  dn = dn - daysin4*(dn/daysin4);
    
  if ((dn/daysinyear)% 4 == 0) {
    *year = *year + (dn-1)/daysinyear;
    dn = dn - daysinyear*((dn-1)/daysinyear);
  } else {
    *year = *year + dn/daysinyear;
    dn = dn - daysinyear*(dn/daysinyear);
  }

  *month = 1;
  while (dn >= daysInMonth(*month, *year)) {
    dn = dn - daysInMonth(*month, *year);
    (*month)++;
  }

  return;
}


// Routine to convert year, month to a month number
int Date::toMonthNum(int year, int month) const {
  int mn;

  mn = (year-1800)*12;
  mn = mn + month - 1;

  return(mn);
}

// Routine to invert the above.
void Date::mnToYearMonth(int mn, int *year, int *month) const {

  *year = mn/12 + 1800;
  *month = mn - (*year-1800)*12 + 1;

  return;
}

// Routine to convert a year, month, day into a single day number. The number is
// guaranteed to be properly ordered i.e. daynumbers are in the same order 
// as the dates to which they correspond. They are also guaranteed to be
// contiguous i.e. there are no 'holes' in the sequence of daynumbers which
// don't correspond to any date_ 
//
int Date::dateToDayNum(int year, int month, int day) const {
  int daynum;

  daynum = toDayNum(year, month);
  daynum = daynum + day - 1;

  return(daynum);
}


// Reverse of the above routine. Converts from a daynumber to the corresponding
// year, month, and day 
void Date::DNtoDate(int dayNum, int *year, int *month, int *day) const {
  
  toYearMonth(dayNum, year, month);
  *day = dayNum - toDayNum(*year, *month);
  *day = *day + 1;
  return;
}


Date::Date(int year, int month, int day) {
  dayNumber = dateToDayNum(year, month, day);
}

Date::Date(int dayNum) {
  dayNumber = dayNum;
}

void Date::encode(int year, int month, int day) {
  dayNumber = dateToDayNum(year, month, day);
}

void Date::decode(int *year, int *month, int *day) const {
  DNtoDate(dayNumber, year, month, day);
}

bool operator== (const Date &x, const Date &y){
  return (x.dayNumber == y.dayNumber);
}

bool operator< (const Date &x, const Date &y){
  return (x.dayNumber < y.dayNumber);
}

bool operator> (const Date &x, const Date &y){
  return (x.dayNumber > y.dayNumber);
}

#ifdef USE_IOSTREAM
class ostream& operator << (ostream &o, const Date &d){
  int day, month, year;
  d.decode(&year, &month, &day);
  o << day << "/" << month << "/" << year;
  return o;
}
#endif

