#ifndef DATE_H
#define DATE_H

class Date {
 private:
  int dayNumber;

  void DNtoDate(int dayNum, int *year, int *month, int *day) const;
  int dateToDayNum(int year, int month, int day) const;
  void mnToYearMonth(int mn, int *year, int *month) const;
  int toMonthNum(int year, int month) const;
  void toYearMonth(int dayNum, int *year, int *month) const;
  int toDayNum(int year, int month) const;
  int leapYear(int year) const;
  int daysInMonth(int month, int year)const;
 public:
  Date(int, int, int);
  Date(int);
  Date() {dayNumber = 0;};
  void encode(int, int, int);
  void decode(int*, int*, int*) const;
  int  intval(){return dayNumber;};
  friend bool operator== (const Date &, const Date &);
  friend bool operator< (const Date &, const Date &);
  friend bool operator> (const Date &, const Date &);
};
int leapYear(int);

#ifdef USE_IOSTREAM
class ostream& operator << (ostream &, const Date &);
#endif

#endif
