#if !defined(__GDATECOL_H)
#define __GDATECOL_H

#include <gobjs\gcolumn.h>
#include <gobjs\gdate.h>
#include <assert.h>
#include <stdio.h>

// *******************************************************************
       class GOBJS_CLASS_EXPORT GDate_column : public GColumn  {
// *******************************************************************

//  Short description:
//      Base data object for storing a REAL column of data.

//  Notes:

//  Changes:
//      DPH 5/2/96

// ------------------------- Private section -------------------------

   private :

//   Private data
//      none

//   Private member functions
//      none

// ------------------------- Protected section -----------------------

   protected :

//   Protected data
      GDate *Date_array;               // date dynamic array
      char Conversion[MAX_CHAR_SIZE];  // used for conversion purposes.

//   Protected member functions
      void Init(int Max_items);        // initialise object.

// -------------------------- Public section -------------------------

   public :

//   Public data
//      none

//   Public member functions
      GDate_column(void)
         { };                          // constructor.

      GDate_column(int Max_items)
         {
         Init(Max_items);
         };                            // constructor
      ~GDate_column(void);             // destructor

      void Add_char(char *c)
         {
         GDate d;
         d.Read(c);
         AddAt_date(Num_elements, d);
         Num_elements++;
         };                            // add a real number to array.
      void Add_date(GDate& d)
         {
         assert (Num_elements < Max_elements);
         AddAt_date(Num_elements, d);
         Num_elements++;
         };                            // add a real number to array.
      void AddAt_date(int loc, GDate& d)
         {
         assert (loc >= 0 && loc < Max_elements);
         Date_array[loc] = d;
         float f = d.Get_jday();
         Minimum = min(Minimum, f);
         Maximum = max(Maximum, f);
         };                            // set a real value at specified location

      float Get_real(int loc)
         {
         assert (loc <= Max_elements);
         return Date_array[loc].Get_jday();
         };                            // return real number
      const char *Get_char(int loc)
         {
         assert (loc <= Max_elements);
         Date_array[loc].Write(Conversion);
         return Conversion;
         };                            // return character string.

      DECLARE_STREAMABLE(GOBJS_CLASS_EXPORT, GDate_column, 1);
   };

#endif
