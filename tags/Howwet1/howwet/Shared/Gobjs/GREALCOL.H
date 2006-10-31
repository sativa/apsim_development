#if !defined(__GREALCOL_H)
#define __GREALCOL_H

#include <gobjs\gcolumn.h>
#include <assert.h>
#include <stdio.h>

// *******************************************************************
       class GOBJS_CLASS_EXPORT GReal_column : public GColumn  {
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
      float *Real_array;               // real dynamic array
      char Conversion[MAX_CHAR_SIZE];  // used for conversion purposes.

//   Protected member functions
      void Init(int Max_items);        // initialise object.

// -------------------------- Public section -------------------------

   public :

//   Public data
//      none

//   Public member functions
      GReal_column(void)
         { };                          // constructor.

      GReal_column(int Max_items)
         {
         Init(Max_items);
         };                            // constructor
      ~GReal_column(void);             // destructor

      void Add_real(float f)
         {
         assert (Num_elements < Max_elements);
         AddAt_real(Num_elements, f);
         Num_elements++;
         };                            // add a real number to array.
      virtual void Add_char(char *Char_ptr)
         {
         assert (Num_elements < Max_elements);
         AddAt_char(Num_elements, Char_ptr);
         Num_elements++;
         };                            // add a character to array.
      virtual void AddAt_real(int loc, float f)
         {
         assert (loc >= 0 && loc < Max_elements);
         Real_array[loc] = f;
         if (f < 1e10)
            {
            Minimum = min(Minimum, Real_array[loc]);
            Maximum = max(Maximum, Real_array[loc]);
            }
         };                            // set a real value at specified location
      virtual void AddAt_char(int loc, char *Char_ptr)
         {
         assert (loc >= 0 && loc < Max_elements);

         char *End_pos;
         float Number = strtod(Char_ptr, &End_pos);
         if (*End_pos != 0)
            Number = 0.0;
         AddAt_real(loc, Number);
         };                            // set a char value at specified location

      float Get_real(int loc)
         {
         assert (loc <= Max_elements);
         return Real_array[loc];
         };                            // return real number
      const char *Get_char(int loc)
         {
         assert (loc <= Max_elements);
         sprintf(Conversion, "%8.3f", Real_array[loc]);
         return Conversion;
         };                            // return character string.

      DECLARE_STREAMABLE(GOBJS_CLASS_EXPORT, GReal_column, 1);
   };

#endif

