#if !defined(__GCHARCOL_H)
#define __GCHARCOL_H

#include <gobjs\gcolumn.h>
#include <assert.h>
#include <stdio.h>

struct Char
   {
   char St[MAX_CHAR_SIZE];
   };

// *******************************************************************
       class GOBJS_CLASS_EXPORT GChar_column : public GColumn  {
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
      Char *Char_array;                // char dynamic array

//   Protected member functions
      void Init(int Max_items);        // initialise object.

// -------------------------- Public section -------------------------

   public :

//   Public data
//      none

//   Public member functions
      GChar_column(void)
         {
         Max_elements = 0;
         };                            // constructor
      GChar_column(int Max_items)
         {
         Init(Max_items);
         };                            // constructor
      ~GChar_column(void);             // destructor

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
      void AddAt_real(int loc, float f)
         {
         assert (loc >= 0 && loc < Max_elements);
         sprintf(Char_array[loc].St, "%8.3f", f);
         Minimum = 0;
         Maximum = Num_elements;
         };                            // set a real number in array.
      virtual void AddAt_char(int loc, char *Char_ptr)
         {
         assert (loc >= 0 && loc  < Max_elements);
         strncpy(Char_array[loc].St, Char_ptr, MAX_CHAR_SIZE);
         Minimum = 0;
         Maximum = Num_elements;
         };                            // add a character to array.

      float Get_real(int loc)
         {
         assert (loc <= Max_elements);
         return loc + 1;
         };                            // return real number.  add 1 to loc because
                                       // we want to return numbers from 1 to Num_elements
                                       // rather than from 0 to Num_elements - 1
      const char *Get_char(int loc)
         {
         assert (loc <= Max_elements);
         return (const char*) &Char_array[loc].St;
         };                            // return character string.

      DECLARE_STREAMABLE(GOBJS_CLASS_EXPORT, GChar_column, 1);
   };

#endif
