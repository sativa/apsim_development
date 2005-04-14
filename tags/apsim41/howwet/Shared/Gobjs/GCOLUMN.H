#if !defined(__GCOLUMN_H)
#define __GCOLUMN_H

#include <classlib\objstrm.h>
#include <gobjs\gstring.h>
#define MAX_TITLE_SIZE        100
#define MAX_HEADING_SIZE       40
#define MAX_UNIT_SIZE          20
#define MAX_CHAR_SIZE          60

// *******************************************************************
       class GOBJS_CLASS_EXPORT GColumn : public TStreamableBase {
// *******************************************************************

//  Short description:
//      Base data object for storing a column of data.

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
      GString Heading;                 // heading of column
      GString Unit;                    // units for column
      int Num_elements;                // number of elements in array
      double Dummy;                    // dummy variable
      int Max_elements;                // maximum number of elements that can fit
                                       // in array.
      float Minimum;
      float Maximum;

//   Protected member functions

// -------------------------- Public section -------------------------

   public :

//   Public data
//      none

//   Public member functions
      GColumn(void);         // constructor
      virtual ~GColumn(void);// destructor

      void Set_heading(const char *Head)
         {
         Heading = Head;
         };                            // set heading
      void Set_unit(const char *Un)
         {
         Unit = Un;
         };                            // set unit
      const char *Get_heading(void)
         {
         return Heading.c_str();
         };                            // return heading to caller
      const char *Get_unit(void)
         {
         return Unit.c_str();
         };                            // return unit to caller


      virtual void Add_real(float)
         {
         };                            // add a real number to array.
      virtual void Add_char(char *)
         {
         };                            // add a character to array.
      virtual void AddAt_real(int loc, float)
         {
         };                            // set a real value at specified location
      virtual void AddAt_char(int loc, char *)
         {
         };                            // set a char value at specified location

      virtual float Get_real(int loc)
         {
         return 123.0;
         };                            // return real number
      virtual const char *Get_char(int loc)
         {
         return "";
         };                            // return character string.

      int Get_num_elements (void)
         {
         return Num_elements;
         };                            // return number of elements in array.
      void Flush(void)
         {
         Num_elements = 0;
         };                            // clear all elements in list
      float Get_minimum(void)
         {
         return Minimum;
         };                            // return minimum value.
      float Get_maximum(void)
         {
         return Maximum;
         };                            // return minimum value.

      DECLARE_STREAMABLE(GOBJS_CLASS_EXPORT, GColumn, 1);
   };

#endif