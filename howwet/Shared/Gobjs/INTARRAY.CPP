#include <consts.h>
#include <gobjs\intarray.h>

// *******************************************************************
      int Find_int_in_array(int Integer, int Arr[], int Num_elements)  {
// *******************************************************************

//  Short description:
//    Find the specified integer in the specified integer array.

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables
      bool Found = FALSE;              // Found the element yet?

// -------------------- Executable code section ----------------------

   int Element;
   for (Element = 0;
        Element < Num_elements && !Found;
        Element++)
      {
      Found = (Arr[Element] == Integer);
      }
   if (Found)
      return Element - 1;

   else
      return -1;
   }

// *******************************************************************
      int Find_maximum_in_int_array(int Arr[], int Num_elements)  {
// *******************************************************************

//  Short description:
//    Find the maximum integer in the specified integer array.

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables
      int Max;

// -------------------- Executable code section ----------------------

   Max = Arr[0];

   for (int Element = 1;
        Element < Num_elements;
        Element++)
      {
      if (Arr[Element] > Max)
         Max = Arr[Element];
      }

   return Max;
   }

// *******************************************************************
      int Count_ints_in_array(int Integer, int Arr[], int Num_elements)  {
// *******************************************************************

//  Short description:
//    Count the specified integer in the specified integer array.

//  Notes:

//  Changes:
//    DPH 8/8/95

//  Calls:

//  Internal variables
      int Num_found = 0;               // Number found in array

// -------------------- Executable code section ----------------------

   for (int Element = 0;
        Element < Num_elements;
        Element++)
      {
      if (Arr[Element] == Integer)
         Num_found++;
      }

   return Num_found;
   }

