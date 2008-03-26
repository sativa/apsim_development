#pragma hdrstop

#include "ProtocolVector.h"
#include <general/string_functions.h>
#include <general/platform.h>
#include <stdexcept>
namespace protocol {
// ------------------------------------------------------------------
//  Short description:
//     Display a "too many items error message".

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
void EXPORT tooManyError(unsigned int maxCount)
   {
   char st[500];
   strcpy(st, "Internal Error - Too many items have been inserted in the \n");
   strcat(st, "component interface vector.  Maximum number of items: ");
   strcat(st, itoa(maxCount).c_str());
   throw std::runtime_error(st);
   }

// ------------------------------------------------------------------
//  Short description:
//     Display a "range error message".

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
void EXPORT rangeError(unsigned int index, unsigned int maxCount)
   {
   char st[500];
   strcpy(st, "Internal Error - Invalid item index passed into \n");
   strcat(st, "component interface vector. \n");
   throw std::runtime_error(st);
   }
} // namespace protocol
