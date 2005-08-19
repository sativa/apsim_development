#include <windows.h>
#pragma hdrstop

#include "ProtocolVector.h"

namespace protocol {
// ------------------------------------------------------------------
//  Short description:
//     Display a "too many items error message".

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
void _export tooManyError(unsigned int maxCount)
   {
   char st[500];
   strcpy(st, "Internal Error - Too many items have been inserted in the \n");
   strcat(st, "component interface vector.  Maximum number of items: ");
   itoa(maxCount, &st[strlen(st)], 10);
   ::MessageBox(NULL, st, "Internal error", MB_ICONSTOP | MB_OK);
   }

// ------------------------------------------------------------------
//  Short description:
//     Display a "range error message".

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
void _export rangeError(unsigned int index, unsigned int maxCount)
   {
   char st[500];
   strcpy(st, "Internal Error - Invalid item index passed into \n");
   strcat(st, "component interface vector. \n");
   strcat(st, "Index: ");
   itoa(index, &st[strlen(st)], 10);
   strcat(st, " Maximum number of items: ");
   itoa(maxCount, &st[strlen(st)], 10);
   ::MessageBox(NULL, st, "Internal error", MB_ICONSTOP | MB_OK);
   }
} // namespace protocol
