//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ApsimDataTypeData.h"
#include <general\string_functions.h>

#pragma package(smart_init)

// ------------------------------------------------------------------
//  Short description:
//     Return the number of fields this ApsimDataTypeData has.

//  Changes:
//    DPH 19/11/2001
// ------------------------------------------------------------------
unsigned ApsimDataTypeData::getNumFields(void) const
   {
   unsigned count = 0;
   iterator i = begin();
   while (i != end())
      {
      ++count;
      i++;
      }
   return count;
   }


