#include <windows.h>
#pragma hdrstop

#include <stdexcept>
#include "Variants.h"
using namespace protocol;

// ------------------------------------------------------------------
//  Short description:
//     Return the number of Variant objects in specified variants.

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" unsigned __stdcall variants_size(const Variants& variants)
   {
   return variants.size();
   }

