#include <windows.h>
#pragma hdrstop
#include <stdexcept>
#include "variant.h"
#include "ProtocolVector.h"
using namespace protocol;

// ------------------------------------------------------------------
//  Short description:
//     Delete a FORTRAN variant - really a messageData

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
/*extern "C" void __stdcall unpack_string
   (Variant** variant, char* value, unsigned int valueLength)
   {
   FString st;
   (*variant)->unpack(st);
   FString(value, valueLength) = st;
  }
// ------------------------------------------------------------------
//  Short description:
//    return a double precision value from message

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" void __stdcall unpack_string_array
   (Variant** variant, char* values, unsigned* maxValues, unsigned* numValues,
    unsigned valuesLength)
   {
   FStrings strings(values, valuesLength, *maxValues, 0);
  (*variant)->unpack(strings);
   *numValues = strings.getNumElements();
   }

// ------------------------------------------------------------------
//  Short description:
//    unpack an integer from message.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" void __stdcall unpack_integer4
   (Variant** variant, int* value)
   {
   (*variant)->unpack(*value);
   }
// ------------------------------------------------------------------
//  Short description:
//    return a vector of integers from message

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" void __stdcall unpack_integer4_array
   (Variant** variant, int* values, unsigned* maxValues, unsigned* numValues)
   {
   vector<int> vectorValues(values, 0, *maxValues);
   (*variant)->unpack(vectorValues);
   *numValues = vectorValues.size();
   }

// ------------------------------------------------------------------
//  Short description:
//    return a real value from message

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" void __stdcall unpack_single
   (Variant** variant, float* value)
   {
   (*variant)->unpack(*value);
   }
// ------------------------------------------------------------------
//  Short description:
//    return a double precision value from message

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" void __stdcall unpack_single_array
   (Variant** variant, float* values, unsigned* maxValues, unsigned* numValues)
   {
   vector<float> vectorValues(values, 0, *maxValues);
   (*variant)->unpack(vectorValues);
   *numValues = vectorValues.size();
   }

// ------------------------------------------------------------------
//  Short description:
//    return a double precision value from message

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" void __stdcall unpack_double
   (Variant** variant, double* value)
   {
   (*variant)->unpack(*value);
   }
// ------------------------------------------------------------------
//  Short description:
//    return a double precision value from message

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" void __stdcall unpack_double_array
   (Variant** variant, double* values, unsigned* maxValues, unsigned* numValues)
   {
   vector<double> vectorValues(values, 0, *maxValues);
   (*variant)->unpack(vectorValues);
   *numValues = vectorValues.size();
   }

// ------------------------------------------------------------------
//  Short description:
//    return a logical value from message

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" void __stdcall unpack_boolean
   (Variant** variant, unsigned int* value)
   {
   bool b;
   (*variant)->unpack(b);
   *value = b;
   }

*/
