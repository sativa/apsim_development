#include "MessageData.h"
#include "message.h"
#include "ProtocolVector.h"
using namespace protocol;
/*// ------------------------------------------------------------------
//  Short description:
//     FORTRAN: Create a messageData object

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" unsigned  __stdcall new_variant(Message** message, Message** dummy)
   {
   return (unsigned) new MessageData(*message);
   }

// ------------------------------------------------------------------
//  Short description:
//     FORTRAN: Delete a messageData object.

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" void  __stdcall delete_variant(MessageData** messageData)
   {
   delete *messageData;
   }

// ------------------------------------------------------------------
//  Short description:
//    Store a integer*4 in message

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" void __stdcall pack_integer4
   (MessageData** mesData, int* value)
   {
   MessageData& messageData = **mesData;
   messageData << *value;
   }
// ------------------------------------------------------------------
//  Short description:
//    store a vector of integers in message

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" void __stdcall pack_integer4_array
   (MessageData** mesData, int* values, unsigned* numValues)
   {
   MessageData& messageData = **mesData;
   vector<int> v(values, *numValues, *numValues);
   messageData << v;
   }

// ------------------------------------------------------------------
//  Short description:
//    store a real value in message

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" void __stdcall pack_single
   (MessageData** mesData, float* value)
   {
   MessageData& messageData = **mesData;
   messageData << *value;
   }
// ------------------------------------------------------------------
//  Short description:
//    store a double precision value in message

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" void __stdcall pack_single_array
   (MessageData** mesData, float* values, unsigned* numValues)
   {
   MessageData& messageData = **mesData;
   vector<float> v(values, *numValues, *numValues);
   messageData << v;
   }

// ------------------------------------------------------------------
//  Short description:
//    Store a string in message

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" void __stdcall pack_string
   (MessageData** mesData, const char* value, unsigned int valueLength)
   {
   MessageData& messageData = **mesData;
   messageData << FString(value, valueLength);
   }

// ------------------------------------------------------------------
//  Short description:
//    store a string array in message

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" void __stdcall pack_string_array
   (MessageData** mesData, char* values, unsigned* numValues,
    unsigned valuesLength)
   {
   FStrings strings(values, valuesLength, *numValues, *numValues);
   (**mesData) << strings;
   }

// ------------------------------------------------------------------
//  Short description:
//    store a double precision value in message

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" void __stdcall pack_double
   (MessageData** mesData, double* value)
   {
   MessageData& messageData = **mesData;
   messageData << *value;
   }

// ------------------------------------------------------------------
//  Short description:
//    store a double precision value in message

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" void __stdcall pack_double_array
   (MessageData** mesData, double* values, unsigned* numValues)
   {
   MessageData& messageData = **mesData;
   vector<double> v(values, *numValues, *numValues);
   messageData << v;
   }

// ------------------------------------------------------------------
//  Short description:
//    store a logical value in message

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" void __stdcall pack_boolean
   (MessageData** mesData, unsigned int* value)
   {
   MessageData& messageData = **mesData;
   messageData << (bool)*value;
   }
*/
