#include <stdlib.h>
#include <string.h>
#include <stdexcept>

#include <ApsimShared/fstring.h>

#include "MessageData.h"
#include "message.h"
#include "ProtocolVector.h"

namespace protocol {

// FSTRING specialisations
MessageData& _export operator>>(MessageData& messageData, FString& value)
   {
   unsigned int numChars;
   messageData >> (int)numChars;
   value.aliasTo(messageData.ptr(), numChars);
   messageData.movePtrBy(numChars);
   return messageData;
   };
MessageData& _export operator<<(MessageData& messageData, const FString& value)
   {
   messageData << (int)value.length();
   messageData.copyFrom(value.f_str(), value.length());
   return messageData;
   }
unsigned int _export memorySize(const FString& value)
   {
   return value.length() + sizeof(int);
   }

// FSTRINGS specialisations
MessageData& _export operator>> (MessageData& messageData, FStrings& strings)
   {
   unsigned numElements;
   messageData >> numElements;
   for (unsigned int i = 0; i < numElements; i++)
      {
      FString rhs;
      messageData >> rhs;
      strings.addString(rhs);
      }
   return messageData;
   }
MessageData& _export operator<< (MessageData& messageData, const FStrings& strings)
   {
   messageData << strings.getNumElements();
   for (unsigned int i = 0; i < strings.getNumElements(); i++)
      messageData << strings.getString(i);

   return messageData;
   }
unsigned int _export memorySize(const FStrings& strings)
   {
   unsigned size = 4;
   for (unsigned int i = 0; i < strings.getNumElements(); i++)
      size += memorySize(strings.getString(i));
   return size;
   }

} // end namespace protocol

