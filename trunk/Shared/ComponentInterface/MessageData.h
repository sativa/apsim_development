//---------------------------------------------------------------------------
#ifndef MessageDataH
#define MessageDataH
#include <ApsimShared\fstring.h>
#include "message.h"

namespace protocol {

inline unsigned int memorySize(const bool& value)
   {
   return 1;
   }
inline unsigned int memorySize(const byte& value)
   {
   return 1;
   }
inline unsigned int memorySize(const short& value)
   {
   return 2;
   }
inline unsigned int memorySize(const int& value)
   {
   return 4;
   }
inline unsigned int memorySize(const unsigned int& value)
   {
   return 4;
   }
inline unsigned int memorySize(const long int& value)
   {
   return 8;
   }
inline unsigned int memorySize(const float& value)
   {
   return 4;
   }
inline unsigned int memorySize(const double& value)
   {
   return 8;
   }
inline unsigned int memorySize(const char& value)
   {
   return 1;
   }
inline unsigned int memorySize(const WCHAR& value)
   {
   return 2;
   }

// turn of the warnings about "Functions containing for are not expanded inline.
#pragma warn -inl

class MessageData
   {
   private:
      const char* startBuffer;
      char* currentPtr;
      unsigned int numBytesData;
   public:
      MessageData(void) : startBuffer(NULL), currentPtr(NULL) { }
      MessageData(char* dataPtr, unsigned int numBytes)
         : startBuffer((char*)dataPtr),
           currentPtr(dataPtr),
           numBytesData(numBytes)
         {
         }
      MessageData(Message* msg)
         : startBuffer((char*)msg->dataPtr),
           currentPtr(msg->dataPtr),
           numBytesData(msg->nDataBytes)
         {
         }

      void reset(void) {currentPtr = (char*)startBuffer;}
      bool isValid(void) const {return (startBuffer != NULL && currentPtr != NULL
                                        && (unsigned)(currentPtr-startBuffer) < numBytesData);}
      const char* ptr(void) const {return currentPtr;}
      unsigned int dataSize(void) const {return numBytesData;}
      void movePtrBy(unsigned int numBytes)
         {
         currentPtr += numBytes;
         }
      void copyFrom(const char* from, unsigned int numBytes)
         {
         memcpy(currentPtr, from, numBytes);
         currentPtr += numBytes;
         }
      void copyFrom(const MessageData& from)
         {
         memcpy(currentPtr, from.startBuffer, from.numBytesData);
         currentPtr += from.numBytesData;
         }
      void copyTo(char* destination) const
         {
         // assumes that destination is big enough to hold all data.
         memcpy(destination, startBuffer, numBytesData);
         }
      MessageData newMessageDataFromCurrent(void)
         {
         return MessageData(currentPtr, numBytesData - (currentPtr - startBuffer));
         }

      // boolean
      inline MessageData& operator>> (bool& value)
         {
         value = *((byte*)currentPtr);
         currentPtr += memorySize(value);
         return *this;
         }
      inline MessageData& operator<< (const bool& value)
         {
         *((byte*)currentPtr) = value;
         currentPtr += memorySize(value);
         return *this;
         }
      // byte
      inline MessageData& operator>> (byte& value)
         {
         value = *((byte*)currentPtr);
         currentPtr += memorySize(value);
         return *this;
         }
      inline MessageData& operator<< (const byte& value)
         {
         *((byte*)currentPtr) = value;
         currentPtr += memorySize(value);
         return *this;
         }
      // short
      inline MessageData& operator>> (short& value)
         {
         value = *((short*)currentPtr);
         currentPtr += memorySize(value);
         return *this;
         }
      inline MessageData& operator<< (const short& value)
         {
         *((short*)currentPtr) = value;
         currentPtr += memorySize(value);
         return *this;
         }
      // int
      inline MessageData& operator>> (int& value)
         {
         value = *((int*)currentPtr);
         currentPtr += memorySize(value);
         return *this;
         }
      inline MessageData& operator<< (const int& value)
         {
         *((int*)currentPtr) = value;
         currentPtr += memorySize(value);
         return *this;
         }
      // int
      inline MessageData& operator>> (unsigned int& value)
         {
         value = *((unsigned int*)currentPtr);
         currentPtr += memorySize((const int)value);
         return *this;
         }
      inline MessageData& operator<< (const unsigned int& value)
         {
         *((unsigned int*)currentPtr) = value;
         currentPtr += memorySize((const int)value);
         return *this;
         }
      // long int
      inline MessageData& operator>> (long int& value)
         {
         value = *((long int*)currentPtr);
         currentPtr += memorySize(value);
         return *this;
         }
      inline MessageData& operator<< (const long int& value)
         {
         *((long int*)currentPtr) = value;
         currentPtr += memorySize(value);
         return *this;
         }
      // float
      inline MessageData& operator>> (float& value)
         {
         value = *((float*)currentPtr);
         currentPtr += memorySize(value);
         return *this;
         }
      inline MessageData& operator<< (const float& value)
         {
         *((float*)currentPtr) = value;
         currentPtr += memorySize(value);
         return *this;
         }
      // double
      inline MessageData& operator>> (double& value)
         {
         value = *((double*)currentPtr);
         currentPtr += memorySize(value);
         return *this;
         }
      inline MessageData& operator<< (const double& value)
         {
         *((double*)currentPtr) = value;
         currentPtr += memorySize(value);
         return *this;
         }
      // char
      inline MessageData& operator>> (char& value)
         {
         value = *((char*)currentPtr);
         currentPtr += memorySize(value);
         return *this;
         }
      inline MessageData& operator<< (const char& value)
         {
         *((char*)currentPtr) = value;
         currentPtr += memorySize(value);
         return *this;
         }
      // wchar
      inline MessageData& operator>> (WCHAR& value)
         {
         value = *((WCHAR*)currentPtr);
         currentPtr += memorySize(value);
         return *this;
         }
      inline MessageData& operator<< (const WCHAR& value)
         {
         *((WCHAR*)currentPtr) = value;
         currentPtr += memorySize(value);
         return *this;
         }
   };

// FSTRING specialisations
inline MessageData& operator>>(MessageData& messageData, FString& value)
   {
   unsigned int numChars;
   messageData >> (int)numChars;
   value.aliasTo(messageData.ptr(), numChars);
   messageData.movePtrBy(numChars);
   return messageData;
   };
inline MessageData& operator<<(MessageData& messageData, const FString& value)
   {
   messageData << (int)value.length();
   messageData.copyFrom(value.f_str(), value.length());
   return messageData;
   }
inline unsigned int memorySize(const FString& value)
   {
   return value.length() + sizeof(int);
   }
// FSTRINGS specialisations
inline MessageData& operator>> (MessageData& messageData, FStrings& strings)
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
inline MessageData& operator<< (MessageData& messageData, const FStrings& strings)
   {
   messageData << strings.getNumElements();
   for (unsigned int i = 0; i < strings.getNumElements(); i++)
      messageData << strings.getString(i);

   return messageData;
   }

// restore the warnings about "Functions containing for are not expanded inline.
#pragma warn .inl

   } // end namespace protocol
#endif
