//---------------------------------------------------------------------------
#ifndef MessageDataH
#define MessageDataH
#include <ApsimShared\fstring.h>
#include "message.h"
//#include "debughook.h"

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
      unsigned int bufferSize;
   public:
      MessageData(void) : startBuffer(NULL), currentPtr(NULL), bufferSize(0) { }
      MessageData(char* dataPtr, unsigned int numBytes)
         : startBuffer((char*)dataPtr),
           currentPtr(dataPtr),
           bufferSize(numBytes)
         {
         }
      MessageData(const Message* msg)
         : startBuffer((char*)msg->dataPtr),
           currentPtr(msg->dataPtr),
           bufferSize(msg->nDataBytes)
         {
         }

      void reset(void) {currentPtr = (char*)startBuffer;}
      bool isValid(void) const
         {
         return (startBuffer != NULL && currentPtr != NULL
                 && bytesRead() < totalBytes());}
      char* ptr(void) const {return currentPtr;}
      const char* start(void) const {return startBuffer;}
      void seek(char* ptr) {currentPtr = ptr;}
      unsigned totalBytes(void)  const {return bufferSize;}
      unsigned bytesRead(void)   const {return currentPtr-startBuffer;}
      unsigned bytesUnRead(void) const
         {return totalBytes()-bytesRead();}

      void movePtrBy(unsigned int numBytes)
         {
         currentPtr += numBytes;
         }
      void copyFrom(const char* from, unsigned int numBytes)
         {
         memcpy(currentPtr, from, numBytes);
         currentPtr += numBytes;
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
inline unsigned int memorySize(const FStrings& strings)
   {
   unsigned size = 4;
   for (unsigned int i = 0; i < strings.getNumElements(); i++)
      size += memorySize(strings.getString(i));
   return size;
   }

// restore the warnings about "Functions containing for are not expanded inline.
#pragma warn .inl

   } // end namespace protocol
#endif
