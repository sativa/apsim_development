//---------------------------------------------------------------------------
#ifndef MessageDataH
#define MessageDataH

#include "message.h"
#include <string>
#include <vector>
#include <general/string_functions.h>
#ifdef __WIN32__
   #include <mem.h>
#endif

// ------------------------------------------------------------------
// Provides a mechanism to read and write values to the data
// section of a message.
// ------------------------------------------------------------------
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
      MessageData(const Message& msg)
         : startBuffer((char*)msg.dataPtr),
           currentPtr(msg.dataPtr),
           bufferSize(msg.nDataBytes)
         {
         }

      void reset(void) {currentPtr = (char*)startBuffer;}
      bool isValid(void) const
         {
         return (startBuffer != NULL && currentPtr != NULL
                 && bytesRead() < totalBytes());
         }
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

   };

// ------------------------------------------------------------------
// Creates a new message, packing the specified data automatically.
// ------------------------------------------------------------------
template <class T>
Message& newMessage(Message::Type messageType,
                    unsigned int fromID,
                    unsigned int toID,
                    bool acknowledgementRequired,
                    T& data)
   {
   Message& msg = constructMessage(messageType, fromID, toID, acknowledgementRequired,
                                   memorySize(data));
   MessageData messageData(msg);
   pack(messageData, data);
   return msg;
   }


// ------ boolean ------
inline unsigned int memorySize(const bool& value)
   {return 1;}
inline void unpack(MessageData& messageData, bool& value)
   {
   value = *((char*)messageData.ptr());
   messageData.movePtrBy(memorySize(value));
   }
inline void pack(MessageData& messageData, bool value)
   {
   *((char*)messageData.ptr()) = value;
   messageData.movePtrBy(memorySize(value));
   }
std::string DDML(bool value)
   {return "<type kind=\"boolean\"/>";}

// ------ int ------
inline unsigned int memorySize(const int& value)
   {return 4;}
inline void unpack(MessageData& messageData, int& value)
   {
   value = *((int*)messageData.ptr());
   messageData.movePtrBy(memorySize(value));
   }
inline void pack(MessageData& messageData, int value)
   {
   *((int*)messageData.ptr()) = value;
   messageData.movePtrBy(memorySize(value));
   }
std::string DDML(int value)
   {return "<type kind=\"integer4\"/>";}

// ------ float ------
inline unsigned int memorySize(const float& value)
   {return 4;}
inline void unpack(MessageData& messageData, float& value)
   {
   value = *((float*)messageData.ptr());
   messageData.movePtrBy(memorySize(value));
   }
inline void pack(MessageData& messageData, float value)
   {
   *((float*)messageData.ptr()) = value;
   messageData.movePtrBy(memorySize(value));
   }
std::string DDML(float value)
   {return "<type kind=\"single\"/>";}

// ------ double ------
inline unsigned int memorySize(const double& value)
   {return 8;}
inline void unpack(MessageData& messageData, double& value)
   {
   value = *((double*)messageData.ptr());
   messageData.movePtrBy(memorySize(value));
   }
inline void pack(MessageData& messageData, double value)
   {
   *((double*)messageData.ptr()) = value;
   messageData.movePtrBy(memorySize(value));
   }
std::string DDML(double value)
   {return "<type kind=\"double\"/>";}

// ------ char ------
inline unsigned int memorySize(const char& value)
   {return 1;}
inline void unpack(MessageData& messageData, char& value)
   {
   value = *((char*)messageData.ptr());
   messageData.movePtrBy(memorySize(value));
   }
inline void pack(MessageData& messageData, char value)
   {
   *((char*)messageData.ptr()) = value;
   messageData.movePtrBy(memorySize(value));
   }
std::string DDML(char value)
   {return "<type kind=\"char\"/>";}

// ------ wchar ------
typedef wchar_t WCHAR;
inline unsigned int memorySize(const WCHAR& value)
   {return 2;}
inline void unpack(MessageData& messageData, WCHAR& value)
   {
   value = *((WCHAR*)messageData.ptr());
   messageData.movePtrBy(memorySize(value));
   }
inline void pack(MessageData& messageData, WCHAR value)
   {
   *((WCHAR*)messageData.ptr()) = value;
   messageData.movePtrBy(memorySize(value));
   }
std::string DDML(WCHAR value)
   {return "<type kind=\"wchar\"/>";}

// ------ std::string ------
inline unsigned int memorySize(const std::string& value)
   {return value.length() + 4;}
inline void unpack(MessageData& messageData, std::string& value)
   {
   int numChars;
   unpack(messageData, numChars);
   value = std::string(messageData.ptr(), numChars);
   messageData.movePtrBy(numChars);
   }
inline void pack(MessageData& messageData, const std::string& value)
   {
   pack(messageData, (int) value.length());
   messageData.copyFrom(value.c_str(), value.length());
   }
std::string DDML(const std::string& value)
   {return "<type kind=\"string\"/>";}

// ------ std::vector ------
template <class T>
unsigned int memorySize(const std::vector<T>& values)
   {
   unsigned size = 4;
   for (unsigned i = 0; i != values.size(); i++)
      size += memorySize(values[i]);
   return size;
   }
template <class T>
inline void unpack(MessageData& messageData, std::vector<T>& values)
   {
   values.erase(values.begin(), values.end());
   int numValues;
   unpack(messageData, numValues);
   for (unsigned int i = 0; i < numValues; i++)
      {
      T value;
      unpack(messageData, value);
      values.push_back(value);
      }
   }
template <class T>
inline void pack(MessageData& messageData, const std::vector<T>& values)
   {
   pack(messageData, (int)values.size());
   for (unsigned int i = 0; i < values.size(); i++)
      pack(messageData, values[i]);
   }
template <class T>
std::string DDML(std::vector<T>& value)
   {
   T dummy;
   std::string ddml = DDML(dummy);
   addAttributeToXML(ddml, "array=\"T\"");
   return ddml;
   }

#endif
