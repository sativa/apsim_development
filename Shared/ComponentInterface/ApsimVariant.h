//---------------------------------------------------------------------------
#ifndef ApsimVariantH
#define ApsimVariantH
#include "MessageData.h"
#include "Type.h"
#include "Variant.h"
namespace protocol {

//---------------------------------------------------------------------------
// This class encapsulate an apsim variant that is capable of being
// sent to another module.  Variables of different types can be stored
// in the variant and retrieved in any order.
//---------------------------------------------------------------------------

class ApsimVariant
   {
   public:
      ApsimVariant(void) : messageData(buffer, sizeof(buffer)) { }
      ApsimVariant(const Variant& variant) : messageData(variant.getMessageData()) { }

      void clear(void)
         {
         messageData.reset();
         }

      template <class T>
      void store(const FString& variableName, DataTypeCode typeCode, const T& value)
         {
         messageData << variableName << protocol::memorySize(value)+4 << typeCode << value;
         }
      template <class T>
      bool get(const FString& variableName, DataTypeCode typeCode, T& value)
         {
         char* savedPtr = messageData.ptr();
         FString varName;
         unsigned numBytes;
         messageData >> varName >> numBytes;
         bool found;
         while (!(found = (varName == variableName)))
            {
            messageData.movePtrBy(numBytes);
            if (!messageData.isValid()) break;
            messageData >> varName >> numBytes;
            }
         if (found)
            {
            int code;
            messageData >> code;
            if (code == typeCode)
               {
               messageData >> value;
               messageData.seek(savedPtr);
               return true;
               }
            }
         messageData.seek(savedPtr);
         return false;
         }

      void initFrom(MessageData& fromMessageData)
         {
         messageData.copyFrom(fromMessageData);
         }
      void writeTo(MessageData& fromMessageData) const
         {
         fromMessageData.copyFrom(messageData);
         }
      unsigned memorySize(void) const
         {
         return messageData.ptr() - buffer;
         }

   private:
      char buffer[2000];
      MessageData messageData;


   };
inline MessageData& operator>> (MessageData& messageData, ApsimVariant& variant)
   {
   variant.initFrom(messageData);
   return messageData;
   }
inline MessageData& operator<< (MessageData& messageData, const ApsimVariant& variant)
   {
   variant.writeTo(messageData);
   return messageData;
   }
inline unsigned int memorySize(const ApsimVariant& variant)
   {
   return variant.memorySize();
   }

} // namespace protocol

#endif
