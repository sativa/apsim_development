#ifndef VariantH
#define VariantH
#include "MessageData.h"
#include "TypeConverter.h"
#include "Type.h"
namespace protocol {
// ------------------------------------------------------------------
//  Short description:
//     Variant class for handling different data types.

//  Notes:
//     When a Variant is created from another variant, it
//     takes a copy of the message data bytes so that
//     it is completely indendant of other variants or messages

//  Changes:
//    DPH 20/6/2001

// ------------------------------------------------------------------
class Variant
   {
   public:
      Variant(void) : newDataPtr(NULL), typeConverter(NULL) { }
      Variant(const Variant& from)
         : typeConverter(NULL)
         {
         copyFrom(from);
         }
      ~Variant(void) {delete [] newDataPtr;}
      Variant& operator= (const Variant& rhs)
         {
         delete [] newDataPtr;
         copyFrom(rhs);
         return *this;
         }
      bool isValid(void) {return ourMessageData.isValid();}
      Type& getType(void) {return type;}
      unsigned getFromId(void) {return fromId;}
      void setFromId(unsigned id) {fromId = id;}

      template <class T>
      void unpack(T& obj)
         {
         if (typeConverter != NULL)
            typeConverter->getValue(ourMessageData, obj);

         else
            ourMessageData >> obj;
         }
      void setTypeConverter(TypeConverter* typeconv)
         {
         typeConverter = typeconv;
         }

      void copyTo(MessageData& messageData) const
         {
         messageData << type;
         // just copy the data part of the variable
         messageData.copyFrom(ourMessageData.ptr(),
                              ourMessageData.dataSize()-memorySize(type));
         }
      void initFrom(MessageData& messageData)
         {
         ourMessageData = messageData.newMessageDataFromCurrent();
         ourMessageData >> type;
         }
      const MessageData& getMessageData(void) const {return ourMessageData;}

   private:
      char* newDataPtr;
      Type type;
      MessageData ourMessageData;
      TypeConverter* typeConverter;
      unsigned fromId;

      void copyFrom(const Variant& from)
         {
         newDataPtr = new char[from.ourMessageData.dataSize()];
         from.ourMessageData.copyTo(newDataPtr);
         ourMessageData = MessageData(newDataPtr, from.ourMessageData.dataSize());
         ourMessageData >> type;
         }

   };

inline MessageData& operator>> (MessageData& messageData, Variant& value)
   {
   value.initFrom(messageData);
   return messageData;
   }

inline MessageData& operator<< (MessageData& messageData, const Variant& value)
   {
   value.copyTo(messageData);
   return messageData;
   }
inline unsigned int memorySize(Variant& value)
   {
   return value.getMessageData().dataSize();
   }

} // namespace protocol
#endif