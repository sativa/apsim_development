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

      // this constructor is needed for storing a copy of a variant in a
      // variants class.
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

      bool isValid(void) {return messageData.isValid();}
      Type& getType(void) {return type;}
      unsigned getFromId(void) {return fromId;}
      void setFromId(unsigned id) {fromId = id;}

      template <class T>
      void unpack(T& obj)
         {
         if (typeConverter != NULL)
            typeConverter->getValue(messageData, obj);

         else
            messageData >> obj;
         }
      void setTypeConverter(TypeConverter* typeconv)
         {
         typeConverter = typeconv;
         }

      void aliasTo(Variant& variant)
         {
         messageData = MessageData(variant.messageData.ptr(), variant.messageData.bytesUnRead());
         }
      void aliasTo(MessageData& fromMessageData)
         {
         // extract the type and simply alias to the remaining unread bytes.
         fromMessageData >> type;
         messageData = MessageData(fromMessageData.ptr(), fromMessageData.bytesUnRead());
         }
      void writeTo(MessageData& toMessageData) const
         {
         // write type and then the data.
         toMessageData << type;
         toMessageData.copyFrom(messageData.start(), messageData.totalBytes());
         }
      unsigned size(void) const {return memorySize(type) + messageData.totalBytes();}
      const MessageData& getMessageData(void) const {return messageData;}

   private:
      char* newDataPtr;
      Type type;
      MessageData messageData;
      TypeConverter* typeConverter;
      unsigned fromId;

      void copyFrom(const Variant& from)
         {
         newDataPtr = new char[from.messageData.totalBytes()+1];
         messageData = MessageData(newDataPtr, from.messageData.totalBytes());
         messageData.copyFrom(from.messageData.start(), from.messageData.totalBytes());
         messageData.reset();
         type = from.type;
         typeConverter = NULL;
         }

   };

inline MessageData& operator>> (MessageData& messageData, Variant& value)
   {
   value.aliasTo(messageData);
   return messageData;
   }

inline MessageData& operator<< (MessageData& messageData, const Variant& value)
   {
   value.writeTo(messageData);
   return messageData;
   }
inline unsigned int memorySize(Variant& value)
   {
   return value.size();
   }

} // namespace protocol
#endif