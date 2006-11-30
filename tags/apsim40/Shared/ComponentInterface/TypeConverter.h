//---------------------------------------------------------------------------
#ifndef TypeConverterH
#define TypeConverterH
#include "MessageData.h"
#include "Type.h"

namespace protocol {
class Component;

// ------------------------------------------------------------------
//  Short description:
//     This class encapsulates a DDML data type converter

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
class __declspec(dllexport) TypeConverter
   {
   protected:
      MessageData& bufferMessageData;

   public:
      TypeConverter(void);
      virtual ~TypeConverter(void) { }

      template <class T>
      void getValue(MessageData& messageData, T& obj)
         {
         bufferMessageData.reset();
         doConvert(messageData);
         bufferMessageData.reset();
         bufferMessageData >> obj;
         }
      virtual TypeConverter* clone(void) = 0;

      // don't call directly.  Call getValue instead.
      virtual void doConvert(MessageData& messageData) = 0;

   };

// ------------------------------------------------------------------
//  Short description:
//     Return a data type converter if possible or NULL if none
//     available.  The returned typeconverter should NEVER be
//     deleted.  Returns true if all is ok.  If false is returned
//     then error has already been called.

//  Changes:
//    DPH 7/6/2001
// ------------------------------------------------------------------
bool getTypeConverter(Component* parent,
                      const FString& name,
                      const Type& sourceType,
                      const Type& destType,
                      TypeConverter*& converter);

// ------------------------------------------------------------------
// Return a data type converter if possible or NULL if none
// available.
// ------------------------------------------------------------------
bool protocol::getTypeConverter(Component* parent,
                                const FString& name,
                                protocol::DataTypeCode sourceTypeCode,
                                protocol::DataTypeCode destTypeCode,
                                bool isSourceArray,
                                bool isDestArray,
                                TypeConverter*& converter);


} // namespace protocol
#endif