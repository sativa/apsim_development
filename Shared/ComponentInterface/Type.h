#ifndef TypeH
#define TypeH
#include "MessageData.h"
#include <ApsimShared\fstring.h>

namespace protocol {

enum DataTypeCode  {DTint1 = 0,
                    DTint2 = 1,
                    DTint4 = 2,
                    DTint8 = 3,
                    DTsingle = 4,
                    DTdouble = 5,
                    DTboolean = 6,
                    DTchar = 7,
                    DTstring = 8,
                    DTwchar = 9,
                    DTwstring = 10,
                    DTunknown = 11};

#define DTint1String     "<type kind=\"integer1\"/>"
#define DTint2String     "<type kind=\"integer2\"/>"
#define DTint4String     "<type kind=\"integer4\"/>"
#define DTint8String     "<type kind=\"integer8\"/>"
#define DTsingleString   "<type kind=\"single\"/>"
#define DTdoubleString   "<type kind=\"double\"/>"
#define DTbooleanString  "<type kind=\"boolean\"/>"
#define DTcharString     "<type kind=\"char\"/>"
#define DTstringString   "<type kind=\"string\"/>"
#define DTwcharString    "<type kind=\"wchar\"/>"
#define DTwstringString  "<type kind=\"wstring\"/>"

// ------------------------------------------------------------------
//  Short description:
//     Encapsulates a DDML type string.

//  Changes:
//    DPH 18/7/2001

// ------------------------------------------------------------------
class Type
   {
   public:
      Type(void) : code(DTunknown) { };
      Type(const char* typeString) : type(typeString), code(DTunknown) { }
      Type(const FString& typeString) : type(typeString), code(DTunknown) { }

      void initFrom(MessageData& messageData)
         {
         messageData >> type;
         }
      void writeTo(MessageData& messageData) const
         {
         messageData << type;
         }
      unsigned int memorySize(void) const
         {
         return protocol::memorySize(type);
         }

      FString getUnit(void) const {return getAttribute("units");}
      bool isArray(void) const {return (getAttribute("array") == "T");}
      DataTypeCode getCode(void)
         {
         if (code == DTunknown)
            determineType();
         return code;
         }
      FString& getTypeString(void) {return type;}

      static FString codeToString(DataTypeCode code);

   private:
      FString type;
      DataTypeCode code;

      void determineType(void);
      FString getAttribute(const char* attributeName) const;

   };
inline MessageData& operator>> (MessageData& messageData, Type& type)
   {
   type.initFrom(messageData);
   return messageData;
   }
inline MessageData& operator<< (MessageData& messageData, const Type& type)
   {
   type.writeTo(messageData);
   return messageData;
   }
inline unsigned int memorySize(const Type& type)
   {
   return type.memorySize();
   }

} // namespace protocol

#endif