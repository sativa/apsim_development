//---------------------------------------------------------------------------
#ifndef RegistrationItemH
#define RegistrationItemH
#include "Variants.h"
#include <ApsimShared\fstring.h>
#define MAX_TYPE_STRING_SIZE 500
namespace protocol {

// ------------------------------------------------------------------
//  Short description:
//     This class encapsulates a single registration item:
//        A published event, a subscribed event, a variable send
//        a variable receive ...

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
class RegistrationItem
   {
   public:
      RegistrationItem(Component* p, RegistrationType k,
                       const FString& n, const Type& t)
         : variants(p, t), kind(k),
           haveCreatedTypeConverter(false), parent(p)
         {
         name = new char[n.length()+1];
         strncpy(name, n.f_str(), n.length());
         name[n.length()] = 0;
         typeString[0] = 0;
         strncat(typeString, t.getTypeString().f_str(), t.getTypeString().length());
         registeredType = Type(typeString);
         isError = true;
         }
      ~RegistrationItem(void)
         {
         delete [] name;
         }
      const FString getName(void) {return name;}
      RegistrationType getKind(void) {return kind;}
      Variants& getVariants(void) {return variants;}
      Type& getType(void) {return registeredType;}

      void empty(void) {variants.empty();}
      unsigned countResponses(void) {return variants.size();}
      void addReturnValueMessage(unsigned int fromID, ReturnValueData& returnValueData)
         {
         if (!haveCreatedTypeConverter)
            isError = !createTypeConverter(returnValueData.variant.getType());

         if (!isError)
            {
            returnValueData.variant.setFromId(fromID);
            variants.addVariant(returnValueData.variant);
            }
         }
      bool createTypeConverter(Type& actualType)
         {
         TypeConverter* converter;
         bool ok = getTypeConverter(parent, name, actualType, registeredType, converter);
         if (ok)
            variants.setTypeConverter(converter);
         haveCreatedTypeConverter = true;
         return ok;
         }

   private:
      Component* parent;
      RegistrationType kind;
      char* name;
      char typeString[MAX_TYPE_STRING_SIZE];
      Type registeredType;
      Variants variants;
      bool haveCreatedTypeConverter;
      bool isError;

   };

} // namespace protocol
#endif
