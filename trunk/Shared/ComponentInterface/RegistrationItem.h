//---------------------------------------------------------------------------
#ifndef RegistrationItemH
#define RegistrationItemH
#include "Variants.h"
#include "ArraySpecifier.h"
#include "Messages.h"
#include <ApsimShared\fstring.h>

namespace protocol {

// ------------------------------------------------------------------
// This class encapsulates a single registration item plus any
// return value variants if it is a getVariable registration.
// ------------------------------------------------------------------
class RegistrationItem
   {
   public:
      // ------------------------------------------------------------------
      // constructors and destructors
      // ------------------------------------------------------------------
      RegistrationItem(Component* parent, RegistrationType kind,
                       const FString& name, const Type& type,
                       const FString& componentName);
      ~RegistrationItem(void)
         {
         delete [] name;
         delete [] type;
         delete [] componentName;
         delete arraySpecifier;
         }

      // ------------------------------------------------------------------
      // Return true if this registration item matches the specified parameters.
      // ------------------------------------------------------------------
      bool isMatch(RegistrationType rhsKind, const FString& rhsName,
                   const FString& rhsComponentName)
         {
         return (kind == rhsKind && rhsName == name
                 && rhsComponentName == componentName);
         }

      // ------------------------------------------------------------------
      // Return the destination component ID or -1 if not found.
      // ------------------------------------------------------------------
      int getComponentID(void);

      // ------------------------------------------------------------------
      // Return the fully qualified name of the registration
      // e.g. componentname.name
      // ------------------------------------------------------------------
      void getFQN(char* name);

      // ------------------------------------------------------------------
      // Return the type string of the registration to caller.
      // ------------------------------------------------------------------
      const char* getType(void) {return type;}


      const FString getName(void) {return name;}
      RegistrationType getKind(void) {return kind;}
      Variants& getVariants(void) {return variants;}

      void empty(void) {variants.empty();}
      unsigned countResponses(void) {return variants.size();}

      // ------------------------------------------------------------------
      // Add a return value message to this registration item.
      // ------------------------------------------------------------------
      void addReturnValueMessage(unsigned int fromID, ReturnValueData& returnValueData);

   private:
      Component* parent;
      RegistrationType kind;
      char* name;
      char* type;
      char* componentName;

      Variants variants;
      bool haveCreatedTypeConverter;
      bool isError;
      ArraySpecifier* arraySpecifier;

   };

} // namespace protocol
#endif
