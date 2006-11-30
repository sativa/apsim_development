//---------------------------------------------------------------------------
#ifndef RegistrationsH
#define RegistrationsH

#include "Type.h"
#include "Protocolvector.h"
#include "Messages.h"
#include "RegistrationItem.h"
#include <ApsimShared\fstring.h>
namespace protocol {
//---------------------------------------------------------------------------
// This class keeps track of all registrations for this component.
//---------------------------------------------------------------------------
class Registrations
   {
   public:
      //---------------------------------------------------------------------------
      // constructors and destructors.
      //---------------------------------------------------------------------------
      Registrations(Component* parent);
      ~Registrations(void);

      //---------------------------------------------------------------------------
      // Find a registration item and return a pointer to it.
      // Returns NULL if not found.
      //---------------------------------------------------------------------------
      RegistrationItem* find(RegistrationType kind,
                             const FString& name,
                             const FString& componentNameOrID="");

      //---------------------------------------------------------------------------
      // Return the number of matching registrations
      //---------------------------------------------------------------------------
      unsigned count(RegistrationType kind,
                     const FString& name,
                     const FString& componentNameOrID);

      //---------------------------------------------------------------------------
      // Add a registration and return a pointer to it if registration was added.  If
      // the registration already exists, then return NULL.
      //---------------------------------------------------------------------------
      RegistrationItem* Registrations::add(RegistrationType kind,
                                           const FString& name,
                                           const Type& type,
                                           const FString& componentNameOrID="");

   private:
      Component* parent;
      protocol::vector<RegistrationItem*> registrations;

   };

} // namespace protocol

#endif