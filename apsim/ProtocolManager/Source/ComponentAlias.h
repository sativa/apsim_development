//---------------------------------------------------------------------------
#ifndef ComponentAliasH
#define ComponentAliasH
#include "PMRegistrationItem.h"
#include "Interfaces.h"
#include <map>
// ------------------------------------------------------------------
//  Short description:
//     Encapsulates a component.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
class ComponentAlias
   {
   public:
      ComponentAlias(const std::string& name,
                     const std::string& dllFileName,
                     unsigned int ComponentAliasId,
                     unsigned int parentId);
      ~ComponentAlias(void);

      bool isOk(void) const {return (computation != NULL && computation->isOk());}
      std::string getName(void) {return name;}
      unsigned int ID;

      typedef std::map<unsigned int, PMRegistrationItem*> Registrations;
      Registrations getRegistrations;
      Registrations setRegistrations;
      Registrations eventRegistrations;
      Registrations respondToGetRegistrations;
      Registrations respondToSetRegistrations;
      Registrations respondToEventRegistrations;
      Registrations* getRegistrationsForKind(protocol::RegistrationType kind);
   private:
      protocol::IComputation* computation;
      std::string name;
   };


#endif
