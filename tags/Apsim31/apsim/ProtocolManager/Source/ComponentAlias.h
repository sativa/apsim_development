#include <general\pch.h>
#pragma hdrstop

#ifndef ComponentAliasH
#define ComponentAliasH
#include "PMRegistrationItem.h"
#include <ComponentInterface\Interfaces.h>
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
                     unsigned int ComponentAliasId);
      ComponentAlias(const std::string& name,
                     const std::string& dllFileName,
                     unsigned int ComponentAliasId,
                     unsigned int parentId);
      ~ComponentAlias(void);

      std::string getName(void) {return name;}
      std::string getExecutable(void)
         {
         if (computation != NULL)
            return computation->getExecutable();
         else
            return "";
         }
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
