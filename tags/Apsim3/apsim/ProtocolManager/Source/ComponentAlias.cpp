#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ComponentAlias.h"
#include <Protocol\computation.h>

using namespace std;
using namespace protocol;
// ------------------------------------------------------------------
// constructor used only by the GOD PM.
// ------------------------------------------------------------------
ComponentAlias::ComponentAlias(const string& n,
                               unsigned int componentAliasID)
   : ID(componentAliasID), name(n), computation(NULL)
   { }

// ------------------------------------------------------------------
//  Short description:
//     constructor

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
ComponentAlias::ComponentAlias(const string& n,
                               const string& dllFileName,
                               unsigned int componentAliasID,
                               unsigned int parentID)
   : ID(componentAliasID), name(n)
   {
   computation = new Computation(name, dllFileName, componentAliasID, parentID);
   }

// ------------------------------------------------------------------
//  Short description:
//     destructor

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
ComponentAlias::~ComponentAlias(void)
   {
   delete computation;
   ComponentAlias::Registrations::iterator i;
   for (i = getRegistrations.begin(); i != getRegistrations.end(); i++)
      delete i->second;
   for (i = setRegistrations.begin(); i != setRegistrations.end(); i++)
      delete i->second;
   for (i = eventRegistrations.begin(); i != eventRegistrations.end(); i++)
      delete i->second;

   for (i = respondToGetRegistrations.begin(); i != respondToGetRegistrations.end(); i++)
      delete i->second;
   for (i = respondToSetRegistrations.begin(); i != respondToSetRegistrations.end(); i++)
      delete i->second;
   for (i = respondToEventRegistrations.begin(); i != respondToEventRegistrations.end(); i++)
      delete i->second;
   }
// ------------------------------------------------------------------
//  Short description:
//     return a registrations array to caller.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
ComponentAlias::Registrations* ComponentAlias::getRegistrationsForKind(RegistrationType kind)
   {
    switch (kind)
      {
      case getVariableReg : return &getRegistrations;
      case setVariableReg : return &setRegistrations;
      case eventReg       : return &eventRegistrations;
      case methodCallReg  : return &eventRegistrations;

      case respondToGetReg : return &respondToGetRegistrations;
      case respondToSetReg : return &respondToSetRegistrations;
      case respondToEventReg       : return &respondToEventRegistrations;
      case respondToMethodCallReg  : return &respondToEventRegistrations;
      };
   return NULL;
   }

