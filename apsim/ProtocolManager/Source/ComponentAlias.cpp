#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ComponentAlias.h"
#include <Protocol\computation.h>
#include <general\stristr.h>

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
// constructor
// ------------------------------------------------------------------
ComponentAlias::ComponentAlias(const string& n,
                               const string& dllFileName,
                               const string& componentInterfaceFileName,
                               unsigned int componentAliasID,
                               unsigned int parentID)
   : ID(componentAliasID), name(n)
   {
   computation = new Computation(name, dllFileName, componentInterfaceFileName, componentAliasID, parentID);
   }
// ------------------------------------------------------------------
// destructor
// ------------------------------------------------------------------
ComponentAlias::~ComponentAlias(void)
   {
   delete computation;
   }
// ------------------------------------------------------------------
// Return true if this component is a system.
// ------------------------------------------------------------------
bool ComponentAlias::isSystem(void)
   {
   return (stristr(getExecutable().c_str(), "protocolmanager.dll") != NULL);
   }
