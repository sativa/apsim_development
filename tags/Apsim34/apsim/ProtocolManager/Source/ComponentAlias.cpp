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
// constructor
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
// destructor
// ------------------------------------------------------------------
ComponentAlias::~ComponentAlias(void)
   {
   delete computation;
   }

