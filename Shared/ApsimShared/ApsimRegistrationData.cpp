//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ApsimRegistrationData.h"

// ------------------------------------------------------------------
// return true if this registration is an auto-register.
// ------------------------------------------------------------------
bool ApsimRegistrationData::doAutoRegister(void) const
   {
   return !Str_i_Eq(node.getAttribute("autoregister"), "F");
   }
// ------------------------------------------------------------------
// return the name of the registration.
// ------------------------------------------------------------------
std::string ApsimRegistrationData::getName(void) const
   {
   return node.getAttribute("name");
   }
// ------------------------------------------------------------------
// return the type of the registration.
// ------------------------------------------------------------------
std::string ApsimRegistrationData::getType(void) const
   {
   return node.getName();
   }
// ------------------------------------------------------------------
// return data type name of the registration.
// ------------------------------------------------------------------
std::string ApsimRegistrationData::getDataTypeName(void) const
   {
   return node.getAttribute("type");
   }

