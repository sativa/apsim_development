//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ApsimRegistrationData.h"

// ------------------------------------------------------------------
// return true if this registration is an auto-register.
// ------------------------------------------------------------------
bool ApsimRegistration::doAutoRegister(void) const
   {
   return !Str_i_Eq(node.getAttribute("autoregister"), "F");
   }
// ------------------------------------------------------------------
// return the name of the registration.
// ------------------------------------------------------------------
std::string ApsimRegistration::getName(void) const
   {
   return node.getAttribute("name");
   }
// ------------------------------------------------------------------
// return the type of the registration.
// ------------------------------------------------------------------
std::string ApsimRegistration::getType(void) const
   {
   return node.getName();
   }
// ------------------------------------------------------------------
// return data type name of the registration.
// ------------------------------------------------------------------
std::string ApsimRegistration::getDataTypeName(void) const
   {
   return node.getAttribute("type");
   }

