//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ApsimDataTypeData.h"
#include <general\string_functions.h>

#pragma package(smart_init)

// ------------------------------------------------------------------
// Return the number of fields this ApsimDataTypeData has.
// ------------------------------------------------------------------
unsigned ApsimDataTypeData::getNumFields(void) const
   {
   unsigned count = 0;
   iterator i = begin();
   while (i != end())
      {
      ++count;
      i++;
      }
   return count;
   }
// ------------------------------------------------------------------
// Return the number of fields this ApsimDataTypeData has.
// ------------------------------------------------------------------
bool ApsimDataTypeData::isArray(void) const
   {
   return (strcmpi(node.getAttribute("array").c_str(), "T") == 0);
   }
// ------------------------------------------------------------------
// Return the number of fields this ApsimDataTypeData has.
// ------------------------------------------------------------------
bool ApsimDataTypeData::isBuiltIn(void) const
   {
   return (strcmpi(node.getAttribute("builtin").c_str(), "T") == 0);
   }
// ------------------------------------------------------------------
// Return the number of fields this ApsimDataTypeData has.
// ------------------------------------------------------------------
bool ApsimDataTypeData::isArrayElement(void) const
   {
   return (strcmpi(node.getName().c_str(), "element") == 0);
   }
// ------------------------------------------------------------------
// Return the number of fields this ApsimDataTypeData has.
// ------------------------------------------------------------------
std::string ApsimDataTypeData::getName(void) const
   {
   return node.getAttribute("name");
   }
// ------------------------------------------------------------------
// Return the number of fields this ApsimDataTypeData has.
// ------------------------------------------------------------------
std::string ApsimDataTypeData::getKind(void) const
   {
   return node.getAttribute("kind");
   }
// ------------------------------------------------------------------
// Return the number of fields this ApsimDataTypeData has.
// ------------------------------------------------------------------
std::string ApsimDataTypeData::getDescription(void) const
   {
   return node.getAttribute("description");
   }
// ------------------------------------------------------------------
// Return the number of fields this ApsimDataTypeData has.
// ------------------------------------------------------------------
std::string ApsimDataTypeData::getUnit(void) const
   {
   return node.getAttribute("unit");
   }
// ------------------------------------------------------------------
// Return the number of fields this ApsimDataTypeData has.
// ------------------------------------------------------------------
std::string ApsimDataTypeData::getLowerBound(void) const
   {
   return node.getAttribute("lower_bound");
   }
// ------------------------------------------------------------------
// Return the number of fields this ApsimDataTypeData has.
// ------------------------------------------------------------------
std::string ApsimDataTypeData::getUpperBound(void) const
   {
   return node.getAttribute("upper_bound");
   }

