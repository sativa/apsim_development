#include <string>
#include <vector>
#include <general/TreeNodeIterator.h>
#include <general/xml.h>
#include <general/stl_functions.h>
#include <general/string_functions.h>
#include <general/stristr.h>
#include <general/path.h>
#include <general/xml.h>
#include "FStringExt.h"
#include "ApsimRegistrationData.h"
#include "ApsimDataTypeData.h"


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
// Return true if data type is a structure.
// ------------------------------------------------------------------
bool ApsimDataTypeData::isStructure(void) const
   {
   return begin() != end();
   }
// ------------------------------------------------------------------
// Return true if data type is an array.
// ------------------------------------------------------------------
bool ApsimDataTypeData::isArray(void) const
   {
   return (strcmpi(node.getAttribute("array").c_str(), "T") == 0);
   }
// ------------------------------------------------------------------
// Return true if data type is a built in type
// ------------------------------------------------------------------
bool ApsimDataTypeData::isBuiltIn(void) const
   {
   return (strcmpi(node.getAttribute("builtin").c_str(), "T") == 0);
   }
// ------------------------------------------------------------------
// Return true if data type is a message
// ------------------------------------------------------------------
bool ApsimDataTypeData::isMessage(void) const
   {
   return (strcmpi(node.getAttribute("message").c_str(), "T") == 0);
   }
// ------------------------------------------------------------------
// Return true if data type is an array element
// ------------------------------------------------------------------
bool ApsimDataTypeData::isArrayElement(void) const
   {
   return (strcmpi(node.getName().c_str(), "element") == 0);
   }
// ------------------------------------------------------------------
// Return true if data type is an array element
// ------------------------------------------------------------------
bool ApsimDataTypeData::isEvent(void) const
   {
   return (strcmpi(node.getName().c_str(), "event") == 0);
   }

// ------------------------------------------------------------------
// Return data type name
// ------------------------------------------------------------------
std::string ApsimDataTypeData::getName(void) const
   {
   return node.getAttribute("name");
   }
// ------------------------------------------------------------------
// Return data type kind
// ------------------------------------------------------------------
std::string ApsimDataTypeData::getKind(void) const
   {
   return node.getAttribute("kind");
   }
// ------------------------------------------------------------------
// Return data type description
// ------------------------------------------------------------------
std::string ApsimDataTypeData::getDescription(void) const
   {
   return node.getAttribute("description");
   }
// ------------------------------------------------------------------
// Return data type units
// ------------------------------------------------------------------
std::string ApsimDataTypeData::getUnit(void) const
   {
   return node.getAttribute("unit");
   }
// ------------------------------------------------------------------
// Return data type lower bound
// ------------------------------------------------------------------
std::string ApsimDataTypeData::getLowerBound(void) const
   {
   return node.getAttribute("lower_bound");
   }
// ------------------------------------------------------------------
// Return data type upper bound.
// ------------------------------------------------------------------
std::string ApsimDataTypeData::getUpperBound(void) const
   {
   return node.getAttribute("upper_bound");
   }
// ------------------------------------------------------------------
// Return full type string.
// ------------------------------------------------------------------
std::string ApsimDataTypeData::getTypeString(void) const
   {
   return node.write();
   }
// ------------------------------------------------------------------
// Return full type string.
// ------------------------------------------------------------------
std::string ApsimDataTypeData::getType(void) const
   {
   return node.getAttribute("type");
   }

ApsimDataTypeData::iterator ApsimDataTypeData::begin(void) const {return iterator(TreeNodeIterator<XMLNode>(node.begin()));};
ApsimDataTypeData::iterator ApsimDataTypeData::end(void) const  {return iterator(TreeNodeIterator<XMLNode>(node.end()));};
