#include <windows.h>
#pragma hdrstop

#include "Type.h"
using namespace protocol;

// ------------------------------------------------------------------
//  Short description:
//     Return an attribute of the type.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
FString Type::getAttribute(const char* attributeName) const
   {
   char stringToLocate[100];
   strcpy(stringToLocate, attributeName);
   strcat(stringToLocate, "=\"");
   unsigned posAttr = type.find(stringToLocate);
   if (posAttr == MAXINT)
      return "";
   else
      {
      posAttr += strlen(stringToLocate);
      FString attrPlusRemainder = type.substr(posAttr);
      unsigned posEndQuote = attrPlusRemainder.find("\"");
      return attrPlusRemainder.substr(0, posEndQuote);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     determine the data type from the string passed in.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
void Type::determineType(void)
   {
   FString kind = getAttribute("kind");
   if (kind == "integer1")
      code = DTint1;
   else if (kind == "integer2")
      code = DTint2;
   else if (kind == "integer4")
      code = DTint4;
   else if (kind == "integer8")
      code = DTint8;
   else if (kind == "single")
      code = DTsingle;
   else if (kind == "double")
      code = DTdouble;
   else if (kind == "boolean")
      code = DTboolean;
   else if (kind == "char")
      code = DTchar;
   else if (kind == "string")
      code = DTstring;
   else
      code = DTunknown;
   }
// ------------------------------------------------------------------
// convert a type code to a type string.
// ------------------------------------------------------------------
FString Type::codeToString(DataTypeCode code)
   {
   if (code == DTint1)
      return "integer1";
   else if (code == DTint2)
      return "integer2";
   else if (code == DTint4)
      return "integer4";
   else if (code == DTint8)
      return "integer8";
   else if (code == DTsingle)
      return "single";
   else if (code == DTdouble)
      return "double";
   else if (code == DTboolean)
      return "boolean";
   else if (code == DTchar)
      return "char";
   else if (code == DTstring)
      return "string";
   else
      return "unknown";
   }

