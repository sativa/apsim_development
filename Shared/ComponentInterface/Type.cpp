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

