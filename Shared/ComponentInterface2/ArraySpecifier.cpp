//---------------------------------------------------------------------------
#pragma hdrstop
#include <stdexcept>
#ifdef __WIN32__
   #include <stdlib>
#endif
#include "ArraySpecifier.h"
#include <general/string_functions.h>

using namespace std;

//---------------------------------------------------------------------------
// Create an array specifier if necessary.  The caller assumes ownership
// of the returned arraySpecifier and should delete it accordingly.
// Returns NULL if no array specified found.
//---------------------------------------------------------------------------
ArraySpecifier* ArraySpecifier::create(const std::string& arraySpec)
   {
   unsigned posOpenBracket = arraySpec.find('(');
   if (posOpenBracket == string::npos)
      return NULL;

   unsigned posCloseBracket = arraySpec.rfind(')');
   if (posOpenBracket == string::npos || posCloseBracket == string::npos)
      throw runtime_error("Badly formed array specifier: " + arraySpec);

   ArraySpecifier* arraySpecifier = new ArraySpecifier();
   arraySpecifier->oldStyleSum = false;
   arraySpecifier->parseFunctionName(arraySpec.substr(0, posOpenBracket));
   string bracketedValue = arraySpec.substr(posOpenBracket+1, posCloseBracket-posOpenBracket-1);
   stripLeadingTrailing(bracketedValue, " ");
   arraySpecifier->startIndex = -1;
   arraySpecifier->endIndex = -1;
   arraySpecifier->varName = arraySpec.substr(0, posOpenBracket);
   if (bracketedValue == "")
      {
      arraySpecifier->functionName = "sum";
      arraySpecifier->oldStyleSum = true;
      }

   else
      {
      // The bracketed value can now be:
      // 1. variable name            e.g. varname
      // 2. number                   e.g. 5
      // 3. number-number            e.g. 2-5
      // 4. number:number            e.g. 2:5
      // 5. var name(number:number)  e.g. varname(2:5)

      posOpenBracket = bracketedValue.find('(');
      posCloseBracket = bracketedValue.rfind(')');

      // strip variable name from case 5.
      if (posOpenBracket != string::npos && posCloseBracket != string::npos)
         {
         arraySpecifier->varName = bracketedValue.substr(0, posOpenBracket);
         bracketedValue = bracketedValue.substr(posOpenBracket+1, posCloseBracket-posOpenBracket-1);
         }

      // now look for our delimiter
      char* endPtr;
      arraySpecifier->startIndex = strtol(bracketedValue.c_str(), &endPtr, 10);
      if (*endPtr == '\0')
         {
         // case 2 : single number
         arraySpecifier->endIndex = arraySpecifier->startIndex;
         }
      else if (*endPtr == '-' || *endPtr == ':')
         {
         // case 3,4 or 5
         endPtr++;
         arraySpecifier->endIndex = strtol(endPtr, &endPtr, 10);
         }
      else
         {
         arraySpecifier->startIndex = -1;
         arraySpecifier->endIndex = -1;
         arraySpecifier->varName = bracketedValue;
         // case 1 : variable name.
         }
      }
   return arraySpecifier;
   }

//---------------------------------------------------------------------------
// parse the specified string for a function name.
//---------------------------------------------------------------------------
void ArraySpecifier::parseFunctionName(const std::string& name)
   {
   if (Str_i_Eq(name, "sum"))
      functionName = "sum";
   }

//---------------------------------------------------------------------------
// Sometimes (e.g. during a query), a componentinterface will want to
// readorn a variable name with the array specifier bits. This method
// will do that.
//---------------------------------------------------------------------------
void ArraySpecifier::adornVariableName(std::string& variableName)
   {
   if (oldStyleSum)
      variableName += "()";
   else
      {
      if (startIndex  != -1)
         {
         variableName += "(" + itoa(startIndex);
         if (endIndex != -1 && startIndex != endIndex)
            variableName += "-" + itoa(endIndex);
         variableName += ")";
         }

      if (functionName != "")
         variableName = functionName + "(" + variableName + ")";
      }
   }
