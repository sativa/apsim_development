//---------------------------------------------------------------------------
#include <windows.h>
#pragma hdrstop

#include "ArraySpecifier.h"
#include <string.h>

#pragma package(smart_init)
using namespace protocol;

//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
ArraySpecifier::ArraySpecifier(unsigned number)
   : number1(number), number2(number)
   {
   doSum = (number == 0);
   }
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
ArraySpecifier::ArraySpecifier(unsigned num1, unsigned num2, char separator)
   : number1(num1), number2(num2)
   {
   doSum = (separator == '-');
   }
//---------------------------------------------------------------------------
// Create an array specifier if necessary.  The caller assumes ownership
// of the returned arraySpecifier and should delete it accordingly.
// NB This routine will also change name and registeredType if it creates
// an array specifier.  Name will have the array specification eg. (2-5)
// removed and registeredType will become and array e.g. array="T"
//---------------------------------------------------------------------------
ArraySpecifier* ArraySpecifier::create(char* name, char* type)
   {
   ArraySpecifier* arraySpecifier = NULL;
   char* posOpenBracket = strchr(name, '(');
   if (posOpenBracket != NULL)
      {
      char* posCloseBracket = strchr(name, ')');
      if (posCloseBracket != NULL && posCloseBracket > posOpenBracket)
         {
         posOpenBracket++;

         // We potentially have an array specifier. see if it is
         // 1. blank           e.g. ()
         // 2. number          e.g. (5)
         // 3. number-number   e.g. (2-5)
         // 4. number:number   e.g. (2:5)
         char* endNumber1;
         int number1 = strtol(posOpenBracket, &endNumber1, 10);
         if (*endNumber1 == ')')
            arraySpecifier = new ArraySpecifier(number1);
         else if (*endNumber1 == '-' || *endNumber1 == ':')
            {
            char* endNumber2;
            int number2 = strtol(++endNumber1, &endNumber2, 10);
            if (*endNumber2 == ')')
               arraySpecifier = new ArraySpecifier(number1, number2, *--endNumber1);
            }
         }
      // If we have created an arraySpecifier then remove it from the name passed in
      // and add array="T" to the type.
      if (arraySpecifier != NULL)
         {
         *--posOpenBracket = 0;  // removes the array specifier.
         FString typeString(type, 1000, CString);
         Type(typeString).setArray(true);  // assume there is plenty of space!
         }
      }
   return arraySpecifier;
   }

