//---------------------------------------------------------------------------
#include <windows.h>
#pragma hdrstop

#include "ArraySpecifier.h"

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
ArraySpecifier::ArraySpecifier(unsigned num1, unsigned num2, bool sum)
   : number1(num1), number2(num2)
   {
   doSum = sum;
   }
//---------------------------------------------------------------------------
// return the lower bounds of the array.
//---------------------------------------------------------------------------
unsigned ArraySpecifier::getLowerBound(void)
   {
   if (doSum)
      return 1;
   else
      return number1;
   }
//---------------------------------------------------------------------------
// return the upper bounds of the array.
//---------------------------------------------------------------------------
unsigned ArraySpecifier::getUpperBound(void)
   {
   if (doSum)
      return 1;
   else
      return number2;
   }

//---------------------------------------------------------------------------
// Convert the array of numbers in the specified messageData
// to the required element(s).
//---------------------------------------------------------------------------
void ArraySpecifier::convert(MessageData& messageData, DataTypeCode typeCode)
   {
   if (typeCode == DTint4)
      {
      vector<int> values;
      convertArray(messageData, values);
      }
   else if (typeCode == DTsingle)
      {
      vector<float> values;
      convertArray(messageData, values);
      }
   else if (typeCode == DTdouble)
      {
      vector<double> values;
      convertArray(messageData, values);
      }
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
   static const char* sumString = "sum(";
   bool doSum = (strncmp(name, sumString, strlen(sumString)) == 0);
   if (doSum)
      {
      strcpy(name, name+strlen(sumString)); // remove the sum(
      if (name[strlen(name)-1] == ')')
         name[strlen(name)-1] = 0;          // remove the trailing )
      }

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
               arraySpecifier = new ArraySpecifier(number1, number2, doSum);
            }
         }
      // If we have created an arraySpecifier then remove it from the name passed in
      // and add array="T" to the type.
      if (arraySpecifier != NULL)
         {
         *--posOpenBracket = 0;  // removes the array specifier.
//         FString typeString(type, 1000, CString);
//         Type(typeString).setArray(true);  // assume there is plenty of space!
         }
      }
   // Need to catch the case: sum(sw) ie. no range specified
   if (doSum && arraySpecifier == NULL)
      arraySpecifier = new ArraySpecifier(0);
   return arraySpecifier;
   }

