#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "StringVariant.h"
#include <ComponentInterface\MessageDataExt.h>
#include <general\string_functions.h>
#include <general\math_functions.h>
#include <ComponentInterface\Component.h>

using namespace std;
using namespace protocol;
// ------------------------------------------------------------------
//  Short description:
//     constructor

//  Notes:

//  Changes:
//    DPH 27/6/2001

// ------------------------------------------------------------------
StringVariant::StringVariant(Component* p, const string& n)
   : name(n), parent(p)
   {
   }

// ------------------------------------------------------------------
//  Short description:
//     constructor

//  Notes:

//  Changes:
//    DPH 27/6/2001

// ------------------------------------------------------------------
StringVariant::StringVariant(Component* p, const string& n, const string& u,
   const string& value)
   : name(n), units(u), parent(p)
   {
   addValues(value);
   }

// ------------------------------------------------------------------
//  Short description:
//     add 1 or more values from the specified string to this variant

//  Notes:

//  Changes:
//    DPH 27/6/2001

// ------------------------------------------------------------------
void StringVariant::addValues(const string& valueString)
   {
   Values localValues;
   Split_string(valueString, " ", localValues);
   if (values.size() == 0 || values.size() == localValues.size())
      copy(localValues.begin(), localValues.end(), back_inserter(values));
   else
      {
      AnsiString msg = "The INPUT module was expecting " + IntToStr(values.size());
      msg +=" values on line\nbut only received " + IntToStr(values.size())
           + " values.\nLine: ";
      msg += valueString.c_str();
      parent->error(msg.c_str(), true);
      }

   if (typeString == "" && values.size() > 0)
      determineType();
   }

// ------------------------------------------------------------------
//  Short description:
//     add the specified value to this variant taking into account
//     the specified array index.

//  Notes:

//  Changes:
//    DPH 27/6/2001

// ------------------------------------------------------------------
void StringVariant::addValue(const string& valueString, unsigned arrayIndex)
   {
   if (arrayIndex == values.size())
      values.push_back(valueString);
   else if (arrayIndex < values.size())
      values[arrayIndex] = valueString;
   else
      {
      AnsiString msg = "Non-consecutive array indexes found in INPUT module\n"
                       "Array index: " + IntToStr(arrayIndex) + "\nValue: ";
      msg += valueString.c_str();
      parent->error(msg.c_str(), true);
      }
   if (typeString == "" && values.size() > 0)
      determineType();
   }

// ------------------------------------------------------------------
//  Short description:
//     send the values of this variant to system via a
//     ReturnValue message

//  Notes:

//  Changes:
//    DPH 27/6/2001

// ------------------------------------------------------------------
void StringVariant::sendVariable(QueryValueData& queryData)
   {
   std::vector<float> realArray;
   std::vector<int>   integerArray;
   switch (type)
      {
      case Real:         StringContainerToDoubleContainer(values, realArray);
                         parent->sendVariable(queryData,
                                             realArray[0]);
                         break;
      case Integer:      StringContainerToIntegerContainer(values, integerArray);
                         parent->sendVariable(queryData,
                                             integerArray[0]);
                         break;
      case String:       parent->sendVariable(queryData,
                                             values[0]);
                         break;
      case RealArray:    StringContainerToDoubleContainer(values, realArray);
                         parent->sendVariable(queryData,
                                             realArray);
                         break;
      case IntegerArray: StringContainerToIntegerContainer(values, integerArray);
                         parent->sendVariable(queryData,
                                             integerArray);
                         break;
      case StringArray:  parent->sendVariable(queryData,
                                             values);
                         break;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     set the values of this variant

//  Changes:
//    DPH 27/6/2001
// ------------------------------------------------------------------
void StringVariant::setVariable(QuerySetValueData& setValueData)
   {
   protocol::TypeConverter* converter = NULL;
   if (getTypeConverter(parent,
                        name.c_str(),
                        setValueData.variant.getType(),
                        protocol::Type(typeString.c_str()),
                        converter))
      setValueData.variant.setTypeConverter(converter);

   switch (type)
      {
      case Real:         {float realValue;
                         setValueData.variant.unpack(realValue);
                         addValue(ftoa(realValue, 3), 0);
                         break;}
      case Integer:      {int integerValue;
                         setValueData.variant.unpack(integerValue);
                         char buffer[40];
                         itoa(integerValue, buffer, 10);
                         addValue(buffer, 0);
                         break;}
      case String:       {string st;
                         setValueData.variant.unpack(st);
                         addValue(st, 0);
                         break;}
      case RealArray:    {std::vector<float> realArray;
                         setValueData.variant.unpack(realArray);
                         DoubleContainerToStringContainer(realArray, values);
                         break;}
      case IntegerArray: {std::vector<int> integerArray;
                         setValueData.variant.unpack(integerArray);
                         IntegerContainerToStringContainer(integerArray, values);
                         break;}
      case StringArray:  {setValueData.variant.unpack(values);
                         break;}
      }
   delete converter;
   }


// ------------------------------------------------------------------
//  Short description:
//     Return the value of this variant as an integer.  Returns
//     -1 if variant has no values.

//  Notes:

//  Changes:
//    DPH 27/6/2001

// ------------------------------------------------------------------
bool StringVariant::asInteger(int& value)
   {
   if (values.size() >= 1)
      {
      value = StrToInt(values[0].c_str());
      return true;
      }
   return -1;
   }
// ------------------------------------------------------------------
//  Short description:
//     Return the value of this variant as a logical.

//  Changes:
//    DPH 27/6/2001
// ------------------------------------------------------------------
bool StringVariant::asLogical(bool& value)
   {
   if (values.size() >= 1)
      {
      value = Str_i_Eq(values[0], "yes");
      return true;
      }
   return false;
   }
// ------------------------------------------------------------------
//  Short description:
//     Return the value of this variant as a float.  Returns
//     true if variant has a value.

//  Changes:
//    DPH 27/6/2001
// ------------------------------------------------------------------
bool StringVariant::asFloat(float& value)
   {
   if (values.size() >= 1)
      {
      value = StrToFloat(values[0].c_str());
      return true;
      }
   return false;
   }

// ------------------------------------------------------------------
//  Short description:
//     Determine the type of the variant from the current values.

//  Notes:

//  Changes:
//    DPH 27/6/2001

// ------------------------------------------------------------------
void StringVariant::determineType(void)
   {
   if (values.size() == 0)
      {
      type = String;
      typeString = "<type kind=\"string\"";
      }
   else if (values[0].length() > 0)
      {
      if (Is_numerical(values[0].c_str()))
         {
         if (Str_i_Eq(name, "year") || Str_i_Eq(name, "day"))
            {
            type = Integer;
            typeString = "<type kind=\"integer4\"";
            }
         else
            {
            if (values.size() > 1)
               {
               type = RealArray;
               typeString = "<type kind=\"single\" array=\"T\"";
               }
            else
               {
               type = Real;
               typeString = "<type kind=\"single\"";
               }
            }
         }
      else
         {
         if (values.size() > 1)
            {
            type = StringArray;
            typeString = "<type kind=\"string\"/ array=\"T\"";
            }
         else
            {
            type = String;
            typeString = "<type kind=\"string\"";
            }
         }
      }
   typeString += "units=\"" + units + "\"/>";
   }
// ------------------------------------------------------------------
//  Short description:
//     Register all variables using specified parent.

//  Notes:

//  Changes:
//    DPH 27/6/2001

// ------------------------------------------------------------------
unsigned StringVariant::doRegistration()
   {
   regID = parent->addRegistration(respondToGetSetReg, name.c_str(), typeString.c_str());
   return regID;
   }

