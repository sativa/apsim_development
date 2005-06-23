#include <fstream>
#include <boost/date_time/gregorian/gregorian.hpp>
#include <general/string_functions.h>
#include <general/stl_functions.h>
#include <general/math_functions.h>

#include <ComponentInterface/Component.h>

#include "StringVariant.h"

using namespace std;
using namespace protocol;

// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
StringVariant::StringVariant(Value* v, Component* p)
   : value(v), parent(p), secondaryValue(NULL)
   {
   determineType();
   }
// ------------------------------------------------------------------
// send the values of this variant to system via a ReturnValue message
// ------------------------------------------------------------------
void StringVariant::sendVariable(QueryValueData& queryData, bool useMainValue)
   {
   Value* valueToUse = value;
   if (!useMainValue)
      valueToUse = secondaryValue;
   if (valueToUse != NULL)
      {
      std::vector<float> realArray;
      std::vector<int>   integerArray;
      switch (type)
         {
         case Real:         StringContainerToDoubleContainer(valueToUse->values, realArray);
                            parent->sendVariable(queryData, realArray[0]);
                            break;
         case Integer:      StringContainerToIntegerContainer(valueToUse->values, integerArray);
                            parent->sendVariable(queryData, integerArray[0]);
                            break;
         case String:       parent->sendVariable(queryData, valueToUse->values[0]);
                            break;
         case RealArray:    StringContainerToDoubleContainer(valueToUse->values, realArray);
                            parent->sendVariable(queryData, realArray);
                            break;
         case IntegerArray: StringContainerToIntegerContainer(valueToUse->values, integerArray);
                            parent->sendVariable(queryData, integerArray);
                            break;
         case StringArray:  parent->sendVariable(queryData, valueToUse->values);
                            break;
         }
      }
   }

// ------------------------------------------------------------------
// set the values of this variant
// ------------------------------------------------------------------
void StringVariant::setVariable(QuerySetValueData& setValueData)
   {
   protocol::TypeConverter* converter = NULL;
   if (getTypeConverter(value->name.c_str(),
                        setValueData.variant.getType(),
                        protocol::Type(typeString.c_str()),
                        converter))
      setValueData.variant.setTypeConverter(converter);

   value->values.erase(value->values.begin(), value->values.end());
   switch (type)
      {
      case Real:         {float realValue;
                         setValueData.variant.unpack(realValue);
                         value->values.push_back(ftoa(realValue, 3));
                         break;}
      case Integer:      {int integerValue;
                         setValueData.variant.unpack(integerValue);
                         char buffer[40];
                         itoa(integerValue, buffer, 10);
                         value->values.push_back(buffer);
                         break;}
      case String:       {string st;
                         setValueData.variant.unpack(st);
                         value->values.push_back(st);
                         break;}
      case RealArray:    {std::vector<float> realArray;
                         setValueData.variant.unpack(realArray);
                         DoubleContainerToStringContainer(realArray, value->values);
                         break;}
      case IntegerArray: {std::vector<int> integerArray;
                         setValueData.variant.unpack(integerArray);
                         IntegerContainerToStringContainer(integerArray, value->values);
                         break;}
      case StringArray:  {setValueData.variant.unpack(value->values);
                         break;}
      }
   delete converter;
   }
// ------------------------------------------------------------------
// Determine the type of the variant from the current values.
// ------------------------------------------------------------------
void StringVariant::determineType(void)
   {
   if (value->values.size() == 0)
      {
      type = String;
      typeString = "<type kind=\"string\"";
      }
   else if (value->values[0].length() > 0)
      {
      if (Is_numerical(value->values[0].c_str()))
         {
         if (Str_i_Eq(value->name, "year") || Str_i_Eq(value->name, "day"))
            {
            type = Integer;
            typeString = "<type kind=\"integer4\"";
            }
         else
            {
            if (value->values.size() > 1)
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
         if (value->values.size() > 1)
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
   string units = splitOffBracketedValue(value->units, '(', ')');
   typeString += " units=\"" + units + "\"";
   typeString += "/>";
   }
// ------------------------------------------------------------------
// Register this variable
// ------------------------------------------------------------------
unsigned StringVariant::doRegistration()
   {
   return parent->addRegistration(RegistrationType::respondToGetSet, value->name.c_str(), typeString.c_str());
   }
// ------------------------------------------------------------------
// return the value of this variable as a float.
// ------------------------------------------------------------------
float StringVariant::asFloat()
   {
   return atof(value->values[0].c_str());
   }
// ------------------------------------------------------------------
// return the value of this variable as an integer
// ------------------------------------------------------------------
float StringVariant::asInteger()
   {
   return atoi(value->values[0].c_str());
   }
// ------------------------------------------------------------------
// The value object passed in is the main value to use for this variable.
// The value object already in 'value' is the secondary (backup) value
// so swap them around. This only ever happens when a temporal variable
// has the same name as a constant variable. When sparse data is allowed,
// sometimes we'll have to use the secondaryValue when the main temporal
// value is not valid (ie when the date isn't the same as today's date.
// ------------------------------------------------------------------
void StringVariant::setTemporalValue(Value* v)
   {
   secondaryValue = value;
   value = v;
   }

