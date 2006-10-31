//---------------------------------------------------------------------------
#ifndef RotationValuesH
#define RotationValuesH
#include <map>
#include <string>
#include <vector>
#include <set>
#include <TAPSTable.h>
#include <general\string_functions.h>
#include <stdlib.h>
#include "CropFields.h"
#include <ApsimShared\ApsimSettings.h>
// ------------------------------------------------------------------
// This class keeps track of value for a given year, field and file
// ------------------------------------------------------------------
class RotationValue
   {
   private:
      enum FieldValueType {str, numerical, unknown};
      FieldValueType type;
      std::string stringValue;
      float total;
      unsigned count;

      void discoverType(const std::string& value)
         {
         if (type == unknown)
            {
            if (Is_numerical(value.c_str()))
               type = numerical;
            else
               {
               type = str;
               stringValue = value;
               }
            }
         }

   public:
      RotationValue(void)
         : total(0.0), count(0), type(unknown) {}

      unsigned getCount(void) const {return count;}
      void addValue(const std::string& value)
         {
         discoverType(value);
         if (type == numerical)
            {
            char* endPtr;
            total += strtod(value.c_str(), &endPtr);
            count++;
            }
         }
      bool getValue(bool doAverage, float& value) const
         {
         if (count == 0)
            value = 0.0;
         else if (doAverage)
            value = total / count;
         else
            value = total;
         return (type != str);
         }
      bool getValue(std::string& value) const
         {
         value = stringValue;
         return (type == str);
         }
   };

// ------------------------------------------------------------------
// This class keeps track of all values for all years, fields and files
// ------------------------------------------------------------------
class RotationValues
   {
   private:
      ApsimSettings settings;
   
      typedef std::map<unsigned, RotationValue> FileValues;
      typedef std::map<unsigned, FileValues> FieldValues;
      typedef std::map<unsigned, FieldValues> YearValues;
      YearValues yearValues;
      std::vector<std::string> fieldsToAverage;
      std::vector<std::string> fieldsDividedByNumFiles;
      std::vector<std::string> fieldNames;
      CropFields cropFields;
      std::set<std::string> warnings;

      std::string getValue(const FileValues& fileValues, bool doAverage,
                           unsigned numDataBlocks) const;
      bool isAveragedField(const std::string& fieldName) const;

   public:
      RotationValues(const std::vector<std::string>& fields);

      void addValue(unsigned year, unsigned fieldI, unsigned fileI, const std::string& value);
      void writeToDataset(const std::string& rotationName, TAPSTable& data,
                          unsigned firstYear, unsigned lastYear,
                          unsigned numDataBlocks) const;
   };
#endif
