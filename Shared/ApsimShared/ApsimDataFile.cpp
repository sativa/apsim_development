//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ApsimDataFile.h"
#include <vector>
#include <string>
#include <general\string_functions.h>
#include <general\inifile.h>

#pragma package(smart_init)

using namespace std;

//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
ApsimDataFile::ApsimDataFile(const string& filename)  throw(runtime_error)
   : fileName(filename)
   {
   open();
   }
//---------------------------------------------------------------------------
// read in the contents of the specified file into this TSEGTable.
//---------------------------------------------------------------------------
void ApsimDataFile::open(void) throw(runtime_error)
   {
   if (!FileExists(fileName.c_str()))
      throw runtime_error("Cannot open file: " + fileName + ". File doesn't exist");

   in.open(fileName.c_str());
   readApsimHeader(in);
   readNextRecord(in);
   }
//---------------------------------------------------------------------------
// return a list of field names to caller.
//---------------------------------------------------------------------------
void ApsimDataFile::getFieldNames(std::vector<std::string>& names) const
   {
   names = fieldNames;
   }
//---------------------------------------------------------------------------
// return a list of field units to caller.
//---------------------------------------------------------------------------
void ApsimDataFile::getFieldUnits(std::vector<std::string>& units) const
   {
   units = fieldUnits;
   }
//---------------------------------------------------------------------------
// return a list of field values to caller.
//---------------------------------------------------------------------------
void ApsimDataFile::getFieldValues(std::vector<std::string>& values) const
   {
   values = fieldValues;
   }
//---------------------------------------------------------------------------
// return a single field value for the specified field name.  Returns
// a blank string if not found.
//---------------------------------------------------------------------------
std::string ApsimDataFile::getFieldValue(const std::string& fieldName) const
   {
   vector<string>::const_iterator fieldI = find(fieldNames.begin(),
                                                fieldNames.end(),
                                                fieldName);
   if (fieldI == fieldNames.end())
      return "";
   else
      return fieldValues[fieldI - fieldNames.begin()];
   }
//---------------------------------------------------------------------------
// return a single field value for the specified field index.  Throws
// on an invalid index.
//---------------------------------------------------------------------------
std::string ApsimDataFile::getFieldValue(unsigned fieldIndex) const throw(runtime_error)
   {
   if (fieldIndex >= fieldValues.size())
      {
      string msg = "Invalid field index: ";
      msg += IntToStr(fieldIndex).c_str();
      msg += " while trying to reference file: " + fileName;
      throw runtime_error(msg);
      }
   return fieldValues[fieldIndex];
   }
//---------------------------------------------------------------------------
// advance to the next record.
//---------------------------------------------------------------------------
bool ApsimDataFile::next(void)
   {
   return readNextRecord(in);
   }
//---------------------------------------------------------------------------
// return a list of constant names to caller.
//---------------------------------------------------------------------------
void ApsimDataFile::getConstantNames(std::vector<std::string>& names) const
   {
   for (Constants::const_iterator i = constants.begin();
                                  i != constants.end();
                                  i++)
      names.push_back(i->second);
   }
//---------------------------------------------------------------------------
// return the value of the specified constant.  Returns a blank
// string if constant doesn't exist.
//---------------------------------------------------------------------------
string ApsimDataFile::getConstant(const string& name) const
   {
   Constants::const_iterator i = constants.find(name);
   if (i == constants.end())
      return "";
   else
      return i->second;
   }
// ------------------------------------------------------------------
// read in the header part of an apsim output file.  The header part
// consists of a title= line, followed by a line of field names
// and a line of unit names.
// ------------------------------------------------------------------
void ApsimDataFile::readApsimHeader(istream& in) throw(runtime_error)
   {
   // loop through all lines looking for heading line.
   string line, previousLine;
   bool foundHeadings = false;
   while (!foundHeadings && getline(in, line))
      {
      unsigned posComment = line.find('!');
      if (posComment != string::npos)
         line.erase(posComment);

      string key, value;
      getKeyNameAndValue(line, key, value);
      if (key != "")
         constants.insert(Constants::value_type(key, value));

      // If the first non-blank character on the line is a open bracket '('
      // then we have found the units line.  The previous line is then
      // assumed to be the headings line.
      unsigned int posFirstNonBlankChar = line.find_first_not_of (" ");
      if (posFirstNonBlankChar != string::npos &&
          line[posFirstNonBlankChar] == '(')
         {
         foundHeadings = true;
         Split_string(line, " ", fieldUnits);
         Split_string(previousLine, " ", fieldNames);

         // check for duplicate fields
         vector<string> temp;
         copy(fieldNames.begin(), fieldNames.end(), back_inserter(temp));
         sort(temp.begin(), temp.end());
         if (unique(temp.begin(), temp.end()) != temp.end())
            throw runtime_error("Non-unique column names encountered.  "
                        "An APSIM output file must not have two columns with "
                        "the same name.");
         }
      previousLine = line;
      }
   }
// ------------------------------------------------------------------
// Read in the next record.  Return true if values are returned.
// ------------------------------------------------------------------
bool ApsimDataFile::readNextRecord(istream& in) throw(runtime_error)
   {
   string line;
   if (getline(in, line) && line.length() > 0)
      {
      Split_string(line, " ", fieldValues);
      if (fieldValues.size() != fieldNames.size())
         throw runtime_error("Not enough values on line: " + line);
      return true;
      }
   else
      return false;
   }

