//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ApsimDataFile.h"
#include <vector>
#include <string>
#include <general\string_functions.h>
#include <general\stl_functions.h>
#include <general\inifile.h>
#include "fstring.h"
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

   in.open(fileName.c_str(), ios::binary);
   readApsimHeader(in);
   firstRecordPos = in.tellg();
   readNextRecord(in);
   lookForDateField();
   }
//---------------------------------------------------------------------------
// retrieve the next line from the input stream.
//---------------------------------------------------------------------------
istream& ApsimDataFile::getline(string& line)
   {
   std::getline(in, line);
   if (line[line.size()-1] == '\r')
      line.erase(line.size()-1);
   return in;
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
// position at first record.
//---------------------------------------------------------------------------
bool ApsimDataFile::first(void)
   {
   in.seekg(firstRecordPos);
   in.clear();
   return readNextRecord(in);
   }
//---------------------------------------------------------------------------
// advance to the last record.
//---------------------------------------------------------------------------
bool ApsimDataFile::last(void)
   {
   in.seekg(-1, ios::end);
   in.clear();

   // skip over any blank lines.
   while (in && in.peek() == '\n' || in.peek() == '\r')
      in.seekg(-1, ios::cur);

   // skip to previous \n
   while (in && in.peek() != '\n')
      in.seekg(-1, ios::cur);

   // move forward 1 char to get past the \n
   in.seekg(1, ios::cur);
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
      names.push_back(i->first);
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
   while (!foundHeadings && getline(line))
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
   if (getline(line) && line.length() > 0)
      {
      Split_string(line, " ", fieldValues);
      if (fieldValues.size() != fieldNames.size())
         {
         throw runtime_error("Not enough values on line: " + line);
         }
      return true;
      }
   else
      return false;
   }
// ------------------------------------------------------------------
// Look at the columns names to see if we sufficient columns to
// build a date.
// ------------------------------------------------------------------
void ApsimDataFile::lookForDateField(void)
   {
   vector<string>::iterator i;

   // look for year column
   i = find_if(fieldNames.begin(), fieldNames.end(),
               CaseInsensitiveStringComparison("year"));
   if (i != fieldNames.end())
      yearI = i - fieldNames.begin();

   // look for day column
   i = find_if(fieldNames.begin(),
               fieldNames.end(),
               CaseInsensitiveStringComparison("day"));
   if (i != fieldNames.end())
      dayOfYearI = i - fieldNames.begin();

   // look for day_of_month column
   i = find_if(fieldNames.begin(),
               fieldNames.end(),
               CaseInsensitiveStringComparison("day_of_month"));
   if (i != fieldNames.end())
      dayOfMonthI = i - fieldNames.begin();

   // look for month column
   i = find_if(fieldNames.begin(),
               fieldNames.end(),
               CaseInsensitiveStringComparison("month"));
   if (i != fieldNames.end())
      monthI = i - fieldNames.begin();
   }
// ------------------------------------------------------------------
// return the date on the current record.
// ------------------------------------------------------------------
TDateTime ApsimDataFile::getDate(void) const throw(std::runtime_error)
   {
   bool ok = (yearI != NULL &&
           (dayOfYearI != NULL ||
            (dayOfMonthI != NULL && monthI != NULL)));
   if (!ok)
      throw runtime_error("APSIM input files must have year and day OR day, "
                          "month and year columns.");

   int year = StrToInt(fieldValues[yearI].c_str());

   if (dayOfYearI != NULL)
      {
      int dayOfYear = StrToInt(fieldValues[dayOfYearI].c_str());
      TDateTime date(year, 1, 1);
      return date + dayOfYear - 1;
      }
   else
      {
      int day = StrToInt(fieldValues[dayOfMonthI].c_str());
      int month = StrToInt(fieldValues[monthI].c_str());
      return TDateTime(year, month, day);
      }
   }
// ------------------------------------------------------------------
// ------------------------------------------------------------------
extern "C" unsigned _export __stdcall newApsimDataFile
   (const char* filename, unsigned filenameLength)
   {
   string fileName(filename, filenameLength);
   return (unsigned) new ApsimDataFile(fileName);
   }
extern "C" void _export __stdcall deleteApsimDataFile
   (ApsimDataFile* dataFile)
   {
   delete dataFile;
   }
extern "C" unsigned _export __stdcall ApsimDataFile_getFieldValue
   (ApsimDataFile** dataFile, unsigned* fieldIndex, char* value, unsigned valueLength)
   {
   try
      {
      string valueString = (*dataFile)->getFieldValue(*fieldIndex);
      FString(value, valueLength) = valueString.c_str();
      return true;
      }
   catch (...)
      {
      return false;
      }
   }
extern "C" unsigned _export __stdcall ApsimDataFile_next
   (ApsimDataFile** dataFile)
   {
   try
      {
      return (*dataFile)->next();
      }
   catch (const runtime_error& err)
      {
      ::MessageBox(NULL, err.what(), "Error", MB_ICONSTOP | MB_OK);
      return false;
      }
   }

