//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ApsimFileReader.h"
#include <fstream>
#include <general\db_functions.h>
#include <general\stl_functions.h>
#include <general\string_functions.h>
#include <general\io_functions.h>
#include <general\path.h>
#include <dir.h>
#include <general\xml.h>
using namespace std;

// ------------------------------------------------------------------
// read in the header part of an apsim output file.  The header part
// consists of a title= line, followed by a line of field names
// and a line of unit names.
// ------------------------------------------------------------------
void readApsimHeader(istream& in, vector<string>& fieldNames, string& title)
   {
   // loop through all lines looking for heading line.
   string line, previousLine;
   bool foundHeadings = false;
   while (!foundHeadings && getline(in, line))
      {
      // look for 'title=' on line and store it away for later.
      string keyValue = getKeyValue(line, "title");
      if (keyValue != "")
         title = keyValue;

      // If the first non-blank character on the line is a open bracket '('
      // then we have found the units line.  The previous line is then
      // assumed to be the headings line.
      unsigned int posFirstNonBlankChar = line.find_first_not_of (" ");
      if (posFirstNonBlankChar != string::npos &&
          line[posFirstNonBlankChar] == '(')
         {
         foundHeadings = true;
         Split_string(previousLine, " ", fieldNames);
         }
      previousLine = line;
      }
   }

// ------------------------------------------------------------------
// Read in the next record.  Return true if values are returned.
// ------------------------------------------------------------------
bool readNextRecord(istream& in, vector<string>& fieldValues)
   {
   string line;
   if (getline(in, line) && line.length() > 0)
      {
      SplitStringHonouringQuotes(line, " ", fieldValues);

      // get rid of missing values - assume a * is a missing value.
      for (unsigned i = 0; i != fieldValues.size(); i++)
         {
         replaceAll(fieldValues[i], "\"", "");
         if (fieldValues[i] == "*" || fieldValues[i] == "?")
            fieldValues[i] = "";
         }
      return true;
      }
   else
      return false;
   }
// ------------------------------------------------------------------
// split up title into factors and store as fields.
// ------------------------------------------------------------------
void splitTitleIntoFactors(const string& title, vector<string>& factorNames,
                           vector<string>& factorValues)
   {
   vector<string> namesAndValues;
   Split_string(title, ";", namesAndValues);
   string factorName, factorValue;
   for (unsigned i = 0; i != namesAndValues.size(); i++)
      {
      getKeyNameAndValue(namesAndValues[i], factorName, factorValue);
      factorNames.push_back(factorName);
      factorValues.push_back(factorValue);
      }
   }

//---------------------------------------------------------------------------
// Get a list of file names for this reader - takes care of filespecs.
//---------------------------------------------------------------------------
vector<string> getFileNames(const XMLNode& properties)
   {
   vector<string> fileNames;
   vector<string> fileSpecs = properties.childValues("filename");
   for (unsigned i = 0; i != fileSpecs.size(); i++)
      {
      if (fileSpecs[i].find('*') == string::npos &&
          fileSpecs[i].find('?') == string::npos)
          fileNames.push_back(fileSpecs[0]);
      else
         getDirectoryListing(Path(fileSpecs[i]).Get_directory(),
                             Path(fileSpecs[i]).Get_name(),
                             fileNames,
                             FA_NORMAL,
                             true);
      }
   return fileNames;                    
   }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
// this function reads all data from 1 or more APSIM output files.
//---------------------------------------------------------------------------
void processApsimFileReader(DataContainer& parent,
                            const XMLNode& properties,
                            TDataSet& result)
   {
   vector<string> fileNames = getFileNames(properties);
   bool parseTitle = Str_i_Eq(properties.childValue("parseTitle"), "yes");

   // Read all headings.
   result.Active = false;
   result.FieldDefs->Clear();

   if (fileNames.size() == 0)
      return;

   if (!FileExists(fileNames[0].c_str()))
      throw runtime_error("Cannot find file: " + fileNames[0]);

   ifstream in(fileNames[0].c_str());
   vector<string> fieldNames;
   string title;
   readApsimHeader(in, fieldNames, title);
   if (title != "")
      {
      // split up title into factors and store as fields.
      vector<string> factorNames, factorValues;
      if (parseTitle)
         splitTitleIntoFactors(title, factorNames, factorValues);
      else
         {
         factorNames.push_back("title");
         factorValues.push_back(title);
         }
      if (factorNames.size() > 0 && factorNames[0] != "")
         addDBFields(&result, factorNames, factorValues);
      }

   // Read headings and store as field names.
   vector<string> fieldValues;
   if (readNextRecord(in, fieldValues))
      addDBFields(&result, fieldNames, fieldValues);

   // Now read all data.
   if (result.FieldDefs->Count > 0)
      {
      result.Active = true;
      for (unsigned f = 0; f != fileNames.size(); f++)
         {
         if (!FileExists(fileNames[f].c_str()))
            throw runtime_error("Cannot find file: " + fileNames[f]);

         ifstream in(fileNames[f].c_str());
         vector<string> fieldNames;
         string title;
         readApsimHeader(in, fieldNames, title);
         vector<string> factorNames, factorValues;
         if (title != "")
            {
            // split up title into factors and store as fields.
            if (parseTitle)
               splitTitleIntoFactors(title, factorNames, factorValues);
            else
               {
               factorNames.push_back("title");
               factorValues.push_back(title);
               }

            copy(factorNames.begin(), factorNames.end(), back_inserter(fieldNames));
            }

         // Copy all rows to result dataset.
         vector<string> fieldValues;
         while (readNextRecord(in, fieldValues))
            {
            copy(factorValues.begin(), factorValues.end(), back_inserter(fieldValues));
            appendDBRecord(&result, fieldNames, fieldValues);
            }
         }
      }
   }


