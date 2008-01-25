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
#include <ApsimShared\ApsimDirectories.h>
#include "DataContainer.h"
using namespace std;

// ------------------------------------------------------------------
// Read in the next record.  Return true if values are returned.
// ------------------------------------------------------------------
bool readNextRecord(istream& in, int index, vector<string>& fieldValues)
   {
   string line;
   if (getline(in, line) && line.length() > 0)
      {
      int n = line.length();
      int start = line.find_first_not_of(' ');
      while ((start >= 0) && (start < n))
         {
         int stop;
         if (line[start] == '"')
            stop = line.find_first_of('"', start+1)+1;
         else
            stop = line.find_first_of(' ', start);
         if ((stop < 0) || (stop > n)) stop = n;
         string word = line.substr(start, stop - start);
         if (word == "*" || word == "?")
            word = "";
         if (word[0] == '"' && word[word.length()-1] == '"')
            word = word.substr(1, word.length()-2);
         if (fieldValues.size() <= (unsigned)index)
            fieldValues.push_back(word);
         else
            fieldValues[index] = word;
         start = line.find_first_not_of(' ', stop+1);
         index++;
         }
      return true;
      }
   else
      return false;
   }

// ------------------------------------------------------------------
// read in the header part of an apsim output file.  The header part
// consists of a series of constants (e.g. keyword = value lines),
// followed by a line of field names and a line of unit names.
// Returns the number of 'constants' found. The 'in' stream
// will be position at the start of the 2nd line of data.
// ------------------------------------------------------------------
int readApsimHeader(istream& in,
                    vector<string>& fieldNames,
                    vector<string>& fieldValues)
   {
   int numConstants;
   // loop through all lines looking for heading line.
   string line, previousLine;
   bool foundHeadings = false;
   while (!foundHeadings && getline(in, line))
      {
      // look for keyword = keyvalue on line and store it away for later.
      int posEquals = line.find('=');
      if (posEquals != string::npos)
         {
         string KeyName = line.substr(0, posEquals);
         stripLeadingTrailing(KeyName, " ");
         string KeyValue = line.substr(posEquals+1);
         stripLeadingTrailing(KeyValue, " ");
         fieldNames.push_back(KeyName);
         fieldValues.push_back(KeyValue);
         }

      // If the first non-blank character on the line is a open bracket '('
      // then we have found the units line.  The previous line is then
      // assumed to be the headings line.
      unsigned int posFirstNonBlankChar = line.find_first_not_of (" ");
      if (posFirstNonBlankChar != string::npos &&
          line[posFirstNonBlankChar] == '(')
         {
         foundHeadings = true;
         numConstants = fieldNames.size();
         SplitStringHonouringQuotes(previousLine, " ", fieldNames);
         break;
         }
      previousLine = line;
      }
   if (numConstants >= 0)
      readNextRecord(in, numConstants, fieldValues);

   return numConstants;
   }
   
//---------------------------------------------------------------------------
// Get a list of file names for this reader - takes care of filespecs.
//---------------------------------------------------------------------------
vector<string> getFileNames(DataContainer& parent, const XMLNode& properties)
   {
   vector<string> fileNames;
   vector<string> fileSpecs = parent.reads(properties, "filename");
   for (unsigned i = 0; i != fileSpecs.size(); i++)
      {
      if (fileSpecs[i].find('*') == string::npos &&
          fileSpecs[i].find('?') == string::npos)
          fileNames.push_back(fileSpecs[i]);
      else
         {
         string dir = Path(fileSpecs[i]).Get_directory();
         if (dir == "")
            dir = Path::getCurrentFolder().Get_path();
         getDirectoryListing(dir,
                             Path(fileSpecs[i]).Get_name(),
                             fileNames,
                             FA_NORMAL,
                             true);
         }
      }
   for (unsigned i = 0; i != fileNames.size(); i++)
      replaceAll(fileNames[i], "%apsuite", getApsimDirectory());
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
   vector<string> fileNames = getFileNames(parent, properties);

   // Read all headings.
   result.Active = false;
   result.FieldDefs->Clear();

   for (unsigned f = 0; f != fileNames.size(); f++)
      {
      if (FileExists(fileNames[f].c_str()))
         {
         // add fields to our result dataset.
         ifstream in(fileNames[f].c_str());
         vector<string> fieldNames, fieldValues;
         int numConstants = readApsimHeader(in, fieldNames, fieldValues);
         if (!result.Active)
            {
            addDBFields(&result, fieldNames, fieldValues);
            result.Active = true;
            }
         if (fieldValues.size() > 0)
            {
            // Copy all rows to result dataset.
            do
               {
               appendDBRecord(&result, fieldNames, fieldValues);
               }
            while (readNextRecord(in, numConstants, fieldValues));
            }
         }
      }
   }


