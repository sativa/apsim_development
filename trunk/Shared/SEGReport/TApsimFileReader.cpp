//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TApsimFileReader.h"
#include <vector>
#include <string>
#include <fstream>
#include <general\inifile.h>
#include <general\db_functions.h>
#include <general\vcl_functions.h>
#include <general\string_functions.h>
#include <general\path.h>
#include <general\io_functions.h>

using namespace std;
#pragma package(smart_init)
#pragma resource "ComponentRegistration.res"
//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
__fastcall TApsimFileReader::TApsimFileReader(TComponent* owner)
   : TSEGTable(owner)
   {
   files = new TStringList;
   doInterpretTitles = false;
   }

//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
__fastcall TApsimFileReader::~TApsimFileReader()
   {
   delete files;
   }
//---------------------------------------------------------------------------
// Component has loaded - assume the current dir is the reportDirectory.
//---------------------------------------------------------------------------
void __fastcall TApsimFileReader::Loaded(void)
   {
   TSEGTable::Loaded();
   reportDirectory = GetCurrentDir();
   }
//---------------------------------------------------------------------------
// set the 'apsimFiles' property and refresh all data.
//---------------------------------------------------------------------------
void __fastcall TApsimFileReader::setFileNames(TStrings* apsimFiles)
   {
   files->Assign(apsimFiles);
   absoluteToRelativeFiles();
   if (files->Count > 0)
      forceRefresh();
   }
//---------------------------------------------------------------------------
// If we already have a report directory then the current paths will be
// relative to that dir.  Convert to absolute.
//---------------------------------------------------------------------------
void TApsimFileReader::relativeToAbsoluteFiles(void)
   {
   if (reportDirectory != "")
      {
      SetCurrentDir(reportDirectory);
      for (int i = 0; i != files->Count; i++)
         files->Strings[i] = ExpandFileName(files->Strings[i]);
      }
   }
//---------------------------------------------------------------------------
// If we already have a report directory then the current paths will be
// relative to that dir.  Convert to absolute.
//---------------------------------------------------------------------------
void TApsimFileReader::absoluteToRelativeFiles(void)
   {
   if (reportDirectory != "")
      {
      for (int i = 0; i != files->Count; i++)
         files->Strings[i] = ExtractRelativePath(reportDirectory + "\\", files->Strings[i]);
      }
   }
//---------------------------------------------------------------------------
// Called by SEGReport to give components a chance to know the current
// report directory.  Used by ApsimFileReader to use relative paths.
//---------------------------------------------------------------------------
void TApsimFileReader::setReportDirectory(AnsiString reportDir)
   {
   relativeToAbsoluteFiles();
   reportDirectory = reportDir;
   absoluteToRelativeFiles();
   }
//---------------------------------------------------------------------------
// Called by our base class to allow us to add any fielddefs we may want to.
// The table will be closed (Active=false) when this routine is called.
//---------------------------------------------------------------------------
bool TApsimFileReader::createFields(void) throw(runtime_error)
   {
   FieldDefs->Clear();
   titles.erase(titles.begin(), titles.end());
   for (int fileIndex = 0; fileIndex < files->Count; fileIndex++)
      {
      try
         {
         relativeToAbsoluteFiles();
         readAndStoreFields(files->Strings[fileIndex].c_str());
         absoluteToRelativeFiles();
         }
      catch (const runtime_error& error)
         {
         string msg = error.what();
         msg += " File: ";
         msg += files->Strings[fileIndex].c_str();
         throw runtime_error(msg);
         }
      }
   return (files->Count > 0);
   }
//---------------------------------------------------------------------------
// Called by our base class to allow us to add records to the table.
// The table is open (Active = true) when this routine is called.
//---------------------------------------------------------------------------
void TApsimFileReader::storeRecords(void) throw(runtime_error)
   {
   for (int fileIndex = 0; fileIndex < files->Count; fileIndex++)
      {
      vector<string> factorNames, factorValues;
      if (doInterpretTitles)
         splitTitleIntoFactors(titles[fileIndex], factorNames, factorValues);
      else
         {
         factorNames.push_back("title");
         factorValues.push_back(titles[fileIndex]);
         }

      try
         {
         relativeToAbsoluteFiles();
         readAndStoreRecords(files->Strings[fileIndex].c_str(),
                             factorNames, factorValues);
         absoluteToRelativeFiles();
         }
      catch (const runtime_error& error)
         {
         string msg = error.what();
         msg += " File: ";
         msg += files->Strings[fileIndex].c_str();
         throw runtime_error(msg);
         }
      }
   }
//---------------------------------------------------------------------------
// read in the contents of the specified file into this TSEGTable.
//---------------------------------------------------------------------------
void TApsimFileReader::readAndStoreFields(const string& filename) throw(runtime_error)
   {
   if (FileExists(filename.c_str()))
      {
      ifstream in(filename.c_str());
      vector<string> fieldNames;
      string title;
      readApsimHeader(in, fieldNames, title);
      titles.push_back(title);

      // split up title into factors and store as fields.
      vector<string> factorNames, factorValues;
      if (doInterpretTitles)
         splitTitleIntoFactors(title, factorNames, factorValues);
      else
         {
         factorNames.push_back("title");
         factorValues.push_back(title);
         }
      if (factorNames.size() > 0 && factorNames[0] != "")
         addDBFields(this, factorNames, factorValues);

      vector<string> fieldValues;
      if (readNextRecord(in, fieldValues))
         addDBFields(this, fieldNames, fieldValues);
      }
   else
      throw runtime_error("Cannot find APSIM output file.");
   }

//---------------------------------------------------------------------------
// read in the contents of the specified file into this TSEGTable.
//---------------------------------------------------------------------------
void TApsimFileReader::readAndStoreRecords(const string& filename,
                                           vector<string>& factorNames,
                                           vector<string>& factorValues) throw(runtime_error)
   {
   if (FileExists(filename.c_str()))
      {
      ifstream in(filename.c_str());
      vector<string> fieldNames;
      string title;
      readApsimHeader(in, fieldNames, title);

      vector<string> fieldValues;
      copy(factorNames.begin(), factorNames.end(), back_inserter(fieldNames));
      while (readNextRecord(in, fieldValues))
         {
         copy(factorValues.begin(), factorValues.end(), back_inserter(fieldValues));
         appendDBRecord(this, fieldNames, fieldValues);
         }
      }
   else
      throw runtime_error("Cannot find APSIM output file.");
   }
// ------------------------------------------------------------------
// read in the header part of an apsim output file.  The header part
// consists of a title= line, followed by a line of field names
// and a line of unit names.
// ------------------------------------------------------------------
void TApsimFileReader::readApsimHeader(istream& in,
                                       vector<string>& fieldNames,
                                       string& title) throw(runtime_error)
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
bool TApsimFileReader::readNextRecord(istream& in,
                                      vector<string>& fieldValues)
   {
   string line;
   if (getline(in, line) && line.length() > 0)
      {
      Split_string(line, " ", fieldValues);
      return true;
      }
   else
      return false;
   }
// ------------------------------------------------------------------
// split up title into factors and store as fields.
// ------------------------------------------------------------------
void TApsimFileReader::splitTitleIntoFactors(const string& title,
                                             vector<string>& factorNames,
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
// ------------------------------------------------------------------
// set one of our properties.
// ------------------------------------------------------------------
void TApsimFileReader::setProperty(const std::string& propertyName,
                                   const std::string& propertyValue)
   {
   if (Str_i_Eq(propertyName, "filenames"))
      {
      SetCurrentDir(reportDirectory);
      Path p(ExpandFileName(propertyValue.c_str()).c_str());
      
      vector<string> matchingFiles;
      getDirectoryListing(p.Get_directory(),
                          p.Get_name(),
                          matchingFiles,
                          FA_NORMAL,
                          true);
      TStringList* files = new TStringList;
      Stl_2_tstrings(matchingFiles, files);
      setFileNames(files);
      delete files;
      }
   }

