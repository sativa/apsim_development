//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ApsimSettings.h"
#include "ApsimDirectories.h"
#include <general\inifile.h>
#include <general\path.h>
#include <sstream>
using namespace std;
using namespace boost;

// ------------------------------------------------------------------
//	constructor
// ------------------------------------------------------------------
ApsimSettings::ApsimSettings(void)
	{
   string originalPath = getApsimDirectory() + "\\apsim.ini";
   original = new IniFile(originalPath);
   }
// ------------------------------------------------------------------
//	destructor
// ------------------------------------------------------------------
ApsimSettings::~ApsimSettings(void)
   {
   delete original;
   }
// ------------------------------------------------------------------
// refresh.
// ------------------------------------------------------------------
void ApsimSettings::refresh(void)
   {
   original->refresh();
   }
// ------------------------------------------------------------------
// return the section name from the specified key.
// ------------------------------------------------------------------
string ApsimSettings::getSection(const std::string& key) const
   {
   int posBar = key.find('|');
   if (posBar != string::npos)
      return key.substr(0, posBar);
   else
      return "";
   }
// ------------------------------------------------------------------
// return the key name from the specified key.
// ------------------------------------------------------------------
string ApsimSettings::getKey(const std::string& key) const
   {
   int posBar = key.find('|');
   if (posBar != string::npos)
      return key.substr(posBar+1);
   else
      return "";
   }

// ------------------------------------------------------------------
// read in a string value for the specified key.
// ------------------------------------------------------------------
void ApsimSettings::read(const std::string& key, std::string& value,
                         bool replaceMacros) const
   {
   original->read(getSection(key), getKey(key), value);
   if (replaceMacros)
      replaceAll(value, "%apsuite", getApsimDirectory());
   }

// ------------------------------------------------------------------
// read in a string value for the specified key.
// ------------------------------------------------------------------
void ApsimSettings::read(const string& key, int& value) const throw(bad_lexical_cast)
   {
   string stringValue;
   read(key, stringValue);
   value = lexical_cast<int> (stringValue);
   }
// ------------------------------------------------------------------
// read in a string value for the specified key.
// ------------------------------------------------------------------
void ApsimSettings::read(const string& key, double& value) const throw(bad_lexical_cast)
   {
   string stringValue;
   read(key, stringValue);
   value = lexical_cast<double> (stringValue);
   }
// ------------------------------------------------------------------
// Read in the contents of a given section.
// ------------------------------------------------------------------
void ApsimSettings::readSection(const std::string& sectionName, std::string& contents) const
   {
//   working->readSection(sectionName, contents);
//   if (contents == "")
      original->readSection(sectionName, contents);
   }
// ------------------------------------------------------------------
// Write out the contents of a given section.
// ------------------------------------------------------------------
void ApsimSettings::writeSection(const std::string& sectionName, std::string& contents) const
   {
//   working->readSection(sectionName, contents);
//   if (contents == "")
      original->writeSection(sectionName, contents);
   }
// ------------------------------------------------------------------
// Write out the contents of a given section.
// ------------------------------------------------------------------
void ApsimSettings::deleteSection(const std::string& sectionName) const
   {
//   working->readSection(sectionName, contents);
//   if (contents == "")
      original->deleteSection(sectionName);
   }
// ------------------------------------------------------------------
// Read and return a list of values for the specified key.
// ------------------------------------------------------------------
void ApsimSettings::read(const string& key, vector<string>& values,
                         bool replaceMacros) const
   {
   original->read(getSection(key), getKey(key), values);
   if (replaceMacros)
      {
      for (unsigned v = 0; v != values.size(); v++)
         replaceAll(values[v], "%apsuite", getApsimDirectory());
      }
   }
// ------------------------------------------------------------------
//	Write a key to ini file
// ------------------------------------------------------------------
void ApsimSettings::write(const string& key, const string& value)
   {
   original->write(getSection(key), getKey(key), value);
   }
// ------------------------------------------------------------------
//	Write a key to ini file
// ------------------------------------------------------------------
void ApsimSettings::write(const string& key, int value)
   {
   original->write(getSection(key), getKey(key), lexical_cast<string>(value));
   }
// ------------------------------------------------------------------
//	Write a key to ini file
// ------------------------------------------------------------------
void ApsimSettings::write(const string& key, double value)
   {
   original->write(getSection(key), getKey(key), lexical_cast<string>(value));
   }
// ------------------------------------------------------------------
//	Write a key to ini file
// ------------------------------------------------------------------
void ApsimSettings::write(const string& key, const vector<string>& values)
   {
   original->write(getSection(key), getKey(key), values);
   }
// ------------------------------------------------------------------
// Return a complete list of all keys under the specified key.
// ------------------------------------------------------------------
void ApsimSettings::getKeysUnder(const string& key, vector<string>& keys)
	{
//   working->getKeysInSection(getSection(key), keys);
//   if (keys.size() == 0)
      original->getKeysInSection(getSection(key), keys);
   }
// ------------------------------------------------------------------
// Return a complete list of all section names.
// ------------------------------------------------------------------
void ApsimSettings::getSectionNames(vector<string>& sections) const
	{
   original->readSectionNames(sections);
   }

// ------------------------------------------------------------------
// Erase the specified section.
// ------------------------------------------------------------------
void ApsimSettings::deleteSection(const std::string& section)
   {
   original->deleteSection(section);
   }
// ------------------------------------------------------------------
// Erase the specified key
// ------------------------------------------------------------------
void ApsimSettings::deleteKey(const std::string& key)
   {
   original->deleteKey(getSection(key), getKey(key));
   }

