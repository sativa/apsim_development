//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ApsimSettings.h"
#include "ApsimDirectories.h"
#include <general\inifile.h>
#include <sstream>
using namespace std;
using namespace boost;

// ------------------------------------------------------------------
// Return the folder where all APSIM settings are located.
// ------------------------------------------------------------------
string ApsimSettings::getSettingsFolder(void) throw(runtime_error)
   {
   string settingsFolder = getApsimDirectory() + "\\settings";
   if (!DirectoryExists(settingsFolder.c_str()))
      CreateDir(settingsFolder.c_str());
   return settingsFolder;
   }
// ------------------------------------------------------------------
//	constructor
// ------------------------------------------------------------------
ApsimSettings::ApsimSettings(void)
	{
   original = new IniFile(ChangeFileExt(Application->ExeName, ".ini").c_str());
   string workingPath = getSettingsFolder() + "\\"
                      + ExtractFileName(Application->ExeName).c_str();
   workingPath = ChangeFileExt(workingPath.c_str(), ".ini").c_str();
   working = new IniFile(workingPath);
   }
// ------------------------------------------------------------------
//	destructor
// ------------------------------------------------------------------
ApsimSettings::~ApsimSettings(void)
   {
   delete original;
   delete working;
   }

// ------------------------------------------------------------------
// return the section name from the specified key.
// ------------------------------------------------------------------
string ApsimSettings::getSection(const std::string& key)
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
string ApsimSettings::getKey(const std::string& key)
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
void ApsimSettings::read(const std::string& key, std::string& value)
   {
   working->read(getSection(key), getKey(key), value);
   if (value == "")
      original->read(getSection(key), getKey(key), value);
   }

// ------------------------------------------------------------------
// read in a string value for the specified key.
// ------------------------------------------------------------------
void ApsimSettings::read(const string& key, int& value) throw(bad_lexical_cast)
   {
   string stringValue;
   read(key, stringValue);
   value = lexical_cast<int> (stringValue);
   }
// ------------------------------------------------------------------
// read in a string value for the specified key.
// ------------------------------------------------------------------
void ApsimSettings::read(const string& key, double& value) throw(bad_lexical_cast)
   {
   string stringValue;
   read(key, stringValue);
   value = lexical_cast<double> (stringValue);
   }
// ------------------------------------------------------------------
// Read and return a list of values for the specified key.
// ------------------------------------------------------------------
void ApsimSettings::read(const string& key, vector<string>& values)
   {
   working->read(getSection(key), getKey(key), values);
   if (values.size() == 0)
      original->read(getSection(key), getKey(key), values);
   }
// ------------------------------------------------------------------
//	Write a key to ini file
// ------------------------------------------------------------------
void ApsimSettings::write(const string& key, const string& value)
   {
   working->write(getSection(key), getKey(key), value);
   }
// ------------------------------------------------------------------
//	Write a key to ini file
// ------------------------------------------------------------------
void ApsimSettings::write(const string& key, int value)
   {
   working->write(getSection(key), getKey(key), lexical_cast<string>(value));
   }
// ------------------------------------------------------------------
//	Write a key to ini file
// ------------------------------------------------------------------
void ApsimSettings::write(const string& key, double value)
   {
   working->write(getSection(key), getKey(key), lexical_cast<string>(value));
   }
// ------------------------------------------------------------------
//	Write a key to ini file
// ------------------------------------------------------------------
void ApsimSettings::write(const string& key, const vector<string>& values)
   {
   working->write(getSection(key), getKey(key), values);
   }
// ------------------------------------------------------------------
// Return a complete list of all keys under the specified key.
// ------------------------------------------------------------------
void ApsimSettings::getKeysUnder(const string& key, vector<string>& keys)
	{
   working->getKeysInSection(getSection(key), keys);
   if (keys.size() == 0)
      original->getKeysInSection(getSection(key), keys);
   }

// ------------------------------------------------------------------
// Erase the specified key.  If key is a section then all child keys
// will also be removed.
// ------------------------------------------------------------------
void ApsimSettings::erase(const std::string& key)
   {
   working->deleteSection(getSection(key));
   }
   
