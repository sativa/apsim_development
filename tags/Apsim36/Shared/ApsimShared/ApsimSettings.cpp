//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ApsimSettings.h"
#include "ApsimDirectories.h"
#include <boost\lexical_cast.hpp>
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
void ApsimSettings::read(const string& key, int& value) const
   {
   string stringValue;
   read(key, stringValue);
   value = lexical_cast<int> (stringValue);
   }
// ------------------------------------------------------------------
// read in a string value for the specified key.
// ------------------------------------------------------------------
void ApsimSettings::read(const string& key, double& value) const
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
// ------------------------------------------------------------------
// Save the specified form position to the apsim settings .ini file.
// ------------------------------------------------------------------
void saveFormPosition(TForm* form)
   {
   if (form->Visible)
      {
      string section = form->Caption.c_str();
      unsigned posFile = section.find(" -");
      if (posFile != string::npos)
         section.erase(posFile);
      section += " Pos";
      ApsimSettings settings;
      settings.write(section + "|FormLeft", IntToStr(form->Left).c_str());
      settings.write(section + "|FormTop", IntToStr(form->Top).c_str());
      settings.write(section + "|FormWidth", IntToStr(form->Width).c_str());
      settings.write(section + "|FormHeight", IntToStr(form->Height).c_str());
      }
   }
// ------------------------------------------------------------------
// Restore the specified form position from the apsim settings .ini file.
// ------------------------------------------------------------------
void loadFormPosition(TForm* form)
   {
   string section = form->Caption.c_str();
   unsigned posFile = section.find(" -");
   if (posFile != string::npos)
      section.erase(posFile);
   section += " Pos";
   ApsimSettings settings;
   string leftSt, topSt, widthSt, heightSt;
   settings.read(section + "|FormLeft", leftSt);
   settings.read(section + "|FormTop", topSt);
   settings.read(section + "|FormWidth", widthSt);
   settings.read(section + "|FormHeight", heightSt);
   if (leftSt != "" && topSt != "" && widthSt != "" && heightSt != "")
      {
      form->Left = atoi(leftSt.c_str());
      form->Top = atoi(topSt.c_str());
      form->Width = atoi(widthSt.c_str());
      form->Height = atoi(heightSt.c_str());
      }
   }

extern "C" void _export __stdcall SettingsRead(const char* key,
                                               char* values,
                                               int replaceMacros)
   {
   try
      {
      ApsimSettings settings;
      vector<string> valueList;
      settings.read(key, valueList, replaceMacros);
      string valueString;
      Build_string(valueList, "|", valueString);
      strcpy(values, valueString.c_str());
      }
   catch (const exception& err)
      {
      ShowMessage(err.what());
      }
   }
extern "C" void _export __stdcall SettingsWrite(const char* key,
                                                const char* values)
   {
   try
      {
      ApsimSettings settings;
      vector<string> valueList;
      Split_string(values, "|", valueList);
      settings.write(key, valueList);
      }
   catch (const exception& err)
      {
      ShowMessage(err.what());
      }
   }

