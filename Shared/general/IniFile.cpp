#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include <general\IniFile.h>
#include <general\string_functions.h>
#include <fstream.h>

#define INI_STRING_SIZE 2000
// ------------------------------------------------------------------
// Constructor
// ------------------------------------------------------------------
IniFile::IniFile(void)
   {
   }
// ------------------------------------------------------------------
// Constructor
// ------------------------------------------------------------------
IniFile::IniFile(const string& fileName)
   {
   setFileName(fileName);
   }
// ------------------------------------------------------------------
// Destructor
// ------------------------------------------------------------------
IniFile::~IniFile(void)
   {
   flush();
   }
// ------------------------------------------------------------------
// Set the file name of the ini file.
// ------------------------------------------------------------------
void IniFile::setFileName(const string& filename)
   {
   fileName = filename;
   }
// ------------------------------------------------------------------
// Tell windows (95 especially) to flush the ini cache to disk.
// ------------------------------------------------------------------
void IniFile::flush(void)
   {
   WritePrivateProfileString(NULL, NULL, NULL, fileName.c_str());
   }
// ------------------------------------------------------------------
// Read and return a string from the .ini file.
// ------------------------------------------------------------------
void IniFile::read(const string& section, const string& key, string& value)
   {
   char st[INI_STRING_SIZE];
   GetPrivateProfileString(section.c_str(), key.c_str(), "",
                           st, sizeof(st), fileName.c_str());
   value = st;
   }
// ------------------------------------------------------------------
// Read and return a list of strings
// ------------------------------------------------------------------
void IniFile::read(const string& section, const string& key, vector<string>& values)
	{
   values.erase(values.begin(), values.end());
   string line;

   flush();

   // Go find the section in the .ini file.  Echo all lines up to the section
   // to the output stream.
   ifstream in(fileName.c_str());
   bool found = false;
   while (!found && getline(in, line, '\n'))
      found = Str_i_Eq(getSectionName(line), section);

   // If we've found our section then look for all keys matching ours.
   if (found)
      {
      found = false;
      while (!found && getline(in, line, '\n'))
         {
         string iniValue = getKeyValue(line, key);
         if (iniValue != "")
            values.push_back(iniValue);
         found = (getSectionName(line) != "");
         }
      }
   }
// ------------------------------------------------------------------
// Read and return a list of section names.
// ------------------------------------------------------------------
void IniFile::readSectionNames(vector<string>& sections)
	{
   sections.erase(sections.begin(), sections.end());

   char st[INI_STRING_SIZE];
   GetPrivateProfileString(NULL, "", "", st, sizeof(st), fileName.c_str());
   getWordsFromDoubleNullSt(st, sections);
   }
// ------------------------------------------------------------------
// Read and return the contents of the specified section.
// ------------------------------------------------------------------
void IniFile::readSection(const string& section, string& contents)
	{
   contents = "";
   string line;
   flush();

   // Go find the section in the .ini file.  Echo all lines up to the section
   // to the output stream.
   ifstream in(fileName.c_str());
   bool found = false;
   while (!found && getline(in, line, '\n'))
      found = Str_i_Eq(getSectionName(line), section);

   // If we've found our section then copy all lines until the start
   // of the next section.
   if (found)
      {
      found = false;
      while (!found && getline(in, line, '\n'))
         {
         found = (getSectionName(line) != "");
         if (!found)
            {
            contents += line;
            contents += "\n";
            }
         }

      // remove last CR
      if (contents.length() > 0)
         contents.erase(contents.length()-1);
      }
   }
// ------------------------------------------------------------------
// Write contents to a section in file.
// ------------------------------------------------------------------
void IniFile::writeSection(const string& section, string& contents)
	{
   flush();
   string line;
   ifstream in(fileName.c_str());

   // We going to create a temporary file to write which we'll
   // rename later.
   string tempFileName = ChangeFileExt(fileName.c_str(), ".tmp").c_str();
   ofstream out(tempFileName.c_str());

   // Go find the section in the .ini file.  Echo all lines up to and including
   // the section to the output stream.
   bool found = false;
   while (!found && getline(in, line, '\n'))
      {
      found = Str_i_Eq(getSectionName(line), section);
      out << line << endl;
      }
   if (!found)
      out << "[" << section << "]" << endl;
   else
      {
      // Skip all lines in matched section.
      found = false;
      while (!found && getline(in, line, '\n'))
         found = (getSectionName(line) != "");
      }

   // put our contents into file.
   out << contents << endl;

   // Simply copy all remaining lines to output stream.
   while (in)
      {
      out << line << endl;
      getline(in, line, '\n');
      }

   // Close all files, delete current .ini file an rename our .ini file
   // to the new name.
   in.close();
   out.close();
   unlink(fileName.c_str());
   rename(tempFileName.c_str(), fileName.c_str());
   }
// ------------------------------------------------------------------
// Write a string to ini file.
// ------------------------------------------------------------------
void IniFile::write(const string& section, const string& key, const string& value)
	{
   WritePrivateProfileString(section.c_str(),
   								  key.c_str(),
       							  value.c_str(),
                             fileName.c_str());
   }
// ------------------------------------------------------------------
// Write a string list to ini file.
// ------------------------------------------------------------------
void IniFile::write(const string& section, const string& key,
                    const vector<string>& values)
	{
   string line;
   flush();
   ifstream in(fileName.c_str());

   // We going to create a temporary file to write which we'll
   // rename later.
   string tempFileName = ChangeFileExt(fileName.c_str(), ".tmp").c_str();
   ofstream out(tempFileName.c_str());

   // Go find the section in the .ini file.  Echo all lines up to and including
   // the section to the output stream.
   bool found = false;
   while (!found && getline(in, line, '\n'))
      {
      found = Str_i_Eq(getSectionName(line), section);
      out << line << endl;
      }
   if (!found)
      out << "[" << section << "]" << endl;

   // Copy all lines to temp file except for ones matching our section.
   found = false;
   while (!found && getline(in, line, '\n'))
      {
      found = (getSectionName(line) != "");
      if (getKeyValue(line, key) == "" && !found && line != "")
         out << line << endl;
      }

   // put our values into file.
   for (vector<string>::const_iterator valueI = values.begin();
                                 valueI != values.end();
                                 valueI++)
      out << key << " = " << *valueI << endl;

   // Simply copy all remaining lines to output stream.
   while (in)
      {
      out << line << endl;
      getline(in, line, '\n');
      }

   // Close all files, delete current .ini file an rename our .ini file
   // to the new name.
   in.close();
   out.close();
   unlink(fileName.c_str());
   rename(tempFileName.c_str(), fileName.c_str());
   }
// ------------------------------------------------------------------
// Delete the key name from the specified section
// ------------------------------------------------------------------
void IniFile::deleteKey(const string& section, const string& key)
	{
   WritePrivateProfileString (section.c_str(),
   									key.c_str(),
      								NULL,
                              fileName.c_str());
   }
// ------------------------------------------------------------------
// Delete the section from the .ini file.  Return true if section was
// deleted.
// ------------------------------------------------------------------
void IniFile::deleteSection(const string& section)
	{
   WritePrivateProfileString (section.c_str(),
   									NULL,
      								NULL,
                              fileName.c_str());
   }
// ------------------------------------------------------------------
// Return a complete list of all keys in the specified section.
// ------------------------------------------------------------------
void IniFile::getKeysInSection(const string& section, vector<string>& keys)
	{
   string sectionContents;
   readSection(section, sectionContents);

   // loop through all lines in contents and store in key_names and key_values.
   istringstream in(sectionContents);
   string line;
   while (getline(in, line, '\n'))
      {
      if (line != "")
         {
         string keyFromSection;
         string value;
         getKeyNameAndValue(line, keyFromSection, value);
         if (keyFromSection != "")
            keys.push_back(keyFromSection);
         }
      }
   }
// ------------------------------------------------------------------
// Helper function - Get a section name from the specified line.
// ie look for [section] on the line passed in.
// Returns name if found.  Blank otherwise.
// ------------------------------------------------------------------
string getSectionName(const std::string& line)
   {
   string section;
   unsigned int posOpen = line.find_first_not_of (" \t");
   if (posOpen != string::npos && line[posOpen] == '[')
      {
      int posClose = line.find(']');
      if (posClose != string::npos)
         section = line.substr(posOpen+1, posClose-posOpen-1);
      }
   return section;
   }
// ------------------------------------------------------------------
// Get a value from an .ini line. ie look for keyname = keyvalue
// on the line passed in.  Returns the value if found or blank otherwise.
// ------------------------------------------------------------------
string getKeyValue(const string& line, const string& key)
   {
   string keyFromLine;
   string valueFromLine;
   getKeyNameAndValue(line, keyFromLine, valueFromLine);
   if (keyFromLine == key)
      return valueFromLine;
   else
      return "";
   }
// ------------------------------------------------------------------
// Return the key name and value on the line.
// ------------------------------------------------------------------
void getKeyNameAndValue(const string& line, string& key, string& value)
   {
   int posEquals = line.find('=');
   if (posEquals > 0)
      {
      key = line.substr(0, posEquals);
      Strip(key, " ");
      value = line.substr(posEquals+1);
      Strip(key, " ");
      }
   else
      {
      key = "";
      value = "";
      }
   }

