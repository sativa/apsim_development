#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include <general\IniFile.h>
#include <general\string_functions.h>
#include <general\stl_functions.h>
#include <fstream.h>

using namespace std;
typedef vector<pair<unsigned, unsigned> > Indexes;
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
   }
// ------------------------------------------------------------------
// Set the file name of the ini file.
// ------------------------------------------------------------------
void IniFile::setFileName(const string& file)
   {
   fileName = file;
   if (ExtractFileDir(fileName.c_str()) == "")
      {
      string fullPath = string(GetCurrentDir().c_str()) + "\\" + fileName;
      fileName = fullPath;
      }
   parse();
   }
// ------------------------------------------------------------------
// Re-read the .ini file.
// ------------------------------------------------------------------
void IniFile::refresh(void)
   {
   parse();
   }
// ------------------------------------------------------------------
// Parse the .ini file to get positions of sections.
// ------------------------------------------------------------------
void IniFile::parse(void)
   {
   sectionNames.erase(sectionNames.begin(), sectionNames.end());
   sectionIndexes.erase(sectionIndexes.begin(), sectionIndexes.end());

   ifstream in(fileName.c_str());
   ostringstream s;
   s << in.rdbuf();
   contents = s.str();

   unsigned posStartLine = 0;
   unsigned posEol = contents.find('\n');
   while (posEol != string::npos)
      {
      unsigned int posOpen = contents.find_first_not_of (" \t", posStartLine);
      if (posOpen != string::npos && contents[posOpen] == '[')
         {
         int posClose = contents.find(']', posOpen);
         if (posClose != string::npos)
            {
            string sectionName = contents.substr(posOpen+1, posClose-posOpen-1);
            Strip(sectionName, " ");
            sectionNames.push_back(sectionName);
            sectionIndexes.push_back(posOpen);
            }
         posOpen = posClose;
         }
      posEol = contents.find('\n', posOpen);
      posStartLine = posEol + 1;
      }
   }
// ------------------------------------------------------------------
// Get the next line (tabs removed) starting from pos.
// Return true on success and updates pos to reflect the start of
// next line.
// ------------------------------------------------------------------
bool getIniLine(const string& contents, unsigned& pos, string& line)
   {
   if (pos == string::npos || pos == contents.length())
      return false;

   unsigned posEol = contents.find('\n', pos);
   if (posEol == string::npos)
      {
      line = contents.substr(pos);
      pos = string::npos;
      }
   else
      {
      line = contents.substr(pos, posEol - pos);
      pos = posEol + 1;
      }

   return true;
   }
// ------------------------------------------------------------------
// Strip all comments and tab characters from specified line.
// ------------------------------------------------------------------
void stripComments(std::string& line)
   {
   // remove tabs.
   replaceAll(line, "\t", "   ");

   unsigned posComment = line.find_first_of("!");
   if (posComment != string::npos)
      line.erase(posComment);
   }
// ------------------------------------------------------------------
// Find lines in the specified section that match the specified key.
// Return each lines start and end pos in the section.
// Return true if keys were found.
// ------------------------------------------------------------------
bool findLinePosForKeys(const string& contents,
                        const string& key,
                        Indexes& indexes,
                        bool allowMultiple)
   {
   string line;
   unsigned startPos = 0;
   unsigned endPos = 0;
   while(getIniLine(contents, endPos, line))
      {
      stripComments(line);
      string iniValue = getKeyValue(line, key);
      if (iniValue != "")
         {
         indexes.push_back(make_pair(startPos, endPos-startPos-1));
         if (!allowMultiple)
            return true;
         }
      startPos = endPos;
      }
   return (indexes.size() > 0);
   }
// ------------------------------------------------------------------
// delete the lines from conents as specified in indexes.
// ------------------------------------------------------------------
void deleteLinePos(string& contents, Indexes& indexes)
   {
   // remove all key lines in reverse order.  Make sure the carraige
   // return is also deleted.  It is not included in the
   // indexes[i-1].second variable hence the +1 below.
   for (unsigned i = indexes.size(); i != 0; i--)
      contents.erase(indexes[i-1].first, indexes[i-1].second+1);
   }
// ------------------------------------------------------------------
// Read and return a string from the .ini file.
// ------------------------------------------------------------------
bool IniFile::read(const string& sectionName, const string& key, string& value) const
   {
   vector<string> values;
   if (findMatchingKeys(sectionName, key, values, true))
      {
      value = values[0];
      return true;
      }
   else
      {
      value = "";
      return false;
      }
   }
// ------------------------------------------------------------------
// Read and return a list of strings
// ------------------------------------------------------------------
bool IniFile::read(const string& sectionName, const string& key,
                   vector<string>& values) const
   {
   return findMatchingKeys(sectionName, key, values, true);
   }
// ------------------------------------------------------------------
// Read and return a list of values matching the specified key.
// Returns true if values were found.
// ------------------------------------------------------------------
bool IniFile::findMatchingKeys(const string& sectionName, const string& key,
                               vector<string>& values, bool allowMultiple) const
	{
   string line;
   string sectionContents;
   readSection(sectionName, sectionContents);

   Indexes indexes;
   findLinePosForKeys(sectionContents, key, indexes, allowMultiple);
   for (Indexes::iterator i = indexes.begin();
                          i != indexes.end();
                          i++)
      {
      string line = sectionContents.substr(i->first, i->second);
      stripComments(line);
      string value = getKeyValue(line, key);

      values.push_back(value);
      stripComments(values[values.size()-1]);
      }
   return (values.size() > 0);
   }
// ------------------------------------------------------------------
// Read and return a list of section names.
// ------------------------------------------------------------------
void IniFile::readSectionNames(vector<string>& sections) const
	{
   sections = sectionNames;
   }
// ------------------------------------------------------------------
// Read and return the contents of the specified section.
// ------------------------------------------------------------------
void IniFile::readSection(const string& section, string& contentsString) const
	{
   unsigned posStartSection;
   unsigned posEndSection;
   if (getSectionPosition(section, posStartSection, posEndSection))
      contentsString = contents.substr(posStartSection, posEndSection - posStartSection + 1);
   }
// ------------------------------------------------------------------
// Return the start and end positions of a section.
// ------------------------------------------------------------------
bool IniFile::getSectionPosition(const string& section,
                                 unsigned& posStartSection,
                                 unsigned& posEndSection) const
	{
   vector<string>::const_iterator i = find_if(sectionNames.begin(),
                                              sectionNames.end(),
                                              CaseInsensitiveStringComparison(section));
   if (i != sectionNames.end())
      {
      unsigned posSectionName = sectionIndexes[i-sectionNames.begin()];
      posStartSection = contents.find('\n', posSectionName);
      if (posStartSection != string::npos)
         {
         posStartSection++;
         i++;
         if (i == sectionNames.end())
            posEndSection = contents.length();
         else
            posEndSection = sectionIndexes[i-sectionNames.begin()] - 1;
         }
      return true;
      }
   return false;
   }
// ------------------------------------------------------------------
// update all section indexes by the specified number after the specified section.
// ------------------------------------------------------------------
void IniFile::updateIndexesAfter(const string& section, unsigned numChars)
   {
   vector<string>::const_iterator i = find(sectionNames.begin(),
                                           sectionNames.end(),
                                           section);
   if (i != sectionNames.end())
      {
      i++;
      while (i != sectionNames.end())
         {
         sectionIndexes[i-sectionNames.begin()] += numChars;
         i++;
         }
      }
   }
// ------------------------------------------------------------------
// Write contents to a section in file.
// ------------------------------------------------------------------
void IniFile::writeSection(const string& section, const string& newContents)
	{
   unsigned posStartSection;
   unsigned posEndSection;
   if (getSectionPosition(section, posStartSection, posEndSection))
      {
      // make sure we have a carriage return before the start of the next
      // section.
      if (newContents[newContents.length()-1] != '\n')
         posEndSection--;
      unsigned numCharsReplaced = posEndSection-posStartSection+1;
      contents.replace(posStartSection, numCharsReplaced, newContents);
      updateIndexesAfter(section, newContents.length() - numCharsReplaced);
      }
   else
      {
      // make sure contents ends with a \n\n
      if (contents.length() > 2)
         {
         char ch1 = contents[contents.length()-1];
         char ch2 = contents[contents.length()-2];
         if (ch1 == '\n')
            {
            if (ch2 != '\n')
               contents += "\n";
            }
         else
            contents = "\n\n";
         }
      sectionNames.push_back(section);
      sectionIndexes.push_back(contents.length());

      contents += "[" + section + "]\n";
      contents += newContents;
      }
   ofstream out(fileName.c_str());
   out << contents;
   }
// ------------------------------------------------------------------
// Write a string to ini file.
// ------------------------------------------------------------------
void IniFile::write(const string& section, const string& key, const string& value)
	{
   if (value == "")
      deleteKey(section, key);
   else
      {
      vector<string> values;
      values.push_back(value);
      write(section, key, values);
      }
   }
// ------------------------------------------------------------------
// Write a string list to ini file.
// ------------------------------------------------------------------
void IniFile::write(const string& section, const string& key,
                    const vector<string>& values)
	{
   unsigned insertPos;
   string contents;
   readSection(section, contents);
   Indexes indexes;
   if (findLinePosForKeys(contents, key, indexes, true))
      {
      insertPos = indexes[0].first;
      deleteLinePos(contents, indexes);
      }
   else
      insertPos = contents.length();

   // insert new key lines.
   string newContents;
   for (unsigned i = 0; i != values.size(); i++)
      newContents += key + " = " + values[i] + "\n";
   contents.insert(insertPos, newContents);
   writeSection(section, contents);
   }
// ------------------------------------------------------------------
// Delete all keys that match key from the specified section.  This
// method handles the situation where keys may exist multiple times
// in a section.
// ------------------------------------------------------------------
void IniFile::deleteKey(const string& section, const string& key)
	{
   unsigned insertPos;
   string contents;
   readSection(section, contents);
   Indexes indexes;
   if (findLinePosForKeys(contents, key, indexes, true))
      {
      deleteLinePos(contents, indexes);
      writeSection(section, contents);
      }
   }
// ------------------------------------------------------------------
// Delete the section from the .ini file.  Return true if section was
// deleted.
// ------------------------------------------------------------------
void IniFile::deleteSection(const string& section)
	{
   vector<string>::iterator i = find(sectionNames.begin(),
                                     sectionNames.end(),
                                     section);
   if (i != sectionNames.end())
      {
      vector<string>::iterator nextI = i + 1;
      unsigned posStart = sectionIndexes[i - sectionNames.begin()];
      unsigned numCharsToDelete;
      if (nextI == sectionNames.end())
         numCharsToDelete = string::npos;

      else
         {
         numCharsToDelete = sectionIndexes[nextI - sectionNames.begin()] - posStart;
         int delta = numCharsToDelete;
         updateIndexesAfter(*i, -delta);
         }
      sectionNames.erase(i);
      sectionIndexes.erase(sectionIndexes.begin() + (i-sectionNames.begin()));

      contents.erase(posStart, numCharsToDelete);
      ofstream out(fileName.c_str());
      out << contents;
      }

   }
// ------------------------------------------------------------------
// Return a complete list of all keys in the specified section.
// ------------------------------------------------------------------
void IniFile::getKeysInSection(const string& section,
                               vector<string>& keys) const
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
         stripComments(line);
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
   Strip(section, " ");
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
   if (Str_i_Eq(keyFromLine, key))
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
   if (posEquals != string::npos)
      {
      key = line.substr(0, posEquals);
      Strip(key, " ");
      value = line.substr(posEquals+1);
      Strip(value, " ");
      }
   else
      {
      key = "";
      value = "";
      }
   }
// ------------------------------------------------------------------
// rename the specified section
// ------------------------------------------------------------------
void IniFile::renameSection(const string& oldSection,
                            const string& newSection)
	{
   vector<string>::iterator i = find(sectionNames.begin(),
                                     sectionNames.end(),
                                     oldSection);
   if (i != sectionNames.end())
      {
      *i = newSection;
      updateIndexesAfter(newSection, newSection.length() - oldSection.length());

      unsigned posSectionName = contents.find(oldSection,
                                              sectionIndexes[i-sectionNames.begin()]);
      contents.replace(posSectionName, oldSection.length(), newSection);
      ofstream out(fileName.c_str());
      out << contents;
      }
   }
// ------------------------------------------------------------------
// rename the specified key.  Return true if key was modified.
// ------------------------------------------------------------------
bool IniFile::renameKey(const std::string& section,
                        const std::string& oldKey,
                        const std::string& newKey)
   {
   string sectionContents;
   readSection(section, sectionContents);
   Indexes indexes;
   if (findLinePosForKeys(sectionContents, oldKey, indexes, true))
      {
      for (unsigned i = indexes.size(); i > 0; i--)
         {
         string line = sectionContents.substr(indexes[i-1].first, indexes[i-1].second);
         string key, value;
         getKeyNameAndValue(line, key, value);
         line = newKey + " = " + value;
         sectionContents.replace(indexes[i-1].first, indexes[i-1].second, line);
         }
      writeSection(section, sectionContents);
      return true;
      }
   return false;
   }

