#ifndef IniFileH
#define IniFileH

#include <vector>
#include <string>
// ------------------------------------------------------------------
//  Short description:
//    Class handling the reading and writting to/from INI files.

//  Notes:

//  Changes:
//    DPH 17/11/94
//    DPH 13/5/1997 - reworked to use standard template library.
//    dph 30/3/1999 - added Write_section_contents
//    dph 1/4/1999  - added Read_section_names

// ------------------------------------------------------------------
class IniFile
	{
   public:
      IniFile(void);
      ~IniFile(void);
      IniFile(const std::string& File_name);

      void setFileName(const std::string& fileName);

      void read(const std::string& section,
                const std::string& key,
                std::string& value) const;
      void read(const std::string& section,
                const std::string& key,
                std::vector<std::string>& values) const;
      void readSectionNames(std::vector<std::string>& sections) const;
      void readSection(const std::string& section, std::string& contents) const;
      void writeSection(const std::string& section, const std::string& contents);

      void write(const std::string& section,
                 const std::string& key,
                 const std::string& value);
      void write(const std::string& section,
                 const std::string& key,
                 const std::vector<std::string>& values);

      void deleteKey(const std::string& section, const std::string& key);
      void deleteKeys(const std::string& section, const std::string& key);
      void deleteSection(const std::string& Section);
      void getKeysInSection(const std::string& section,
                            std::vector<std::string>& keys) const;
      void renameSection(const std::string& oldSection,
                         const std::string& newSection) const;

   private:
      void flush(void) const;
      std::string fileName;
   };

// ------------------------------------------------------------------
// Helper function - Get a section name from the specified line.
// ie look for [section] on the line passed in.
// Returns name if found.  Blank otherwise.
// ------------------------------------------------------------------
std::string getSectionName(const std::string& line);

// ------------------------------------------------------------------
// Get a value from an .ini line. ie look for keyname = keyvalue
// on the line passed in.  Returns the value if found or blank otherwise.
// ------------------------------------------------------------------
std::string getKeyValue(const std::string& line, const std::string& key);

// ------------------------------------------------------------------
// Return the key name and value on the line.
// ------------------------------------------------------------------
void getKeyNameAndValue(const std::string& line,
                        std::string& key,
                        std::string& value);

#endif
