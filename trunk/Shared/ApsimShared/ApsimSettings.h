//---------------------------------------------------------------------------
#ifndef ApsimSettingsH
#define ApsimSettingsH

#include <string>
#include <vector>
class IniFile;
// ------------------------------------------------------------------
// This class looks after all storing of apsim settings.
// Settings are stored under keys - much like the Windows registry.
// Keys can be hierarchical and are in the format:
//           section1|section2|...|name
// ------------------------------------------------------------------
class __declspec(dllexport) ApsimSettings
    {
    public:
      ApsimSettings(void);
      ~ApsimSettings(void);

      void refresh(void);

      // Read in a setting for the specified key.  Key should be in the format:
      //    section1|section2|...|keyvalue.
      // Will throw if key value cannot be converted to int or double.
      void read(const std::string& key, std::string& value, bool replaceMacros = false) const;
      void read(const std::string& key, int& value) const;
      void read(const std::string& key, bool& value) const;
      void read(const std::string& key, double& value) const;
      void read(const std::string& key, std::vector<std::string>& values, bool replaceMacros = false) const;

      // Write a setting for the specified key.
      void write(const std::string& key, const std::string& value);
      void write(const std::string& key, int value);
      void write(const std::string& key, bool value);
      void write(const std::string& key, double value);
      void write(const std::string& key, const std::vector<std::string>& values);

      // Return a complete list of all keys under the specified key.
      void getKeysUnder(const std::string& key, std::vector<std::string>& keys);
      void getSectionNames(std::vector<std::string>& sections) const;

      // Erase the specified section or key.
      void deleteSection(const std::string& section);
      void deleteKey(const std::string& key);

      // Read and write section contents.
      void readSection(const std::string& sectionName, std::string& contents) const;
      void writeSection(const std::string& sectionName, std::string& contents) const;
      void deleteSection(const std::string& sectionName) const;

   private:
      IniFile* original;

      std::string getSection(const std::string& key) const;
      std::string getKey(const std::string& key) const;
   };

// ------------------------------------------------------------------
// Save the specified form position to the apsim settings .ini file.
// ------------------------------------------------------------------
void _export saveFormPosition(TForm* form);

// ------------------------------------------------------------------
// Restore the specified form position from the apsim settings .ini file.
// ------------------------------------------------------------------
void _export loadFormPosition(TForm* form);
   

#endif
