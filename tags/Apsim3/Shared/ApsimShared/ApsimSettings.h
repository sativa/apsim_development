//---------------------------------------------------------------------------
#ifndef ApsimSettingsH
#define ApsimSettingsH

#include <string>
#include <vector>
#include <boost\lexical_cast.hpp>
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

      // Read in a setting for the specified key.  Key should be in the format:
      //    section1|section2|...|keyvalue.
      // Will throw if key value cannot be converted to int or double.
      void read(const std::string& key, std::string& value) const;
      void read(const std::string& key, int& value) const throw(boost::bad_lexical_cast);
      void read(const std::string& key, double& value) const throw(boost::bad_lexical_cast);
      void read(const std::string& key, std::vector<std::string>& values) const;

      // Write a setting for the specified key.
      void write(const std::string& key, const std::string& value);
      void write(const std::string& key, int value);
      void write(const std::string& key, double value);
      void write(const std::string& key, const std::vector<std::string>& values);

      // Return a complete list of all keys under the specified key.
      void getKeysUnder(const std::string& key, std::vector<std::string>& keys);
      void getSectionNames(vector<string>& sections) const;

      // Erase the specified key.  If key is a section then all child keys
      // will also be removed.
      void erase(const std::string& key);

      // Read and write section contents.
      void readSection(const std::string& sectionName, std::string& contents) const;
      void writeSection(const std::string& sectionName, std::string& contents) const;
      void deleteSection(const std::string& sectionName) const;

      // Return the folder where all APSIM settings are located.
      // Will throw if the current application is not in the apsim directory.
      static std::string getSettingsFolder(void) throw(std::runtime_error);

   private:
      IniFile* original;
//      IniFile* working;

      std::string getSection(const std::string& key) const;
      std::string getKey(const std::string& key) const;
   };

#endif