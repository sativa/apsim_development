#ifndef ApsimLicence_H
#define ApsimLicence_H

#include <string>
#include <vector>

// ------------------------------------------------------------------
// Class to read and write a licence
// ------------------------------------------------------------------
class ApsimLicence
   {
   protected:
      bool isValid;

      bool convertToASCII(unsigned char *binaryImage, long sizeOfBinaryImage);
      void convertToBinary(unsigned char *ASCIIImage, long sizeOfBinaryImage);

   public:
      ApsimLicence(void);
      void read(const std::string& fileName) throw(runtime_error);
      void write(const std::string& fileName);
      bool isLicenced(const std::string& module);
      bool isSource(const std::string& module);

      std::string getName(void);
      std::string getOrganisation(void);
      std::string getLocation(void);
      void setName(const std::string& name);
      void setOrganisation(const std::string& organisation);
      void setLocation(const std::string& location);
      void addModule(const string& moduleName, bool source, bool licenced) throw(std::runtime_error);

   };


#endif

