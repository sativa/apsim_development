//---------------------------------------------------------------------------
#ifndef ApsimSimulationFileH
#define ApsimSimulationFileH
#include "ApsimComponentData.h"
#include "ApsimSystemData.h"
#include "ApsimServiceData.h"
#include <string>
#include <vector>
class XMLDocument;
// ------------------------------------------------------------------
// This class encapsulates an APSIM simulation file (.SIM)
// ------------------------------------------------------------------
class __declspec(dllexport) ApsimSimulationFile
   {
   public:
      ApsimSimulationFile(void);
      ApsimSimulationFile(const std::string& filename) throw (std::runtime_error);
      ApsimSimulationFile(const std::string& xml, bool dummy) throw (std::runtime_error);
      ~ApsimSimulationFile(void);

      void write(void) const;

      std::string getFileName(void) const {return fileName;}
      std::string getName(void) const;
      std::string getExecutableFileName(void) const;
      std::string getTitle(void) const;
      void setFileName(const std::string& file) {fileName = file;}
      void setName(const std::string& name);
      void setExecutableFileName(const std::string& executableFileName);
      void setTitle(const std::string& title);

      // system methods.
      void getSystemNames(std::vector<std::string>& systemNames) const;
      ApsimSystemData getSystem(const std::string& name) const throw(std::runtime_error);
      ApsimSystemData addSystem(const std::string& name);

      // component methods.
      void getComponentNames(std::vector<std::string>& componentNames) const;
      ApsimComponentData getComponent(const std::string& name) const throw(std::runtime_error);
      ApsimComponentData addComponent(const std::string& name);
      bool deleteComponent(const std::string& name);

      // service methods.
      void getServiceNames(std::vector<std::string>& serviceNames) const;
      ApsimServiceData getService(const std::string& name) const throw(std::runtime_error);
      ApsimServiceData addService(const std::string& name);

      // return true if simulation is dirty.
      bool isDirty(void) const {return xmlDoc->isDirty();}
   protected:
      XMLDocument* xmlDoc;
      std::string fileName;
   };
#endif
