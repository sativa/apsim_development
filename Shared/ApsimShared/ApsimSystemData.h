//---------------------------------------------------------------------------
#ifndef ApsimSystemDataH
#define ApsimSystemDataH

#include "ApsimComponentData.h"
// ------------------------------------------------------------------
// This class encapsulates the data in a component section of
// an APSIM simulation file(.SIM).
// ------------------------------------------------------------------
class __declspec(dllexport) ApsimSystemData
   {
   public:
      ApsimSystemData(const XMLNode& n);

      std::string getExecutableFileName(void) const;
      std::string getName(void) const;
      void setName(const std::string& name);
      void setExecutableFileName(const std::string& executable);

      // system methods
      void getSystemNames(std::vector<std::string>& systemNames) const;
      ApsimSystemData getSystem(const std::string& name) const throw(std::runtime_error);
      ApsimSystemData addSystem(const std::string& name);

      // component methods
      void getComponentNames(std::vector<std::string>& componentNames) const;
      ApsimComponentData getComponent(const std::string& name) const throw(std::runtime_error);
      ApsimComponentData addComponent(const std::string& name);
      ApsimComponentData addComponent(ApsimComponentData& component);
      bool deleteComponent(const std::string& name);

      std::string getXML(void) const;

      ApsimComponentData asComponent(void);

   private:
      XMLNode node;
   };
#endif
