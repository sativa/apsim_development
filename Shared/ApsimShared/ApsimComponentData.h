//---------------------------------------------------------------------------
#ifndef ApsimComponentDataH
#define ApsimComponentDataH

#include <general\xml.h>
#include <vector>
#include "ApsimRegistrationData.h"
#include "ApsimDataTypeData.h"
#include "ApsimDataTypesFile.h"
// ------------------------------------------------------------------
// This class encapsulates the data in a component section of
// an APSIM simulation file(.SIM).
// ------------------------------------------------------------------
class __declspec(dllexport) ApsimComponentData
   {
   public:
      ApsimComponentData(void);
      ApsimComponentData(const std::string& xml);
      ApsimComponentData(const XMLNode& n);
      ~ApsimComponentData(void);

      void copyAllFrom(ApsimComponentData& from);

      std::string getName(void) const;
      std::string getExecutableFileName(void) const;
      void setName(const std::string& name);
      void setExecutableFileName(const std::string& executable);

      // property methods
      std::string getProperty(const std::string& propertyType,
                              const std::string& name) const;
      void getProperties(const std::string& propertyType,
                         std::vector<std::string>& names,
                         std::vector<std::string>& values) const;
      void setProperty(const std::string& propertyType,
                       const std::string& groupName,
                       const std::string& name,
                       const std::string& value);
      bool replaceProperty(const std::string& propertyType,
                           const std::string& name,
                           const std::string& value);
      void clearProperties(const std::string& propertyType);

      void getGroupNames(const std::string& propertyType,
                         std::vector<std::string>& groupNames);

      std::string getXML(void) const;

      // variable methods.
      void clearVariables(void);
      void getVariables(std::vector<std::string>& variables) const;
      void addVariable(const std::string& name);

      // rule methods.  All rules have a unique name.
      void clearRules(void);
      void getRuleNames(std::vector<std::string>& names) const;
      void getRule(const std::string& name,
                   std::string& condition,
                   std::string& contents) const;
      void addRule(const std::string& name,
                   const std::string& condition,
                   const std::string& contents);

      // registration methods.
      typedef TreeNodeAliasIterator< TreeNodeIterator<XMLNode>, ApsimRegistrationData> RegIterator;
      RegIterator regBegin(void) const;
      RegIterator regEnd(void) const;
      ApsimDataTypeData getDataType(const std::string& name) const throw(std::runtime_error);

      // return the name of the interface file for this component
      std::string getInterfaceFileName(void) const;

   private:
      XMLDocument* xmlDoc;
      XMLNode node;
      XMLNode getInitData(void) const;
      mutable ApsimDataTypesFile* dataTypesFile;
   };
#endif
