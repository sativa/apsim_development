//---------------------------------------------------------------------------
#ifndef ApsimComponentDataH
#define ApsimComponentDataH

#include <general\xml.h>
#include <vector>
// ------------------------------------------------------------------
// This class encapsulates the data in a component section of
// an APSIM simulation file(.SIM).
// ------------------------------------------------------------------
class __declspec(dllexport) ApsimComponentData
   {
   public:
      ApsimComponentData(const std::string& xml);
      ApsimComponentData(const XMLNode& n);
      ~ApsimComponentData(void);

      std::string getName(void) const;
      std::string getExecutableFileName(void) const;
      void setName(const std::string& name);
      void setExecutableFileName(const std::string& executable);

      // property methods
      std::string getProperty(const std::string& name) const;
      void setProperty(const std::string& group,
                       const std::string& name,
                       const std::string& value);

      std::string getXML(void) const;
                       
      // variable methods.
      void clearVariables(void);
      void getVariables(std::vector<std::string>& variables) const;
      void addVariable(const std::string& name);

      // rule methods.
      void clearRules(void);
      void getRuleNames(std::vector<std::string>& names) const;
      std::string getRule(const std::string& name) const;
      void addRule(const std::string& name, const std::string& rule);

   private:
      XMLDocument* xmlDoc;
      XMLNode node;
      XMLNode getInitData(void) const;

   };
#endif
