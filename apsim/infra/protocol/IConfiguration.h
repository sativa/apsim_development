//---------------------------------------------------------------------------
#ifndef IConfigurationH
#define IConfigurationH
#include <string>
#include <list>
// ------------------------------------------------------------------
//  Short description:
//    Interface to get configuration of a component

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
class IComponentConfiguration
   {
   public:
      virtual ~IComponentConfiguration(void) { };

      virtual std::string getName(void) const = 0;
      virtual std::string getDllFilename(void) const = 0;
      virtual void write(std::ostream& out) const = 0;
   };

// ------------------------------------------------------------------
//  Short description:
//    Interface to get configuration of a system.

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
class ISystemConfiguration
   {
   public:
      virtual ~ISystemConfiguration(void) { };

      virtual std::string getName(void) const = 0;

      virtual IComponentConfiguration* getIComponent(const std::string& componentName) = 0;
      virtual void getComponentNames(std::list<std::string>& names) const = 0;

      virtual ISystemConfiguration* getISystem(const std::string& systemName) = 0;
      virtual void getSystemNames(std::list<std::string>& names) const = 0;

   };

// ------------------------------------------------------------------
//  Short description:
//    Interface to get configuration of a simulation

//  Notes:

//  Changes:
//    dph 22/2/2000

// ------------------------------------------------------------------
class ISimulationConfiguration
   {
   public:
      virtual ~ISimulationConfiguration(void) { };

      virtual std::string getName(void) const = 0;

      virtual ISystemConfiguration* getISystem(const std::string& systemName) = 0;
      virtual void getSystemNames(std::list<std::string>& names) const = 0;
   };

#endif