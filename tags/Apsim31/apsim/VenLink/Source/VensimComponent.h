//---------------------------------------------------------------------------
#ifndef VensimComponentH
#define VensimComponentH
#include <ComponentInterface\Component.h>
#include <map>

// ------------------------------------------------------------------
// This component acts as the interface between an instance of a
// VENSIM model and an APSIM simulation.
// ------------------------------------------------------------------
class VensimComponent : public protocol::Component
   {
   public:
      VensimComponent(void);
      ~VensimComponent(void);
      virtual void doInit1(const FString& sdml);
      virtual void doInit2(void);
      virtual void respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);
      virtual void respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData);
      virtual bool respondToSet(unsigned int& fromID, protocol::QuerySetValueData& setValueData);
   private:
      typedef std::map<unsigned, std::string> VariableMap;
      VariableMap variables;
      std::string fileName;
      unsigned processID;

      void doTimestep(void);

      // ------------------------------------------------------------------
      // Register all model variables.
      // ------------------------------------------------------------------
      void registerModelVariables(void);

   };


#endif
