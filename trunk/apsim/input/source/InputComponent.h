//---------------------------------------------------------------------------
#ifndef InputComponentH
#define InputComponentH
#include <ComponentInterface\Component.h>
#include "StringVariant.h"
#include <map>
#include <ApsimShared\ApsimDataFile.h>
#include <boost\date_time\gregorian\gregorian.hpp>
// ------------------------------------------------------------------
// Encapsulates the APSIM INPUT module
// ------------------------------------------------------------------
class InputComponent : public protocol::Component
   {
   public:
      InputComponent(void);
      ~InputComponent(void);
      virtual void doInit1(const FString& sdml);
      virtual void doInit2(void);
      virtual void respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData);
      virtual void respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);
      virtual bool respondToSet(unsigned int& fromID, protocol::QuerySetValueData& setValueData);

   protected:
      typedef std::map<unsigned, StringVariant> Variables;
      Variables variables;
      bool iAmMet;
      bool allowSparseData;
      ApsimDataFile data;
      std::string fileName;

      unsigned newmetID;
      unsigned preNewmetID;
      unsigned tickID;
      unsigned daylengthID;
      boost::gregorian::date todaysDate;
      boost::gregorian::date fileDate;

      void addVariable(Value& value);
      void registerAllVariables(void);
      void checkForSparseData(void);
      boost::gregorian::date advanceToTodaysData(void);
      float getVariableValue(const std::string& name);
      Variables::iterator findVariable(const std::string& name);
      void publishNewMetEvent(void);
      float calcDayLength(void);
      float dayLength(int dyoyr, float lat, float sun_angle);
   };
#endif
