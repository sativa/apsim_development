//---------------------------------------------------------------------------
#ifndef InputComponentH
#define InputComponentH
#include <ComponentInterface\Component.h>
#include "StringVariant.h"
#include <map>
class ApsimDataFile;
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
      typedef std::map<unsigned, StringVariant*> InputVariables;
      typedef std::vector<StringVariant*> TemporalVariables;
      InputVariables variables;
      TemporalVariables temporalVariables;
      bool iAmMet;
      bool allowSparseData;
      ApsimDataFile* data;
      std::string fileName;

      unsigned newmetID;
      unsigned preNewmetID;
      unsigned tickID;
      unsigned daylengthID;
      double todaysDate;
      StringVariant* yearI;
      StringVariant* dayOfYearI;
      StringVariant* dayOfMonthI;
      StringVariant* monthI;

      void readConstants(void);
      void readHeadings(void);
      InputVariables::iterator findVariable(const string& name);
      bool removeArraySpec(const std::string& fieldName,
                           std::string& fieldNameMinusSpec,
                           unsigned int& arraySpec);
      void doRegistrations(void);
      void checkForSparseData(void);
      bool advanceToTodaysData(void);
      unsigned long getFileDate(void);
      void dateFieldsOk(void);
      void readLineFromFile();
      bool getVariableValue(const std::string& name, float& value);
      void publishNewMetEvent(void);
      void addVariable(const std::string& name,
                       const std::string& units,
                       const std::string& value,
                       unsigned arrayIndex,
                       bool isTemporal,
                       bool isArray);
      float calcDayLength(void);
      float dayLength(int dyoyr, float lat, float sun_angle);
   };
#endif
