//---------------------------------------------------------------------------
#ifndef InputComponentH
#define InputComponentH
#include <Component.h>
#include "StringVariant.h"
#include <map>
// ------------------------------------------------------------------
//  Short description:
//     Encapsulates the APSIM INPUT module

//  Notes:

//  Changes:
//    DPH 27/6/2001

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

   private:
      typedef std::map<unsigned, StringVariant*> InputVariables;
      typedef std::vector<StringVariant*> TemporalVariables;
      InputVariables variables;
      TemporalVariables temporalVariables;
      bool iAmMet;
      bool allowSparseData;
      APSIMExternalTable* externalTable;

      unsigned newmetID;
      unsigned tickID;
      double todaysDate;
      StringVariant* yearI;
      StringVariant* dayOfYearI;
      StringVariant* dayOfMonthI;
      StringVariant* monthI;

      void openTable(const std::string& groupName);
      bool readConstants(void);
      bool readHeadings(void);
      InputVariables::iterator findVariable(const string& name);
      bool removeArraySpec(const std::string& fieldName,
                           std::string& fieldNameMinusSpec,
                           unsigned int& arraySpec);
      void doRegistrations(void);
      void checkForSparseData(void);
      bool advanceToTodaysData(void);
      unsigned long getFileDate(void);
      bool dateFieldsOk(void);
      bool readLineFromFile();
      bool getVariableValue(const std::string& name, float& value);
      void publishNewMetEvent(void);
      void addVariable(const std::string& name,
                       const std::string& value,
                       unsigned arrayIndex,
                       bool isTemporal = false);
   };
#endif
