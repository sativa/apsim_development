//---------------------------------------------------------------------------
#ifndef ReportComponentH
#define ReportComponentH
#include <Component.h>
#include <aps\APSIMOutputFile.h>
#include <string>
#include <vector>
#include <list>
class Field
   {
   public:
      Field (protocol::Component* parent,
             const std::string& ModuleName,
             const std::string& VariableName,
             const std::string& VariableAlias,
             bool CSVFormat);

      void writeHeadings(std::ostream& headingOut, std::ostream& unitOut);
      void writeValue(std::ostream& out);
      void writeToSummary(void);

   private:
      protocol::Component* parent;
      std::string ModuleName;
      std::string VariableName;
      std::string VariableAlias;
      std::string VariableUnits;
      bool CSVFormat;
      unsigned variableID;
      unsigned int fieldWidth;
      std::vector<std::string> values;
      std::string unit;

      bool getValues(void);
      void calcFieldWidth(protocol::Variant* variant, bool ok);
      void writeTo(std::ostream& out, const std::string& value);

   };
// ------------------------------------------------------------------
//  Short description:
//     INIT entry point

//  Notes:
//     ACTIONS:
//        'do_output' - immediately write a line of output to output file.
//        'do_end_day_output' - write a line of output to output file at end of day.

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
class ReportComponent : public protocol::Component
   {
   public:
      ReportComponent(void);
      ~ReportComponent(void);
      virtual void doInit1(const FString& sdml);
      virtual void doInit2(void);
      virtual void respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData);
      virtual void respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);
      virtual void respondToMethod(unsigned int& fromID, unsigned int& methodID, protocol::Variant& variant);

   private:
      APSIMOutputFile* Out;
      bool OutputOnThisDay;
      typedef std::list<Field> Fields;
      Fields fields;
      int DaysSinceLastReport;
      bool CSVFormat;

      unsigned repEventID;
      unsigned doOutputID;
      unsigned doEndDayOutputID;
      unsigned daysSinceLastReportVariableID;
      unsigned tempID;

      void writeHeadings(void);
      void WriteLineOfOutput(void);
   };


#endif
