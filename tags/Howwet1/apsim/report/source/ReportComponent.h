//---------------------------------------------------------------------------
#ifndef ReportComponentH
#define ReportComponentH
#include <ComponentInterface/Component.h>
#include <string>
#include <vector>
class Field
   {
   public:
      Field (protocol::Component* parent,
             std::string variable,
             bool csvformat,
             const std::string& nastring,
             unsigned int precision);

      void writeHeadings(std::ostream& headingOut, std::ostream& unitOut);
      void writeValue(std::ostream& out);
      void writeToSummary(void);
      void FormatValues(void);

   private:
      protocol::Component* parent;
      std::string VariableName;
      std::string VariableFormat;
      std::string NAString;
      bool CSVFormat;
      unsigned variableID;
      unsigned int Precision;
      std::vector<std::string> headings;
      std::vector<std::string> units;
      std::vector<std::string> values;
      std::vector<unsigned> fieldWidths;

      void getValues(bool setupHeadings);
      void storeValue(protocol::Variant* variant, bool setupHeadings,
                      bool includeModuleNameInHeading);
      void calcFieldWidths();
      void writeValuesTo(std::ostream& out, std::vector<std::string>& values);
      void formatAsFloats(void);
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

   private:
      std::ofstream out;
      bool OutputOnThisDay;
      typedef std::vector<Field> Fields;
      Fields fields;
      int DaysSinceLastReport;
      bool CSVFormat;
      bool haveWrittenHeadings;
      unsigned int Precision;
      std::vector<unsigned> frequencyIds;
      std::string NAString;

      unsigned titleID;
      unsigned summaryFileID;
      unsigned repEventID;
      unsigned doOutputID;
      unsigned doEndDayOutputID;
      unsigned daysSinceLastReportVariableID;
      unsigned reportedID;

      std::string calcFileName();
      void writeHeadings(void);
      void WriteLineOfOutput(void);
   };


#endif