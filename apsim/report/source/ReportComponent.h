//---------------------------------------------------------------------------
#ifndef ReportComponentH
#define ReportComponentH
#include <string>
#include <vector>
#include <fstream>
class ScienceAPI;

// ------------------------------------------------------------------
// A class encapsulating a user specified field (column) that needs
// to be written to an ostream. A field can have multiple values if
// it is an array.
// ------------------------------------------------------------------
class Field
   {
   public:
      Field (ScienceAPI& scienceAPI,
             const std::string& fqn,
             const std::string& units,
             const std::string& alias,
             const std::string& nastring,
             const std::string& format,
             bool csv);

      void writeHeadings(std::ostream& out);
      void writeUnits(std::ostream& out);
      void writeValue(std::ostream& out);
      void writeToSummary(void);

   private:
      ScienceAPI& scienceAPI;
      std::string fqn;
      std::string units;
      std::string kind;
      std::string alias;
      std::string nastring;
      std::string format;
      bool csv;

      std::vector<unsigned> widths;
      std::vector<string> values;

      void getValues();
      void writeValueTo(std::ostream& out, const std::string& value, unsigned fieldWidth);
      void formatValues(void);
   };
// ------------------------------------------------------------------
// Main report component
// ------------------------------------------------------------------
class ReportComponent
   {
   public:
      ReportComponent(ScienceAPI& scienceAPI);
      virtual void onInit1();
      virtual void onInit2();

   private:
      ScienceAPI& scienceAPI;
      std::ofstream file;
      bool outputOnThisDay;
      std::vector<std::string> variableLines;
      std::vector<Field> fields;
      int daysSinceLastReport;
      bool csv;
      bool haveWrittenHeadings;
      int precision;
      std::string nastring;

      void createVariable(const std::string& name);
      void onReport();
      void onDoOutput();
      void onDoEndDayOutput();
      void onFrequency();      

      std::string calcFileName();
      void writeHeadings(void);
      void writeLineOfOutput(void);
   };


#endif