//---------------------------------------------------------------------------
#ifndef ReportComponentH
#define ReportComponentH
#include <APSIMComponent.h>
#include <APSIMVariant.h>

class Field
   {
   public:
      Field (EventInterface* eventInterface,
             const string& ModuleName,
             const string& VariableName,
             const string& VariableAlias,
             bool CSVFormat);

      void init(void);

      void writeHeading(std::ostream& out);
      void writeUnits(std::ostream& out);
      void writeValue(std::ostream& out);
      void writeToSummary(void);
      void accumulateValue(void);
      bool isFunction(void) const {return (FunctionName.length() > 0);}

   private:
      EventInterface* eventInterface;
      std::string ModuleName;
      std::string VariableName;
      std::string VariableAlias;
      std::string VariableUnits;
      std::string FunctionName;
      int NumTimesAccumulated;
      bool CSVFormat;

      APSIMVariant::TypeCodesEnum VariableType;
      bool VariableFound;
      unsigned int NumElements;
   public:
      vector<string> Values;
   private:
      vector<double> FunctionValues;

      string truncateSt (const string& st, unsigned int Width);
      unsigned int getWidth();
      bool retrieveValue(void);
      void WriteString (std::ostream& out, const string& st);

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
class ReportComponent : public APSIMComponent
   {
   public:
      ReportComponent(const FString& name,
                      IComputation& computation,
                      const std::string& ssdl)
         : APSIMComponent(name, computation, ssdl)
         {
         OutputOnThisDay = false;
         HaveAccumulatedVarsToday = false;
         HaveWrittenHeadings = false;
         }

      virtual void init ();
      virtual bool getVariable (const FString& VariableName);
      virtual bool doAction(const FString& Action);

   private:
      APSIMOutputFile* Out;
      bool HaveAccumulatedVarsToday;
      bool OutputOnThisDay;
      list<Field> Fields;
      bool SomeFieldsAreFuntions;
      int DaysSinceLastReport;
      bool CSVFormat;
      bool HaveWrittenHeadings;

      void Setup(void);
      void WriteLineOfOutput(void);
      void AccumulateVariables(void);
   };


#endif
