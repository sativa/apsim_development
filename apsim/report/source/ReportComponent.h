//---------------------------------------------------------------------------
#ifndef ReportComponentH
#define ReportComponentH
#include <APSIMComponent.h>
#include <APSIMReportVariables.h>
class Field
   {
   public:
      Field (const string& ModuleName,
             const string& VariableName,
             const string& VariableAlias,
             bool CSVFormat);

      void writeHeading(ostream& out);
      void writeUnits(ostream& out);
      void writeValue(ostream& out);
      void accumulateValue(void);
      bool isFunction(void) {return (FunctionName.length() > 0);}

   private:
      string ModuleName;
      string VariableName;
      string VariableAlias;
      string VariableUnits;
      string FunctionName;
      int NumTimesAccumulated;
      bool CSVFormat;

      APSIMVariant::TypeCodesEnum VariableType;
      bool VariableFound;
      unsigned int NumElements;
      vector<string> Values;
      vector<double> FunctionValues;

      string truncateSt (const string& st, unsigned int Width);
      unsigned int getWidth();
      void retrieveValue(void);
      void WriteString (ostream& out, const string& st);

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
      ReportComponent(const string& Name)
         : APSIMComponent(Name)
         {
         OutputOnThisDay = false;
         HaveAccumulatedVarsToday = false;
         }

      virtual void Init (void);
      virtual bool GetVariable (const char* VariableName);
      virtual bool DoAction(const char* Action);

   private:
      APSIMReportVariables Variables;
      APSIMOutputFile Out;
      bool HaveAccumulatedVarsToday;
      bool OutputOnThisDay;
      list<Field> Fields;
      bool SomeFieldsAreFuntions;
      int DaysSinceLastReport;
      bool CSVFormat;

      void Setup(void);
      void WriteLineOfOutput(void);
      void AccumulateVariables(void);

   };


#endif
