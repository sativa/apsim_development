//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "ReportComponent.h"
#include <general\math_functions.h>
#include <general\stl_functions.h>
#include <strstream>
#include <apsimproperty.h>
using std::ostrstream;
#pragma package(smart_init)

static const char* MES_Prepare = "prepare";
static const char* MES_Report = "rep";

// ------------------------------------------------------------------
//  Short description:
//     constructor

//  Notes:

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
Field::Field (const string& modulename,
              const string& variablename,
              const string& variablealias,
              bool csvformat)
   {
   ModuleName = modulename;
   VariableName = variablename;
   VariableAlias = variablealias;
   CSVFormat = csvformat;

   // extract a function name if found.
   size_t PosAmpersand = VariableName.find("@");
   if (PosAmpersand != string::npos)
      {
      FunctionName = VariableName.substr(0, PosAmpersand);
      if (Str_i_Eq(FunctionName, "avg") || Str_i_Eq(FunctionName, "sum"))
         {
         if (VariableAlias == "-")
            VariableAlias = VariableName;
         VariableName.erase(0, PosAmpersand+1);
         }
      else
         FunctionName = "";
      }

   // get the value of specified variable.
   APSIMVariant Variable;
   if (ApsimSystem().Loader.GetOtherVar(ModuleName, VariableName, Variable, true))
      {
      VariableUnits = Variable.getUnits();
      if (VariableUnits == "")
         VariableUnits = "()";
      VariableType = Variable.getType();
      NumElements = Variable.getCount();
      VariableFound = true;

      // if this is a function name then make sure we are dealing with
      // either an integer or a real.  Give error if not.
      if (FunctionName.length() > 0)
         {
         if (VariableType == APSIMVariant::booleanType ||
             VariableType == APSIMVariant::stringType)
            {
            ApsimSystem().Error.Fatal("Cannot apply a REPORT function (eg avg) to a string variable.\n"
                                      "Variable must be an integer or a real.\n"
                                      "Variable name = " + VariableName);

            }
         }
      }
   else
      {
      VariableUnits = "(?)";
      NumElements = 0;
      VariableFound = false;
      }
   NumTimesAccumulated = 0;
   }


// ------------------------------------------------------------------
//  Short description:
//     return the width that this field obect should take up.

//  Notes:

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
unsigned int Field::getWidth(void)
   {
   switch (VariableType)
      {
      case APSIMVariant::realType : return 15;
      case APSIMVariant::integerType : return 7;
      case APSIMVariant::booleanType : return 7;
      case APSIMVariant::stringType : return 15;
      }
   return 15;
   }

// ------------------------------------------------------------------
//  Short description:
//     truncate the specified string so that it fits in a field.

//  Notes:

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
string Field::truncateSt (const string& st, unsigned int Width)
   {
   string returnst = st;
   if (returnst.length() > Width)
      returnst.erase(Width);
   return returnst;
   }

// ------------------------------------------------------------------
//  Short description:
//     write out the field heading to the specified stream.

//  Notes:

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
void Field::writeHeading (ostream& out)
   {
   string FieldName = VariableAlias;
   if (FieldName == "-")
      FieldName = VariableName;

   if (NumElements <= 1)
      {
      out.width(getWidth());
      out << truncateSt(FieldName, getWidth()-1);
      if (CSVFormat)
         out << ',';
      }
   else
      {
      string ThisFieldName;
      for (unsigned int elem = 1; elem <= NumElements; elem++)
         {
         out.width(getWidth());
         ThisFieldName = FieldName + "(" + IntToStr(elem).c_str() + ")";
         out << truncateSt(ThisFieldName, getWidth()-1);
         if (CSVFormat)
            out << ',';
         }
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     write out the field units to the specified stream.

//  Notes:

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
void Field::writeUnits (ostream& out)
   {
   for (unsigned int elem = 1; elem <= max((unsigned)1, NumElements); elem++)
      {
      out.width(getWidth());
      out << truncateSt(VariableUnits, getWidth()-1);
      if (CSVFormat)
         out << ',';
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     write out the field values to the specified stream.

//  Notes:

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
void Field::writeValue(ostream& out)
   {
   retrieveValue();

   if (VariableFound)
      {
      // write all values.
      for (unsigned int i = 0; i < NumElements; i++)
         {
         out.width(getWidth());
         if (i < Values.size())
            out << truncateSt(Values[i], getWidth()-1);
         else
            out << "????";
         if (CSVFormat)
            out << ',';
         }
      }
   else
      {
      out.width(getWidth());
      out << "????";
      if (CSVFormat)
         out << ',';
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     Go accumulate

//  Notes:

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
void Field::accumulateValue(void)
   {
   if (FunctionName.length() > 0)
      {
      NumTimesAccumulated++;
      APSIMVariant Variable;
      if (ApsimSystem().Loader.GetOtherVar(ModuleName, VariableName, Variable, true))
         {
         if (FunctionValues.size() == 0)
            FunctionValues = Variable.asRealArray();
         else
            {
            vector<double> DoubleValues = Variable.asRealArray();
            if (DoubleValues.size() == FunctionValues.size())
               add (FunctionValues, DoubleValues);
            else
               ApsimSystem().Error.Fatal("The number of array elements for variable: " + VariableName + "\n"
                                         "has changed.  A REPORT function (eg avg) cannot be applied to this variable");
            }
         }
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     Go accumulate

//  Notes:

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
void Field::retrieveValue(void)
   {
   Values.erase(Values.begin(), Values.end());
   if (FunctionName.length() == 0)
      {
      APSIMVariant Variable;
      if (ApsimSystem().Loader.GetOtherVar(ModuleName, VariableName, Variable, true))
         {
         vector<double> DoubleValues;
         // get all values.
         if (VariableType == APSIMVariant::realType)
            {
            DoubleValues = Variable.asRealArray();
            Double2string (DoubleValues, Values, 3);
            }
         else if (VariableType == APSIMVariant::integerType)
            {
            DoubleValues = Variable.asRealArray();
            Double2string (DoubleValues, Values, 0);
            }
         else
            {
            Values = Variable.asStringArray();
            // see if this variable is really a string or not.
            for (vector<string>::iterator i = Values.begin();
                                          i != Values.end();
                                          i++)
               {
               char *endptr;
               double value = strtod((*i).c_str(), &endptr);
               if (*endptr == NULL)
                  DoubleValues.push_back (value);
               else
                  break;
               }
            if (Values.size() == DoubleValues.size())
               Double2string (DoubleValues, Values, 3);

            }
         }
      }
   else
      {
      if (Str_i_Eq(FunctionName, "avg"))
         devide_value(FunctionValues, NumTimesAccumulated);
      if (VariableType == APSIMVariant::realType)
         Double2string (FunctionValues, Values, 3);
      else
         Double2string (FunctionValues, Values, 0);
      FunctionValues.erase(FunctionValues.begin(), FunctionValues.end());
      NumTimesAccumulated = 0;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     ForEach function that is called for every APSIM Report Variable.
//     This function will setup a list of field objects based on
//     all variables.

//  Notes:

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
class SetupFieldsFunction : public ForEachFunction<APSIMReportVariable>
   {
   public:
      SetupFieldsFunction(list<Field>& fields, bool csvformat)
         : Fields(fields), SomeAreFunctions(false), CSVFormat(csvformat)
         {}
      virtual void ForEach(APSIMReportVariable& Var)
         {
         Field F(Var.ModuleName, Var.VariableName, Var.VariableAlias, CSVFormat);
         SomeAreFunctions = (SomeAreFunctions || F.isFunction());
         Fields.push_back (F);
         }

      bool SomeAreFunctions;

   private:
      list<Field>& Fields;
      bool CSVFormat;

   };

// ------------------------------------------------------------------
//  Short description:
//     ForEach function that is called for every Field.
//     This function will setup a headings line and a units line.

//  Notes:

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
class FieldHeadingUnitFunction
   {
   public:
      FieldHeadingUnitFunction(ostream& headings, ostream& units)
         : Headings(headings), Units(units)
         {}

      void operator () (Field& field)
         {
         field.writeHeading(Headings);
         field.writeUnits(Units);
         }

   private:
      ostream& Headings;
      ostream& Units;

   };

// ------------------------------------------------------------------
//  Short description:
//     ForEach function that is called for every Field.
//     This function will write each field to an output stream.

//  Notes:

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
class FieldValueFunction
   {
   public:
      FieldValueFunction(ostream& out)
         : Out(out)
         {}

      void operator () (Field& field)
         {
         field.writeValue(Out);
         }

   private:
      ostream& Out;

   };

// ------------------------------------------------------------------
//  Short description:
//     ForEach function that is called for every Field.
//     This function will tell each field to go accumulate itself.

//  Notes:

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
class FieldAccumulateFunction
   {
   public:
      FieldAccumulateFunction(void) { }

      void operator () (Field& field)
         {
         field.accumulateValue();
         }
   };


// ------------------------------------------------------------------
//  Short description:
//     INIT entry point

//  Notes:

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
APSIMComponent* CreateComponent(const string& Name)
   {
   return new ReportComponent(Name);
   }

// ------------------------------------------------------------------
//  Short description:
//     initialise the REPORT component.

//  Notes:

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
void ReportComponent::Init (void)
   {
   if (!ApsimSystem().Data.Get (Name + ".ReportVariables", Variables))
      ApsimSystem().Error.Fatal ("Cannot find REPORT variable names in parameter file.\n"
                                 "Cannot create output file.");

   APSIMFilename File;
   if (ApsimSystem().Data.Get (Name + ".OutputFile", File))
      Out.Open(File);

   else
      ApsimSystem().Error.Fatal ("Cannot find name of output file in parameter file.\n"
                                 "Cannot create output file.");

   APSIMProperty FormatProperty;
   ApsimSystem().Data.Get (Name + ".Format", FormatProperty);
   CSVFormat = Str_i_Eq(FormatProperty.GetValue(), "csv");
   DaysSinceLastReport = 0;

   // write out all initial conditions.
   string msg = "Output file = " + Out.getFilename();
   ApsimSystem().Summary.WriteLine(msg);
   msg = "Format = ";
   if (CSVFormat)
      msg += "csv";
   else
      msg += "normal";
   ApsimSystem().Summary.WriteLine(msg);
   ApsimSystem().Summary.Write(Variables);
   }

// ------------------------------------------------------------------
//  Short description:
//     perform the specified action.

//  Notes:

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
void ReportComponent::DoAction(const char* Action)
   {
   if (strcmpi(Action, MES_Prepare) == 0)
      HaveAccumulatedVarsToday = false;

   else if (strcmpi(Action, "do_output") == 0)
      WriteLineOfOutput();

   else if (stricmp(Action, "do_end_day_output") == 0)
      OutputOnThisDay = true;

   else if (stricmp(Action, MES_Report) == 0)
      {
      DaysSinceLastReport++;

      if (OutputOnThisDay)
         WriteLineOfOutput();
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     return a variable to caller.  Return true if we own variable.

//  Notes:

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
bool ReportComponent::GetVariable (const char* VariableName)
   {
   if (stricmp(VariableName, "days_since_last_report") == 0)
      {
      APSIMVariant Property(DaysSinceLastReport, "(days)");
      Property.PutInPostbox(VariableName);

      return true;
      }
   return false;
   }

// ------------------------------------------------------------------
//  Short description:
//     write a line of output to output file.

//  Notes:

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
void ReportComponent::WriteLineOfOutput(void)
   {
   Setup();
   if (!HaveAccumulatedVarsToday)
      AccumulateVariables();

   ostrstream Line;
   FieldValueFunction FieldValues(Line);
   std::for_each (Fields.begin(), Fields.end(), FieldValues);
   Line << std::ends;
   Out.WriteLine(Line.str());
   delete Line.str();

   DaysSinceLastReport = 0;
   }

// ------------------------------------------------------------------
//  Short description:
//     setup this object

//  Notes:

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
void ReportComponent::Setup(void)
   {
   if (Fields.size() == 0)
      {
      // setup our list of fields.
      SetupFieldsFunction SetupFields(Fields, CSVFormat);
      Variables.EnumerateVariables (SetupFields);
      SomeFieldsAreFuntions = SetupFields.SomeAreFunctions;

      // write output file header.
      string Line = "Summary_file = " + ApsimSystem().Summary.getFilename();
      Out.WriteLine (Line);
      Line = "Title = " + ApsimSystem().getTitle();
      Out.WriteLine (Line);

      ostrstream Headings, Units;
      FieldHeadingUnitFunction HeadingsUnits(Headings, Units);

      std::for_each (Fields.begin(), Fields.end(), HeadingsUnits);
      Headings << std::ends;
      Units << std::ends;
      Out.WriteLine (Headings.str());
      Out.WriteLine (Units.str());
      delete Headings.str();
      delete Units.str();
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     Accumulate variables.

//  Notes:

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
void ReportComponent::AccumulateVariables(void)
   {
   HaveAccumulatedVarsToday = true;
   if (SomeFieldsAreFuntions)
      {
      FieldAccumulateFunction AcumulateFields;
      std::for_each (Fields.begin(), Fields.end(), AcumulateFields);
      }
   }


