//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "ReportComponent.h"
#include <general\math_functions.h>
#include <general\stl_functions.h>
#include <general\treenode.h>
#include <sstream>
#include <aps\apsimproperty.h>
#include <aps\APSIMOutputVariable.h>
#include <eventinterface.h>
#pragma package(smart_init)
using namespace std;

static const char* MES_Prepare = "prepare";
static const char* MES_Report = "rep";

// ------------------------------------------------------------------
//  Short description:
//     constructor

//  Notes:

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
Field::Field (EventInterface* eInterface,
              const string& modulename,
              const string& variablename,
              const string& variablealias,
              bool csvformat)
   : eventInterface(eInterface)
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
   To_lower(VariableName);
   }

// ------------------------------------------------------------------
//  Short description:
//     initialise field.

//  Notes:

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
void Field::init(void)
   {
   // get the value of specified variable.

   // DPH - Need to remove any array specifier from variable name.
   APSIMVariant Variable;
   Variable.NewPostbox();

   bool ok;
   if (ModuleName == "-")
      ok = eventInterface->getVariable(VariableName.c_str());
   else
      ok = eventInterface->getVariable(ModuleName.c_str(), VariableName.c_str());

   if (ok)
      {
      Variable.GetFromPostbox(VariableName.c_str());

      VariableUnits = Variable.getUnits();
      if (VariableUnits == "")
         VariableUnits = "()";
      VariableType = Variable.getType();
      NumElements = Variable.getCount();
      VariableFound = true;
      }
   else
      {
      VariableType = APSIMVariant::unknownType;
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
      case APSIMVariant::unknownType : return 15;
      }
   return 15;
   }

// ------------------------------------------------------------------
//  Short description:
//     write and format a string.

//  Notes:

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
void Field::WriteString (ostream& out, const string& st)
   {
   if (CSVFormat)
      {
      string strippedst = st;
      Strip(strippedst," ");
      out << strippedst << ',';
      }
   else
      {
      out.width(getWidth());
      out << truncateSt(st, getWidth()-1);
      }
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
   if (FieldName == "")
      {
      FieldName = VariableName;

      if (FunctionName != "")
         FieldName = FunctionName + "@" + FieldName;
      }

   if (NumElements <= 1)
      WriteString(out, FieldName);

   else
      {
      string ThisFieldName;
      for (unsigned int elem = 1; elem <= NumElements; elem++)
         {
         ThisFieldName = FieldName + "(" + IntToStr(elem).c_str() + ")";
         WriteString(out, ThisFieldName);
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
      WriteString(out, VariableUnits);
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
   VariableFound = retrieveValue();

   if (VariableFound)
      {
      string value;
      // write all values.
      for (unsigned int i = 0; i < NumElements; i++)
         {
         if (i < Values.size())
            value = Values[i];
         else
            value = "????";
         WriteString(out,value);
         }
      }
   else
      WriteString(out,"????");
   }

// ------------------------------------------------------------------
//  Short description:
//     write this field to summary file

//  Notes:

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
void Field::writeToSummary(void)
   {
   string line;
   line = "   " + ModuleName + ".";
   if (FunctionName != "")
      line = line + FunctionName + "@";
   line += VariableName;
   if (VariableAlias != "")
      line += " (alias: " + VariableAlias + ")";
   ApsimSystem().Summary->writeSummaryLine(eventInterface->getComponentName().c_str(),
                                           line.c_str());
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
      Variable.NewPostbox();

      bool ok;
      if (ModuleName == "-")
         ok = eventInterface->getVariable(VariableName.c_str());
      else
         ok = eventInterface->getVariable(ModuleName.c_str(), VariableName.c_str());

      if (ok)
         {
         Variable.GetFromPostbox(VariableName.c_str());

         if (FunctionValues.size() == 0)
            Variable.asRealArray(FunctionValues);
         else
            {
            vector<double> DoubleValues;
            Variable.asRealArray(DoubleValues);
            if (DoubleValues.size() == FunctionValues.size())
               FunctionValues = add (FunctionValues, DoubleValues);
            else
               ApsimSystem().Error.Fatal("The number of array elements for variable: " + VariableName + "\n"
                                         "has changed.  A REPORT function (eg avg) cannot be applied to this variable",
                                         "infrastructure");
            }
         }
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     Go retrieve a value from the system.  Return true if found.

//  Notes:

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
bool Field::retrieveValue(void)
   {
   Values.erase(Values.begin(), Values.end());
   if (FunctionName.length() == 0)
      {
      APSIMVariant Variable;
      Variable.NewPostbox();

      bool ok;
      if (ModuleName == "-")
         ok = eventInterface->getVariable(VariableName.c_str());
      else
         ok = eventInterface->getVariable(ModuleName.c_str(), VariableName.c_str());

      if (ok)
         {
         Variable.GetFromPostbox(VariableName.c_str());
         vector<double> DoubleValues;
         if (VariableType == APSIMVariant::unknownType)
            {
            // it is possible that a variable's type wasn't known at startup -
            // but is now.  Go get the type and assume that the number of elements
            // is 1.
            VariableType = Variable.getType();
            NumElements = 1;
            }

         // get all values.
         if (VariableType == APSIMVariant::realType)
            {
            Variable.asRealArray(DoubleValues);
            if (CSVFormat)
               Double2string (DoubleValues, Values, 6);
            else
               Double2string (DoubleValues, Values, 3);
            }
         else if (VariableType == APSIMVariant::integerType)
            {
            Variable.asRealArray(DoubleValues);
            Double2string (DoubleValues, Values, 0);
            }
         else
            {
            Variable.asStringArray(Values);
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
               {
               if (CSVFormat)
                  Double2string (DoubleValues, Values, 6);
               else
                  Double2string (DoubleValues, Values, 3);
               }
            }
         }
      else
         return false;
      }
   else
      {
      if (FunctionValues.size() == 0)
         return false;
      if (Str_i_Eq(FunctionName, "avg"))
         devide_value(FunctionValues, NumTimesAccumulated);

      if (CSVFormat)
         Double2string (FunctionValues, Values, 6);
      else
         Double2string (FunctionValues, Values, 3);

      FunctionValues.erase(FunctionValues.begin(), FunctionValues.end());
      NumTimesAccumulated = 0;
      }
   return true;
   }

// ------------------------------------------------------------------
//  Short description:
//     ForEach function that is called for every field.
//     Its objective is to determine if we have any fields
//     that are functions.

//  Notes:

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
class CheckForFunctions
   {
   public:
      CheckForFunctions(bool& somearefunctions)
         : someAreFunctions(somearefunctions)
         {}
      void operator()(const Field& field)
         {
         someAreFunctions = (someAreFunctions || field.isFunction());
         }

   private:
      bool& someAreFunctions;
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
APSIMComponent* CreateInstance(const FString& name,
                               IComputation& computation,
                               const std::string& ssdl)
   {
   return new ReportComponent(name, computation, ssdl);
   }

// ------------------------------------------------------------------
//  Short description:
//     initialise the REPORT component.

//  Notes:

//  Changes:
//    DPH 29/7/99
//    dph 19/12/00

// ------------------------------------------------------------------
ReportComponent::ReportComponent(const FString& name,
                                 IComputation& computation,
                                 const std::string& ssdl)
   : APSIMComponent(name, computation, ssdl), Out(NULL)
   {
   OutputOnThisDay = false;
   HaveAccumulatedVarsToday = false;
   HaveWrittenHeadings = false;

   // added event registration stuff.
   eventInterface->registerSubscribedEvent("prepare");
   eventInterface->registerSubscribedEvent("rep");
   }

// ------------------------------------------------------------------
//  Short description:
//     initialise the REPORT component.

//  Notes:

//  Changes:
//    DPH 29/7/99
//    dph 19/12/00

// ------------------------------------------------------------------
ReportComponent::~ReportComponent(void)
   {
   delete Out;
   }

// ------------------------------------------------------------------
//  Short description:
//     callback class for all APSIMOutputVariables

//  Notes:

//  Changes:
//    DPH 19/10/2000

// ------------------------------------------------------------------
class CreateFields : CallbackFunction<APSIMOutputVariable*>
   {
   private:
      list<Field>& fields;
      bool csvFormat;
      EventInterface* eventInterface;
   public:
      CreateFields(EventInterface* eInterface,
                   list<Field>& f,
                   bool csv)
         : eventInterface(eInterface), fields(f), csvFormat(csv) { }
      virtual void callback(APSIMOutputVariable* variable)
         {
         fields.push_back(Field(eventInterface,
                                variable->getOwnerModule(),
                                variable->getName(),
                                variable->getAlias(),
                                csvFormat));
         }
   };

// ------------------------------------------------------------------
//  Short description:
//     initialise the REPORT component.

//  Notes:

//  Changes:
//    DPH 29/7/99
//    dph 19/12/00

// ------------------------------------------------------------------
void ReportComponent::init(void)
   {
   SOMPropertyGroup group = componentData->findGroupWithProperty("outputfile",
                                                                 "",
                                                                 "outputfile");
   bool ok = group.isValid();
   if (ok)
      {
      Out = new APSIMOutputFile(group.getProperty("outputfile", "outputfile"));
      ok = Out->isValid();
      }
   if (ok)
      Out->open();

   else
      ApsimSystem().Error.Fatal ("Cannot find name of output file in parameter file.\n"
                                 "Cannot create output file.",
                                 "infrastructure");

   // get format specifier.
   SOMPropertyGroup formatGroup = componentData->findGroupWithProperty("format",
                                                                       "",
                                                                       "property");

   APSIMProperty formatProperty(formatGroup.getProperty("property", "format"));
   CSVFormat = (Str_i_Eq(formatProperty.getValue(), "csv"));

   // enumerate through all output variables in all groups
   // and create a field for each.
   list<string> groupNames;
   componentData->getGroupNames(groupNames);
   for (list<string>::iterator groupNameI = groupNames.begin();
                               groupNameI != groupNames.end();
                               groupNameI++)
      {
      SOMPropertyGroup group = componentData->getGroup(*groupNameI);
      list<string> propertyNames;
      group.getPropertyNames("outputvariable", propertyNames);
      for (list<string>::iterator propertyNameI = propertyNames.begin();
                                  propertyNameI != propertyNames.end();
                                  propertyNameI++)
         {
         APSIMOutputVariable variable(group.getProperty("outputvariable", *propertyNameI));
         if (variable.isValid())
            Fields.push_back(Field(eventInterface,
                                   variable.getOwnerModule(),
                                   variable.getVariableName(),
                                   variable.getAlias(),
                                   CSVFormat));
         }
      }

   DaysSinceLastReport = 1;

   // write out all initial conditions.
   string msg = "Output file = " + Out->getFilename();
   ApsimSystem().Summary->writeSummaryLine(name.c_str(), msg.c_str());
   msg = "Format = ";
   if (CSVFormat)
      msg += "csv";
   else
      msg += "normal";
   ApsimSystem().Summary->writeSummaryLine(name.c_str(), msg.c_str());

   // write all fields to summary file.
   ApsimSystem().Summary->writeSummaryLine(name.c_str(), "Output variables:");
   for_each(Fields.begin(), Fields.end(), mem_fun_ref(&Field::writeToSummary));
   }

// ------------------------------------------------------------------
//  Short description:
//     perform the specified action.

//  Notes:

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
bool ReportComponent::doAction(const FString& Action)
   {
   ApsimSystem().CallStack->Push ("Report_DoAction", 15);

   bool Used = true;
   if (Action == MES_Prepare)
      HaveAccumulatedVarsToday = false;

   else if (Action == "do_output")
      {
      WriteLineOfOutput();
      }

   else if (Action == "do_end_day_output")
      OutputOnThisDay = true;

   else if (Action == MES_Report)
      {
      DaysSinceLastReport++;

      if (OutputOnThisDay)
         WriteLineOfOutput();
      else if (!HaveAccumulatedVarsToday)
         AccumulateVariables();
      OutputOnThisDay = false;
      }
   else
      Used = false;

   ApsimSystem().CallStack->Pop ("Report_DoAction", 15);
   return Used;
   }

// ------------------------------------------------------------------
//  Short description:
//     return a variable to caller.  Return true if we own variable.

//  Notes:

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
bool ReportComponent::getVariable (const FString& VariableName)
   {
   if (VariableName == "days_since_last_report")
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
   if (!HaveAccumulatedVarsToday)
      AccumulateVariables();

   ostringstream Line;
   FieldValueFunction FieldValues(Line);
   std::for_each (Fields.begin(), Fields.end(), FieldValues);
   Line << std::ends;
   Out->writeLine(Line.str().c_str());

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
   if (!HaveWrittenHeadings)
      {
      HaveWrittenHeadings = true;

      // initialise each field.
      for_each(Fields.begin(), Fields.end(), mem_fun_ref(&Field::init));

      // find out if any of our fields are functions
      for_each(Fields.begin(), Fields.end(), CheckForFunctions(SomeFieldsAreFuntions));

      // write output file header.
      string Line = "Summary_file = " + ApsimSystem().Summary->getFilename();
      Out->writeLine (Line.c_str());
      Line = "Title = " + ApsimSystem().getTitle();
      Out->writeLine (Line.c_str());

      ostringstream Headings, Units;
      FieldHeadingUnitFunction HeadingsUnits(Headings, Units);

      std::for_each (Fields.begin(), Fields.end(), HeadingsUnits);
      Headings << std::ends;
      Units << std::ends;
      Out->writeLine (Headings.str().c_str());
      Out->writeLine (Units.str().c_str());
      HaveWrittenHeadings = true;
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
   Setup();
   HaveAccumulatedVarsToday = true;
   if (SomeFieldsAreFuntions)
      {
      FieldAccumulateFunction AcumulateFields;
      std::for_each (Fields.begin(), Fields.end(), AcumulateFields);
      }
   }


