#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ReportComponent.h"
#include <ComponentInterface\MessageDataExt.h>
#include <ApsimShared\FStringExt.h>
#include <ApsimShared\ApsimComponentData.h>
#include <general\math_functions.h>
#include <general\stl_functions.h>
#include <general\StringTokenizer.h>
#include <sstream>
#include <variant.h>
#pragma package(smart_init)
using namespace std;
using namespace protocol;

static const char* daysSinceLastReportType = "<type kind=\"integer4\" units=\"d\">";
static const char* stringArrayType = "<type kind=\"string\" array=\"T\">";
// ------------------------------------------------------------------
//  Short description:
//     constructor

//  Notes:

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
Field::Field (protocol::Component* p,
              const string& variable,
              bool csvformat)
   {
   parent = p;
   CSVFormat = csvformat;
   fieldWidth = 0;

   unsigned posPeriod = variable.find('.');
   if (posPeriod != string::npos)
      {
      ModuleName = variable.substr(0, posPeriod);
      VariableName = variable.substr(posPeriod+1);
      unsigned posAlias = VariableName.find('(');
      if (posAlias != string::npos)
         {
         VariableAlias = VariableName.substr(posAlias+1);
         VariableAlias.erase(VariableAlias.length()-1, 1);
         VariableName = VariableName.substr(0, posAlias);
         }

      // at this stage simply register an interest in the variable.
      variableID = parent->addRegistration(protocol::getVariableReg,
                                           VariableName.c_str(),
                                           stringArrayType,
                                           "",
                                           ModuleName.c_str());
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     Go get a variable from system.

//  Notes:

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
bool Field::getValues(void)
   {
   values.erase(values.begin(), values.end());
   protocol::Variant* variant;
   bool ok = parent->getVariable(variableID, variant, true);
   if (ok)
      {
      variant->unpack(values);
      unit = asString(variant->getType().getUnit());
      }
   else
      unit = "(?)";
   if (fieldWidth == 0)
      calcFieldWidth(variant, ok);

   return ok;
   }
// ------------------------------------------------------------------
//  Short description:
//     write this field's heading to the specified output stream.

//  Notes:

//  Changes:
//    DPH 20/6/2001

// ------------------------------------------------------------------
void Field::writeHeadings(ostream& headingOut, ostream& unitOut)
   {
   getValues();
   if (values.size() == 0)
      unit = "(?)";
   if (values.size() <= 1)
      {
      writeTo(headingOut, VariableName);
      writeTo(unitOut, unit);
      }
   else
      {
      for (unsigned int v = 0; v < values.size(); v++)
         {
         string arrayVariableName = VariableName + "(";
         arrayVariableName += IntToStr(v+1).c_str();
         arrayVariableName += ")";
         writeTo(headingOut, arrayVariableName);
         writeTo(unitOut, unit);
         }
      }
   }
// ------------------------------------------------------------------
//  Short description:
//     write this field's value to the specified output stream.

//  Notes:

//  Changes:
//    DPH 20/6/2001

// ------------------------------------------------------------------
void Field::writeValue(ostream& out)
   {
   getValues();
   for (unsigned v = 0; v != values.size(); v++)
      writeTo(out, values[v]);
   if (values.size() == 0)
      writeTo(out, "?");
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
   line = "   " + VariableName;
   parent->writeString(line.c_str());
   }

// ------------------------------------------------------------------
//  Short description:
//     write this field to summary file

//  Notes:

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
void Field::writeTo(ostream& out, const string& value)
   {
   out.width(fieldWidth);
   out << value;
   }
// ------------------------------------------------------------------
//  Short description:
//     calculate a field width for this variable.

//  Notes:

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
void Field::calcFieldWidth(protocol::Variant* variant, bool ok)
   {
   if (!ok)
      fieldWidth = 6;
   else if (variant->getType().getCode() == DTint4)
      fieldWidth = 10;
   else
      fieldWidth = 15;
   fieldWidth = max(fieldWidth, VariableName.length() + 1);
   }

// ------------------------------------------------------------------
//  Short description:
//     Create an instance of the REPORT module

//  Notes:

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
Component* createComponent(void)
   {
   return new ReportComponent;
   }

// ------------------------------------------------------------------
//  Short description:
//     initialise the REPORT component.

//  Notes:

//  Changes:
//    DPH 29/7/99
//    dph 19/12/00

// ------------------------------------------------------------------
ReportComponent::ReportComponent(void)
   {
   OutputOnThisDay = false;
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
   }

// ------------------------------------------------------------------
//  Short description:
//     initialise the REPORT component - STAGE 1.

//  Notes:

//  Changes:
//    DPH 29/7/99
//    dph 19/12/00

// ------------------------------------------------------------------
void ReportComponent::doInit1(const FString& sdml)
   {
   try
      {
      Component::doInit1(sdml);

      repEventID = addRegistration(respondToEventReg, "rep", "");
      doOutputID = addRegistration(respondToMethodCallReg, "do_output", "");
      doEndDayOutputID = addRegistration(respondToMethodCallReg, "do_end_day_output", "");
      daysSinceLastReportVariableID = addRegistration(respondToGetReg,
                                                      "days_since_last_report",
                                                      daysSinceLastReportType);
      string fileName = componentData->getProperty("parameters", "outputfile");
      if (fileName == "")
         throw runtime_error("Cannot find name of output file in parameter file. ");

      out.open(fileName.c_str());

      // get format specifier.
      CSVFormat = Str_i_Eq(componentData->getProperty("parameters", "format"), "csv");

      // enumerate through all output variables
      // and create a field for each.
      std::vector<string> variables;
      componentData->getVariables(variables);
      for (std::vector<string>::iterator variableI = variables.begin();
                                         variableI != variables.end();
                                         variableI++)
         fields.push_back(Field(this, *variableI, CSVFormat));

      DaysSinceLastReport = 1;

      // write out all initial conditions.
      string msg = "Output file = " + fileName;
      writeString(msg.c_str());
      msg = "Format = ";
      if (CSVFormat)
         msg += "csv";
      else
         msg += "normal";
      writeString(msg.c_str());

      // write all fields to summary file.
      writeString("Output variables:");
      for_each(fields.begin(), fields.end(), mem_fun_ref(&Field::writeToSummary));
      }
   catch (const runtime_error& err)
      {
      error(err.what(), true);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     perform the specified action.

//  Notes:

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
void ReportComponent::respondToMethod(unsigned int& fromID, unsigned int& methodID, protocol::Variant& variant)
   {
   if (methodID == doOutputID)
      WriteLineOfOutput();
   else if (methodID == doEndDayOutputID)
      OutputOnThisDay = true;
   }

// ------------------------------------------------------------------
//  Short description:
//    Event handler.

//  Notes:

//  Changes:
//    DPH 23/5/2001

// ------------------------------------------------------------------
void ReportComponent::respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
   {
   if (eventID == repEventID)
      {
      DaysSinceLastReport++;

      if (OutputOnThisDay)
         WriteLineOfOutput();
      OutputOnThisDay = false;
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     return a variable to caller.  Return true if we own variable.

//  Notes:

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
void ReportComponent::respondToGet(unsigned int& fromID, QueryValueData& queryData)
   {
   if (queryData.ID == daysSinceLastReportVariableID)
      sendVariable(queryData, DaysSinceLastReport);
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
   static bool haveWrittenHeadings = false;
   if (!haveWrittenHeadings)
      {
      haveWrittenHeadings = true;
      writeHeadings();
      }

   for (Fields::iterator f = fields.begin();
                         f != fields.end();
                         f++)
      (*f).writeValue(out);

   out << endl;

   DaysSinceLastReport = 0;
   }

// ------------------------------------------------------------------
//  Short description:
//     setup this object

//  Notes:

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
void ReportComponent::writeHeadings(void)
   {
   // write out headings and units.
   ostringstream headingLine;
   ostringstream unitLine;
   for (Fields::iterator f = fields.begin();
                         f != fields.end();
                         f++)
      (*f).writeHeadings(headingLine, unitLine);

   headingLine << ends;
   unitLine << ends;
   out << endl << endl;
   out << headingLine.str() << endl;
   out << unitLine.str() << endl;
   }

void ReportComponent::doInit2(void)
   {
   }

