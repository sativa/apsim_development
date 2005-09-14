#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include <ComponentInterface\MessageDataExt.h>
#include "ReportComponent.h"
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
              bool csvformat,
              const std::string& nastring,
              unsigned int precision)
   {
   parent = p;
   CSVFormat = csvformat;
   fieldWidth = 0;
   managerVariable = false;
   NAString = nastring;
   Precision = precision;
   unsigned posPeriod = variable.find('.');
   if (posPeriod != string::npos)
      {
      ModuleName = variable.substr(0, posPeriod);
      VariableName = variable.substr(posPeriod+1);
      }
   else
      {
      ModuleName = "";
      VariableName = variable;
      }

   unsigned posAlias = VariableName.find(" as ");
   if (posAlias != string::npos)
      {
      VariableAlias = VariableName.substr(posAlias+strlen(" as "));
      VariableName.erase(posAlias, VariableName.length()-posAlias);
      stripLeadingTrailing(VariableName, " ");
      stripLeadingTrailing(VariableAlias, " ");
      }

   unsigned posFormat = VariableName.find(" format ");
   if (posFormat != string::npos)
      {
      VariableFormat = VariableName.substr(posFormat+strlen(" format "));
      VariableName.erase(posFormat, VariableName.length()-posFormat);
      stripLeadingTrailing(VariableName, " ");
      stripLeadingTrailing(VariableFormat, " ");
      }

   // at this stage simply register an interest in the variable.
   variableID = parent->addRegistration(RegistrationType::get,
                                        VariableName.c_str(),
                                        stringArrayType,
                                        "",
                                        ModuleName.c_str());
   if (posAlias != string::npos)
      VariableName = VariableAlias;
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
      try
         {
         bool ok = variant->unpack(values);
         unit = asString(variant->getType().getUnits());
         arrayIndex = variant->getLowerBound();
         if (unit[0] != '(')
            unit = "(" + unit + ")";
         if (!ok)
            {
            string msg = "Cannot use array notation on a scalar variable.\n"
                         "Variable name: " + VariableName;
            parent->error(msg.c_str(), false);
            }
         FormatValues();
         }
      catch (const runtime_error& err)
         {
         string msg = err.what();
         msg += " Variable name = " + VariableName;
         parent->error(msg.c_str(), true);
         }
      }
   else
      unit = "(?)";
   if (fieldWidth == 0)
      calcFieldWidth(variant, ok);

   return ok;
   }
// ------------------------------------------------------------------
// Try and format the values as floats ie 3 decimal places.
// ------------------------------------------------------------------
void Field::formatAsFloats(void)
   {
   for (unsigned i = 0; i != values.size(); i++)
      {
      if (values[i].find('.') != string::npos &&
          values[i] != NAString)
         {
         char* endptr;
         double value = strtod(values[i].c_str(), &endptr);
         if (*endptr == '\0')
            values[i] = ftoa(value, Precision);
         }
      }
   }

// ------------------------------------------------------------------
// Format the values according to the format string.
// ------------------------------------------------------------------
void Field::FormatValues(void)
   {
   vector<string> FormatBits;
   splitIntoValues(VariableFormat, " ", FormatBits);
   if (FormatBits.size() != 2)
      throw runtime_error("Invalid reporting format: " + VariableFormat);

   if (Str_i_Eq(FormatBits[0], "date"))
      {
      TDateTime(
      }
   else
      throw runtime_error("Invalid reporting format: " + VariableFormat);

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
   if (unit == "(man)")
      {
      unit = "()";
      managerVariable = true;
      }
   if (values.size() == 0)
      unit = "(?)";
   if (values.size() <= 1)
      {
      writeTo(headingOut, VariableName);
      writeTo(unitOut, unit);
      }
   else
      {
      string baseName = VariableName;
      unsigned posArraySpec = baseName.find('(');
      if (posArraySpec != string::npos)
         baseName.erase(posArraySpec);
      for (unsigned int v = 0; v < values.size(); v++)
         {
         string arrayVariableName = baseName + "(";
         arrayVariableName += IntToStr(arrayIndex++).c_str();
         arrayVariableName += ")";
         if (CSVFormat && v != 0)
            {
            headingOut << ',';
            unitOut << ',';
            }
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
   if (!CSVFormat)
      formatAsFloats();

   for (unsigned v = 0; v != values.size(); v++)
      {
      if (CSVFormat && v != 0)
         out << ',';
      writeTo(out, values[v]);
      }

   if (values.size() == 0)
      writeTo(out, NAString);
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
   if (CSVFormat)
      out << value;
   else
      {
      out.width(fieldWidth);
      if (value.length() >= fieldWidth)
         value.erase(fieldWidth-1);
      out << value;
      }
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
      fieldWidth = 15;
   else if (variant->getType().getCode() == DTint4)
      fieldWidth = 10;
   else
      fieldWidth = 15;
   fieldWidth = max(fieldWidth, VariableName.length() + 1);
   fieldWidth = max(fieldWidth, unit.length() + 1);
   if (values.size() > 1)
      fieldWidth += strlen("(xx)");
   }

// ------------------------------------------------------------------
//  Short description:
//     Return a blank string when requested to indicate that we
//     don't need a wrapper DLL.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" _export void __stdcall wrapperDLL(char* wrapperDll)
   {
   strcpy(wrapperDll, "");
   }
extern "C" void __stdcall getDescriptionInternal(char* initScript,
                                                 char* description);
// ------------------------------------------------------------------
// Return component description info.
// ------------------------------------------------------------------
extern "C" _export void __stdcall getDescription(char* initScript, char* description)
   {
   getDescriptionInternal(initScript, description);
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
   Component::doInit1(sdml);
   static const char* stringDDML = "<type kind=\"string\"\\>";
   titleID = addRegistration(RegistrationType::get, "title", stringDDML);
   summaryFileID = addRegistration(RegistrationType::get, "summaryFile", stringDDML);
   repEventID = addRegistration(RegistrationType::respondToEvent, "report", "");
   doOutputID = addRegistration(RegistrationType::respondToEvent, "do_output", "");
   doEndDayOutputID = addRegistration(RegistrationType::respondToEvent, "do_end_day_output", "");
   daysSinceLastReportVariableID = addRegistration(RegistrationType::respondToGet,
                                                   "days_since_last_report",
                                                   daysSinceLastReportType);
   reportedID = addRegistration(RegistrationType::event, "reported", "<type/>");
   }
// ------------------------------------------------------------------
//  Short description:
//     initialise the REPORT component - STAGE 2.

//  Notes:

//  Changes:
//    DPH 29/7/99
//    dph 19/12/00

// ------------------------------------------------------------------
void ReportComponent::doInit2(void)
   {
   try
      {
      string fileName = componentData->getProperty("parameters", "outputfile");
      if (fileName == "")
         throw runtime_error("Cannot find name of output file in parameter file. ");

      out.open(fileName.c_str());
      if (!out)
         {
         string msg = "Cannot open output file (sharing violation?): " + fileName;
         error(msg.c_str(), true);
         }

      // get output frequencies
      std::vector<string> frequencies;
      splitIntoValues(componentData->getProperty("parameters", "outputfrequency"),
                      " ", frequencies);
      if (frequencies.size() > 0)
         writeString("Output frequency:");
      for (unsigned f = 0; f != frequencies.size(); f++)
         {
         stripLeadingTrailing(frequencies[f], " ");
         string name = "   " + frequencies[f];
         writeString(name.c_str());
         frequencyIds.push_back(addRegistration(RegistrationType::respondToEvent, frequencies[f].c_str(), ""));
         }

      NAString = componentData->getProperty("parameters", "NAString");
      if (NAString == "") NAString = "?";

      // get format specifier.
      CSVFormat = Str_i_Eq(componentData->getProperty("parameters", "format"), "csv");

      if (readParameter("parameters", "precision", Precision, 0, 15, 1) == false)
        {
        Precision = 3;
        }

      // enumerate through all output variables
      // and create a field for each.
      writeString("Output variables:");
      std::vector<string> variables;
      componentData->getVariables(variables);
      for (std::vector<string>::iterator variableI = variables.begin();
                                         variableI != variables.end();
                                         variableI++)
         {
         string name = *variableI;
         name = "   " + name;
         writeString(name.c_str());
         fields.push_back(Field(this, *variableI, CSVFormat, NAString, Precision));
         }
      writeString("");

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
      haveWrittenHeadings = false;
      }
   catch (const runtime_error& err)
      {
      error(err.what(), true);
      }
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
   else if (eventID == doOutputID)
      WriteLineOfOutput();
   else if (eventID == doEndDayOutputID)
      OutputOnThisDay = true;
   else
      {
      if (find(frequencyIds.begin(), frequencyIds.end(), eventID) != frequencyIds.end())
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
   if (!haveWrittenHeadings)
      {
      haveWrittenHeadings = true;
      writeHeadings();
      }

   for (Fields::iterator f = fields.begin();
                         f != fields.end();
                         f++)
      {
      if (CSVFormat && f != fields.begin())
         out << ',';
      (*f).writeValue(out);
      }

   out << endl;

   DaysSinceLastReport = 0;
   publish(reportedID, "");
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
      {
      if (CSVFormat && f != fields.begin())
         {
         headingLine << ',';
         unitLine << ',';
         }
      (*f).writeHeadings(headingLine, unitLine);
      }

//   headingLine << ends;
//   unitLine << ends;

   // output summary_file
   protocol::Variant* variant;
   string summaryFile;
   if (getVariable(summaryFileID, variant, true))
      variant->unpack(summaryFile);
   out << "Summary_file = " << summaryFile << endl;

   // output title
   string title;
   if (getVariable(titleID, variant, true))
      variant->unpack(title);
   out << "Title = " << title << endl;

   // output headings and units
   out << headingLine.str() << endl;
   out << unitLine.str() << endl;
   }

