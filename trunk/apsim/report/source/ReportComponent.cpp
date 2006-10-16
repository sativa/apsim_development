#include <general/pch.h>
#pragma hdrstop

#include <ComponentInterface/MessageDataExt.h>
#include <ApsimShared/FStringExt.h>
#include <ApsimShared/ApsimComponentData.h>
#include <general/math_functions.h>
#include <general/stl_functions.h>
#include <general/string_functions.h>
#include <general/date_class.h>
#include <general/StringTokenizer.h>
#include <sstream>
#include <Variant.h>

#include "ReportComponent.h"
#pragma package(smart_init)
using namespace std;
using namespace protocol;

static const char* daysSinceLastReportType = "<type kind=\"integer4\" unit=\"d\"/>";
static const char* stringArrayType = "<type kind=\"string\" array=\"T\"/>";
static const char* stringType = "<type kind=\"string\"/>";
// ------------------------------------------------------------------
//  Short description:
//     constructor

//  Notes:

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
Field::Field (protocol::Component* p,
              string variable,
              bool csvformat,
              const std::string& nastring,
              unsigned int precision)
   {
   string ModuleName;
   string VariableAlias;
   parent = p;
   CSVFormat = csvformat;
   NAString = nastring;
   Precision = precision;

   // look for format.
   unsigned pos = variable.find(" format ");
   if (pos != string::npos)
      {
      VariableFormat = variable.substr(pos + strlen(" format "));
      To_upper(VariableFormat);
      stripLeadingTrailing(VariableFormat, " ");
      variable.erase(pos);
      }

   // look for alias.
   pos = variable.find(" as ");
   if (pos != string::npos)
      {
      VariableAlias = variable.substr(pos + strlen(" as "));
      stripLeadingTrailing(VariableAlias, " ");
      variable.erase(pos);
      }

   // look for module name.
   pos = variable.find('.');
   if (pos != string::npos)
      {
      VariableName = variable.substr(pos+1);
      ModuleName = variable.substr(0, pos);
      }
   else
      {
      ModuleName = "";
      VariableName = variable;
      }
   stripLeadingTrailing(ModuleName, " ");
   stripLeadingTrailing(VariableName, " ");

   // at this stage simply register an interest in the variable.
   variableID = parent->addRegistration(RegistrationType::get,
                                        VariableName.c_str(),
                                        stringArrayType,
                                        "",
                                        ModuleName.c_str());
   if (VariableAlias != "")
      VariableName = VariableAlias;
   }

// ------------------------------------------------------------------
//  Short description:
//     Go get a variable from system.

//  Notes:

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
void Field::getValues(bool setupHeadings)
   {
   values.erase(values.begin(), values.end());

   protocol::Variants* variants;
   if (parent->getVariables(variableID, variants))
      {
      unsigned numResponses = variants->size();

      for (unsigned v = 0; v != numResponses; v++)
         {
         protocol::Variant* variant = variants->getVariant(v);
         bool includeModuleNameInHeading = (numResponses > 1);
         storeValue(variant, setupHeadings, includeModuleNameInHeading);
         }
      }
   else
      storeValue(NULL, setupHeadings, false);

   FormatValues();
   if (setupHeadings)
      calcFieldWidths();
   }

// ------------------------------------------------------------------
// Store the values in the specified variant.
// ------------------------------------------------------------------
void Field::storeValue(protocol::Variant* variant, bool setupHeadings, bool includeModuleNameInHeading)
   {
   std::vector<string> localValues;
   if (variant != NULL)
      {
      bool ok = variant->unpack(localValues);
      if (!ok)
         {
         string msg = "Cannot use array notation on a scalar variable.\n"
                      "Variable name: " + VariableName;
         parent->error(msg.c_str(), false);
         }
      }
   bool noValues = (localValues.size() == 0);
   if (noValues)
      localValues.push_back(NAString);
   else
      copy(localValues.begin(), localValues.end(), back_inserter(values));

   if (setupHeadings)
      {
      string unit;
      unsigned arrayIndex = 1;
      if (noValues || variant == NULL)
         unit = "(?)";
      else
         {
         unit = asString(variant->getType().getUnits());
         if (unit[0] != '(')
            unit = "(" + unit + ")";
         arrayIndex = variant->getLowerBound();
         }

      string fromModuleName;
      if (includeModuleNameInHeading)
         {
         char buffer[500];
         strcpy(buffer, "\0");
         FString compName(buffer, sizeof(buffer), CString);
         parent->componentIDToName(variant->getFromId(), compName);
         fromModuleName = asString(compName);
         }
      for (unsigned i = 0; i != localValues.size(); i++)
         {
         string heading = VariableName;
         if (includeModuleNameInHeading)
            {
            if (fromModuleName != "")
               heading = fromModuleName + "." + VariableName;
            }
         if (localValues.size() > 1)
            {
            heading += "(" + itoa(arrayIndex) + ")";
            arrayIndex++;
            }
         headings.push_back(heading);
         units.push_back(unit);
         }
      }
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
   if (VariableFormat != "")
      {
      for (unsigned i = 0; i != values.size(); i++)
         {
         GDate d;
         d.Set(atoi(values[i].c_str()));
         d.Set_write_format(VariableFormat.c_str());
         d.Write(values[i]);
         }
      }
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
   headings.erase(headings.begin(), headings.end());
   units.erase(units.begin(), units.end());
   getValues(true);
   writeValuesTo(headingOut, headings);
   writeValuesTo(unitOut, units);
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
   getValues(false);
   if (!CSVFormat)
      formatAsFloats();
   writeValuesTo(out, values);
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
void Field::writeValuesTo(ostream& out, std::vector<string>& values)
   {
   for (unsigned v = 0; v != values.size(); v++)
      {
      if (CSVFormat)
         {
         if (v != 0)
            out << ',';
         out << values[v];
         }
      else
         {
         out.width(fieldWidths[v]);
         if (values[v].length() >= fieldWidths[v])
            values[v].erase(fieldWidths[v]-1);
         out << values[v];
         }
      }

   }
// ------------------------------------------------------------------
//  Short description:
//     calculate a field width for this variable.

//  Notes:

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
void Field::calcFieldWidths()
   {
   fieldWidths.erase(fieldWidths.begin(), fieldWidths.end());
   for (unsigned index = 0; index != headings.size(); index++)
      {
      int fieldWidth = 15;
      fieldWidth = max(fieldWidth, headings[index].length() + 1);
      if (units.size() > 0)
         fieldWidth = max(fieldWidth, units[index].length() + 1);
      fieldWidths.push_back(fieldWidth);
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     Return a blank string when requested to indicate that we
//     don't need a wrapper DLL.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" EXPORT void STDCALL wrapperDLL(char* wrapperDll)
   {
   strcpy(wrapperDll, "");
   }
extern "C" void STDCALL getDescriptionInternal(char* initScript,
                                                 char* description);
// ------------------------------------------------------------------
// Return component description info.
// ------------------------------------------------------------------
extern "C" EXPORT void STDCALL getDescription(char* initScript, char* description)
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
         fileName = calcFileName();

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
// Calculate a file name based on simulation title and PM name.
// ------------------------------------------------------------------
string ReportComponent::calcFileName()
   {
   string title, pmName;
   unsigned titleID = addRegistration(RegistrationType::get,
                                      "title",
                                      stringType);
   protocol::Variant* variant;
   if (getVariable(titleID, variant, true))
      variant->unpack(title);

   char buffer[500];
   strcpy(buffer, "\0");
   FString parentName(buffer, sizeof(buffer), CString);
   componentIDToName(parentID, parentName);
   pmName = asString(parentName);

   string fileName = title;
   if (!Str_i_Eq(pmName, "paddock") && !Str_i_Eq(pmName, "masterpm"))
      {
      if (fileName != "")
         fileName += " ";
      fileName += pmName;
      }

   if (!Str_i_Eq(name, "outputfile"))
      {
      if (fileName != "")
         fileName += " ";
      fileName += name;
      }
   fileName += ".out";
   return fileName;
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
   std::string T;
   publish(reportedID, T);
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

