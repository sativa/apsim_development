#pragma hdrstop

#include <ComponentInterface2/ScienceAPI.h>
#include <general/math_functions.h>
#include <general/stl_functions.h>
#include <general/string_functions.h>
#include <general/date_class.h>
#include <general/StringTokenizer.h>
#include <sstream>

#include "ReportComponent.h"
using namespace std;

// ------------------------------------------------------------------
// Field constructor
// ------------------------------------------------------------------
Field::Field (ScienceAPI& scienceAPI,
              const std::string& fqn,
              const std::string& units,
              const std::string& alias,
              const std::string& nastring,
              const std::string& format,
              bool csv)
   : scienceAPI(scienceAPI)
   {
   this->fqn = fqn;
   this->units = units;
   this->alias = alias;
   this->nastring = nastring;
   this->format = format;
   this->csv = csv;

   if (units[0] != '(')
      this->units = "(" + this->units + ")";

   if (fqn.find('.') == string::npos)
      throw runtime_error("Invalid fqn variable name found in report variable: " + fqn);
   }

// ------------------------------------------------------------------
// write this field's heading to the specified output stream.
// ------------------------------------------------------------------
void Field::writeHeadings(ostream& out)
   {
   getValues();

   for (unsigned i = 0; i != values.size(); i++)
      {
      if (i > 0 && csv)
         out << ',';

      // Work out a heading.
      string heading;
      if (alias == "")
         heading = fqn.substr(fqn.find('.')+1);
      else
         heading = alias;

      // If we have more than 1 value then we have an array. Add array index.
      if (values.size() > 1)
         heading += "(" + itoa(i+1) + ")";

      // Now calculate a field width.
      int fieldWidth = max(15, heading.length() + 1);
      fieldWidth = max(fieldWidth, values[i].length() + 1);
      fieldWidth = max(fieldWidth, units.length() + 1);
      widths.push_back(fieldWidth);

      writeValueTo(out, heading, fieldWidth);
      }
   }
// ------------------------------------------------------------------
// write this field's units to the specified output stream.
// ------------------------------------------------------------------
void Field::writeUnits(ostream& out)
   {
   for (unsigned i = 0; i != values.size(); i++)
      {
      if (i > 0 && csv)
         out << ',';
      writeValueTo(out, units, widths[i]);
      }
   }


// ------------------------------------------------------------------
// Go get a variable from system.
// ------------------------------------------------------------------
void Field::getValues()
   {
   scienceAPI.get(fqn, "", true, values);
   if (values.size() == 0)
      values.push_back(nastring);
   else
      formatValues();
   }

// ------------------------------------------------------------------
// Format the values according to the format string. Format string
// can be a date format (e.g. dd/mm/yyyy or a precision number)
// ------------------------------------------------------------------
void Field::formatValues(void)
   {
   if (format != "" && units != "(year)" && units != "(day)")
      {
      for (unsigned i = 0; i != values.size(); i++)
         {
         if (Is_numerical(format.c_str()))
            {
            int precision = atoi(format.c_str());
            char* endptr;
            double value = strtod(values[i].c_str(), &endptr);
            if (*endptr == '\0')
               values[i] = ftoa(value, precision);
            }
         else
            {
            GDate d;
            d.Set(atoi(values[i].c_str()));
            d.Set_write_format(format.c_str());
            d.Write(values[i]);
            }
         }
      }
   }

// ------------------------------------------------------------------
// write this field to summary file
// ------------------------------------------------------------------
void Field::writeValueTo(ostream& out, const std::string& value, unsigned fieldWidth)
   {
   if (csv)
      out << value;
   else
      {
      out.width(fieldWidth);
      if (value.length() >= fieldWidth)
         out << value.substr(0, fieldWidth-1);
      else
         out << value;
      }
   }

// ------------------------------------------------------------------
// write this field to specified output stream.
// ------------------------------------------------------------------
void Field::writeValue(ostream& out)
   {
   getValues();
   for (unsigned i = 0; i != values.size(); i++)
      {
      if (csv && i > 0)
         out << ',';
      writeValueTo(out, values[i], widths[i]);
      }
   }

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// Create an instance of the REPORT module
// ------------------------------------------------------------------
extern "C" ReportComponent* STDCALL EXPORT createComponent(ScienceAPI& scienceAPI)
   {
   return new ReportComponent(scienceAPI);
   }
extern "C" void STDCALL EXPORT deleteComponent(ReportComponent* component)
   {
   delete component;
   }
// ------------------------------------------------------------------
// initialise the REPORT component.
// ------------------------------------------------------------------
ReportComponent::ReportComponent(ScienceAPI& scienceapi)
   : scienceAPI(scienceapi)
   {
   outputOnThisDay = false;
   scienceAPI.subscribe("init1", nullFunction(&ReportComponent::onInit1));
   csv = false;
   }

// ------------------------------------------------------------------
// initialise the REPORT component - STAGE 1.
// ------------------------------------------------------------------
void ReportComponent::onInit1()
   {
   scienceAPI.subscribe("init2", nullFunction(&ReportComponent::onInit2));
   scienceAPI.subscribe("report", nullFunction(&ReportComponent::onReport));
   scienceAPI.subscribe("do_output", nullFunction(&ReportComponent::onDoOutput));
   scienceAPI.subscribe("do_end_day_output", nullFunction(&ReportComponent::onDoEndDayOutput));
   scienceAPI.expose("days_since_last_report", "day", "The number of days since the last output",
                     false, daysSinceLastReport);
   }

// ------------------------------------------------------------------
// initialise the REPORT component - STAGE 2.
// ------------------------------------------------------------------
void ReportComponent::onInit2(void)
   {
   // work out a filename.
   string fileName;
   if (!scienceAPI.read("outputfile", fileName, true))
      fileName = calcFileName();
   file.open(fileName.c_str());
   if (!file)
      throw runtime_error("Cannot open output file (sharing violation?): " + fileName);

   // get output frequencies
   std::vector<string> frequencies;
   if (scienceAPI.read("outputfrequency", frequencies, true))
      {
      if (frequencies.size() > 0)
         scienceAPI.write("Output frequency:");
      for (unsigned f = 0; f != frequencies.size(); f++)
         {
         stripLeadingTrailing(frequencies[f], " ");
         string name = "   " + frequencies[f];
         scienceAPI.write(name);
         scienceAPI.subscribe(frequencies[f], nullFunction(&ReportComponent::onFrequency));
         }
      }

   if (!scienceAPI.read("NAString", nastring, true))
      nastring = "?";

   // get format specifier.
   string csvString;
   if (scienceAPI.read("format", csvString, true))
      csv = Str_i_Eq(csvString, "csv");

   // get precision.
   if (!scienceAPI.read("precision", precision, true))
      {
      if (csv)
         precision = 6;
      else
         precision = 3;
      }

   // enumerate through all output variables
   // and create a field for each.
   scienceAPI.write("Output variables:");
   scienceAPI.read("variable", variableLines, true);
   for (unsigned i = 0; i != variableLines.size(); i++)
      scienceAPI.write("   " + variableLines[i]);

   scienceAPI.write("");

   daysSinceLastReport = 1;

   // write out all initial conditions.
   string msg = "Output file = " + fileName;
   scienceAPI.write(msg);
   msg = "Format = ";
   if (csv)
      msg += "csv";
   else
      msg += "normal";
   scienceAPI.write(msg);
   haveWrittenHeadings = false;
   }

// ------------------------------------------------------------------
// parese the variable line and create a variable object.
// ------------------------------------------------------------------
void ReportComponent::createVariable(const string& name)
   {
   string variable = name;
   string format, alias;
   vector<string> variableNames;

   // look for format.
   unsigned pos = variable.find(" format ");
   if (pos != string::npos)
      {
      format = variable.substr(pos + strlen(" format "));
      To_upper(format);
      stripLeadingTrailing(format, " ");
      variable.erase(pos);
      }

   // look for alias.
   pos = variable.find(" as ");
   if (pos != string::npos)
      {
      alias = variable.substr(pos + strlen(" as "));
      stripLeadingTrailing(alias, " ");
      variable.erase(pos);
      }

  stripLeadingTrailing(variable, " ");

   // find out who owns what out there in the simulation
   vector<QueryMatch> matches;
   if (variable.find('.') == string::npos)
      scienceAPI.query("*." + variable, matches);
   else
      scienceAPI.query(variable, matches);

   if (format == "")
      format = itoa(precision);

   if (matches.size() == 0)
      fields.push_back(Field(scienceAPI, variable, "?", alias,
                             nastring, format, csv));
   else
      {
      for (unsigned i = 0; i != matches.size(); i++)
         {
         string thisAlias = alias;
         if (alias == "" && matches.size() > 1)
            thisAlias = matches[i].name;

         fields.push_back(Field(scienceAPI, matches[i].name, matches[i].units, thisAlias,
                                nastring, format, csv));
         }
      }
   }

// ------------------------------------------------------------------
// Calculate a file name based on simulation title and PM name.
// ------------------------------------------------------------------
string ReportComponent::calcFileName()
   {
   string title, pmName;

   scienceAPI.read("title", title, true);

   pmName = scienceAPI.parent();

   string fileName = title;
   if (!Str_i_Eq(pmName, "paddock") && !Str_i_Eq(pmName, "masterpm"))
      {
      if (fileName != "")
         fileName += " ";
      fileName += pmName;
      }

   if (!Str_i_Eq(scienceAPI.name(), "outputfile"))
      {
      if (fileName != "")
         fileName += " ";
      fileName += scienceAPI.name();
      }
   fileName += ".out";
   return fileName;
   }

// ------------------------------------------------------------------
// Handle the report event.
// ------------------------------------------------------------------
void ReportComponent::onReport()
   {
   daysSinceLastReport++;

   if (outputOnThisDay)
      writeLineOfOutput();
   outputOnThisDay = false;
   }

// ------------------------------------------------------------------
// Handle one of the frequency events.
// ------------------------------------------------------------------
void ReportComponent::onFrequency()
   {
   writeLineOfOutput();
   }

// ------------------------------------------------------------------
// Handle the doOutput event.
// ------------------------------------------------------------------
void ReportComponent::onDoOutput()
   {
   writeLineOfOutput();
   }

// ------------------------------------------------------------------
// Handle the doEndDayOutput event.
// ------------------------------------------------------------------
void ReportComponent::onDoEndDayOutput()
   {
   outputOnThisDay = true;
   }

// ------------------------------------------------------------------
// write a line of output to output file.
// ------------------------------------------------------------------
void ReportComponent::writeLineOfOutput(void)
   {
   if (!haveWrittenHeadings)
      {
      for (unsigned i = 0; i != variableLines.size(); i++)
         createVariable(variableLines[i]);

      haveWrittenHeadings = true;
      writeHeadings();
      }

   for (vector<Field>::iterator f = fields.begin();
                                f != fields.end();
                                f++)
      {
      if (csv && f != fields.begin())
         file << ',';
      (*f).writeValue(file);
      }

   file << endl;

   daysSinceLastReport = 0;
   scienceAPI.publish("reported");
   }

// ------------------------------------------------------------------
// write out headings and units.
// ------------------------------------------------------------------
void ReportComponent::writeHeadings(void)
   {
   // write out headings and units.
   ostringstream headingLine;
   ostringstream unitLine;
   for (vector<Field>::iterator f = fields.begin();
                                f != fields.end();
                                f++)
      {
      if (csv && f != fields.begin())
         {
         headingLine << ',';
         unitLine << ',';
         }
      f->writeHeadings(headingLine);
      f->writeUnits(unitLine);
      }

   // output summary_file
   string summaryFile;
   scienceAPI.get("summaryfile", "", true, summaryFile);
   file << "Summary_file = " << summaryFile << endl;

   // output title
   string title;
   scienceAPI.get("title", "", true, title);
   file << "Title = " << title << endl;

   // output headings and units
   file << headingLine.str() << endl;
   file << unitLine.str() << endl;
   }

