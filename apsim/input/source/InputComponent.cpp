#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "InputComponent.h"
#include <ApsimShared\ApsimComponentData.h>
#include <ComponentInterface\MessageDataExt.h>
#include <ComponentInterface\ApsimVariant.h>
#include <general\string_functions.h>
#include <general\stl_functions.h>
#include <general\date_class.h>
#include <ApsimShared\ApsimDataFile.h>
#include <list>

using namespace std;

static const char* newmetType =
   "<type name=\"newmet\">"
   "   <field name=\"radn\"  kind=\"single\" units=\"MJ/m2/d\">"
   "   <field name=\"maxt\"  kind=\"single\" units=\"mm/d\">"
   "   <field name=\"mint\"  kind=\"single\" units=\"mm/d\">"
   "   <field name=\"rain\"  kind=\"single\" units=\"mm/d\">"
   "   <field name=\"vp\"    kind=\"single\" units=\"????\">"
   "</type>";

// ------------------------------------------------------------------
// createComponent
// ------------------------------------------------------------------
protocol::Component* createComponent(void)
   {
   return new InputComponent;
   }

// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
InputComponent::InputComponent(void)
   {
   data = NULL;
   }

// ------------------------------------------------------------------
// initialise the REPORT component.
// ------------------------------------------------------------------
InputComponent::~InputComponent(void)
   {
   delete data;
   for (InputVariables::iterator i = variables.begin();
                                 i != variables.end();
                                 i++)
      delete i->second;
   }
// ------------------------------------------------------------------
// INIT1 method handler.
// ------------------------------------------------------------------
void InputComponent::doInit1(const FString& sdml)
   {
   protocol::Component::doInit1(sdml);

   iAmMet = (stricmp(name, "met") == 0);

   try
      {
      string fileName = componentData->getProperty("parameters", "filename");
      if (fileName == "")
         throw runtime_error("Cannot find a filename parameter for module: "
                             + string(name));
      data = new ApsimDataFile(fileName);

      readConstants();
      readHeadings();
      dateFieldsOk();

      // read in a line from the file so that the fields know what data
      // type they're dealing with.
      readLineFromFile();

      // register a few things.
      tickID = addRegistration(protocol::respondToEventReg, "tick", "");
      newmetID = addRegistration(protocol::eventReg, "newmet", "");

      checkForSparseData();
      if (allowSparseData)
         writeString("Sparse data is allowed");
      else
         writeString("Sparse data is not allowed");
      string msg = "INPUT File name: " + fileName;
      writeString(msg.c_str());
      }
   catch (const runtime_error& errormsg)
      {
      error(errormsg.what(), true);
      }
   }

// ------------------------------------------------------------------
// INIT 2 - temporary
// ------------------------------------------------------------------
void InputComponent::doInit2(void)
   {
   }
// ------------------------------------------------------------------
// read in all constants from file.
// ------------------------------------------------------------------
void InputComponent::readConstants(void)
   {
   vector<string> constantNames;
   data->getConstantNames(constantNames);

   // read in each constant
   for (vector<string>::iterator constantI = constantNames.begin();
                                 constantI != constantNames.end();
                                 constantI++)
      addVariable(*constantI, data->getConstant(*constantI), 1);
   }
// ------------------------------------------------------------------
// read in all headings from file.
// ------------------------------------------------------------------
void InputComponent::readHeadings(void)
   {
   vector<string> fieldNames, fieldUnits;
   data->getFieldNames(fieldNames);
   data->getFieldUnits(fieldUnits);

   // Loop through all field names and look for an array specifier.
   // If one is found then strip it off, then add the variable to
   // our variables container.
   for (unsigned int fieldI = 0; fieldI != fieldNames.size(); fieldI++)
      {
      if (Str_i_Eq(fieldNames[fieldI], "year") ||
          Str_i_Eq(fieldNames[fieldI], "day"))
         fieldNames[fieldI] = string(name) + "_" + fieldNames[fieldI];
      string fieldNameMinusSpec;
      unsigned int arraySpec;
      removeArraySpec(fieldNames[fieldI], fieldNameMinusSpec, arraySpec);

      addVariable(fieldNameMinusSpec,
                  data->getFieldValue(fieldI),
                  arraySpec, true);
      }
   }

// ------------------------------------------------------------------
// Find a specific variable whose name matches the one passed in.
// ------------------------------------------------------------------
InputComponent::InputVariables::iterator InputComponent::findVariable(const string& name)
   {
   for (InputVariables::iterator inputVariable = variables.begin();
                                 inputVariable != variables.end();
                                 inputVariable++)
      {
      if (Str_i_Eq(inputVariable->second->getName(), name))
         return inputVariable;
      }
   return variables.end();
   }
// ------------------------------------------------------------------
// Strip off the array specifier if found.  Return the variable
// name with the array specifier removed, and the array index.
// Returns true if all went ok.
// ------------------------------------------------------------------
void InputComponent::removeArraySpec(const string& fieldName,
                                     string& fieldNameMinusSpec,
                                     unsigned int& arraySpec)
   {
   unsigned posStartArraySpec = fieldName.find("(");
   if (posStartArraySpec != string::npos)
      {
      unsigned posEndArraySpec = fieldName.find(")");
      if (posEndArraySpec == fieldName.length()-1)
         {
         fieldNameMinusSpec = fieldName.substr(0, posStartArraySpec);
         arraySpec = atoi(fieldName.substr(posStartArraySpec+1,
                                           posEndArraySpec-posStartArraySpec-1).c_str());
         }
      else
         {
         string msg = "Invalid array specification on INPUT field name: ";
         msg += fieldName;
         throw runtime_error(msg);
         }
      }
   else
      {
      fieldNameMinusSpec = fieldName;
      arraySpec = 1;
      }
   }

// ------------------------------------------------------------------
// Register all our variables.
// ------------------------------------------------------------------
void InputComponent::addVariable(const std::string& name,
                                 const std::string& value,
                                 unsigned arrayIndex,
                                 bool isTemporal)
   {
   StringVariant* stringVariant;
   InputVariables::iterator variableI = findVariable(name);
   if (variableI == variables.end())
      {
      stringVariant = new StringVariant(this, name, value);
      if (isTemporal)
         temporalVariables.push_back(stringVariant);
      }
   else
      stringVariant = variableI->second;
   if (value.find(" ") != string::npos)
      stringVariant->addValues(value);
   else
      stringVariant->addValue(value, arrayIndex-1);

   // Last step - do the registration now that the stringVariant has a value
   // use the registration ID as the index into the variables map.
   unsigned int regID = stringVariant->doRegistration();
   variables.insert(InputVariables::value_type(regID, stringVariant));
   }
// ------------------------------------------------------------------
// Check to see if we need to handle sparse data or not.
// ------------------------------------------------------------------
void InputComponent::checkForSparseData(void)
   {
   InputVariables::iterator i = findVariable("allow_sparse_data");
   if (i == variables.end())
      allowSparseData = false;
   else
      i->second->asLogical(allowSparseData);
   }
// ------------------------------------------------------------------
// Advance the file to todays date.
// ------------------------------------------------------------------
bool InputComponent::advanceToTodaysData(void)
   {
   double fileDate;

   do
      {
      readLineFromFile();
      fileDate = getFileDate();
      }
   while (fileDate < todaysDate && data->next());

   if (fileDate == todaysDate)
      {
      data->next();
      return true;
      }
   else
      return false;
   }
// ------------------------------------------------------------------
// Get a file date from the variables container.
// ------------------------------------------------------------------
unsigned long InputComponent::getFileDate(void)
   {
   int year;
   yearI->asInteger(year);

   if (dayOfYearI != NULL)
      {
      int day_of_year;
      dayOfYearI->asInteger(day_of_year);
      GDate date;
      date.Set(day_of_year, year);
      return date.Get_jday();
      }
   else
      {
      int day;
      dayOfMonthI->asInteger(day);
      int month;
      monthI->asInteger(month);
      GDate date;
      date.Set(day, month, year);
      return date.Get_jday();
      }
   }
// ------------------------------------------------------------------
// If dates are not ok then throw error.
// ------------------------------------------------------------------
void InputComponent::dateFieldsOk(void)
   {
   InputVariables::iterator i = findVariable(string(name) + "_year");
   if (i != variables.end())
      yearI = i->second;
   i = findVariable(string(name) + "_day");
   if (i != variables.end())
      dayOfYearI = i->second;
   i = findVariable("day_of_month");
   if (i != variables.end())
      dayOfMonthI = i->second;
   i = findVariable("month");
   if (i != variables.end())
      monthI = i->second;
   bool ok = (yearI != NULL &&
           (dayOfYearI != NULL ||
            (dayOfMonthI != NULL && monthI != NULL)));
   if (!ok)
      throw runtime_error("APSIM input files must have year and day OR day, "
                          "month and year columns.");
   }

// ------------------------------------------------------------------
// Reads in a line of data from file and stores values in
// 'variables'.  Return true if values read ok.
// ------------------------------------------------------------------
void InputComponent::readLineFromFile()
   {
   // loop through all temporal variables (ie. goto end of variables
   // container) and get a value from the line and store into the
   // variable.  Each variable knows how many values to expect from
   // the line.
   TemporalVariables::iterator variableI = temporalVariables.begin();
   string value;
   unsigned fieldNumber = 0;
   while (variableI != temporalVariables.end())
      {
      for (unsigned valueI = 0; valueI != (*variableI)->numValues(); valueI++)
         {
         value = data->getFieldValue(fieldNumber++);
         (*variableI)->addValue(value, valueI);
         }
      variableI++;
      }
   }
// ------------------------------------------------------------------
// return a variable to caller.
// ------------------------------------------------------------------
void InputComponent::respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData)
   {
   variables[queryData.ID]->sendVariable(queryData);
   }

// ------------------------------------------------------------------
// set the value of one of our variables.
// ------------------------------------------------------------------
bool InputComponent::respondToSet(unsigned int& fromID, protocol::QuerySetValueData& setValueData)
   {
   variables[setValueData.ID]->setVariable(setValueData);
   return true;
   }

// ------------------------------------------------------------------
// Event handler.
// ------------------------------------------------------------------
void InputComponent::respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
   {
   if (eventID == tickID)
      {
      protocol::ApsimVariant apsimVariant(variant);
      double jday;
      apsimVariant.get("jday", protocol::DTdouble, jday);

      todaysDate = jday;
      if (!advanceToTodaysData() && !allowSparseData)
         {
         GDate errorDate;
         errorDate.Set((unsigned long) todaysDate);

         string msg = "Cannot find data in INPUT file for day: ";
         msg += IntToStr(errorDate.Get_day_of_year()).c_str();
         msg += " and year: ";
         msg += IntToStr(errorDate.Get_year()).c_str();
         error(msg.c_str(), true);
         }
      else
         publishNewMetEvent();
      }
   }
// ------------------------------------------------------------------
// Find a value and return it's numerical value.  Returns true if
// variable is found and it has a value.
// ------------------------------------------------------------------
bool InputComponent::getVariableValue(const string& name, float& value)
   {
   InputVariables::iterator i = findVariable(name);
   if (i != variables.end())
      return i->second->asFloat(value);
   else
      return false;
   }
// ------------------------------------------------------------------
// Publish a tick event.
// ------------------------------------------------------------------
namespace protocol {
struct NewMet
   {
   float radn;
   float maxt;
   float mint;
   float rain;
   float vp;
   };
inline protocol::MessageData& operator<<(protocol::MessageData& messageData, const NewMet& newmet)
   {
   messageData << newmet.radn << newmet.maxt << newmet.mint << newmet.rain << newmet.vp;
   return messageData;
   }
inline unsigned int memorySize(const NewMet& newmet)
   {
   return protocol::memorySize(newmet.maxt) * 5;
   }
}; // protocol
void InputComponent::publishNewMetEvent(void)
   {
   protocol::NewMet newmet;
   getVariableValue("maxt", newmet.maxt);
   getVariableValue("mint", newmet.mint);
   getVariableValue("radn", newmet.radn);
   getVariableValue("rain", newmet.rain);
   getVariableValue("vp", newmet.vp);
   publish(newmetID, newmet);
   }

