//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
#include <aps\APSIMExternalTable.h>


#include "InputComponent.h"
#include <aps\somcomponent.h>
#include <general\string_functions.h>
#include <general\stl_functions.h>
#include <general\date_class.h>
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
//  Short description:
//     createComponent

//  Notes:

//  Changes:
//    DPH 29/7/99

// ------------------------------------------------------------------
protocol::Component* createComponent(void)
   {
   return new InputComponent;
   }

// ------------------------------------------------------------------
//  Short description:
//     constructor

//  Notes:

//  Changes:
//    dph 27/6/2001

// ------------------------------------------------------------------
InputComponent::InputComponent(void)
   {
   }

// ------------------------------------------------------------------
//  Short description:
//     initialise the REPORT component.

//  Notes:

//  Changes:
//    dph 27/6/2001

// ------------------------------------------------------------------
InputComponent::~InputComponent(void)
   {
   delete externalTable;
   for (InputVariables::iterator i = variables.begin();
                                 i != variables.end();
                                 i++)
      delete i->second;
   }

// ------------------------------------------------------------------
//  Short description:
//     INIT1 method handler.

//  Notes:

//  Changes:
//    dph 27/6/2001

// ------------------------------------------------------------------
void InputComponent::doInit1(const FString& sdml)
   {
   protocol::Component::doInit1(sdml);

   iAmMet = (stricmp(name, "met") == 0);

   // open input file.
   if (iAmMet)
      openTable("weather");
   else
      openTable("input");
   if (externalTable == NULL)
      error("The met/input section cannot be found.\n"
            "Possibly the data/met file cannot be opened\n"
            " or the section is missing.", true);
   else if (readConstants() && readHeadings() && dateFieldsOk())
      {
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
      string msg = "INPUT File name: " + externalTable->getFilename();
      writeString(msg.c_str());
      }
   }

// ------------------------------------------------------------------
//  Short description:
//     INIT 2 - temporary

//  Notes:

//  Changes:
//    dph 27/6/2001

// ------------------------------------------------------------------
void InputComponent::doInit2(void)
   {
   }

// ------------------------------------------------------------------
//  Short description:
//     Get and open the table we're going to read from.

//  Notes:

//  Changes:
//    dph 27/6/2001

// ------------------------------------------------------------------
void InputComponent::openTable(const std::string& groupName)
   {
   SOMPropertyGroup group = componentData->findGroupWithProperty
                            (name, groupName, "table");
   if (group.isValid())
      {
      externalTable = new APSIMExternalTable(group.getProperty("table", name));
      externalTable->open();
      }
   else
      externalTable = NULL;
   }
// ------------------------------------------------------------------
//  Short description:
//     read in all constants from file.

//  Notes:

//  Changes:
//    dph 27/6/2001

// ------------------------------------------------------------------
bool InputComponent::readConstants(void)
   {
   char* sectionName;
   if (iAmMet)
      sectionName = "weather";
   else
      sectionName = "data";

   // get a complete list of property names.
   list<string> propertyNames;
   list<string> groupNames;
   componentData->getGroupNames(groupNames);
   for (list<string>::iterator groupNameI = groupNames.begin();
                               groupNameI != groupNames.end();
                               groupNameI++)
      {
      SOMPropertyGroup group = componentData->getGroup(*groupNameI);
      group.getPropertyNames("property", propertyNames);
      }

   // read in each property.
   for (list<string>::iterator propertyI = propertyNames.begin();
                               propertyI != propertyNames.end();
                               propertyI++)
      {
      char value[100];
      readParameter(sectionName, (*propertyI).c_str(), FString(value, sizeof(value)), false);
      value[99] = 0;
      addVariable(*propertyI, value, 1);
      }

   return true;
   }
// ------------------------------------------------------------------
//  Short description:
//     read in all headings from file.

//  Notes:

//  Changes:
//    dph 27/6/2001

// ------------------------------------------------------------------
bool InputComponent::readHeadings(void)
   {
   list<string> fieldNames = externalTable->getFieldNames();
   list<string> fieldUnits = externalTable->getFieldUnits();

   // Loop through all field names and look for an array specifier.
   // If one is found then strip it off, then add the variable to
   // our variables container.
   bool ok = true;
   list<string>::iterator fieldNameI = fieldNames.begin();
   list<string>::iterator fieldUnitI = fieldUnits.begin();
   unsigned int fieldValueI = 0;
   while (fieldNameI != fieldNames.end() && ok)
      {
      if (Str_i_Eq(*fieldNameI, "year") || Str_i_Eq(*fieldNameI, "day"))
         *fieldNameI = string(name) + "_" + *fieldNameI;
      string fieldNameMinusSpec;
      unsigned int arraySpec;
      ok = removeArraySpec(*fieldNameI, fieldNameMinusSpec, arraySpec);
      if (ok)
         {
         string value;
         externalTable->getValueByIndex(fieldValueI++, value);
         addVariable(fieldNameMinusSpec, value.c_str(), arraySpec, true);
         }

      fieldNameI++;
      fieldUnitI++;
      }
   return ok;
   }

// ------------------------------------------------------------------
//  Short description:
//     Find a specific variable whose name matches the one passed in.

//  Changes:
//    dph 27/6/2001
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
//  Short description:
//     Strip off the array specifier if found.  Return the variable
//     name with the array specifier removed, and the array index.
//     Returns true if all went ok.

//  Notes:

//  Changes:
//    dph 27/6/2001

// ------------------------------------------------------------------
bool InputComponent::removeArraySpec(const string& fieldName,
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
         error(msg.c_str(), true);
         return false;
         }
      }
   else
      {
      fieldNameMinusSpec = fieldName;
      arraySpec = 1;
      }
   return true;
   }

// ------------------------------------------------------------------
//  Short description:
//     Register all our variables.

//  Notes:

//  Changes:
//    dph 27/6/2001

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
//  Short description:
//     Check to see if we need to handle sparse data or not.

//  Notes:

//  Changes:
//    dph 27/6/2001

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
//  Short description:
//     Advance the file to todays date.

//  Notes:

//  Changes:
//    dph 27/6/2001

// ------------------------------------------------------------------
bool InputComponent::advanceToTodaysData(void)
   {
   double fileDate;

   do
      {
      readLineFromFile();
      fileDate = getFileDate();
      }
   while (fileDate < todaysDate && externalTable->next());

   if (fileDate == todaysDate)
      {
      externalTable->next();
      return true;
      }
   else
      return false;
   }

// ------------------------------------------------------------------
//  Short description:
//     Get a file date from the variables container.

//  Notes:

//  Changes:
//    dph 27/6/2001

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
//  Short description:
//     Returns true if the input file has the appropriate date fields.

//  Notes:

//  Changes:
//    dph 27/6/2001

// ------------------------------------------------------------------
bool InputComponent::dateFieldsOk(void)
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
   return (yearI != NULL &&
           (dayOfYearI != NULL ||
            (dayOfMonthI != NULL && monthI != NULL)));
   }

// ------------------------------------------------------------------
//  Short description:
//     Reads in a line of data from file and stores values in
//     'variables'.  Return true if values read ok.

//  Notes:

//  Changes:
//    dph 27/6/2001

// ------------------------------------------------------------------
bool InputComponent::readLineFromFile()
   {
   // loop through all temporal variables (ie. goto end of variables
   // container) and get a value from the line and store into the
   // variable.  Each variable knows how many values to expect from
   // the line.
   TemporalVariables::iterator variableI = temporalVariables.begin();
   string value;
   bool ok = true;
   unsigned fieldNumber = 0;
   while (variableI != temporalVariables.end())
      {
      for (unsigned valueI = 0; valueI != (*variableI)->numValues(); valueI++)
         {
         if (externalTable->getValueByIndex(fieldNumber++, value))
            (*variableI)->addValue(value, valueI);
         else
            {
            string msg = "There are not enough values on the input line to match\n"
                         "the number of headings.";
            error(msg.c_str(), true);
            ok = false;
            }
         }
      variableI++;
      }
   return ok;
   }
// ------------------------------------------------------------------
//  Short description:
//     return a variable to caller.

//  Notes:

//  Changes:
//    dph 27/6/2001

// ------------------------------------------------------------------
void InputComponent::respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData)
   {
   variables[queryData.ID]->sendVariable(queryData);
   }

// ------------------------------------------------------------------
//  Short description:
//     set the value of one of our variables.

//  Notes:

//  Changes:
//    dph 27/6/2001

// ------------------------------------------------------------------
bool InputComponent::respondToSet(unsigned int& fromID, protocol::QuerySetValueData& setValueData)
   {
   variables[setValueData.ID]->setVariable(setValueData);
   return true;
   }

// ------------------------------------------------------------------
//  Short description:
//    Event handler.

//  Notes:

//  Changes:
//    DPH 23/5/2001

// ------------------------------------------------------------------
void InputComponent::respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
   {
   if (eventID == tickID)
      {
      int jday;
      variant.unpack(jday);
      
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
//  Short description:
//    Find a value and return it's numerical value.  Returns true if
//    variable is found and it has a value.

//  Changes:
//    DPH 23/5/2001
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
//  Short description:
//    Publish a tick event.

//  Notes:

//  Changes:
//    DPH 23/5/2001

// ------------------------------------------------------------------
namespace protocol {
struct NewMet
   {
   float maxt;
   float mint;
   float radn;
   float rain;
   float vp;
   };
inline protocol::MessageData& operator<<(protocol::MessageData& messageData, const NewMet& newmet)
   {
   messageData << newmet.maxt << newmet.mint << newmet.radn << newmet.rain << newmet.vp;
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

