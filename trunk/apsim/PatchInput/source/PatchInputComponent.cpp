#include <list>
#include <fstream>
#include <stdexcept>

#include <math.h>
#include <boost/date_time/gregorian/gregorian.hpp>
#include <boost/lexical_cast.hpp>

#include <general/stristr.h>
#include <general/string_functions.h>
#include <general/date_class.h>
#include <general/date_functions.h>

#include <ApsimShared/FStringExt.h>
#include <ApsimShared/ApsimDataFile.h>

#include <ComponentInterface/Component.h>

#include <StringVariant.h>
#include <InputComponent.h>
#include "PatchInputComponent.h"

using namespace std;
using namespace boost::gregorian;
using namespace boost;

// ------------------------------------------------------------------
// createComponent
// ------------------------------------------------------------------
protocol::Component* createComponent(void)
   {
   return new PatchInputComponent;
   }

// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
PatchInputComponent::PatchInputComponent(void)
   : patchDate(1800, 1, 1)
   {
   }

// ------------------------------------------------------------------
// initialise the REPORT component.
// ------------------------------------------------------------------
PatchInputComponent::~PatchInputComponent(void)
   {
   }
// ------------------------------------------------------------------
// INIT1 method handler.
// ------------------------------------------------------------------
void PatchInputComponent::doInit1(const FString& sdml)
   {
   InputComponent::doInit1(sdml);

   haveReadPatchData = false;
   preNewmetID = addRegistration(RegistrationType::respondToEvent, "preNewmet", DDML(protocol::NewMetType()).c_str());
   ApsimDataFile::iterator i = find(data.constantsBegin(),
                                    data.constantsEnd(),
                                    "patch_all_years");
   patchAllYears = (i != data.constantsEnd() && Str_i_Eq(i->values[0], "true"));

   i = find(data.constantsBegin(), data.constantsEnd(), "patch_variables_long_term");
   if (i != data.constantsEnd())
      patchVariablesLongTerm = i->values;
   }
// ------------------------------------------------------------------
// Read all patch dates.
// ------------------------------------------------------------------
void PatchInputComponent::readPatchDates(void)
   {
   if (!data.eof())
      {
      try
         {
         ApsimDataFile::iterator i = find(data.constantsBegin(), data.constantsEnd(), "start_patching_from");
         if (i != data.constantsEnd())
            patchDate = date(from_string(i->values[0]));

         currentRecord = 1;
         minYear = data.getDate().year();
         maxYear = minYear;
         while (!data.eof())
            {
            patchDates.insert(make_pair(data.getDate().julian_day(), currentRecord));
            currentRecord++;
            maxYear = max(maxYear , data.getDate().year());
            data.next();
            }
         data.first();
         currentRecord = 1;
         }
      catch (const exception& err)
         {
         error(err.what(), true);
         }
      }
   haveReadPatchData = true;
   }
// ------------------------------------------------------------------
// Get matching variables from INPUT for the same dates as specified in our
// patch data file.
// ------------------------------------------------------------------
void PatchInputComponent::getDataFromInput(unsigned int fromID)
   {
   static const char* getDataDDML = "<type kind=\"string\" array=\"T\"/>";
   static const char* returnDataDDML =
      "<type name=\"newmet\" array=\"T\">"
      "   <field name=\"today\" kind=\"double\"/>"
      "   <field name=\"radn\" kind=\"single\"/>"
      "   <field name=\"maxt\" kind=\"single\"/>"
      "   <field name=\"mint\" kind=\"single\"/>"
      "   <field name=\"rain\" kind=\"single\"/>"
      "   <field name=\"vp\" kind=\"single\"/>"
      "</type>";

   if (patchVariablesLongTerm.size() > 0)
      {
      vector<string> dataDates;
      for (PatchDates::iterator i = patchDates.begin();
                                i != patchDates.end();
                                i++)
         {
         date d(i->first);
         dataDates.push_back(to_iso_extended_string(d));
         }
      FString fromComponent;
      componentIDToName(fromID, fromComponent);
      string getDataMethodCallString = asString(fromComponent);
      getDataMethodCallString += ".getData";
      getDataMethodID = addRegistration(RegistrationType::event,
                                        getDataMethodCallString.c_str(),
                                        getDataDDML);
      returnDataMethodID = addRegistration(RegistrationType::respondToEvent, "returnData", returnDataDDML);
      publish(getDataMethodID, dataDates);
      }
   }
// ------------------------------------------------------------------
// Advance the file to todays date.
// NB: The patch data file may run over a year boundary eg. for a
//     summer crop - need to handle this situation.
// Returns the date the file is positioned at.
// ------------------------------------------------------------------
date PatchInputComponent::advanceToTodaysPatchData(unsigned int fromID)
   {
   if (!haveReadPatchData)
      {
      readPatchDates();
      getDataFromInput(fromID);
      }
   if (patchDates.size() > 0)
      {
      try
         {
         PatchDates::iterator i = patchDates.find(todaysDate.julian_day());
         if (i == patchDates.end() && patchAllYears)
            {
            for (unsigned tryYear = minYear;
                          tryYear <= maxYear && i == patchDates.end();
                          tryYear++)
               i = patchDates.find(date(tryYear, todaysDate.month(), todaysDate.day()).julian_day());
            }
         if (i != patchDates.end())
            {
            // advance the data file to the correct record.
            unsigned recordToGoTo = i->second;
            if (currentRecord > recordToGoTo)
               {
               currentRecord = 1;
               data.first();
               }
            for (unsigned rec = currentRecord; rec != recordToGoTo; rec++)
               {
               currentRecord++;
               data.next();
               }
            return todaysDate;
            }
         else
            return date(pos_infin);
         }
      catch (const exception& err) // probably caused by a leap year exception.
         {
         return date(pos_infin);
         }
      }
   }
// ------------------------------------------------------------------
// Event handler.
// ------------------------------------------------------------------
void PatchInputComponent::respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
   {
   if (eventID == preNewmetID)
      {
      protocol::NewMetType newmet;
      variant.unpack(newmet);
      todaysDate = newmet.today;

      fileDate = advanceToTodaysPatchData(fromID);
      if (todaysDate == fileDate && todaysDate >= patchDate)
         {
         for (Variables::iterator v = variables.begin();
                                  v != variables.end();
                                  v++)
            {
            StringVariant* var = &(v->second);
            if (stristr(var->getName().c_str(), "day") == NULL &&
                stristr(var->getName().c_str(), "month") == NULL &&
                stristr(var->getName().c_str(), "year") == NULL &&
                stristr(var->getName().c_str(), "allow_sparse_data") == NULL &&
                stristr(var->getName().c_str(), "start_patching_from") == NULL &&
                stristr(var->getName().c_str(), "patch_variables_long_term") == NULL)
               {
               string foreignName = var->getName();
               if (foreignName.find("patch_") == string::npos)
                  {
                  string msg = "Invalid patch variable name: " + foreignName
                             + ".  Variable must start with 'patch_'";
                  error(msg.c_str(), true);
                  break;
                  }
               foreignName.erase(0, strlen("patch_"));

               unsigned variableID = addRegistration(RegistrationType::set,
                                                     foreignName.c_str(),
                                                     DTsingleString,
                                                     "",
                                                     itoa(fromID).c_str());
               setVariable(variableID, var->asFloat());
               }
            }
         if (patchData.size() > 0)
            setPatchData();
         }
      }
   else if (eventID == returnDataMethodID)
      {
      vector<protocol::NewMetType> data;
      variant.unpack(data);
      for (unsigned i = 0; i != data.size(); i++)
         {
         date d(data[i].today);
         unsigned dayNumber = day_of_year(d);
         if (gregorian_calendar::is_leap_year(d.year()))
            dayNumber--;

         patchData.insert(make_pair(dayNumber, data[i]));
         }
      }
   else if (eventID != tickID)  // stop the tick event going to base class.
      InputComponent::respondToEvent(fromID, eventID, variant);

   }

// ------------------------------------------------------------------
// Do a bunch of setVariables back to INPUT for all patchVariablesLongTerm.
// ------------------------------------------------------------------
void PatchInputComponent::setPatchData()
   {
   unsigned dayNumber = day_of_year(todaysDate);
   if (gregorian_calendar::is_leap_year(todaysDate.year()) && dayNumber >= 61)
      dayNumber--;
   PatchData::iterator i = patchData.find(dayNumber);
   if (i == patchData.end())
      {
      string msg = "Cannot find patch data from INPUT component for date ";
      msg += to_iso_extended_string(todaysDate);
      error(msg.c_str(), false);
      }
   else
      {
      if (find(patchVariablesLongTerm.begin(), patchVariablesLongTerm.end(),
               "maxt") != patchVariablesLongTerm.end())
         {
         unsigned maxtID = addRegistration(RegistrationType::set, "maxt", DTsingleString);
         setVariable(maxtID, i->second.maxt);
         }
      if (find(patchVariablesLongTerm.begin(), patchVariablesLongTerm.end(),
               "mint") != patchVariablesLongTerm.end())
         {
         unsigned mintID = addRegistration(RegistrationType::set, "mint", DTsingleString);
         setVariable(mintID, i->second.mint);
         }
      if (find(patchVariablesLongTerm.begin(), patchVariablesLongTerm.end(),
               "radn") != patchVariablesLongTerm.end())
         {
         unsigned radnID = addRegistration(RegistrationType::set, "radn", DTsingleString);
         setVariable(radnID, i->second.radn);
         }
      if (find(patchVariablesLongTerm.begin(), patchVariablesLongTerm.end(),
               "rain") != patchVariablesLongTerm.end())
         {
         unsigned rainID = addRegistration(RegistrationType::set, "rain", DTsingleString);
         setVariable(rainID, i->second.rain);
         }
      }
   }