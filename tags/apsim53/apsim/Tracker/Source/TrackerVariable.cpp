//---------------------------------------------------------------------------
#include <general/pch.h>
#pragma hdrstop

#include "TrackerVariable.h"
#include <ComponentInterface/MessageDataExt.h>
#include <ComponentInterface/Component.h>
#include <general/StringTokenizer.h>
#include <general/string_functions.h>
#include <numeric>
#include <ApsimShared/FStringExt.h>
#include <boost/date_time/gregorian/gregorian.hpp>
#include <general/date_functions.h>

#pragma package(smart_init)
using namespace std;
using namespace boost::gregorian;

//---------------------------------------------------------------------------
// constructor
//---------------------------------------------------------------------------
TrackerVariable::TrackerVariable(protocol::Component* p, const string& fullName)
  : name(fullName), parent(p)
   {
   count = 0;
   last = 0;
   inWindow = false;
   parse(fullName);
   sampleDate = "?";
   }
// ------------------------------------------------------------------
// Parse the name passed in.
// ------------------------------------------------------------------
void TrackerVariable::parse(const string& fullName)
   {
   StringTokenizer tokenizer(fullName, " ");

   // get stat
   parseStat(tokenizer);

   // make sure next word is 'of'
   if (!Str_i_Eq(tokenizer.nextToken(), "of"))
      throw runtime_error("Expected keyword 'of' in tracker variable: " + fullName);

   // get variable.
   if (stat == countStat || stat == dateStat)
      parseEventName(tokenizer);
   else
      variableName = tokenizer.nextToken();

   // Look for an on followed by an event name.
   if (stat != countStat && stat != dateStat)
      {
      if (!Str_i_Eq(tokenizer.nextToken(), "on"))
         throw runtime_error("Expected keyword 'on' in tracker variable: " + fullName);
      parseEventName(tokenizer);
      }

   string keyword = tokenizer.nextToken();
   if (stat != valueStat && Str_i_Eq(keyword, "from"))
      {
      parsePeriod(tokenizer);
      keyword = tokenizer.nextToken();
      }
   else
      inWindow = true;

   if (Str_i_Eq(keyword, "as"))
      parseAs(tokenizer);
   }
// ------------------------------------------------------------------
// Parse a 'stat'
// ------------------------------------------------------------------
void TrackerVariable::parseStat(StringTokenizer& tokenizer)
   {
   string statName = tokenizer.nextToken();
   if (Str_i_Eq(statName, "sum"))
      stat = sumStat;
   else if (Str_i_Eq(statName, "average"))
      stat = averageStat;
   else if (Str_i_Eq(statName, "minimum"))
      stat = minimumStat;
   else if (Str_i_Eq(statName, "maximum"))
      stat = maximumStat;
   else if (Str_i_Eq(statName, "count"))
      stat = countStat;
   else if (Str_i_Eq(statName, "date"))
      stat = dateStat;
   else if (Str_i_Eq(statName, "value"))
      stat = valueStat;
   else
      throw runtime_error("Invalid stat name: " + statName);
   }
// ------------------------------------------------------------------
// Parse a 'from' section
// ------------------------------------------------------------------
void TrackerVariable::parsePeriod(StringTokenizer& tokenizer)
   {
   startPeriod = tokenizer.nextToken();
   unsigned posPeriod = startPeriod.find('.');
   if (posPeriod != string::npos)
      {
      startPeriodComponent = startPeriod.substr(0, posPeriod);
      startPeriod = startPeriod.substr(posPeriod+1);
      }
   if (Str_i_Eq(startPeriod, "reported"))
      inWindow = true;

   if (startPeriod == "" || Str_i_Eq(startPeriod, "to"))
      throw runtime_error("Expected a start of period after a 'from' keyword in tracker variable");

   if (!Str_i_Eq(tokenizer.nextToken(), "to"))
      throw runtime_error("Expected a 'to' keyword in tracker variable.");

   endPeriod = tokenizer.nextToken();
   posPeriod = endPeriod.find('.');
   if (posPeriod != string::npos)
      {
      endPeriodComponent = endPeriod.substr(0, posPeriod);
      endPeriod = endPeriod.substr(posPeriod+1);
      }
   if (endPeriod == "")
      throw runtime_error("Expected an end of period after a 'to' keyword in tracker variable");
   if (Str_i_Eq(endPeriod, "now"))
      endPeriod = "";
   }
// ------------------------------------------------------------------
// Parse a 'last' section
// ------------------------------------------------------------------
void TrackerVariable::parseLast(StringTokenizer& tokenizer)
   {
   string number = tokenizer.nextToken();
   if (!Is_numerical(number.c_str()))
      throw runtime_error("Expected a number following a 'last' keyword.");
   last = atoi(number.c_str());
   }
// ------------------------------------------------------------------
// Parse an 'as' section
// ------------------------------------------------------------------
void TrackerVariable::parseAs(StringTokenizer& tokenizer)
   {
   name = tokenizer.nextToken();
   if (name == "")
      throw runtime_error("Expected a name following an 'as' keyword in tracker variable.");
   }
// ------------------------------------------------------------------
// Parse an 'on' section
// ------------------------------------------------------------------
void TrackerVariable::parseEventName(StringTokenizer& tokenizer)
   {
   eventName = tokenizer.nextToken();
   if (Str_i_Eq(eventName, "last"))
      {
      if (stat == countStat || stat == dateStat)
         throw runtime_error("A 'last' keyword cannot be used with a 'count' or a 'date' "
                             "keyword in a tracker variable");
      parseLast(tokenizer);
      eventName = tokenizer.nextToken();
      }

   unsigned posPeriod = eventName.find('.');
   if (posPeriod != string::npos)
      {
      eventNameComponent = eventName.substr(0, posPeriod);
      eventName = eventName.substr(posPeriod+1);
      }
   if (eventName == "")
      throw runtime_error("Expected an event name.");
   else if (Str_i_Eq(eventName, "start_of_day"))
      eventName = "prepare";
   else if (Str_i_Eq(eventName, "end_of_day"))
      eventName = "post";
   }
// ------------------------------------------------------------------
// Do all necessary registrations.
// ------------------------------------------------------------------
void TrackerVariable::doRegistrations(void)
   {
   static const char* nullDDML = "<type/>";
   static const char* doubleDDML = "<type kind=\"double\"/>";
   static const char* stringDDML = "<type kind=\"string\"/>";
   static const char* singleArrayDDML = "<type kind=\"single\" array=\"T\"/>";
   string typeString = singleArrayDDML;

   eventID = parent->addRegistration(RegistrationType::respondToEvent,
                                        eventName.c_str(),
                                        nullDDML);
   if (variableName != "")
      {
      variableID = parent->addRegistration(RegistrationType::get,
                                           variableName.c_str(),
                                           singleArrayDDML);
      protocol::Variant* variant;
      bool ok = parent->getVariable(variableID, variant, true);
      if (ok)
         {
         protocol::Type t = variant->getType();
         typeString = "<type kind=\"single\" unit=\"" + asString(t.getUnits())
                    + "\" array=\"T\"/>";

         parent->setRegistrationType(variableID, typeString.c_str());
         }
      }


   if (startPeriod != "")
      startPeriodID = parent->addRegistration(RegistrationType::respondToEvent,
                                              startPeriod.c_str(),
                                              nullDDML);
   if (endPeriod != "")
      endPeriodID = parent->addRegistration(RegistrationType::respondToEvent,
                                            endPeriod.c_str(),
                                            nullDDML);
   if (stat == dateStat)
      {
      nameID = parent->addRegistration(RegistrationType::respondToGet,
                                       name.c_str(),
                                       stringDDML);
      todayID = parent->addRegistration(RegistrationType::get,
                                       "today",
                                       doubleDDML);
      }
   else
      {
      nameID = parent->addRegistration(RegistrationType::respondToGet,
                                       name.c_str(),
                                       typeString.c_str());
      parent->setRegistrationType(nameID, typeString.c_str());
      }
   }
// ------------------------------------------------------------------
// Incoming events come through here.
// ------------------------------------------------------------------
void TrackerVariable::respondToEvent(unsigned fromID, unsigned evntID)
   {
   char buffer[1000];
   FString fromName(buffer, sizeof(buffer), CString);
   if (evntID == eventID)
      {
      if (eventNameComponent == ""
             || (parent->componentIDToName(fromID, fromName)
                 && fromName == eventNameComponent.c_str()))
         doSample();
      }
   else if (evntID == startPeriodID)
      {
      if (startPeriodComponent == ""
             || (parent->componentIDToName(fromID, fromName)
                 && fromName == startPeriodComponent.c_str()))
         onStartPeriod();
      }
   else if (evntID == endPeriodID)
      {
      if (endPeriodComponent == ""
             || (parent->componentIDToName(fromID, fromName)
                 && fromName == endPeriodComponent.c_str()))
         onEndPeriod();
      }
   }
// ------------------------------------------------------------------
// Incoming requests for values of variables come through here.
// ------------------------------------------------------------------
void TrackerVariable::respondToGet(unsigned int& fromID,
                                   protocol::QueryValueData& queryData)
   {
   if (queryData.ID == nameID)
      {
      if (stat != dateStat)
         {
         vector<float> values;
         getCurrentValues(values);
         parent->sendVariable(queryData, values);
         }
      else
         parent->sendVariable(queryData, sampleDate);
      }
   }
// ------------------------------------------------------------------
// Perform a sample.
// ------------------------------------------------------------------
void TrackerVariable::doSample(void)
   {
   if (stat == countStat)
      count++;
   else if (stat == dateStat)
      {
      protocol::Variant* variant;
      if (parent->getVariable(todayID, variant))
         {
         double today;
         variant->unpack(today);
         sampleDate = to_dmy(date(today));
         }
      }
   else if (inWindow)
      {
      protocol::Variant* variant;
      bool ok = parent->getVariable(variableID, variant);
      if (ok)
         {
         vector<float> theseValues;
         variant->unpack(theseValues);
         if (stat == valueStat)
            values.erase(values.begin(), values.end());

         values.push_back(theseValues);

         if (last != 0)
            {
            // make sure there are no more than 'last' values in values vector.
            while (values.size() > last)
               values.erase(values.begin());
            }
         }
      }
   }
// ------------------------------------------------------------------
// we're at the start of the sampling window.
// ------------------------------------------------------------------
void TrackerVariable::onStartPeriod(void)
   {
   values.erase(values.begin(), values.end());
   count = 0;
   inWindow = true;
   }
// ------------------------------------------------------------------
// we're at the end of the sampling window.
// ------------------------------------------------------------------
void TrackerVariable::onEndPeriod(void)
   {
   inWindow = false;
   }
// ------------------------------------------------------------------
// return the current value to caller.
// ------------------------------------------------------------------
void TrackerVariable::getCurrentValues(vector<float>& currentValues)
   {
   currentValues.erase(currentValues.begin(), currentValues.end());
   if (stat == countStat)
      currentValues.push_back(count);
   else if (values.size() > 0)
      {
      for (unsigned i = 0; i != values[0].size(); ++i)
         {
         float value = 0.0;
         for (unsigned v = 0; v != values.size(); ++v)
            {
            switch (stat)
               {
               case sumStat     :
               case averageStat : value += values[v][i]; break;
               case minimumStat : value = min(value, values[v][i]); break;
               case maximumStat : value = max(value, values[v][i]); break;
               case countStat   : value++; break;
               case valueStat   : value = values[v][i]; break;
               }
            }
         if (stat == averageStat && values.size() > 0)
            value /= values.size();
         currentValues.push_back(value);
         }
      }
   }

