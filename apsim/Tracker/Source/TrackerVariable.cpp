//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TrackerVariable.h"
#include <ComponentInterface\Component.h>
#include <general\stringtokenizer.h>
#include <general\string_functions.h>
#include <numeric>

#pragma package(smart_init)
using namespace std;
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
      throw runtime_error("Expected keyword 'of'.");

   // get variable.
   variable = tokenizer.nextToken();
   if (variable == "")
      throw runtime_error("Expected sample variable.");

   // get keyword.
   string keyword = tokenizer.nextToken();
   while (keyword != "")
      {
      if (Str_i_Eq(keyword, "between"))
         parseBetween(tokenizer);
      else if (Str_i_Eq(keyword, "last"))
         parseLast(tokenizer);
      else if (Str_i_Eq(keyword, "since"))
         parseSince(tokenizer);
      else if (Str_i_Eq(keyword, "as"))
         parseAs(tokenizer);
      else if (Str_i_Eq(keyword, "on"))
         parseOn(tokenizer);

      keyword = tokenizer.nextToken();
      }
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
   else
      throw runtime_error("Invalid stat name: " + statName);
   }
// ------------------------------------------------------------------
// Parse a 'between' section
// ------------------------------------------------------------------
void TrackerVariable::parseBetween(StringTokenizer& tokenizer)
   {
   startPeriod = tokenizer.nextToken();
   if (!Str_i_Eq(tokenizer.nextToken(), "and"))
      throw runtime_error("Expected keyword 'and'");
   endPeriod = tokenizer.nextToken();
   }
// ------------------------------------------------------------------
// Parse a 'last' section
// ------------------------------------------------------------------
void TrackerVariable::parseLast(StringTokenizer& tokenizer)
   {
   string number = tokenizer.nextToken();
   if (!Is_numerical(number.c_str()))
      throw runtime_error("Expected a number following a 'last' keyword.");
   last = StrToInt(number.c_str());
   inWindow = true;
   }
// ------------------------------------------------------------------
// Parse a 'since' section
// ------------------------------------------------------------------
void TrackerVariable::parseSince(StringTokenizer& tokenizer)
   {
   since = tokenizer.nextToken();
   if (since == "")
      throw runtime_error("Expected an event name following a 'since' keyword.");
   }
// ------------------------------------------------------------------
// Parse an 'as' section
// ------------------------------------------------------------------
void TrackerVariable::parseAs(StringTokenizer& tokenizer)
   {
   name = tokenizer.nextToken();
   if (name == "")
      throw runtime_error("Expected a name following an 'as' keyword.");
   }
// ------------------------------------------------------------------
// Parse an 'on' section
// ------------------------------------------------------------------
void TrackerVariable::parseOn(StringTokenizer& tokenizer)
   {
   on = tokenizer.nextToken();
   if (on == "")
      throw runtime_error("Expected an event name following an 'on' keyword.");
   }
// ------------------------------------------------------------------
// Do all necessary registrations.
// ------------------------------------------------------------------
void TrackerVariable::doRegistrations(void)
   {
   static const char* nullDDML = "<type\\>";
   static const char* singleDDML = "<type kind=\"single\"\\>";

   if (stat == countStat)
      variableID = parent->addRegistration(protocol::respondToEventReg,
                                           variable.c_str(),
                                           nullDDML);
   else
      variableID = parent->addRegistration(protocol::getVariableReg,
                                           variable.c_str(),
                                           singleDDML);

   if (startPeriod != "" && endPeriod != "")
      {
      startPeriodID = parent->addRegistration(protocol::respondToEventReg,
                                              startPeriod.c_str(),
                                              nullDDML);
      endPeriodID = parent->addRegistration(protocol::respondToEventReg,
                                            endPeriod.c_str(),
                                            nullDDML);
      }
   if (since != "")
      sinceID = parent->addRegistration(protocol::respondToEventReg,
                                        since.c_str(),
                                        nullDDML);

   nameID = parent->addRegistration(protocol::respondToGetReg,
                                    name.c_str(),
                                    singleDDML);
   onID = parent->addRegistration(protocol::respondToEventReg,
                                  on.c_str(),
                                  nullDDML);
   }
// ------------------------------------------------------------------
// Incoming events come through here.
// ------------------------------------------------------------------
void TrackerVariable::respondToEvent(unsigned fromID, unsigned eventID)
   {
   if (eventID == onID)
      doSample();
   else if (eventID == startPeriodID || eventID == sinceID)
      onStartPeriod();
   else if (eventID == endPeriodID)
      onEndPeriod();
   }
// ------------------------------------------------------------------
// Incoming requests for values of variables come through here.
// ------------------------------------------------------------------
void TrackerVariable::respondToGet(unsigned int& fromID,
                                   protocol::QueryValueData& queryData)
   {
   if (queryData.ID == nameID)
      parent->sendVariable(queryData, getCurrentValue());
   }
// ------------------------------------------------------------------
// Perform a sample.
// ------------------------------------------------------------------
void TrackerVariable::doSample(void)
   {
   if (stat == countStat)
      count++;
   else if (inWindow)
      {
      protocol::Variant* variant;
      bool ok = parent->getVariable(variableID, variant);
      if (ok)
         {
         float thisValue;
         variant->unpack(thisValue);
         values.push_back(thisValue);

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
float TrackerVariable::getCurrentValue(void)
   {
   if (values.size() == 0)
      return 0.0;

   switch (stat)
      {
      case sumStat     : return accumulate(values.begin(), values.end(), 0.0);
      case averageStat : return accumulate(values.begin(), values.end(), 0.0)
                                / values.size();
      case minimumStat : return *min_element(values.begin(), values.end());
      case maximumStat : return *max_element(values.begin(), values.end());
      case countStat   : return count;
      }
   return 0.0;
   }

