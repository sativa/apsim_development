//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TrackerVariable.h"
#include <ComponentInterface\Component.h>
#include <general\stringtokenizer.h>
#include <general\string_functions.h>
#include <numeric>
#include <ApsimShared\FStringExt.h>
#include <ComponentInterface\MessageDataExt.h>

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
   if (on == "")
      throw runtime_error("Expected an 'on' keyword specifying the sampling event.");
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

   // assume the start of simulation is the beginning of the sampling window
   inWindow = true;
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
   static const char* nullDDML = "<type/>";
   static const char* singleDDML = "<type kind=\"single\"/>";
   static const char* singleArrayDDML = "<type kind=\"single\" array=\"T\"/>";
   string typeString = singleArrayDDML;

   if (stat == countStat)
      variableID = parent->addRegistration(protocol::respondToEventReg,
                                           variable.c_str(),
                                           nullDDML);
   else
      {
      variableID = parent->addRegistration(protocol::getVariableReg,
                                           variable.c_str(),
                                           singleArrayDDML);
      protocol::Variant* variant;
      bool ok = parent->getVariable(variableID, variant, true);
      if (ok)
         {
         protocol::Type t = variant->getType();
         typeString = "<type kind=\"single\" units=\"" + asString(t.getUnits())
                    + "\" array=\"T\"/>";

         parent->setRegistrationType(variableID, typeString.c_str());
         }
      }


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
                                    singleArrayDDML);
   parent->setRegistrationType(nameID, typeString.c_str());

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
      {
      vector<float> values;
      getCurrentValues(values);
      parent->sendVariable(queryData, values);
      }
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
         vector<float> theseValues;
         variant->unpack(theseValues);
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
   if (values.size() > 0)
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
               }
            }
         if (stat == averageStat && values.size() > 0)
            value /= values.size();
         currentValues.push_back(value);
         }
      }
   }

