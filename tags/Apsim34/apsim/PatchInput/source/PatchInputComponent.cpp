#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "PatchInputComponent.h"
#include <general\date_class.h>
#include <general\string_functions.h>
#include <general\stristr.h>
#include <ComponentInterface\datatypes.h>
#include <ApsimShared\ApsimDataFile.h>

using namespace std;
using namespace boost::gregorian;

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

   preNewmetID = addRegistration(protocol::respondToEventReg, "preNewmet", newmetTypeDDML);
   dayI = find(data.fieldsBegin(), data.fieldsEnd(), "day");
   yearI = find(data.fieldsBegin(), data.fieldsEnd(), "year");
   if (dayI == data.fieldsEnd())
      error("Must have a day column and an optional year column in patch input data file", true);
   }
// ------------------------------------------------------------------
// Return the file date.
// ------------------------------------------------------------------
date PatchInputComponent::getFileDate(void)
   {
   date_duration days(atoi(dayI->values[0].c_str())-1);
   if (yearI != data.fieldsEnd())
      return date(atoi(yearI->values[0].c_str()), 1, 1) + days;
   else
      return date(todaysDate.year(), 1, 1) + days;
   }

// ------------------------------------------------------------------
// Advance the file to todays date. Returns the date the file is
// positioned at.
// ------------------------------------------------------------------
date PatchInputComponent::advanceToTodaysPatchData(void)
   {
   while (getFileDate() < todaysDate && !data.eof())
      data.next();
   return getFileDate();
   }
// ------------------------------------------------------------------
// Event handler.
// ------------------------------------------------------------------
void PatchInputComponent::respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
   {
   if (eventID == preNewmetID)
      {
      protocol::newmetType newmet;
      variant.unpack(newmet);
      todaysDate = newmet.today;

      // rewind the file if the patch data files doesn't have year in it.
      // ie we patch all years.
      if (findVariable("year") == variables.end() && todaysDate.day() == 1)
         data.first();

      fileDate = advanceToTodaysPatchData();
      if (fileDate == todaysDate)
         {
         for (Variables::iterator v = variables.begin();
                                  v != variables.end();
                                  v++)
            {
            StringVariant* var = &(v->second);
            if (stristr(var->getName().c_str(), "day") == NULL &&
                stristr(var->getName().c_str(), "month") == NULL &&
                stristr(var->getName().c_str(), "year") == NULL &&
                stristr(var->getName().c_str(), "allow_sparse_data") == NULL)
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

               unsigned variableID = addRegistration(protocol::setVariableReg,
                                                     foreignName.c_str(),
                                                     DTsingleString,
                                                     "",
                                                     IntToStr(fromID).c_str());
               setVariable(variableID, var->asFloat());
               }
            }
         }
      }
   else if (eventID != tickID)  // stop the tick event going to base class.
      InputComponent::respondToEvent(fromID, eventID, variant);
   }

