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

   ApsimDataFile::iterator i = find(data.constantsBegin(),
                                    data.constantsEnd(),
                                    "patch_all_years");
   patchAllYears = (i != data.constantsEnd() && Str_i_Eq(i->values[0], "true"));
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
   }
// ------------------------------------------------------------------
// Advance the file to todays date.
// NB: The patch data file may run over a year boundary eg. for a
//     summer crop - need to handle this situation.
// Returns the date the file is positioned at.
// ------------------------------------------------------------------
date PatchInputComponent::advanceToTodaysPatchData(void)
   {
   if (patchDates.size() == 0)
      readPatchDates();
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

