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
      GDate today;
      today.Set(todaysDate);

      // if we don't have year data and today is 1st jan then rewind the file.
      if (yearI == NULL && today.Get_day_of_year() == 1)
         data->first();

      if (advanceToTodaysData())
         {
         for (TemporalVariables::iterator t = temporalVariables.begin();
                                          t != temporalVariables.end();
                                          t++)
            {
            StringVariant* var = *t;
            if (stristr(var->getName().c_str(), "day") == NULL &&
                stristr(var->getName().c_str(), "month") == NULL &&
                stristr(var->getName().c_str(), "year") == NULL)
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
               float value;
               if (getVariableValue(var->getName(), value))
                  setVariable(variableID, value);
               }
            }
         }
      }
   else if (eventID != tickID)  // stop the tick event going to base class.
      InputComponent::respondToEvent(fromID, eventID, variant);
   }

