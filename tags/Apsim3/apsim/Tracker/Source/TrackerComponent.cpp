#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "TrackerComponent.h"
#include <ApsimShared\ApsimComponentData.h>

#pragma package(smart_init)
using namespace std;

// ------------------------------------------------------------------
// Create an instance of the TRACKER module
// ------------------------------------------------------------------
protocol::Component* createComponent(void)
   {
   return new TrackerComponent;
   }
// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
TrackerComponent::TrackerComponent(void)
   {
   }
// ------------------------------------------------------------------
// Destructor
// ------------------------------------------------------------------
TrackerComponent::~TrackerComponent(void)
   {
   }
// ------------------------------------------------------------------
// Init1 phase.
// ------------------------------------------------------------------
void TrackerComponent::doInit1(const FString& sdml)
   {
   protocol::Component::doInit1(sdml);
   }
// ------------------------------------------------------------------
// Init2 phase.
// ------------------------------------------------------------------
void TrackerComponent::doInit2(void)
   {
   vector<string> vars;
   componentData->getVariables(vars);

   // only keep the values of variables that have a name of trackervariable
   writeString("Tracker variables:");
   for (unsigned i = 0; i != vars.size(); i++)
      {
      try
         {
         string msg = "   " + vars[i];
         writeString(msg.c_str());
         variables.push_back(TrackerVariable(this, vars[i]));
         }
      catch (const exception& err)
         {
         error(err.what(), true);
         }
      }

   for (unsigned i = 0; i != variables.size(); i++)
      variables[i].doRegistrations();
   }
// ------------------------------------------------------------------
// Event handler.
// ------------------------------------------------------------------
void TrackerComponent::respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
   {
   for (unsigned i = 0; i != variables.size(); i++)
      variables[i].respondToEvent(fromID, eventID);
   }
// ------------------------------------------------------------------
// return a variable to caller.  Return true if we own variable.
// ------------------------------------------------------------------
void TrackerComponent::respondToGet(unsigned int& fromID,
                                     protocol::QueryValueData& queryData)
   {
   for (unsigned i = 0; i != variables.size(); i++)
      variables[i].respondToGet(fromID, queryData);
   }

