//---------------------------------------------------------------------------
#include <general/pch.h>
#include <vcl.h>
#pragma hdrstop

#include <ComponentInterface/Component.h>
#include <ComponentInterface/ApsimVariant.h>
#include <ApsimShared/ApsimComponentData.h>
#include <ComponentInterface/MessageDataExt.h>
#include <ApsimShared/FStringExt.h>
#include <general/string_functions.h>
#include <general/stristr.h>
#include <map>

#include "PlantComponent.h"
#include "plant.h"

using namespace std;

// ------------------------------------------------------------------
// Create an instance of the Plant module
// ------------------------------------------------------------------
protocol::Component* createComponent(void)
   {
   return new PlantComponent;
   }
// ------------------------------------------------------------------
// Initialises the Plant component.
// ------------------------------------------------------------------
PlantComponent::PlantComponent()
   {
   plant = new Plant(this);	
   }
// ------------------------------------------------------------------
// Destructor
// ------------------------------------------------------------------
PlantComponent::~PlantComponent(void)
   {
   delete plant;
   }
// ------------------------------------------------------------------
// Stage 1 initialisation.
// ------------------------------------------------------------------
void PlantComponent::doInit1(const FString& sdml)
   {
   protocol::Component::doInit1(sdml);
   plant->doRegistrations();
   }

// ------------------------------------------------------------------
// Stage 2 initialisation.
// ------------------------------------------------------------------
void PlantComponent::doInit2(void)
   {
   protocol::Component::doInit2();
   plant->doInit();
   }

// ------------------------------------------------------------------
// Look for a messages we have an interest in.
// ------------------------------------------------------------------
void PlantComponent::respondToEvent(unsigned int& /*fromID*/, unsigned int& eventID, protocol::Variant& variant)
   {
   plant->doEvent(eventID, variant);
   }
// ------------------------------------------------------------------
// Return a variable to caller. 
// ------------------------------------------------------------------
void PlantComponent::respondToGet(unsigned int& /*fromID*/, protocol::QueryValueData& queryData)
   {
   plant->getVariable(queryData.ID, queryData);
   }
// ------------------------------------------------------------------
// Set the value of a variable for the specified
// variable name.  If this module owns the variable and does
// change it's value, return true.
// ------------------------------------------------------------------
bool PlantComponent::respondToSet(unsigned int& /*fromID*/, protocol::QuerySetValueData& setValueData)
   {
   return (plant->setVariable(setValueData.ID, setValueData));
   }

