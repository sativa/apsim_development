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
#include <math>
#include <map>

#include "PlantLibrary.h"
#include "PlantComponent.h"
#include "Plant.h"

using namespace std;

// ------------------------------------------------------------------
// DLL entry point
// ------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE /*hinst*/, unsigned long /*reason*/, void*)
   {
   return 1;
   }

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

void PlantComponent::respondToMethod(unsigned int& /*fromID*/, unsigned int& eventID, protocol::Variant& variant)
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


// Register a variable to the rest of the system
// returns an identifier for later use..
unsigned int PlantComponent::addGettableVar(const char *systemName,
                                            protocol::DataTypeCode myType,
                                            bool isArray,
                                            const char *units)
   {
      char buffer[200];
      // Build the xml fragment that describes this variable
      strcpy(buffer, "<type kind=\"");
      switch (myType)
        {
      	case protocol::DTint4:   {strcat(buffer, "integer4"); break;}
      	case protocol::DTsingle: {strcat(buffer, "single"); break;}
      	case protocol::DTboolean:{strcat(buffer, "boolean"); break;}
      	case protocol::DTstring: {strcat(buffer, "string"); break;}
      	default: {error("Undefined gettable var type", 1);}
        }
      strcat(buffer, "\" array=\"");
      switch (isArray)
         {
   	 case false: {strcat(buffer, "F"); break;}
   	 case true:  {strcat(buffer, "T"); break;}
         }
      strcat(buffer, "\" units=\"(");
      strcat(buffer, units);
      strcat(buffer, ")\"/>");
      return (this->addRegistration(protocol::respondToGetReg, systemName, buffer));
   }

