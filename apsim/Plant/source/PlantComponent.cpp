//---------------------------------------------------------------------------
#include <general/pch.h>
#include <vcl.h>
#include <boost/function.hpp>
#pragma hdrstop

#include <ComponentInterface/Component.h>
#include <ComponentInterface/ApsimVariant.h>
#include <ApsimShared/ApsimComponentData.h>
#include <ComponentInterface/Type.h>
#include <ComponentInterface/Messages.h>
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
//  Short description:
//     Return a blank string when requested to indicate that we
//     don't need a wrapper DLL.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" _export void __stdcall wrapperDLL(char* wrapperDll)
   {
   strcpy(wrapperDll, "");
   }
extern "C" void __stdcall getDescriptionInternal(char* initScript,
                                                 char* description);
// ------------------------------------------------------------------
// Return component description info.
// ------------------------------------------------------------------
extern "C" _export void __stdcall getDescription(char* initScript, char* description)
   {
   getDescriptionInternal(initScript, description);
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
   plant->doInit1(this);
   plant->doRegistrations(this);
   }

// ------------------------------------------------------------------
// Stage 2 initialisation.
// ------------------------------------------------------------------
void PlantComponent::doInit2(void)
   {
   protocol::Component::doInit2();
   plant->initialise();
   }

void PlantComponent::onApsimGetQuery(protocol::ApsimGetQueryData& apsimGetQueryData)
   {
   plant->onApsimGetQuery(apsimGetQueryData);
   }

// ------------------------------------------------------------------
// Set the value of a variable for the specified
// variable name.  If this module owns the variable and does
// change it's value, return true.
// ------------------------------------------------------------------
bool PlantComponent::respondToSet(unsigned int& fromID, protocol::QuerySetValueData& setValueData)
   {
   return (plant->setVariable(/*fromID, */setValueData.ID, setValueData));
   }

void PlantComponent::warningError (const char *msg)
{
  error(msg, false);
}

void PlantComponent::writeString (const char *line)
{
  protocol::Component::writeString(FString(line));
}
