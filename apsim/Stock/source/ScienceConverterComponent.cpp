#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ScienceConverterComponent.h"
#include <ApsimShared\ApsimComponentData.h>
#include <ComponentInterface\DataTypes.h>

#pragma package(smart_init)
using namespace std;
// ------------------------------------------------------------------
// Return a blank string when requested to indicate that we
// don't need a wrapper DLL.
// ------------------------------------------------------------------
extern "C" _export void __stdcall wrapperDLL(char* wrapperDll)
   {
   strcpy(wrapperDll, "");
   }

// ------------------------------------------------------------------
// Create an instance of the science converter module
// ------------------------------------------------------------------
protocol::Component* createComponent(void)
   {
   return new ScienceConverterComponent;
   }
// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
ScienceConverterComponent::ScienceConverterComponent(void)
   {
   }
// ------------------------------------------------------------------
// Destructor
// ------------------------------------------------------------------
ScienceConverterComponent::~ScienceConverterComponent(void)
   {
   }
// ------------------------------------------------------------------
// Init1 phase.
// ------------------------------------------------------------------
void ScienceConverterComponent::doInit1(const FString& sdml)
   {
   protocol::Component::doInit1(sdml);

   day_lengthID = addRegistration(RegistrationType::get, "day_length", singleTypeDDML);
   dayLengthID = addRegistration(RegistrationType::respondToGet, "dayLength", singleTypeDDML);
   plant2stockID = addRegistration(RegistrationType::respondToGet, "plant2stock", plant2stockTypeDDML);
   removeHerbageID = addRegistration(RegistrationType::respondToEvent, "remove_herbage", remove_herbageTypeDDML);
   }
// ------------------------------------------------------------------
// Init2 phase.
// ------------------------------------------------------------------
void ScienceConverterComponent::doInit2(void)
   {
   }
// ------------------------------------------------------------------
// Event handler.
// ------------------------------------------------------------------
void ScienceConverterComponent::respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
   {
   if (eventID == removeHerbageID)
      {
      protocol::remove_herbageType herbage;
      variant.unpack(herbage);

      }
   }
// ------------------------------------------------------------------
// return a variable to caller.  Return true if we own variable.
// ------------------------------------------------------------------
void ScienceConverterComponent::respondToGet(unsigned int& fromID,
                                             protocol::QueryValueData& queryData)
   {
   if (queryData.ID == dayLengthID)
      {
      protocol::Variant* variant;
      bool ok = getVariable(day_lengthID, variant, true);
      if (ok)
         {
         float dayLength;
         bool ok = variant->unpack(dayLength);
         sendVariable(queryData, dayLength);
         }
      }
   else if (queryData.ID == plant2stockID)
      {
      protocol::plant2stockType feed;
      protocol::herbageType herbage;
      herbage.dm = 4000;        // kg/ha
      herbage.dmd = 0.6;        // kg/ha
      herbage.cp_conc = 0.2;    // kg/ha
      herbage.p_conc = 0.003;   // kg/ha
      herbage.s_conc = 0.0023;  // kg/ha
      herbage.prot_dg = 0.7;    // kg/ha
      herbage.ash_alk = 0.0;    // ??
      herbage.height_ratio = 0.0006;
      feed.herbage.push_back(herbage);

      feed.propn_green = 0.9;
      feed.legume = 1.0;
      feed.select_factor = 0.0; // ??
      sendVariable(queryData, feed);
      }
   }

