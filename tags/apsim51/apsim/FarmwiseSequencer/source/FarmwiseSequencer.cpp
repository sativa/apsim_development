#include <general\pch.h>
#include <vcl.h>
#include <boost/function.hpp>
#pragma hdrstop

#include <math.h>
#include <string>
#include <strstream>
#include <iomanip.h>

#include <general/string_functions.h>
#include <general/stl_functions.h>
#include <ApsimShared/FStringExt.h>
#include <ComponentInterface/MessageDataExt.h>
#include <ComponentInterface/ApsimVariant.h>

#include <ComponentInterface/Component.h>
#include <ComponentInterface/DataTypes.h>
#include "FarmwiseSequencer.h"


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
// Create an instance of the science converter module
// ------------------------------------------------------------------
protocol::Component* createComponent(void)
//===========================================================================
   {
   return new FarmwiseSequencer;
   }
// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
//===========================================================================
FarmwiseSequencer::FarmwiseSequencer(void)
   {
   }
// ------------------------------------------------------------------
// Destructor
// ------------------------------------------------------------------
FarmwiseSequencer::~FarmwiseSequencer(void)
//===========================================================================
   {
   }
// ------------------------------------------------------------------
// Init1 phase.
// ------------------------------------------------------------------
void FarmwiseSequencer::doInit1(const FString& sdml)
//===========================================================================
   {
   protocol::Component::doInit1(sdml);
   }
// ------------------------------------------------------------------
// Init2 phase.
// ------------------------------------------------------------------
void FarmwiseSequencer::doInit2(void)
//===========================================================================
{
   readConstants (); // Read constants
   for (int SubscribeEvent = 0; SubscribeEvent < numSubscribeEvents; SubscribeEvent++)
   {
      SubscribeEventsID[SubscribeEvent] = addRegistration(RegistrationType::respondToEvent, SubscribeEvents[SubscribeEvent].c_str(), nullTypeDDML);
      for (int PublishEvent = 0; PublishEvent < numPublishEvents[SubscribeEvent]; PublishEvent++)
      {
         PublishEventsID[SubscribeEvent][PublishEvent] = addRegistration(RegistrationType::event, PublishEvents[SubscribeEvent][PublishEvent].c_str(), nullTypeDDML);
      }
   }
}

// ------------------------------------------------------------------
// Event handler.
// ------------------------------------------------------------------
void FarmwiseSequencer::respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
//===========================================================================
{
   char* null = "";
   for (int SubscribeEvent = 0; SubscribeEvent < numSubscribeEvents; SubscribeEvent++)
   {
      if (eventID == SubscribeEventsID[SubscribeEvent])
         for (int PublishEvent = 0; PublishEvent < numPublishEvents[SubscribeEvent]; PublishEvent++)
            publish (PublishEventsID[SubscribeEvent][PublishEvent], null);
      else
      {} //not interested an other events
   }

}
void FarmwiseSequencer::readConstants ( void )
//===========================================================================
{
   const char*  section_name = "constants" ;

   writeString (" - reading constants");

//    cDebug = readParameter (section_name, "debug");
   vector<string> Subscribe;
    string scratch = readParameter (section_name, "subscribe_events");
    Split_string(scratch, " ", Subscribe);


//    readParameter (section_name, "subscribe_events", SubscribeEvents, numSubscribeEvents);

   numSubscribeEvents = Subscribe.size();
   for (int SubscribeEvent = 0; SubscribeEvent < numSubscribeEvents; SubscribeEvent++)
   {
      SubscribeEvents[SubscribeEvent] = Subscribe[SubscribeEvent];
//      string events[maxEvents];

      vector<string> events;
      string scratch = readParameter (section_name, Subscribe[SubscribeEvent]);
      Split_string(scratch, " ", events);

//      readParameter (section_name, SubscribeEvents[SubscribeEvent].c_str(), events, numPublishEvents);
      numPublishEvents[SubscribeEvent] = events.size();

      for (int numEvent = 0; numEvent < numPublishEvents[SubscribeEvent]; numEvent++)
         PublishEvents[SubscribeEvent][numEvent] = events[numEvent];
   }

   ostringstream msg;
   for (int SubscribeEvent = 0; SubscribeEvent < numSubscribeEvents; SubscribeEvent++)
   {
      msg << "Subscribe Event: " << SubscribeEvents[SubscribeEvent] << endl;
      msg << "      Publish Event/s: ";
      for (int numEvent = 0; numEvent < numPublishEvents[SubscribeEvent]; numEvent++)
         msg << PublishEvents[SubscribeEvent][numEvent] << " ";
      msg << endl;
   }
   msg << ends;
   writeString (msg.str().c_str());
}

