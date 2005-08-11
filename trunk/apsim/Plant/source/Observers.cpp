#include <stdio.h>
#include <string>
#include <stdexcept>
#include <ComponentInterface/Component.h>
#include <ComponentInterface/ApsimVariant.h>

#include "PlantComponent.h"
#include "PlantLibrary.h"
//#include "Environment.h"
#include "Plant.h"
#include "Observers.h"

// An "event observer". Waits for events to happen and records date/das for later reporting.
eventObserver::eventObserver(const string& eventOfInterest, Plant *p)
   {
   myEvent = eventOfInterest;
   myDas = 0;
   myDate = 0;
   dasCounter = -1;
   myPlant = p;
   }

// Register our variables.
void eventObserver::doRegistrations(protocol::Component *s)
   {
   s->addGettableVar((myEvent + "_das").c_str(), myDas, 
                     "days", ("Days from sowing to " + myEvent).c_str());

   s->addGettableVar((myEvent + "_date").c_str(), myDate, 
                     "doy", ("Day number of " + myEvent).c_str());

   setupEvent(s, "tick", RegistrationType::respondToEvent, &eventObserver::onTick);
   }

void eventObserver::onTick(unsigned &, unsigned &, protocol::Variant &)
   {
   if (dasCounter >= 0) dasCounter++;
   }

void eventObserver::onPlantEvent(const string &event)
   {
   if (event == "sowing") 
      {
      dasCounter = 1; 
      } 
   if (event == myEvent)
      {
      myDate = myPlant->getDayOfYear();
      if (dasCounter >= 0) { myDas = dasCounter; }
      }
   if (event == "end_crop") 
      { 
      myDas = 0;
      myDate = 0;
      dasCounter = -1;
      }
   }
