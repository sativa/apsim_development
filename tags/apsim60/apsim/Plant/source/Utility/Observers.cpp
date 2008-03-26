#include <stdio.h>
#include <math.h>
#include <stdexcept>
#include <string>
#include "PlantPart.h"

#include "Plant.h"
#include "Observers.h"

// An "event observer". Waits for events to happen and records date/das for later reporting.
eventObserver::eventObserver(ScienceAPI& scienceAPI, const string& eventOfInterest, Plant *p)
   : plantThing(scienceAPI)
   {
   myEvent = eventOfInterest;
   myDas = 0;
   myDate = 0;
   dasCounter = -1;
   myPlant = p;
   }

// Register our variables.
void eventObserver::onInit1(protocol::Component *s)
   {
   varName1 = myEvent + "_das";
   desc1 = "Days from sowing to " + myEvent;
   s->addGettableVar(varName1.c_str(), myDas,
                     "days", desc1.c_str());

   varName2 = myEvent + "_date";
   desc2 = "Day number of " + myEvent;
   s->addGettableVar(varName2.c_str(), myDate,
                     "doy", desc2.c_str());

   setupEvent(s, "tick", RegistrationType::respondToEvent, &eventObserver::onTick, DDML(protocol::TimeType()).c_str());
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
