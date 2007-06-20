#ifndef ARBITRATOR_H
#define ARBITRATOR_H

#include "PlantInterface.h"

class Arbitrator : public plantThing
   {
  protected:
      plantInterface *plant;                 // The plant we are attached to

      plantPart* FindPart(vector <plantPart *>& Parts, string name);
  public:
   Arbitrator(ScienceAPI& scienceAPI, plantInterface *p)
      : plantThing(scienceAPI) {plant = p;};
   virtual ~Arbitrator(void) {};

   virtual void readSpeciesParameters (protocol::Component *, vector<string> &) {};
   virtual void partitionDM(float,vector <plantPart *>& Parts, string FruitName) = 0;

   // Unused "thingy" things..
   virtual void undoRegistrations(protocol::Component *) {};
   virtual void onInit1(protocol::Component *) {};
   virtual void onPlantEvent(const string &) {};
   virtual void readConstants (protocol::Component *, const string &) {};
   virtual void readCultivarParameters (protocol::Component *, const string &) {};
   virtual void update(void) {};
   virtual float dltDMWhole(float dlt_dm) {return 0.0;};
   virtual void zeroDeltas(void) {};
   virtual void zeroAllGlobals(void) {};
   };

Arbitrator* constructArbitrator(ScienceAPI& scienceAPI, plantInterface *, const string &type);

#endif

