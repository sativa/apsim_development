#ifndef ALLOMETRICARBITRATOR_H
#define ALLOMETRICARBITRATOR_H

#include "PlantInterface.h"
#include "arbitrator.h"

class allometricArbitrator : public Arbitrator
   {
  private:
   interpolationFunction ratio_stem_leaf;               // stem:leaf ratio per stage
   interpolationFunction ratio_root_shoot;              // root:shoot ratio of new dm per stage ()
   interpolationFunction SLAmaxFn;
   float SLAmin;                                        // mm^2/g
   float SLAcalc;                                       // SLA today (mm^2/g)

  public:
   allometricArbitrator(ScienceAPI& scienceAPI, plantInterface *p)
      : Arbitrator(scienceAPI, p) {};
   ~allometricArbitrator(void) {};

   virtual void onInit1(protocol::Component *);
   virtual void undoRegistrations(protocol::Component *);
   virtual void readSpeciesParameters (protocol::Component *, vector<string> &);
   virtual void partitionDM(float,vector <plantPart *>& Parts, string FruitName);
   virtual void zeroAllGlobals(void) ;
   virtual float dltDMWhole(float dlt_dm);
   };


#endif

