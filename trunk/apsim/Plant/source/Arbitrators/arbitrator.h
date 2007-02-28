#ifndef ARBITRATOR_H
#define ARBITRATOR_H

#include "PlantInterface.h"

class Arbitrator : public plantThing
   {
  protected:
   plantInterface *plant;                 // The plant we are attached to
  public:
   Arbitrator(plantInterface *p) {plant = p;};
   virtual ~Arbitrator(void) {};

   virtual void readSpeciesParameters (protocol::Component *, vector<string> &) {};
   virtual void partitionDM(float,plantPart *,plantLeafPart *,plantPart *,plantPart *) = 0;

   // Unused "thingy" things..
   virtual void undoRegistrations(protocol::Component *) {};
   virtual void doRegistrations(protocol::Component *) {};
   virtual void onPlantEvent(const string &) {};
   virtual void readConstants (protocol::Component *, const string &) {};
   virtual void readCultivarParameters (protocol::Component *, const string &) {};
   virtual void update(void) {};
   virtual float dltDMWhole(float dlt_dm) {return 0.0;};
   virtual void zeroDeltas(void) {};
   virtual void zeroAllGlobals(void) {};
   };


class cerealArbitrator : public Arbitrator
   {
  private:
   float x_stage_no_partition[max_table];
   float y_frac_leaf[max_table];                     // fraction of remaining dm allocated to leaves
   float y_ratio_root_shoot[max_table];              // root:shoot ratio of new dm ()
   int   num_stage_no_partition;

  public:
   cerealArbitrator(plantInterface *p) : Arbitrator(p) {};
   ~cerealArbitrator(void) {};

   virtual void readSpeciesParameters (protocol::Component *, vector<string> &);
   virtual void partitionDM(float,plantPart *,plantLeafPart *,plantPart *,plantPart *);
   virtual void zeroAllGlobals(void) ;
   virtual float dltDMWhole(float dlt_dm);
   };


class allometricArbitrator : public Arbitrator
   {
  private:
   interpolationFunction ratio_stem_leaf;               // stem:leaf ratio per stage
   interpolationFunction ratio_root_shoot;              // root:shoot ratio of new dm per stage ()
   interpolationFunction SLAmaxFn;
   float SLAmin;                                        // mm^2/g
   float SLAcalc;                                       // SLA today (mm^2/g)

  public:
   allometricArbitrator(plantInterface *p) : Arbitrator(p) {};
   ~allometricArbitrator(void) {};

   virtual void doRegistrations(protocol::Component *);
   virtual void undoRegistrations(protocol::Component *);
   virtual void readSpeciesParameters (protocol::Component *, vector<string> &);
   virtual void partitionDM(float,plantPart *,plantLeafPart *,plantPart *,plantPart *);
   virtual void zeroAllGlobals(void) ;
   virtual float dltDMWhole(float dlt_dm);
   };


Arbitrator* constructArbitrator(plantInterface *, const string &type);

#endif

