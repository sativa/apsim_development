// notes 21/2/2005 PdeV
///[tt,days]In(Current)Phase: private
///phenology to provide a lookup service for variables vs stage/phases
///delete previousstageno, name and replace with event
///delete stageCode, stageName, dtt, getDAS
///define phases between any two stages


#ifndef PLANTPHENOLOGY_H
#define PLANTPHENOLOGY_H

#include <algorithm>
#include <vector>
#include <componentinterface\component.h>
#include "plantinterface.h"

#include "CompositePhase.h"
#include "Environment.h"
#include "PlantLibrary.h"

typedef struct {
	float swdef;
	float nfact;
	float swdef_flower;
	float swdef_grainfill;
	float remove_biom_pheno;
} pheno_stress_t ;



class protocol::Component;
class PlantComponent;

// Terminology:
// A "stage" is a point in time.
// A "phase" is the period between two stages.
// A "composite phase" is a group of one or more phases.

// An abstract phenology class.
class PlantPhenology : public plantThing {
 private:
 protected:
   // The plant to talk to for "plant" things
   plantInterface *plant;

   // The system to talk to for "system" things - (XX should be a protocol::Component??)
   PlantComponent *parentPlant;

   // State variables
   std::vector<pPhase>     phases;                        // The list of phases that this plant goes through

   typedef std::map<string, compositePhase> string2composite;
   string2composite   composites;                    // Composite phases we know about

   float previousStage, currentStage, dltStage;
   int   day_of_year;                                // Todays julian daynumber
   int   flowering_das, maturity_das;
   // Parameters
   std::vector<string>   iniSectionList;                  // list of sections to search in ini file
   bool initialOnBiomassRemove;

   float twilight;                                   // twilight in angular distance between
                                                     // sunset and end of twilight - altitude
                                                     // of sun. (deg)

   float phase_fraction(float dlt_tt);               // return the fraction through the current phase we are in
   pPhase *getStage(const string &);

   void get_stage(protocol::Component *, protocol::QueryValueData &);
   void get_stage_name(protocol::Component *, protocol::QueryValueData &);
   void get_stage_code(protocol::Component *, protocol::QueryValueData &);
   void get_phase_tt(protocol::Component *, protocol::QueryValueData &);
   void get_tt_tot(protocol::Component *, protocol::QueryValueData &);
   void get_days_tot(protocol::Component *, protocol::QueryValueData &);


 public:
   PlantPhenology(PlantComponent *s, plantInterface *p);
   virtual void writeCultivarInfo (PlantComponent *)=0;
   virtual void readConstants (protocol::Component *, const string &);                // read structure etc from constants
   virtual void doRegistrations (protocol::Component *);
   virtual void readSpeciesParameters (protocol::Component *, std::vector<string> &);   // read species parameters
   virtual void readCultivarParameters (protocol::Component *, const string &) {}; // read cv parameters from sowing line

   virtual void prepare(const environment_t &sw);
   virtual void process(const environment_t &, const pheno_stress_t &) = 0;
   virtual void update(void);

   bool on_day_of(const string &);

   bool inPhase(const string &);

   int   daysInPhase(const string &phaseName); //XX should be private
   int   daysInCurrentPhase(void);             //XX should be private
   float ttInPhase(const string &phaseName);   //XX should be private
   float ttInCurrentPhase(void);               //XX should be private

   float stageNumber(void) {return currentStage;};//XXX a bad thing

   string stageName(void);//xxxbad
   string previousStageName(void);//xxxbad
   string stageName(int);//xxxbad
   float  stageCode (void);//xxxbad

   virtual void onSow(unsigned &, unsigned &, protocol::Variant &v){};
   virtual void onEndCrop(unsigned &, unsigned &, protocol::Variant &v){};
   virtual void onHarvest(unsigned &, unsigned &, protocol::Variant &v){};
   virtual void onKillStem(unsigned &, unsigned &, protocol::Variant &v){};
   virtual void onRemoveBiomass(float removeBiomPheno){}; // XX arg should be protocol::Variant &v

   virtual float get_dlt_tt(void) = 0;                          // XX remove when leaf number development is finished
   void onPlantEvent(const string &) {};

   virtual void zeroAllGlobals(void);
   virtual void zeroDeltas(void);
};




class LegumeCohortPhenology : public PlantPhenology {
 private:
 public:
  void init(void);
};

class SorghumPhenology : public PlantPhenology {
 public:
  void init(void);
};

class TreePhenology : public PlantPhenology {
 public:
  void init(void);
};



#endif

