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
//#include <ComponentInterface/Component.h>
#include "CompositePhase.h"

class pheno_stress_t {
   public:
    float swdef;
    float nfact;
    float swdef_flower;
    float swdef_grainfill;
    float remove_biom_pheno;
};

class protocol::Component;
class plantInterface;
class Environment;

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

   // State variables
   std::vector<pPhase*>     phases;                        // The list of phases that this plant goes through

   typedef std::map<string, compositePhase> string2composite;
   mutable string2composite   composites;                    // Composite phases we know about

   float previousStage, currentStage, dltStage;
   int   day_of_year;                                // Todays julian daynumber

   // Parameters
   std::vector<string>   iniSectionList;                  // list of sections to search in ini file
   bool initialOnBiomassRemove;

       virtual void setupTTTargets()=0;

       int   das;
       // Rates
   float dlt_tt;
   float dlt_tt_phenol;

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
   pPhase* find(const string& phase_name) const;

       // Parameters
       float shoot_lag;                                  // minimum growing degree days for
                                                         // germination (deg days)
       float shoot_rate;                                 // growing deg day increase with depth
                                                         // for germination (deg day/mm depth)
       float sowing_depth;
       float pesw_germ;                                  // plant extractable soil water in
                                                         // seedling layer inadequate for
                                                         // germination (mm/mm)
       interpolationFunction y_tt, rel_emerg_rate;
       interpolationFunction y_removeFractPheno;
       lookupFunction stage_reduction_harvest;
       lookupFunction stage_reduction_kill_stem;


 public:
   PlantPhenology(ScienceAPI& scienceAPI, plantInterface *p);
   virtual void writeCultivarInfo (protocol::Component *)=0;
   virtual void readConstants (protocol::Component *, const string &);                // read structure etc from constants
   virtual void onInit1(protocol::Component *);
   virtual void readSpeciesParameters (protocol::Component *, std::vector<string> &);   // read species parameters
   virtual void readCultivarParameters (protocol::Component *, const string &) {}; // read cv parameters from sowing line

   virtual void prepare(const Environment &sw);
   virtual void process(const Environment &, const pheno_stress_t &, float fasw_seed, float pesw_seed) = 0;
   virtual void update(void);

   bool on_day_of(const string &);

   bool inPhase(const string &) const;

   int   daysInPhase(const string &phaseName); //XX should be private
   int   daysInCurrentPhase(void);             //XX should be private
   float ttInPhase(const string &phaseName) const;   //XX should be private
   float TTTargetInPhase(const string &phaseName) const;
   float ttInCurrentPhase(void);               //XX should be private

   float stageNumber(void) {return currentStage;};//XXX a bad thing

   string stageName(void);//xxxbad
   string previousStageName(void);//xxxbad
   string stageName(int);//xxxbad
   float  stageCode (void);//xxxbad

   void onSetPhase(float resetPhase);
   virtual void onSow(unsigned &, unsigned &, protocol::Variant &v);
   virtual void onEndCrop(unsigned &, unsigned &, protocol::Variant &v){zeroAllGlobals();};
   virtual void onHarvest(unsigned &, unsigned &, protocol::Variant &v);
   virtual void onKillStem(unsigned &, unsigned &, protocol::Variant &v);
   virtual void onRemoveBiomass(float removeBiomPheno){}; // XX arg should be protocol::Variant &v

   virtual float get_dlt_tt(void) const = 0;                          // XX remove when leaf number development is finished
   void onPlantEvent(const string &) {};
   virtual bool plant_germination(float pesw_germ, float sowing_depth, float pesw_seed);

   virtual void zeroAllGlobals(void);
   virtual void zeroDeltas(void);
};

PlantPhenology * constructPhenology(ScienceAPI& scienceAPI, plantInterface *plant, const string &name);

#endif

