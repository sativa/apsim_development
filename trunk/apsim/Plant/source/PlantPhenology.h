// notes 21/2/2005 PdeV
///[tt,days]In(Current)Phase: private 
///phenology to provide a lookup service for variables vs stage/phases
///delete previousstageno, name and replace with event
///delete stageCode, stageName, dtt, getDAS
///define phases between any two stages


#ifndef PLANTPHENOLOGY_H
#define PLANTPHENOLOGY_H

#include <algorithm>

typedef struct {
	float swdef;
	float nfact;
	float swdef_flower;
	float swdef_grainfill;
	float remove_biom_pheno;
} pheno_stress_t ;

class environment_t {
  public:
	vector<float> dlayer;
	vector<float> sw_dep;
	vector<float> ll_dep;
	vector<float> dul_dep;
   float maxt, mint, latitude;
   int day_of_year;
   int find_layer_no(float) const;
   float daylength(float) const;
   float daylength(int, float) const;
};
// Terminology:
// A "stage" is a point in time.
// A "phase" is the period between two stages.
// A "composite phase" is a group of one or more phases.

/////////////////////////////////////////////////////////////////////////////////////////////
// A phenological phase.
class pPhase
   {
   private:
     string  myName;       // Usually the name of the "stage" that the phase starts from.
     float tt,             // Thermal time spent in this phase
           target,         // Target time we want to spend here
           days;           // Number of days spent in this phase.
     bool empty;
   public:
     pPhase(const string& n) {myName = n; tt = target = days = 0.0; empty = true;};
     pPhase(const char *n) {myName = n; tt = target = days = 0.0; empty = true;};
     const string &name(void) const {return myName;};
     void  add(float dlt_days)               {days += dlt_days;};
     void  add(float dlt_days, float dlt_tt) {days += dlt_days; tt += dlt_tt;};
     void  add(float dlt_days, float dlt_tt, float *balance_days, float *balance_tt);
     void  setTarget(float value) {target = value;};
     float getTT(void) const       {return tt;};
     float getTTTarget(void) const {return target;};
     float getDays(void) const     {return days;};
     void  reset(void)             {tt = target = days = 0.0; empty = true;};
     void  update(void)            {empty = false;};
     bool  isFirstDay(void) const  {return empty == true;};
     bool  isEmpty(void) const {return empty;};
   };

bool operator == (const pPhase &a, const pPhase &b); 

// A collection of phases (eg leaf growth phases, grain filling phases)
class compositePhase {
 private:
   vector<pPhase *> phases;
 public:
   compositePhase()  {};
   void add(pPhase *p) {phases.push_back(p);}
   bool contains(const pPhase &p) const;
   bool isEmpty(void) const {return phases.size() == 0;};
   float getTT(void) const;
   float getDays(void) const;
};

// An abstract phenology class.
class PlantPhenology : public plantThing {
 private:
 protected:
   // The plant to talk to for "plant" things
   plantInterface *plant;

   // The system to talk to for "system" things - (XX should be a protocol::Component??)
   PlantComponent *parentPlant;

   // State variables
   vector<pPhase>     phases;                        // The list of phases that this plant goes through

   typedef std::map<string, compositePhase> string2composite;
   string2composite   composites;                    // Composite phases we know about

   float previousStage, currentStage, dltStage;
   int   day_of_year;                                // Todays julian daynumber
   int   flowering_das, maturity_das;
   // Parameters
   vector<string>   iniSectionList;                  // list of sections to search in ini file
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
   PlantPhenology(plantInterface *p) {plant = p;};
   virtual void writeCultivarInfo (PlantComponent *)=0;
   virtual void initialise (PlantComponent *, const string &);                // read structure etc from constants
   virtual void doRegistrations (protocol::Component *);
   virtual void readSpeciesParameters (protocol::Component *, vector<string> &);   // read species parameters
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

class WheatPhenology : public PlantPhenology {
 private:
   // State variables
   int   das;
   float cumvd;
   float vern_eff;
   float photop_eff;

   // Rates
   float dlt_cumvd;
   float dlt_tt;
   float dlt_tt_phenol;

   // Parameters
   float shoot_lag;                                  // minimum growing degree days for
                                                     // germination (deg days)
   float shoot_rate;                                 // growing deg day increase with depth
                                                     // for germination (deg day/mm depth)
   float sowing_depth;
   float pesw_germ;                                  // plant extractable soil water in
                                                     // seedling layer inadequate for
                                                     // germination (mm/mm)

   float phyllochron, startgf_to_mat, vern_sens, photop_sens;
   interpolationFunction y_tt, rel_emerg_rate;
   interpolationFunction y_removeFractPheno;
   lookupFunction stage_reduction_harvest;
   lookupFunction stage_reduction_kill_stem;

   float wheat_photoperiod_effect(float photoperiod ,float p_photop_sen);
   float wheat_vernaliz_days(float g_maxt, float g_mint, float crownt, float g_snow, float cumvd);
   float wheat_vernaliz_effect(float p_vern_sens, float cumvd, float dlt_cumvd, float reqvd);
   float crown_temp_nwheat (float maxt, float mint, float snow);

   void vernalisation (const environment_t &);
   void setupTTTargets(void);

   void get_zadok_stage(protocol::Component *system, protocol::QueryValueData &qd);

 public:
   WheatPhenology(plantInterface *p) : PlantPhenology(p) {};

   void initialise (PlantComponent *, const string &);              // read structure etc from constants
   void doRegistrations (protocol::Component *);
   void readSpeciesParameters (protocol::Component *, vector<string> &); // read species parameters
   void readCultivarParameters (protocol::Component *, const string &);  // read cv parameters from sowing line
   void writeCultivarInfo (PlantComponent *);

   void prepare(const environment_t &sw);
   void process(const environment_t &e, const pheno_stress_t &ps);
   void update(void);
   
   void onSow(unsigned &, unsigned &, protocol::Variant &v);
   void onEndCrop(unsigned &, unsigned &, protocol::Variant &v);
   void onHarvest(unsigned &, unsigned &, protocol::Variant &v);
   void onKillStem(unsigned &, unsigned &, protocol::Variant &v);
   void onRemoveBiomass(float removeBiomPheno){};

   float get_dlt_tt(void) {return dlt_tt;};                          // XX remove when leaves are finished

   void zeroAllGlobals(void);
   void zeroDeltas(void);
};

class LegumePhenology : public PlantPhenology {
 private:
   // states
   int   das;
   float photoperiod;                                // is really day length..
   float cumvd;                                      // cumulative v days

   // rates
   float dlt_tt;
   float dlt_tt_phenol;
   float dlt_cumvd;

   //parameters
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
   int   est_days_emerg_to_init;                     // estimated days from emergence to floral initiation
   interpolationFunction vernal_days;                // relate temperature to vernalisation

   float tt_emerg_to_endjuv_ub;                      // upper bounds
   float tt_maturity_to_ripe_ub;                     // upper bounds

   interpolationFunction tt_emerg_to_endjuv;         // Growing degree days to complete
                                                     // emerg_to_endjuv stage (emergence to
                                                     // end of emerg_to_endjuv) (deg day)

   interpolationFunction tt_endjuv_to_init;
   interpolationFunction tt_init_to_flower;
   interpolationFunction tt_flower_to_start_grain;
   interpolationFunction tt_start_to_end_grain;

   float tt_flower_to_maturity;                      // Growing degree days to complete
                                                     // grainfill (silking to maturity) (deg day)
   float tt_end_grain_to_maturity;
   float tt_maturity_to_ripe;                        // growing deg day required to for grain dry down (deg day)

   lookupFunction stage_reduction_harvest;
   lookupFunction stage_reduction_kill_stem;

   // private members
   void setupTTTargets(void);
   void updateTTTargets(const environment_t &e);

 public:
   LegumePhenology(plantInterface *p) : PlantPhenology(p) {};
   void prepare(const environment_t &e);
   void process(const environment_t &e, const pheno_stress_t &ps);
   void update(void);

   void doRegistrations (protocol::Component *);
   void initialise (PlantComponent *, const string &);              // read structure etc from constants
   void readSpeciesParameters (protocol::Component *, vector<string> &); // read species parameters
   void readCultivarParameters (protocol::Component *, const string &);  // read cv parameters from sowing line
   void writeCultivarInfo (PlantComponent *);

   void onSow(unsigned &, unsigned &, protocol::Variant &v);
   void onEndCrop(unsigned &, unsigned &, protocol::Variant &v);
   void onHarvest(unsigned &, unsigned &, protocol::Variant &v);
   void onKillStem(unsigned &, unsigned &, protocol::Variant &v);
   void onRemoveBiomass(float removeBiomPheno);

   float get_dlt_tt(void) {return dlt_tt;};                          // XX remove when leaves are finished

   void zeroAllGlobals(void);
   void zeroDeltas(void);
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

// non-class functions
bool plant_germination(float pesw_germ,             //(INPUT)  plant extractable soil water required for germination
                       float sowing_depth,          //(INPUT)  depth of seed (mm)
                       const environment_t &sw);    //(INPUT)  soil water structure

float linint_3hrly_temp (float tmax, float tmin, externalFunction *ttFn);
float temp_3hr (float tmax, float tmin, int period);

#endif
