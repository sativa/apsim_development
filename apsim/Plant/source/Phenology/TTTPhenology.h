#ifndef TTTPhenologyH
#define TTTPhenologyH

#include "CropPhenology.h"

class TTTPhenology : public CropPhenology {
 protected:
   // states

   float photoperiod;                                // is really day length..
       float cumvd;
       float dlt_cumvd;
   //parameters


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

   // private members
   virtual void setupTTTargets(void);
   virtual void updateTTTargets(const environment_t &e);

 public:
   TTTPhenology(plantInterface *p) : CropPhenology(p) {};
   void prepare(const environment_t &e);
   void process(const environment_t &e, const pheno_stress_t &ps, float fasw_seed, float pesw_seed);
   void doRegistrations (protocol::Component *);
   void readConstants (protocol::Component *, const string &);              // read structure etc from constants
   void readSpeciesParameters (protocol::Component *, std::vector<string> &); // read species parameters
   void readCultivarParameters (protocol::Component *, const string &);  // read cv parameters from sowing line
   void writeCultivarInfo (PlantComponent *);


   void onRemoveBiomass(float removeBiomPheno);
   virtual float TT(const environment_t &e);
   float get_dlt_tt(void) {return dlt_tt;};                          // XX remove when leaves are finished

   void zeroAllGlobals(void);
   virtual void zeroDeltas(void);

};

#endif

