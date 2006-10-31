#ifndef BroccoliPhenologyH
#define BroccoliPhenologyH

#include "CropPhenology.h"

class BroccoliPhenology : public CropPhenology {
 protected:
   // states

   float photoperiod;                                // is really day length..

       float cumvd;
       float dlt_cumvd;
   //parameters

   int   est_days_emerg_to_init;                     // estimated days from emergence to floral initiation
   interpolationFunction vernal_days;                // relate temperature to vernalisation

   float tt_emerg_to_endjuv_ub;                      // upper bounds

   interpolationFunction tt_emerg_to_endjuv;         // Growing degree days to complete
                                                     // emerg_to_endjuv stage (emergence to
                                                     // end of emerg_to_endjuv) (deg day)

   interpolationFunction tt_endjuv_to_init;
   float tt_init_to_buttoning;
   float tt_buttoning_to_maturity;

   // private members
   virtual void setupTTTargets(void);
   virtual void updateTTTargets(const environment_t &e);

 public:
   BroccoliPhenology(plantInterface *p) : CropPhenology(p) {};
   void prepare(const environment_t &e);
   void process(const environment_t &e, const pheno_stress_t &ps, float fasw_seed, float pesw_seed);

   void readConstants (protocol::Component *, const string &);              // read structure etc from constants
   void readSpeciesParameters (protocol::Component *, std::vector<string> &); // read species parameters
   void readCultivarParameters (protocol::Component *, const string &);  // read cv parameters from sowing line
   void writeCultivarInfo (PlantComponent *);
   void doRegistrations (protocol::Component *);


   void onRemoveBiomass(float removeBiomPheno);
   virtual float TT(const environment_t &e);
   float VernalDays(const environment_t &e);
   float get_dlt_tt(void) {return dlt_tt;};                          // XX remove when leaves are finished

   void zeroAllGlobals(void);
   void zeroDeltas(void);

};

#endif

