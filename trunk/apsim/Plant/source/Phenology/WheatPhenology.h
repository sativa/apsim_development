#ifndef WHEATPHENOLOGY_H
#define WHEATPHENOLOGY_H

#include "CropPhenology.h"

class WheatPhenology : public PlantPhenology {
 private:
   // State variables

   float vern_eff;
   float photop_eff;

       float cumvd;
       float dlt_cumvd;

   //parameters
   float phyllochron, startgf_to_mat, vern_sens, photop_sens;

   float wheat_photoperiod_effect(float photoperiod ,float p_photop_sen);
   float wheat_vernaliz_days(float g_maxt, float g_mint, float crownt, float g_snow, float cumvd);
   float wheat_vernaliz_effect(float p_vern_sens, float cumvd, float dlt_cumvd, float reqvd);
   float crown_temp_nwheat (float maxt, float mint, float snow);

   void vernalisation (const Environment &);
   void setupTTTargets(void);

   void get_zadok_stage(protocol::Component *system, protocol::QueryValueData &qd);

 public:
   WheatPhenology(ScienceAPI& scienceAPI, plantInterface *p)
      : PlantPhenology(scienceAPI, p) {};

   void readConstants (protocol::Component *, const string &);              // read structure etc from constants
   void onInit1(protocol::Component *);
   void readCultivarParameters (protocol::Component *, const string &);  // read cv parameters from sowing line
   void writeCultivarInfo (protocol::Component *);

   void process(const Environment &e, const pheno_stress_t &ps, float fasw_seed, float pesw_seed);

   void onRemoveBiomass(float removeBiomPheno);

   float get_dlt_tt(void) const {return dlt_tt;};                          // XX remove when leaves are finished

   void zeroAllGlobals(void);
   virtual void zeroDeltas(void);
};
#endif

