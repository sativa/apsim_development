#ifndef GenericPhenologyH
#define GenericPhenologyH

#include "CropPhenology.h"

class GenericPhenology : public PlantPhenology
   {
   protected:
      float photoperiod;
      virtual void setupTTTargets(void);

   public:
      GenericPhenology(ScienceAPI& scienceAPI, plantInterface *p)
         : PlantPhenology(scienceAPI, p) {};
      void prepare(const environment_t &e);
      void process(const environment_t &e, const pheno_stress_t &ps, float fasw_seed, float pesw_seed);
      void readConstants (protocol::Component *, const string &);
      void readSpeciesParameters (protocol::Component *, std::vector<string> &);
      void readCultivarParameters (protocol::Component *, const string &);
      void onSow(unsigned &, unsigned &, protocol::Variant &v);
      void writeCultivarInfo (PlantComponent *);
      void onRemoveBiomass(float removeBiomPheno);
      virtual float TT(const environment_t &e);
      float get_dlt_tt(void) {return dlt_tt;};

   };

#endif
