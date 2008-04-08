#ifndef GenericPhenologyH
#define GenericPhenologyH

#include "Phenology.h"

class GenericPhenology : public Phenology
   {
   protected:
      float photoperiod;
      virtual void setupTTTargets(void);

   public:
      GenericPhenology(ScienceAPI& scienceAPI, plantInterface *p)
         : Phenology(scienceAPI, p) {};
      void prepare();
      void process(const pheno_stress_t &ps, float fasw_seed, float pesw_seed);
      void readConstants (protocol::Component *, const string &);
      void readSpeciesParameters (protocol::Component *, std::vector<string> &);
      void readCultivarParameters (protocol::Component *, const string &);
      void onSow(unsigned &, unsigned &, protocol::Variant &v);
      void writeCultivarInfo (protocol::Component *);
      void onRemoveBiomass(float removeBiomPheno);
      virtual float TT();
      float get_dlt_tt(void) const {return dlt_tt;};

   };

#endif

