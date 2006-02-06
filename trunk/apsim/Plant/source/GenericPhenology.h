#ifndef GenericPhenologyH
#define GenericPhenologyH

#include "CropPhenology.h"

class MaizePhenology : public CropPhenology {
 protected:
   // states

   float photoperiod;                                // is really day length..


   //parameters






   // private members
   virtual void setupTTTargets(void);


 public:
   MaizePhenology(PlantComponent *s, plantInterface *p) : CropPhenology(s, p) {};
   void prepare(const environment_t &e);
   void process(const environment_t &e, const pheno_stress_t &ps);

   void readConstants (protocol::Component *, const string &);              // read structure etc from constants
   void readSpeciesParameters (protocol::Component *, std::vector<string> &); // read species parameters
   void readCultivarParameters (protocol::Component *, const string &);  // read cv parameters from sowing line
   void writeCultivarInfo (PlantComponent *);


   void onRemoveBiomass(float removeBiomPheno);
   virtual float TT(const environment_t &e);
   float get_dlt_tt(void) {return dlt_tt;};                          // XX remove when leaves are finished

   void zeroAllGlobals(void);

};

#endif