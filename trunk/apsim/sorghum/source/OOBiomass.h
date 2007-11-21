//---------------------------------------------------------------------------

#ifndef OOBiomassH
#define OOBiomassH

#include "OOPlantComponents.h"
#include "Utilities.h"

//------------------------------------------------------------------------------------------------

class Biomass : public PlantProcess
   {
   private:
   float effectiveRue;


   public:
// Parameters ----------------------------------------------------------
   vector<float> ratioRootShoot;
   float stem2FlowerFrac;

//  Variables  ---------------------------------------------------------
   float aboveGroundBiomass;
   float aboveGroundGreenBiomass;
   float totalBiomass;
   float stemRachisBiomass;
   float hi;
   float yield;
   float dltDMPotTE;
   float dltDMPotRUE;
   float dltDM;
   float stage;
   float dmStover;
   float biomGreen;
   float biomStover;

   vector<float> greenDM;
   vector<float> senescedDM;
   vector<float> deadDM;
   vector<float> dltDMGreen;
   vector<float> dltDMDetachedSen;
   vector<float> dltDMDetachedDead;
   vector<float> dltDMRetranslocate;

// Private Methods -------------------------------------------------------
   void   doRegistrations(void);
   void   initialize(void);
   float calcDltDMPotTE(void);
   void   calcBiomassPartitioning(void);
   void   calcBiomassRetranslocation(void);

   void   calcBiomassTE(void);        // plant
   void   calcDltBiomass(void);       // plant
   void   calcPartitioning(void);     // plant
   void   calcRetranslocation(void);  // plant
   void   dmScenescence(void);        // plant

// public Methods -------------------------------------------------------
   public:
   Biomass(ScienceAPI &, OOPlant *p);                 // plant
   ~Biomass();                        // plant

   void   readParams (string cultivar);          // plant
   void   updateVars(void);           // plant

   void   process(void);              // plant
   void   calcBiomassRUE(float rue, float radnIntercepted);       // plant


   float getTotalBiomass(void)const{return totalBiomass;}   // grain
   float getAboveGroundBiomass(void)const{return aboveGroundBiomass;}   // grain
   float getDltDM(void)const{return dltDM;}             // grain
   float getDltDMPotRUE(void)const{return dltDMPotRUE;} // plant
   float getDltDMPotTE(void)const{return dltDMPotTE;}
   float getEffectiveRue(void)const{return effectiveRue;}
   void detachment(vector<float> senDetachFrac, vector<float> deadDetachFrac);
   void incorporateResidue(void);

   void getDMGreen(float &);
   void getDMSenesced(float &);
   void getDMDead(float &);
   void getDltDMGreen(float &);
   void getDltDMDetached(vector<float> &);
   void getDltDMDeadDetached(vector<float> &);
   void getDltDMGreenRetrans(vector<float> &);
   void getBiomass(float &);

   void Update(void) {updateVars();}
   void Harvest(void) {initialize();}


   void Summary(void);
};

#endif
