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
   Biomass(OOPlant *p);                 // plant
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

   void getDMGreen(protocol::Component *system, protocol::QueryValueData &qd);
   void getDMSenesced(protocol::Component *system, protocol::QueryValueData &qd);
   void getDMDead(protocol::Component *system, protocol::QueryValueData &qd);
   void getDltDMGreen(protocol::Component *system, protocol::QueryValueData &qd);
   void getDltDMDetached(protocol::Component *system, protocol::QueryValueData &qd);
   void getDltDMDeadDetached(protocol::Component *system, protocol::QueryValueData &qd);
   void getDltDMGreenRetrans(protocol::Component *system, protocol::QueryValueData &qd);
   void getBiomass(protocol::Component *system, protocol::QueryValueData &qd);

   void Update(void) {updateVars();}
   void Harvest(void) {initialize();}


   void Summary(void);
};

#endif
