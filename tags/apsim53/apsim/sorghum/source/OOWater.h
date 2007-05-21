//------------------------------------------------------------------------------------------------

#ifndef OOWaterH
#define OOWaterH

#include "OOPlantComponents.h"
#include "Utilities.h"

//------------------------------------------------------------------------------------------------
class Water : public PlantProcess
   {
   private:

// Parameters ----------------------------------------------------------

   TableFn swPhenoTable;
   TableFn swExpansionTable;

   vector<float> dLayer;
   vector<float> dulDep;
   vector<float> satDep;
   vector<float> bd;
   vector<float> eswCap;           // extractable soil water capacity
   vector<float> ll;
   vector<float> ll15Dep;
   vector<float> kl;
   vector<float> xf;
   vector<float> llDep;

   float profileDepth;
   int    nLayers;

   unsigned int swDepID;
   unsigned int dltSwDepID;


//  Variables  -----------------------------------------------------

   int currentLayer;                   // number of the layer that the roots are in now (starts at 0)
   float lastLayerPropn;           // proportion of the currentLayer occupied
   float totalAvail;
   float totalAvailPot;
   float totalSupply;
   float AccTotalSupply;
   float dltUptake;
   float totalUptake;
   float eswTot;

   vector<float> available;
   vector<float> availablePot;
   vector<float> supply;
   vector<float> swUptake;
   vector<float> swDep;
   vector<float> esw;              // extractable soil water  (swDep[i] - llDep[i])
   vector<float> swDef;
   float ep;
   // uptake
   vector<float> dltSwDep;


   float swDemand;
   float sdRatio;
   // stresses
   float photoStress;
   float phenoStress;
   float expansionStress;

   float rootDepth;

// Private Methods -------------------------------------------------------
   void   doRegistrations(void);
   void   initialize(void);
   void   calcAvailable(void);
   float layerProportion(void);
   void   calcAvailablePot(void);
   void   calcSupply(void);
   float calcSwDefPheno(void);
   float calcSwDefPhoto(void);
   float calcSwDefExpansion(void);

   void   setOtherVariables (void);

// public Methods -------------------------------------------------------
   public:
   Water(OOPlant *p);                   // plant
   ~Water();                          // plant
   vector<float> photoStressTotal;
   vector<float> phenoStressTotal;
   ////////////////////////////////////////////////////////
    void   calcDailySupply(void);      // plant
   void   calcStresses(void);         // plant
   void   calcUptake(void);           // plant
   void   getOtherVariables (void);   // plant
   ////////////////////////////////////////////////////////

   void   readParams (string cultivar);          // plant
   void   updateVars(void);           // plant

   void   process(void);              // plant
   float calcDemand(void);           // plant
   void   doNewProfile(protocol::Variant &v);   // plantActions

   float swAvailRatio(int currentLayer);   // Roots

   float calcPeswSeed(void);                               // Phenology
   float phenologyStress(void){return phenoStress;}        // Phenology
   float photosynthesisStress(void){return photoStress;}   // grain
   float getExpansionStress(void){return expansionStress;} // Leaf
   float getTotalSupply(void)const{return totalSupply;}    // Biomass
   float getSdRatio(void)const{return sdRatio;}
   float swAvailFracLayer(int layer);                      // Nitrogen

   float swDepLayer(int layer){return swDep[layer];}
   float dltSwDepLayer(int layer){return dltSwDep[layer];}

   //Registration functions
   void getEswLayers(protocol::Component *system, protocol::QueryValueData &qd);
   void getSwDefLayers(protocol::Component *system, protocol::QueryValueData &qd);
   void getSwUptakeLayers(protocol::Component *system, protocol::QueryValueData &qd);
   void getEpLayers(protocol::Component *system, protocol::QueryValueData &qd);

   float getESW(void){return sumVector(esw);}
   float getESWAvail(void)const{return totalAvail;}
   };


//------------------------------------------------------------------------------------------------
#endif
