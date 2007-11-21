//------------------------------------------------------------------------------------------------

#ifndef OONitrogenH
#define OONitrogenH

#include "OOPlantComponents.h"
#include "Utilities.h"

//------------------------------------------------------------------------------------------------

class Nitrogen : public PlantProcess
   {
   private:

// Parameters ----------------------------------------------------------

   float diffnConstant;
   int nLayers;
   vector<float> dLayer;

//  Variables  ---------------------------------------------------------
   float profileDepth;

   float phenoStress;
   float expansionStress;
   float photoStress;

   float nBiomass;
   float nStover;
   float nGreenBiomass;
   float nUptakeTotal;
   float nPlant;

   // supply
   vector<float> massFlowSupply;
   vector<float> diffusionSupply;
   vector<float> fixationSupply;

   vector<float> nGreen;
   vector<float> dltNGreen;
   vector<float> dltNRetrans;
   vector<float> nSenesced;
   vector<float> nDead;
   vector<float> dltNDetached;
   vector<float> dltNDetachedDead;

   float sumDiffSupply;            // debug


   float actualMassFlow;
   float actualDiffusion;
   float actualTotal;
   float plantNDemand;    // plant demand - grain demand

   // demand
   float totalDemand;
   float supplyDemandRatio;
   float nSupply;

   vector<float> no3;
   vector<float> no3Min;
   vector<float> dltNo3;

   int currentLayer;                   // number of the layer that the roots are in now (starts at 0)
   float rootDepth;


// Private Methods -------------------------------------------------------
   void doRegistrations(void);
   void initialize(void);
   void calcMassFlow(void);
   void calcDiffusion(void);
   void calcFixation(void);

   void setOtherVariables (void);
   float layerProportion(void);

      ////////////////////////////////////////////////////////
   void getOtherVariables (void);
   void supply(void);                 // plant
   void demand(void);                 // plant
   void uptake(void);                 // plant
   void partition(void);              // plant
   void retranslocate(void);          // plant
   ////////////////////////////////////////////////////////

// public Methods -------------------------------------------------------
   public:
   Nitrogen(ScienceAPI &, OOPlant *p);
   ~Nitrogen();

   void   readParams (string cultivar);          // plant
   void   updateVars(void);           // plant


   void process(void);                // plant
   void onNewProfile(NewProfileType &);  // plantActions


   float getExpansionStress(void){return expansionStress;} // Leaf
   float getPhotoStress(void){return photoStress;}

   float getPhenoStress(void){return phenoStress;}         // phenology
   void detachment(vector<float> senDetachFrac, vector<float> deadDetachFrac);

   void getNGreen(float &);
   void getDltNGreen(vector<float> &);
   void getDltNRetrans(vector<float> &);
   void getNSenesced(float &);
   void getNDead(float &);
   void getDltNDetached(vector<float> &);
   void getDltNDeadDetached(vector<float> &);

   float getNStover(void){return nStover;}
   void Summary(void);

   };  // Nitrogen

//------------------------------------------------------------------------------------------------
#endif
