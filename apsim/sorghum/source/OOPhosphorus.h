//------------------------------------------------------------------------------------------------

#ifndef OOPhosphorusH
#define OOPhosphorusH

#include "OOPlantComponents.h"
#include "Utilities.h"

//------------------------------------------------------------------------------------------------

class Phosphorus : public PlantProcess
   {
   private:
   // Active - has the SoilP module been included?
   bool active;

   float stage;
   vector<PlantPart *> StressParts;


// Parameters ----------------------------------------------------------

//   float diffnConstant;
   int nLayers;
   vector<float> dLayer;

   float phenoSlope;
   float photoSlope;
   float expansionSlope;
   float grainSlope;

   //  Variables  ---------------------------------------------------------
   float profileDepth;


   unsigned int labileID;
   unsigned int uptakeID;

   float phenoStress;
   float expansionStress;
   float photoStress;
   float grainStress;

   float pBiomass;
   float pStover;
   float pGreenBiomass;
   float pUptakeTotal;
   float pPlant;

   // supply

   vector<float> pGreen;
   vector<float> dltPGreen;
   vector<float> dltPRetrans;
   vector<float> pSenesced;
   vector<float> pDead;
   vector<float> dltPDead;
   vector<float> dltPDetached;
   vector<float> dltPDetachedDead;
   vector<float> pDemand;

   float plantPDemand;    // plant demand - grain demand

   // demand
   float totalDemand;
   float supplyDemandRatio;
   float pSupply;


   int currentLayer;                   // number of the layer that the roots are in now (starts at 0)
   float rootDepth;


// Private Methods -------------------------------------------------------
   void doRegistrations(void);
   void initialize(void);

   void setOtherVariables (void);
   float layerProportion(void);

   void calcStress(void);
   float pStress(void);


      ////////////////////////////////////////////////////////
   void getOtherVariables (void);
   void supply(void);                 // plant
   void demand(void);                 // plant
   void uptake(void);                 // plant
   void partition(void);              // plant
   void senescence(void);
   void detachment(void);
   void updateP(void);
   void retranslocate(void);          // plant
   ////////////////////////////////////////////////////////

// public Methods -------------------------------------------------------
   public:
   Phosphorus(ScienceAPI &, OOPlant *p);
   ~Phosphorus();

   void   readParams (string cultivar);          // plant
   void   updateVars(void);           // plant


   void prepare(void);
   void process(void);                // plant
   void onNewProfile(NewProfileType &p);  // plantActions


   float getExpansionStress(void){return expansionStress;} // Leaf
   float getPhotoStress(void){return photoStress;}

   float getPhenoStress(void){return phenoStress;}         // phenology
   void detachment(vector<float> senDetachFrac, vector<float> deadDetachFrac);

   void getPGreen(float &);
   void getDltPGreen(vector<float> &);
   void getDltPRetrans(vector<float> &);
   void getPSenesced(float &);
   void getPDead(float &);
   void getDltPDetached(vector<float> &);
   void getDltPDead(vector<float> &);
   void getDltPDeadDetached(vector<float> &);
   void getPDemand(vector<float> &);

   bool Active(void){return active;}
   void Summary(void);
   float getPStover(void){return pStover;}

   };  // Phosphorus

//------------------------------------------------------------------------------------------------
#endif

