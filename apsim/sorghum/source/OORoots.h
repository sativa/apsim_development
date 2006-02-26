//---------------------------------------------------------------------------

#ifndef OORootsH
#define OORootsH

#include "OOPlantComponents.h"
#include "Utilities.h"

//------------------------------------------------------------------------------------------------

class Roots : public PlantPart
   {
   private:

// Parameters ----------------------------------------------------------

   float initialRootDepth;
   float initialDM;

   vector<float> dLayer;
   vector<float> xf;
   float profileDepth;
   int    nLayers;

   TableFn swRoot;
   TableFn rldFn;

   vector<float> rootDepthRate;    // mm/day for each stage
   float         specificRootLength;

   // senescence
   float dmRootSenFrac;

   // nitrogen
   float initialNConc;
   float targetNConc;
   float nTotal;


//  Variables  -----------------------------------------------------
   float rootDepth;
   float dltRootDepth;             // daily increment
   int    currentLayer;             // number of the layer that the roots are in now (starts at 0)
   float lastLayerPropn;           // proportion of the currentLayer occupied
   float rootDMTot;

   // length

   vector<float> rootLength;
   vector<float> dltRootLength;
   vector<float> dltScenescedRootLength;
   vector<float> rlvFactor;


// Private Methods -------------------------------------------------------
   void   doRegistrations(void);
   void   doLayerRegistrations(void);
   void   initialize(void);
   void   calcInitialLength(void);
   float calcDltRootDepth(float stage);
   float swAvailFactor(int layer);
   float layerProportion(void);


// public Methods -------------------------------------------------------
   public:
   ////////////////////////////////////////////////////
    void   calcSenLength(void);                  // plant
    void   calcRootDistribution(void);           // plant

   /////////////////////////////////////////////////////
   Roots(OOPlant *p);                           // plant
   ~Roots();                                    // plant
   void   readParams (string cultivar);                    // plant
   void   updateVars(void);                     // plant
   void   process(void);                        // plant
   void   doNewProfile(protocol::Variant &v);   // plantActions

//   float getDmGreen(void)const{return dmGreen;} // biomass
   void   calcSenescence(void);                  // biomass
   void   partitionDM(float dltDM);             // biomass

//   float getNGreen(void)const{return nGreen;}       // nitrogen
   float getRootDepth(void)const{return rootDepth;} // nitrogen, water
   float calcNDemand(void);                         // nitrogen

   float calcPDemand(void);

   float totalBiomass(void)const{return dmGreen + dmSenesced + dmDead;}
   float totalN(void)const{return nGreen + nSenesced + nDead;}
   float totalP(void)const{return pGreen + pSenesced + pDead;}

   void incorporateResidue(void);


   //Registration functions
   void getRootLength(protocol::Component *system, protocol::QueryValueData &qd);
   void getRLV(protocol::Component *system, protocol::QueryValueData &qd);

   // phenology
   void phenologyEvent(int);
   };  // Roots

//------------------------------------------------------------------------------------------------
#endif
