//---------------------------------------------------------------------------

#ifndef OOGrainH
#define OOGrainH

#include "OOPlantComponents.h"
#include "Utilities.h"

//------------------------------------------------------------------------------------------------

class Grain : public PlantPart
   {
   private:

// Parameters ----------------------------------------------------------
   float dmPerSeed;
   float waterContent;

   // nitrogen
   float grainFillRate;
   float targetNConc;

// Variables ----------------------------------------------------------
//   float dmGreen,dmDead;
//   float dltDmGreen;

   float totDMGreenFI;          // total plant dm at FI
   float grainNo;
   float finalGrainNo;
   float grainSize;

   float dltDMStressMax;
   float dltDMGrainDemand;
   float addGrainWeight;        // used in source sink demand

// Private Methods -------------------------------------------------------
   void   doRegistrations(void);
   void   initialize(void);
   float calcGrainNumber(void);
   float yieldPartDemandStress(void);
   float calcDMGrainSourceSink(void);

// public Methods -------------------------------------------------------
   public:
    Grain(OOPlant *p);                            // plant
   ~Grain();                                    // plant
   void   readParams (string cultivar);                    // plant
   void   updateVars(void);                     // plant
   void   calcDemandStress(void);               // plant
   void   calcBiomassDemand(void);              // plant

   float calcNDemand(void);                     // nitrogen
//   float getNGreen(void)const{return nGreen;}   // nitrogen
//   float getNDemand(void)const{return nDemand;} // nitrogen
   void   RetranslocateN(float N);              // nitrogen

   float partitionDM(float dltDM);                        // biomass
//   float getDmGreen(void)const{return dmGreen;}            // biomass
   float grainDMDifferential(void);                        // biomass
   void   dmRetrans(float dltDm){dmRetranslocate = dltDm;} // biomass


   // nitrogen
   float getNConc(void)const{return nConc;}            // niitrogen

   // phosphorus
   float calcPDemand(void);
   float calcPRetransDemand(void);
   float getPConc(void)const{return pConc;}            // niitrogen

   // phenology
   void phenologyEvent(int);

   void Summary(void);
   };


//---------------------------------------------------------------------------
#endif
