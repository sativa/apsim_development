//------------------------------------------------------------------------------------------------

#ifndef OOPhenologyH
#define OOPhenologyH

#include "OOPlantComponents.h"
#include "Utilities.h"


typedef enum  {noCrop, sowing, germination, emergence, endJuv, fi, flag, flowering,
                  startGrainFill, endGrainFill, maturity, harvest, endCrop} stages;

#define nStages 13

//------------------------------------------------------------------------------------------------

class Phenology : public PlantProcess
   {
   protected:

// Parameters ----------------------------------------------------------

      vector<string> stageNames;
      vector<float> ttTarget;       // phenology targets for each stage

   // first cut at phenology -    sowing is sowing to germination

      float ttEndJuvInit;
      float ttFlowerMaturity;
      float peswGerm;

      TableFn photoParams;
      TableFn ttParams;
      TableFn ttFmParams;

      float latitude;
      float twilight;
      float stageCode;

//  Variables  ---------------------------------------------------------
      string stageName;

      vector<float> ttTotal;
      vector<float> ttTotalFM;
      vector<float> daysTotal;

      float dltPhase;
      float dltStage;
      float stage;
      float dltTT;
      float dltTTFM;
      float ttCurrStage;

      int floweringDAS;
      int floweringDOY;
      int maturityDAS;
      int maturityDOY;

// Private Methods -------------------------------------------------------
      void   doRegistrations(void);
      void   initialize(void);

      void   checkTargets(void);
      void   calcThermalTimes(Today *today);
      void   calcPhaseDevelopment(void);
      void   calcDevelopment(void);

      float calcStressesTT(void);
      float germinationPhase (void);
      float phaseFraction(float stage, vector<float> ttTotal,float dltTT,vector<float> stageTT);
      float calcDailyTT(Today *today);
      float calcDailyTTFM(Today *today);
      float temp3Hr (float tMax, float tMin, float period);

// public Methods -------------------------------------------------------
   public:
      Phenology(OOPlant *p);               // plant
      Phenology(void){};
      ~Phenology();                      // plant
      void   readParams (string cultivar);          // plant
      void   updateVars(void);           // plant
      virtual void   development(void);          // plant
      void   setStage(float stageNow);  // plant,plantActions

      float currentStage(void) {return stage;}  // plant,roots,leaf,stem,rachis,grain,biomass

      float sumTTtarget(int from, int to);   // leaf,grain
      float sumTTtotal(int from, int to);    // plant,leaf,grain
      float sumTTtotalFM(int from, int to);  // grain
      float sumDaysTotal(int from, int to);  // grain

      float getDltTTFM(void)const{return dltTTFM;}               // Grain
      float getTTtarget(int stage)const{return ttTarget[stage];} // Grain

      float getDltTT(void)const{return dltTT;}            // Leaf
      float dayFracFM(void)const{return dltTTFM / 17.0;}  // Nitrogen

      float getDltStage(void)const{return dltStage;}            // Phenology

      void getTTTot(protocol::Component *system, protocol::QueryValueData &qd);
      void getPhaseTT(protocol::Component *system, protocol::QueryValueData &qd);
      void getStageName(protocol::Component *system, protocol::QueryValueData &qd);

      void Summary(void);
      string returnStageName(void)const{ return stageName;}

   };

#endif
