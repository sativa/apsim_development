//---------------------------------------------------------------------------
#ifndef ResidueHerbageH
#define ResidueHerbageH
#include "HerbageBase.h"
#include "ResiduePool.h"


// ------------------------------------------------------------------
class ResidueHerbage : public HerbageBase
   {
   public:
      ResidueHerbage(protocol::Component *system);
      ~ResidueHerbage(void);
      void doInit2(void);
//      void respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData);
      void doDmdPoolsToHerbageParts(protocol::remove_herbageType &grazed, protocol::removeCropDmType &crop);
      void doDigestibility (void);

      void doRunTimeReg(void);
      void getParts(ResiduePartType &parts, unsigned partsID);
////      void getPstanding(ResiduePartType &pstanding, ResiduePool &dm);
////      void getPlying(ResiduePartType &plying, ResiduePool &dm);
////      void getPDead(ResiduePartType &pDead, ResiduePool &dm);
////      void getHeight(float &height);
////      void getThermalTime(float &thermalTime);
      void getVariables(void);
      void readHerbageModuleParameters ( void );
      void calcDmdDistribution(ResiduePool dmdFraction[], ResiduePool dQ);
      void calcDmdDistributionB(ResiduePool dmdFraction[], ResiduePool dQ);
      void calcDmdClass(ResiduePool &dmdClassMax, ResiduePool &dmdClassMin);
      void calcDmdAverage(void);
      float dmTotal(void);
      float dmTot(int pool);
      float cpConc(int pool);
      float pConc(int pool);
      float ashAlk(int pool);
      float sConc(int pool);
      float proportionGreen(void);
      float proportionLegume(void);
      float selectionFactor ( void );


      void doInit1(const FString& sdml);
      void doGrazed(protocol::remove_herbageType &grazed);
      void readParameters ( void );

      void proportion (float dmdAvg, float dmdMax, float dmdMin, float dmdFraction[]);
      void dmdClass (float dmdMax, float dmdMin, float &dmdClassMax, float &dmdClassMin);
      float hHeight(void);
      float heightRatio(int pool);
      float bD(void);
      float dmdValue(int pool);
      float protDg(int pool);
      int numDmdPools ( void );
      string herbageModuleName(void);
      string debug();

   protected:
////      protocol::Component *system;

      unsigned removeHerbageID;

      unsigned SOMID;
      unsigned dmFeedOnOfferID;
      unsigned dmFeedRemovedID;
      unsigned removeCropBiomassID;

      ResiduePool dmdFraction[maxDmdPools];
      ResiduePool dmdPoolDm[maxDmdPools];
      ResiduePool partFraction[maxDmdPools];
      ResiduePool dmdMax;
      ResiduePool dmdAvg;
      ResiduePool dmdMin;

      ResiduePool dmdClassMax;
      ResiduePool dmdClassMin;

      ResiduePool dm;
      ResiduePool N;
      ResiduePool P;
      ResiduePool dQ;
      float  height;
      float  thermalTime;

         string cHerbageModuleName;
         string cDebug;

         float cDmdValue[maxDmdPools];
         int   cNumDmdPools;

   private:
////      protocol::Component *system;
      float divide (float dividend, float divisor, float default_value);

      unsigned surfaceOMID;

      struct
      {

         float dmdValue[maxDmdPools];
         int   numDmdPools;

         float pConccelluloseDefault;
         float pConccarbohydrateDefault;
         float pConcligninDefault;

         float AshAlkcelluloseDefault;
         float AshAlkligninDefault;
         float AshAlkcarbohydrateDefault;

         float NSRatiocelluloseDefault;
         float NSRatiocarbohydrateDefault;
         float NSRatioligninDefault;

         float NPRatiocelluloseDefault;
         float NPRatiocarbohydrateDefault;
         float NPRatioligninDefault;

         float dmdstanding[3];
         float dmdlying[3];

         float cpNRatio;
         float proportionLegume;

         float dmdWeightingCarbohydrate;
         float dmdWeightingCellulose;
         float dmdWeightingLignin;

      } c;

   };

//class ResiduePoolTypeC
//{
//}

#endif
