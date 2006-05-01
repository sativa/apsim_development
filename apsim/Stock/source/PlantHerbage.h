//---------------------------------------------------------------------------
#ifndef PlantHerbageH
#define PlantHerbageH
#include "HerbageBase.h"
#include "PlantPool.h"


// ------------------------------------------------------------------
class PlantHerbage : public HerbageBase
   {
   public:
      PlantHerbage(protocol::Component *system);
      ~PlantHerbage(void);

      void doInit2(void);
//      void respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData);
      void doDmdPoolsToHerbageParts(protocol::remove_herbageType &grazed, protocol::removeCropDmType &crop);
      void doDigestibility (void);

      void doRunTimeReg(void);
      void getParts(PlantPartType &parts, unsigned partsID);
      void getPGreen(PlantPartType &pGreen, PlantPool &dm);
      void getPSenesced(PlantPartType &pSenesced, PlantPool &dm);
      void getPDead(PlantPartType &pDead, PlantPool &dm);
      void getHeight(float &height);
      void getThermalTime(float &thermalTime);
      void getVariables(void);
      void readHerbageModuleParameters ( void );

      void calcDmdDistribution(PlantPool dmdFraction[], PlantPool dQ);
      void calcDmdDistributionB(PlantPool dmdFraction[], PlantPool dQ);
      void calcDmdClass(PlantPool &dmdClassMax, PlantPool &dmdClassMin);
      void calcDmdDecline(void);
      float dmTotal(void);
      float dmTot(int pool);
      float cpConc(int pool);
      float pConc(int pool);
      float ashAlk(int pool);
      float sConc(int pool);
      float proportionGreen(void);
      float proportionLegume(void);
      float selectionFactor ( void );

         // Belongs to base class when implemented
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

      unsigned dmFeedOnOfferID;
      unsigned dmFeedRemovedID;
      unsigned removeCropBiomassID;

      PlantPool dmdFraction[maxDmdPools];
      PlantPool dmdPoolDm[maxDmdPools];
      PlantPool partFraction[maxDmdPools];
      PlantPool dmdMax;
      PlantPool dmdAvg;
      PlantPool dmdMin;

      PlantPool dmdClassMax;
      PlantPool dmdClassMin;

      PlantPool dm;
      PlantPool N;
      PlantPool P;
      PlantPool dQ;
      float  height;
      float  thermalTime;

         string cHerbageModuleName;
         string cDebug;

         float cDmdValue[maxDmdPools];
         int   cNumDmdPools;

   private:
////      protocol::Component *system;
      float divide (float dividend, float divisor, float default_value);

      unsigned dmGreenID;
      unsigned dmSenescedID;
      unsigned dmDeadID;
      unsigned dmGreenDeltaID;
      unsigned dmGreenRetransDeltaID;
      unsigned dmSenescedDeltaID;
      unsigned dmSenescedDetachedDeltaID;
      unsigned dmGreenDeadDeltaID;
      unsigned dmSenescedDeadDeltaID;
      unsigned dmDeadDetachedDeltaID;
      unsigned nGreenID;
      unsigned nSenescedID;
      unsigned nDeadID;
      unsigned pGreenID;
      unsigned pSenescedID;
      unsigned pDeadID;
      unsigned heightID;
      unsigned thermalTimeID;
      unsigned thermalTimeBGID;

      struct
      {

         float dmdValue[maxDmdPools];
         int   numDmdPools;

         float pConcGreenStemDefault;
         float pConcGreenLeafDefault;
         float pConcDeadStemDefault;
         float pConcDeadLeafDefault;
         float pConcSenescedStemDefault;
         float pConcSenescedLeafDefault;

         float AshAlkGreenStemDefault;
         float AshAlkGreenLeafDefault;
         float AshAlkDeadStemDefault;
         float AshAlkDeadLeafDefault;
         float AshAlkSenescedStemDefault;
         float AshAlkSenescedLeafDefault;

         float NSRatioGreenStemDefault;
         float NSRatioGreenLeafDefault;
         float NSRatioDeadStemDefault;
         float NSRatioDeadLeafDefault;
         float NSRatioSenescedStemDefault;
         float NSRatioSenescedLeafDefault;

         float NPRatioGreenStemDefault;
         float NPRatioGreenLeafDefault;
         float NPRatioDeadStemDefault;
         float NPRatioDeadLeafDefault;
         float NPRatioSenescedStemDefault;
         float NPRatioSenescedLeafDefault;

         float dmdGreenLeaf[3];
         float dmdGreenStem[3];
         float dmdSenescedLeaf[3];
         float dmdSenescedStem[3];
         float dmdDeadLeaf[3];
         float dmdDeadStem[3];

         float cpNRatio;
         float proportionLegume;

         float KQ5Leaf;
         float KQ5Stem;
         float KQ4;

      } c;

   };

//class PlantPoolTypeC
//{
//}

#endif
