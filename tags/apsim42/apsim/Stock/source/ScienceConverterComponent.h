//---------------------------------------------------------------------------
#ifndef ScienceConverterComponentH
#define ScienceConverterComponentH
#include <ComponentInterface\Component.h>
#include <string>
#include <vector>
#include "PlantPool.h"

#define min(A,B) ((A)<(B)?(A):(B))
#define max(A,B) ((A)>(B)?(A):(B))
std::string ftoa(double Float, char *fmtwidth=".2");
std::string itoa(int value, int width);


      const int maxDmdPools = 6;

// ------------------------------------------------------------------
// TRACKER component for APSIM.
// eg of parameter file specification:
//    sum(rain)[1jan-31dec]
//    sum(rain)[sow-harvest]
//    sum(rain)[3]
// ------------------------------------------------------------------
class ScienceConverterComponent : public protocol::Component
   {
   public:
      ScienceConverterComponent(void);
      ~ScienceConverterComponent(void);
      virtual void doInit1(const FString& sdml);
      virtual void doInit2(void);
      virtual void respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData);
      virtual void respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);
      virtual void stockBuy (protocol::Variant &v/*(INPUT) message variant*/);
      virtual void stockSell (protocol::Variant &v/*(INPUT) message variant*/);

   private:
      void doRunTimeReg(void);
      void daylengthRelay (protocol::QueryValueData& queryData);
      void sendFeedOnOffer(protocol::QueryValueData& queryData);
      void sendFeedRemoved(protocol::QueryValueData& queryData);
      void sendPlant2Stock(protocol::QueryValueData& queryData);
      void getParts(PlantPartType &parts, unsigned partsID);
      void getPGreen(PlantPartType &pGreen, PlantPool &dm);
      void getPSenesced(PlantPartType &pSenesced, PlantPool &dm);
      void getPDead(PlantPartType &pDead, PlantPool &dm);
      void getHeight(float &height);
      void getThermalTime(float &thermalTime);
      void getVariables(PlantPool &dm, PlantPool &N, PlantPool &P, float &height, float &thermalTime);
      void readParameters ( void );
      void readHerbageModuleParameters ( void );
      void calcDmdDistribution(PlantPool dmdFraction[], PlantPool dQ);
      void calcDmdDistributionB(PlantPool dmdFraction[], PlantPool dQ);
      void calcDmdDecline(const float &thermalTime, PlantPool &dQ);
      void calcDmdClass(PlantPool &dmdClassMax, PlantPool &dmdClassMin);
      void proportion (float dmdAvg, float dmdMax, float dmdMin, float dmdFraction[]);
      void dmdClass (float dmdMax, float dmdMin, float &dmdClassMax, float &dmdClassMin);
      float divide (float dividend, float divisor, float default_value);

      unsigned day_lengthID;
      unsigned dayLengthID;
      unsigned plant2stockID;
      unsigned removeHerbageID;

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
      unsigned dmFeedOnOfferID;
      unsigned dmFeedRemovedID;
      unsigned stockBuyID;
      unsigned stockSellID;
      unsigned buyID;
      unsigned sellID;
      unsigned removeCropBiomassID;
      bool plant2StockSent;

      protocol::plant2stockType feed;
      protocol::remove_herbageType grazed;

      PlantPool dmdPoolDm[maxDmdPools];
      PlantPool partFraction[maxDmdPools];
      PlantPool dmdMax;
      PlantPool dmdAvg;
      PlantPool dmdMin;

      PlantPool dmdClassMax;
      PlantPool dmdClassMin;

      struct
      {
         string herbageModuleName;
         string debug;

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

         float KQ5Leaf;
         float KQ5Stem;
         float KQ4;

      } c;

   };

//class PlantPoolTypeC
//{
//}

#endif
