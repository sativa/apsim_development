//---------------------------------------------------------------------------
#ifndef PlantHerbageH
#define PlantHerbageH
#include "HerbageBase.h"
#include "PlantPool.h"
#include "SeedPool.h"

// number of plant parts
 const int  max_part = 6 ; // NB. implies for (i=0; i < max_part; max_part++) usage

// ------------------------------------------------------------------
class PlantHerbage : public HerbageBase
   {
   public:
      PlantHerbage(void);
      PlantHerbage(protocol::Component *system);
      ~PlantHerbage(void);

      void doInit2(void);
//      void respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData);
      void doDmdPoolsToHerbageParts(protocol::RemoveHerbageType &grazed, protocol::RemoveCropDmType &crop);
      void doDigestibility (void);

      void doRunTimeReg(void);
      void doRegister(const int partNo, const std::string& partName);
      float getPart(unsigned &partID);
      void getParts(PlantPartType &parts, SeedPartType &partsSeed, unsigned partsID[]);
      void getPGreen(PlantPartType &pGreen, PlantPool &dm, SeedPartType &pGreenSeed, SeedPool &dmSeed, unsigned partsID[]);
      void getPSenesced(PlantPartType &pSenesced, PlantPool &dm, SeedPartType &pSenescedSeed, SeedPool &dmSeed, unsigned partsID[]);
      void getPDead(PlantPartType &pDead, PlantPool &dm, SeedPartType &pDeadSeed, SeedPool &dmSeed, unsigned partsID[]);
      void getHeight(float &height);
      void getTrampling(void);
      void getStage(void);
      void getVariables(void);
      void readHerbageModuleParameters ( void );

      void calcDmdDistribution(PlantPool dmdFraction[]);
//      void calcDmdClass(void);
////      void calcDmdDecline(void);

      float dmTotalVeg(void);
      float dmTotVeg(int pool);
      float cpConcVeg(int pool);
      float pConcVeg(int pool);
      float ashAlkVeg(int pool);
      float sConcVeg(int pool);

      float dmTotalSeed(void);
      float dmTotSeed(int pool);
      float cpConcSeed(int pool);
      float pConcSeed(int pool);
      float ashAlkSeed(int pool);
      float sConcSeed(int pool);

      float proportionGreen(void);
      float proportionLegume(void);
      float selectionFactor ( void );

         // Belongs to base class when implemented
      void doInit1(const protocol::Init1Data&);
      void doGrazed(protocol::RemoveHerbageType &grazed);

      void readParameters ( void );
      void proportion (float dmdAvg, float dmdMax, float dmdMin, float dmdFraction[]);
      void proportion (float dmdMax, float dmdMin, float dmdFraction[]);
//      void dmdClass (float dmdMax, float dmdMin, float &dmdClassMax, float &dmdClassMin);

      float hHeight(void);
      float heightRatioVeg(void);
      float bD(void);
      float dmdValueVeg(int pool);
      float protDgVeg(int pool);
      int numDmdPoolsVeg ( void );

      float heightRatioSeed(void);
      float dmdValueSeed(int pool);
      float protDgSeed(int pool);
      int numDmdPoolsSeed ( void );

      int seedClass(int pool);
      int seedMaturity(void);
      bool ripeSeed(void);
      float trampling(void);

      string herbageModuleName(void);
      string debug();

   protected:
////      protocol::Component *system;

      unsigned removeHerbageID;

      unsigned dmFeedOnOfferID;
      unsigned dmFeedRemovedID;
      unsigned removeCropBiomassID;
      unsigned detachRateID;

      PlantPool dmdFractionVeg[maxDmdPoolsVeg];
      PlantPool dmdPoolDmVeg[maxDmdPoolsVeg];
      PlantPool partFractionVeg[maxDmdPoolsVeg];
      PlantPool dmdMaxVeg;
      PlantPool dmdAvgVeg;
      PlantPool dmdMinVeg;

      SeedPool dmdMaxSeed;
      SeedPool dmdAvgSeed;
      SeedPool dmdMinSeed;

      PlantPool dmdClassMaxVeg;
      PlantPool dmdClassMinVeg;

      PlantPool dmVeg;
      PlantPool NVeg;
      PlantPool PVeg;
      PlantPool dmdVeg;
////      PlantPool dQVeg;
      float  eTrampling;
      float  height;
      int  cropMatureStageNo;
      float  cropStageNo;
      string cropStageName;

      SeedPool dmdFractionSeed[maxDmdPoolsSeed];
      SeedPool dmdPoolDmSeed[maxDmdPoolsSeed];
      SeedPool partFractionSeed[maxDmdPoolsSeed];
      SeedPool dmdUnripeSeed;
      SeedPool dmdRipeSeed;

      SeedPool dmdClassMaxSeed;
      SeedPool dmdClassMinSeed;

      SeedPool dmSeed;
      SeedPool NSeed;
      SeedPool PSeed;
      SeedPool dmdSeed;
////      SeedPool dQSeed;

         string cHerbageModuleName;
         string cDebug;

         float cDmdValueVeg[maxDmdPoolsVeg];
         int   cNumDmdPoolsVeg;

         float cDmdValueSeed[maxDmdPoolsSeed];
         int   cNumDmdPoolsSeed;

   private:
////      protocol::Component *system;
      float divide (float dividend, float divisor, float default_value);
      string addPartToName(const std::string& variableName, const std::string& partName);

      unsigned dmGreenID[max_part];
      unsigned dmSenescedID[max_part];
      unsigned dmDeadID[max_part];
      unsigned nGreenID[max_part];
      unsigned nSenescedID[max_part];
      unsigned nDeadID[max_part];
      unsigned pGreenID[max_part];
      unsigned pSenescedID[max_part];
      unsigned pDeadID[max_part];
      unsigned dmdMaxGreenID[max_part];
      unsigned dmdAvgGreenID[max_part];
      unsigned dmdMinGreenID[max_part];
      unsigned dmdMaxSenescedID[max_part];
      unsigned dmdAvgSenescedID[max_part];
      unsigned dmdMinSenescedID[max_part];
      unsigned dmdMaxDeadID[max_part];
      unsigned dmdAvgDeadID[max_part];
      unsigned dmdMinDeadID[max_part];
      unsigned heightID;
      unsigned tramplingID;
      unsigned stageID;
      unsigned stageNameID;

      struct
      {

         float dmdValueVeg[maxDmdPoolsVeg];
         int   numDmdPoolsVeg;
         float dmdValueSeed[maxDmdPoolsSeed];
         int   numDmdPoolsSeed;
         float specificDetachRate;

         float pConcGreenStemDefault;
         float pConcGreenPodDefault;
         float pConcGreenLeafDefault;
         float pConcGreenMealDefault;
         float pConcGreenOilDefault;
         float pConcDeadStemDefault;
         float pConcDeadPodDefault;
         float pConcDeadLeafDefault;
         float pConcDeadMealDefault;
         float pConcDeadOilDefault;
         float pConcSenescedStemDefault;
         float pConcSenescedPodDefault;
         float pConcSenescedLeafDefault;
         float pConcSenescedMealDefault;
         float pConcSenescedOilDefault;

         float AshAlkGreenStemDefault;
         float AshAlkGreenPodDefault;
         float AshAlkGreenLeafDefault;
         float AshAlkGreenMealDefault;
         float AshAlkGreenOilDefault;
         float AshAlkDeadStemDefault;
         float AshAlkDeadPodDefault;
         float AshAlkDeadLeafDefault;
         float AshAlkDeadMealDefault;
         float AshAlkDeadOilDefault;
         float AshAlkSenescedStemDefault;
         float AshAlkSenescedPodDefault;
         float AshAlkSenescedLeafDefault;
         float AshAlkSenescedMealDefault;
         float AshAlkSenescedOilDefault;

         float NSRatioGreenStemDefault;
         float NSRatioGreenPodDefault;
         float NSRatioGreenLeafDefault;
         float NSRatioGreenMealDefault;
         float NSRatioGreenOilDefault;
         float NSRatioDeadStemDefault;
         float NSRatioDeadPodDefault;
         float NSRatioDeadLeafDefault;
         float NSRatioDeadMealDefault;
         float NSRatioDeadOilDefault;
         float NSRatioSenescedStemDefault;
         float NSRatioSenescedPodDefault;
         float NSRatioSenescedLeafDefault;
         float NSRatioSenescedMealDefault;
         float NSRatioSenescedOilDefault;

         float NPRatioGreenStemDefault;
         float NPRatioGreenPodDefault;
         float NPRatioGreenLeafDefault;
         float NPRatioGreenMealDefault;
         float NPRatioGreenOilDefault;
         float NPRatioDeadStemDefault;
         float NPRatioDeadPodDefault;
         float NPRatioDeadLeafDefault;
         float NPRatioDeadMealDefault;
         float NPRatioDeadOilDefault;
         float NPRatioSenescedStemDefault;
         float NPRatioSenescedPodDefault;
         float NPRatioSenescedLeafDefault;
         float NPRatioSenescedMealDefault;
         float NPRatioSenescedOilDefault;

         float dmdGreenLeaf[3];
         float dmdGreenStem[3];
         float dmdGreenPod[3];
         float dmdGreenMeal[2];
         float dmdGreenOil[2];
         float dmdSenescedLeaf[3];
         float dmdSenescedStem[3];
         float dmdSenescedPod[3];
         float dmdSenescedMeal[2];
         float dmdSenescedOil[2];
         float dmdDeadLeaf[3];
         float dmdDeadStem[3];
         float dmdDeadPod[3];
         float dmdDeadMeal[2];
         float dmdDeadOil[2];

         int seedClass[2];

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
