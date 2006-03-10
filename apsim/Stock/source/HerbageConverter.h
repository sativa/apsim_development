//---------------------------------------------------------------------------
#ifndef HerbageConverter_H
#define HerbageConverter_H
#include <ComponentInterface\Component.h>
#include <string>
#include <vector>
#include "PlantHerbage.h"
#include "ConverterBase.h"

#define min(A,B) ((A)<(B)?(A):(B))
#define max(A,B) ((A)>(B)?(A):(B))
// Maximum number of layers in soil
#define max_layer 100

// ------------------------------------------------------------------
class HerbageConverter : public ConverterBase
   {
   public:
      HerbageConverter(protocol::Component *system);
      ~HerbageConverter(void);
      virtual void doInit1(const FString& sdml);
      virtual void doInit2(void);
      virtual void respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData);
      virtual void respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);

   private:
      void doRunTimeReg(void);
      void sendFeedOnOffer(protocol::QueryValueData& queryData);
      void sendFeedRemoved(protocol::QueryValueData& queryData);
      void sendPlant2Stock(protocol::QueryValueData& queryData);
      void readParameters ( void );
      float divide (float dividend, float divisor, float default_value);

      unsigned tramplingID;
      unsigned plant2stockID;
      unsigned removeHerbageID;

      unsigned dmFeedOnOfferID;
      unsigned dmFeedRemovedID;
      unsigned removeCropBiomassID;
      bool plant2StockSent;

////      protocol::Component *system;
      protocol::plant2stockType feed;
      protocol::remove_herbageType grazed;

      PlantHerbage *conversion;

      PlantPool dmdPoolDm[maxDmdPools];
      PlantPool partFraction[maxDmdPools];
      PlantPool dmdMax;
      PlantPool dmdAvg;
      PlantPool dmdMin;

      PlantPool dmdClassMax;
      PlantPool dmdClassMin;

      string herbage_model;
      struct
      {
         string herbageModuleName;
         string debug;

         float dmdValue[maxDmdPools];
         int   numDmdPools;


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
