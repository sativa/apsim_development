//---------------------------------------------------------------------------
#ifndef ScienceConverterComponentH
#define ScienceConverterComponentH
#include <ComponentInterface\Component.h>
#include <string>
#include <vector>
#include "PlantPool.h"
#include "PlantHerbage.h"

#define min(A,B) ((A)<(B)?(A):(B))
#define max(A,B) ((A)>(B)?(A):(B))
// Maximum number of layers in soil
#define max_layer 100

//std::string ftoa(double Float, char *fmtwidth=".2");
//std::string itoa(int value, int width);


//      const int maxDmdPools = 6;

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
      void sendAddSurfaceOMEvent (const string& omName, const string& omType, protocol::faeces_omType faecesOM);
      void addUrine (protocol::urineType urine);
      void readParameters ( void );
      float divide (float dividend, float divisor, float default_value);

      unsigned day_lengthID;
      unsigned dayLengthID;
      unsigned tramplingID;
      unsigned ureaID;
      unsigned dltUreaID;
      unsigned labilePID;
      unsigned dltLabilePID;
      unsigned plant2stockID;
      unsigned removeHerbageID;
      unsigned addExcretaID;

      unsigned dmFeedOnOfferID;
      unsigned dmFeedRemovedID;
      unsigned stockBuyID;
      unsigned stockSellID;
      unsigned buyID;
      unsigned sellID;
      unsigned removeCropBiomassID;
      unsigned addManureID;
      bool plant2StockSent;

      protocol::plant2stockType feed;
      protocol::remove_herbageType grazed;
      protocol::add_excretaType excreted;

      PlantHerbage *conversion;

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
