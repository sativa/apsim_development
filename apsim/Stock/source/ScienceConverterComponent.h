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


      const int numDmdPools = 6;

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

   private:
      void daylengthRelay (protocol::QueryValueData& queryData);
      void sendFeedOnOffer(protocol::QueryValueData& queryData);
      void sendFeedRemoved(protocol::QueryValueData& queryData);
      void sendPlant2Stock(protocol::QueryValueData& queryData);
      void getDmGreen(PlantPool &dm, vector<float>  &dmGreen);
      void getDmSenesced(PlantPool &dm, vector<float>  &dmSenesced);
      void getDmDead(PlantPool &dm, vector<float>  &dmDead);
      void getNGreen(PlantPool &N, vector<float>  &nGreen, PlantPool &dm, vector<float>  &dmGreen);
      void getNSenesced(PlantPool &N, vector<float>  &nSenesced, PlantPool &dm, vector<float>  &dmSenesced);
      void getNDead(PlantPool &N, vector<float>  &nDead, PlantPool &dm, vector<float>  &dmDead);
      void getPGreen(PlantPool &P, vector<float>  &pGreen, PlantPool &dm, vector<float>  &dmGreen);
      void getPSenesced(PlantPool &P, vector<float>  &pSenesced, PlantPool &dm, vector<float>  &dmSenesced);
      void getPDead(PlantPool &P, vector<float>  &pDead, PlantPool &dm, vector<float>  &dmDead);
      void getHeight(float &height);
      void getVariables(PlantPool &dm, PlantPool &N, PlantPool &P, float &height);
      void read_constants ( void );
      void calcDmdDistribution(PlantPool dmdFraction[]);
      void proportion (float dmdAvg, float dmdMax, float dmdMin, float dmdFraction[]);
      float divide (float dividend, float divisor, float default_value);

      unsigned day_lengthID;
      unsigned dayLengthID;
      unsigned plant2stockID;
      unsigned removeHerbageID;

      unsigned dmGreenID;
      unsigned dmSenescedID;
      unsigned dmDeadID;
      unsigned nGreenID;
      unsigned nSenescedID;
      unsigned nDeadID;
      unsigned pGreenID;
      unsigned pSenescedID;
      unsigned pDeadID;
      unsigned heightID;
      unsigned dmFeedOnOfferID;
      unsigned dmFeedRemovedID;
      unsigned stockBuyID;
      unsigned buyID;
      unsigned removeCropBiomassID;
      bool plant2StockSent;

      protocol::plant2stockType feed;
      protocol::remove_herbageType grazed;

//      struct PlantPartType
//         {
//         float leaf;
//         float stem;
//         };
//
//      struct PlantPoolType
//         {
//         PlantPartType green;
//         PlantPartType senesced;
//         PlantPartType dead;
//         };

      PlantPool partFraction[numDmdPools];

      struct
      {

      } c;

   };

//class PlantPoolTypeC
//{
//}

#endif
