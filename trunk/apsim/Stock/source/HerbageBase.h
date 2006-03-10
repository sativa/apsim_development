//---------------------------------------------------------------------------
#ifndef HerbageBase_H
#define HerbageBase_H

#include <ComponentInterface\Component.h>
#include <string>
#include <vector>
#include <general\pch.h>
#include <vcl.h>
#include <boost/function.hpp>
#pragma hdrstop

#include <math.h>
#include <strstream>
#include <iomanip.h>

#include <general/string_functions.h>
#include <general/stl_functions.h>
#include <ApsimShared/FStringExt.h>
#include <ComponentInterface/MessageDataExt.h>
#include <ComponentInterface/ApsimVariant.h>

#include <ComponentInterface/Component.h>
#include <ComponentInterface/DataTypes.h>

#pragma package(smart_init)

#include "PlantPool.h"

#define min(A,B) ((A)<(B)?(A):(B))
#define max(A,B) ((A)>(B)?(A):(B))
// Maximum number of layers in soil

std::string ftoa(double Float, char *fmtwidth=".2");
std::string itoa(int value, int width);

      const int maxDmdPools = 6;

// ------------------------------------------------------------------
class HerbageBase : public protocol::Component
   {
   public:
      HerbageBase(protocol::Component *system);
      ~HerbageBase(void);
      virtual void doInit1(const FString& sdml);
      virtual void doInit2(void);
//      virtual void respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData);
      void doGrazed(protocol::remove_herbageType &grazed);
      virtual void doDmdPoolsToHerbageParts(protocol::remove_herbageType &grazed, protocol::removeCropDmType &crop);
      virtual void getVariables(void) = 0;

      void doRunTimeReg(void);
      void sendFeedRemoved(protocol::QueryValueData& queryData);
      void getThermalTime(float &thermalTime);
      void readParameters ( void );
      virtual void readHerbageModuleParameters ( void ) = 0;
      virtual void calcDmdDistribution(PlantPool dmdFraction[], PlantPool dQ) = 0;
      virtual void calcDmdDistributionB(PlantPool dmdFraction[], PlantPool dQ) = 0;
      virtual void calcDmdDecline(void) = 0;
      virtual void calcDmdClass(PlantPool &dmdClassMax, PlantPool &dmdClassMin) = 0;
      void proportion (float dmdAvg, float dmdMax, float dmdMin, float dmdFraction[]);
      void dmdClass (float dmdMax, float dmdMin, float &dmdClassMax, float &dmdClassMin);
      float dmTotal(void);
      virtual float dmTot(int pool) = 0;
      virtual float cpConc(int pool) = 0;
      virtual float pConc(int pool) = 0;
      virtual float ashAlk(int pool) = 0;
      virtual float sConc(int pool) = 0;
      float hHeight(void);
      float heightRatio(int pool);
      float bD(void);
      float dmdValue(int pool);
      float protDg(int pool);
      virtual float proportionGreen(void) = 0;
      virtual float proportionLegume(void) = 0;
      virtual float selectionFactor ( void ) = 0;
      int numDmdPools ( void );
      string herbageModuleName(void);
      string debug();

   protected:
      protocol::Component *system;
      float divide (float dividend, float divisor, float default_value);

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
   };

//class PlantPoolTypeC
//{
//}

#endif
