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
      virtual ~HerbageBase(void);
      virtual void doInit1(const FString& sdml);
      virtual void doInit2(void);
//      virtual void respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData);
      virtual void doGrazed(protocol::remove_herbageType &grazed);
      virtual void doDmdPoolsToHerbageParts(protocol::remove_herbageType &grazed, protocol::removeCropDmType &crop);
      virtual void getVariables(void) = 0;

      virtual void doRunTimeReg(void);
      virtual void doDigestibility (void);
      virtual void getThermalTime(float &thermalTime);
      virtual void readParameters ( void );
      virtual void readHerbageModuleParameters ( void ) = 0;
      virtual void calcDmdDistribution(PlantPool dmdFraction[], PlantPool dQ) = 0;
      virtual void calcDmdDistributionB(PlantPool dmdFraction[], PlantPool dQ) = 0;
      virtual void calcDmdDecline(void) = 0;
      virtual void calcDmdClass(PlantPool &dmdClassMax, PlantPool &dmdClassMin) = 0;
      virtual void proportion (float dmdAvg, float dmdMax, float dmdMin, float dmdFraction[]);
      virtual void dmdClass (float dmdMax, float dmdMin, float &dmdClassMax, float &dmdClassMin);
      virtual float dmTotal(void);
      virtual float dmTot(int pool) = 0;
      virtual float cpConc(int pool) = 0;
      virtual float pConc(int pool) = 0;
      virtual float ashAlk(int pool) = 0;
      virtual float sConc(int pool) = 0;
      virtual float hHeight(void);
      virtual float heightRatio(int pool);
      virtual float bD(void);
      virtual float dmdValue(int pool);
      virtual float protDg(int pool);
      virtual float proportionGreen(void) = 0;
      virtual float proportionLegume(void) = 0;
      virtual float selectionFactor ( void ) = 0;
      virtual int numDmdPools ( void );
      virtual string herbageModuleName(void);
      virtual string debug();

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
