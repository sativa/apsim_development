//---------------------------------------------------------------------------
#ifndef HerbageBase_H
#define HerbageBase_H

#include <general/pch.h>
#include <math.h>
#include <strstream>
#include <string>
#include <vector>

#include <iomanip.h>
#include <boost/function.hpp>

#include <general/string_functions.h>
#include <general/stl_functions.h>
#include <ApsimShared/FStringExt.h>
#include <ComponentInterface/MessageDataExt.h>
#include <ComponentInterface/ApsimVariant.h>

#include <ComponentInterface/Component.h>
#include <ComponentInterface/DataTypes.h>


//#include "HerbagePool.h"

#define min(A,B) ((A)<(B)?(A):(B))
#define max(A,B) ((A)>(B)?(A):(B))
// Maximum number of layers in soil

std::string ftoa(double Float);
std::string ftoa(double Float, char *fmtwidth);
std::string itoa(int value, int width);

      const int maxDmdPoolsVeg = 6;
      const int maxDmdPoolsSeed = 2;
      const int maxSeedClasses = 2;

// ------------------------------------------------------------------
class HerbageBase : public protocol::Component
   {
   public:
      HerbageBase(protocol::Component *system);
      virtual ~HerbageBase(void);

    virtual void doInit1(const FString& sdml) = 0;
    virtual void doInit2() = 0;
    virtual void doGrazed(protocol::remove_herbageType &grazed) = 0;
    virtual void doDigestibility() = 0;

    virtual float dmTotalVeg() = 0;
    virtual int numDmdPoolsVeg() = 0;
    virtual float dmTotVeg(int pool) = 0;
    virtual float dmdValueVeg(int pool) = 0;
    virtual float cpConcVeg(int pool) = 0;
    virtual float pConcVeg(int pool) = 0;
    virtual float sConcVeg(int pool) = 0;
    virtual float protDgVeg(int pool) = 0;
    virtual float ashAlkVeg(int pool) = 0;
    virtual float heightRatioVeg(int pool) = 0;
    virtual float bD() = 0;
    virtual float hHeight() = 0;

    virtual float dmTotalSeed() = 0;
    virtual int numDmdPoolsSeed() = 0;
    virtual float dmTotSeed(int pool) = 0;
    virtual float dmdValueSeed(int pool) = 0;
    virtual float cpConcSeed(int pool) = 0;
    virtual float pConcSeed(int pool) = 0;
    virtual float sConcSeed(int pool) = 0;
    virtual float protDgSeed(int pool) = 0;
    virtual float ashAlkSeed(int pool) = 0;
    virtual float heightRatioSeed(int pool) = 0;
    virtual void getStage(void) = 0;
    virtual float trampling(void) = 0;

    virtual float proportionGreen() = 0;
    virtual float proportionLegume() = 0;
    virtual float selectionFactor() = 0; // ??
    virtual int seedClass(int pool) = 0;
    virtual int seedMaturity(void) = 0;

    protected:
        protocol::Component *system;

   };


#endif
