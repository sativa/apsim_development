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

#include "HerbagePool.h"

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

    virtual void doInit1(const FString& sdml) = 0;
    virtual void doInit2() = 0;
    virtual void doGrazed(protocol::remove_herbageType &grazed) = 0;
    virtual void doDigestibility() = 0;
    virtual float dmTotal() = 0;
    virtual int numDmdPools() = 0;
    virtual float dmTot(int pool) = 0;
    virtual float dmdValue(int pool) = 0;
    virtual float cpConc(int pool) = 0;
    virtual float pConc(int pool) = 0;
    virtual float sConc(int pool) = 0;
    virtual float protDg(int pool) = 0;
    virtual float ashAlk(int pool) = 0;
    virtual float heightRatio(int pool) = 0;
    virtual float bD() = 0;
    virtual float hHeight() = 0;
    virtual float proportionGreen() = 0;
    virtual float proportionLegume() = 0;
    virtual float selectionFactor() = 0; // ??

    protected:
        protocol::Component *system;

   };


#endif
