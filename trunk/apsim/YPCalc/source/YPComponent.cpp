#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include <ComponentInterface\MessageDataExt.h>
#include "YPComponent.h"
#include <general\math_functions.h>
#include <general\stl_functions.h>
#pragma package(smart_init)
using namespace std;
using namespace protocol;

// ------------------------------------------------------------------
// Return a blank string when requested to indicate that we
// don't need a wrapper DLL.
// ------------------------------------------------------------------
extern "C" _export void __stdcall wrapperDLL(char* wrapperDll)
   {
   strcpy(wrapperDll, "");
   }
extern "C" void __stdcall getDescriptionInternal(char* initScript,
                                                 char* description);
// ------------------------------------------------------------------
// Return component description info.
// ------------------------------------------------------------------
extern "C" _export void __stdcall getDescription(char* initScript, char* description)
   {
   getDescriptionInternal(initScript, description);
   }
// ------------------------------------------------------------------
// Create an instance of the YP module
// ------------------------------------------------------------------
Component* createComponent(void)
   {
   return new YPComponent;
   }

// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
YPComponent::YPComponent(void)
   {
   }

// ------------------------------------------------------------------
// initialise the YP component - STAGE 1.
// ------------------------------------------------------------------
void YPComponent::doInit1(const protocol::Init1Data& initData)
   {
   Component::doInit1(initData);
   static const char* realArrayType = "<type kind=\"double\" array=\"T\"\\>";
   static const char* realType = "<type kind=\"double\"\\>";

   cllID = addRegistration(::respondToGet,-1, "CLLToday", realType);
   dulID = addRegistration(::respondToGet,-1, "DULToday", realType);
   swID = addRegistration(::respondToGet,-1, "SWToday", realType);
   critSwID = addRegistration(::respondToGet,-1, "CritSWToday", realType);

   rootDepthID = addRegistration(::get,-1, "root_depth", realType);
   lldepID = addRegistration(::get,-1, "ll_dep", realArrayType);
   duldepID = addRegistration(::get,-1, "dul_dep", realArrayType);
   swdepID = addRegistration(::get,-1, "sw_dep", realArrayType);
   dlayerID = addRegistration(::get,-1, "dlayer", realArrayType);
   }
// ------------------------------------------------------------------
// return a variable to caller.
// ------------------------------------------------------------------
void YPComponent::respondToGet(unsigned int& fromID, QueryValueData& queryData)
   {
   getStaticVariables();
   if (queryData.ID == cllID)
      sendVariable(queryData, interpFromArray(cll));
   else if (queryData.ID == dulID)
      sendVariable(queryData, interpFromArray(dul));
   else if (queryData.ID == swID)
      {
      protocol::Variant* variant = NULL;
      bool ok = getVariable(swdepID, &variant, true);
      std::vector<double> sw;
      if (ok)
         ok = variant->unpack(sw);
      if (!ok)
         throw runtime_error("Cannot find sw_dep");
      sw = Accum(sw);
      sendVariable(queryData, interpFromArray(sw));
      }
   else if (queryData.ID == critSwID)
      {
      double llToday = interpFromArray(cll);
      double dulToday = interpFromArray(dul);
      double critSWToday = llToday + (dulToday - llToday) * 0.30;
      sendVariable(queryData, critSWToday);
      }
   }

// ------------------------------------------------------------------
// Calculate a cll value
// ------------------------------------------------------------------
double YPComponent::interpFromArray(std::vector<double>& values)
   {
   getStaticVariables();

   protocol::Variant* variant=NULL;
   bool ok = getVariable(rootDepthID, &variant, true);
   double rootDepth;
   if (ok)
      ok = variant->unpack(rootDepth);

   if (!ok)
      throw runtime_error("Cannot find a root_depth variable belonging to any module");

   if (rootDepth == 0)
      return 0.0;
   else
      {
      bool DidInterp;
      return linear_interp_real (rootDepth, Depth, values, DidInterp);
      }
   }

// ------------------------------------------------------------------
// Calculate a dlayer
// ------------------------------------------------------------------
void YPComponent::getStaticVariables()
   {
   if (cll.size() == 0)
      {
      protocol::Variant* variant = NULL;
      bool ok = getVariable(lldepID, &variant, true);
      if (ok)
         ok = variant->unpack(cll);
      if (!ok)
         throw runtime_error("Cannot find crop lower limits");
      ok = getVariable(duldepID, &variant, true);
      if (ok)
         ok = variant->unpack(dul);
      if (!ok)
         throw runtime_error("Cannot find drained upper limits");
      ok = getVariable(dlayerID, &variant, true);
      if (ok)
         ok = variant->unpack(Depth);
      if (!ok)
         throw runtime_error("Cannot find dlayer variables");

      if (cll.size() != dul.size() &&
          cll.size() != Depth.size())
         throw runtime_error("Number of layers dont match between cll, dul & dlayer");

      cll = Accum(cll);
      dul = Accum(dul);
      Depth = Accum(Depth);
      }
   }

// -----------------------------------
// Accumulate the values in the array.
// -----------------------------------
std::vector<double> YPComponent::Accum(std::vector<double>& values)
   {
   std::vector<double> returnValues;
   returnValues.push_back(0.0);
   double ValueSoFar = 0.0;
   for (unsigned i = 0; i != values.size(); i++)
      {
      ValueSoFar += values[i];
      returnValues.push_back(ValueSoFar);
      }
   return returnValues;
   }
