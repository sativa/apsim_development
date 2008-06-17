#include <general\pch.h>
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
      std::vector<double> sw;
      getVariable(swdepID, sw, 0.0, 10000.0, false);
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

   double rootDepth;
   getVariable(rootDepthID, rootDepth, 0.0, 10000.0, false);

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
      getVariable(lldepID, cll, 0.0, 10000.0, false);
      getVariable(duldepID, dul, 0.0, 10000.0, false);
      getVariable(dlayerID, Depth, 0.0, 10000.0, false);

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
