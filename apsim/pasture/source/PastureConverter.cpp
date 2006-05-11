#include <general\pch.h>
#include <vcl.h>
#include <boost/function.hpp>
#pragma hdrstop

#include <math.h>
#include <string>
#include <strstream>
#include <iomanip.h>

#include <general/string_functions.h>
#include <general/stl_functions.h>
#include <ApsimShared/FStringExt.h>
#include <ComponentInterface/MessageDataExt.h>
#include <ComponentInterface/ApsimVariant.h>

#include <ComponentInterface/Component.h>
#include <ComponentInterface/DataTypes.h>
#include "PastureConverter.h"


#pragma package(smart_init)
using namespace std;


#define doubleArrayTypeDDML "<type  array=\"T\" kind=\"double\"/>"
#define singleArrayTypeDDML "<type  array=\"T\" kind=\"single\"/>"

      const float kg2g = 1000.0 ;
      const float ha2sm = 10000.0 ;
      const float g2kg = 1.0/kg2g ;
      const float sm2ha = 1.0/ha2sm ;
      const float cmol2mol = 1.0/100.0 ;
      const float mm2m = 1.0/1000.0;

      inline bool floatsAreEqual(float A, float B, float C) {return(fabs(A-B)<C);}
      float divide (float dividend, float divisor, float default_value);

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
// Create an instance of the science converter module
// ------------------------------------------------------------------
protocol::Component* createComponent(void)
//===========================================================================
   {
   return new PastureConverter;
   }
// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
//===========================================================================
PastureConverter::PastureConverter(void)
   {
   }
// ------------------------------------------------------------------
// Destructor
// ------------------------------------------------------------------
PastureConverter::~PastureConverter(void)
//===========================================================================
   {
   }
// ------------------------------------------------------------------
// Init1 phase.
// ------------------------------------------------------------------
void PastureConverter::doInit1(const FString& sdml)
//===========================================================================
   {
   protocol::Component::doInit1(sdml);
   sandID = addRegistration(RegistrationType::respondToGet, "sand", doubleArrayTypeDDML);
   vpdID = addRegistration(RegistrationType::respondToGet, "vpd", singleTypeDDML);
   swLayerID = addRegistration(RegistrationType::respondToGet, "sw_layer", soillayersTypeDDML);
   maxtID = addRegistration(RegistrationType::get, "maxt", singleTypeDDML);
   mintID = addRegistration(RegistrationType::get, "mint", singleTypeDDML);
   rainID = addRegistration(RegistrationType::get, "rain", singleTypeDDML);
   radnID = addRegistration(RegistrationType::get, "radn", singleTypeDDML);
   windID = addRegistration(RegistrationType::get, "wind", singleTypeDDML);
   weatherID = addRegistration(RegistrationType::respondToGet, "weather", pastureweatherTypeDDML);
   dlayerID = addRegistration(RegistrationType::get, "dlayer", singleArrayTypeDDML);
   nh4ppmID = addRegistration(RegistrationType::get, "nh4ppm", singleArrayTypeDDML);
   no3ppmID = addRegistration(RegistrationType::get, "no3ppm", singleArrayTypeDDML);
   nh4_ppmID = addRegistration(RegistrationType::respondToGet, "nh4_ppm", doubleArrayTypeDDML);
   no3_ppmID = addRegistration(RegistrationType::respondToGet, "no3_ppm", doubleArrayTypeDDML);
   sowID = addRegistration(RegistrationType::event, "sow", pasturesowTypeDDML);
   sowPastureID = addRegistration(RegistrationType::respondToEvent, "sowpasture", singleTypeDDML);
//   cutID = addRegistration(RegistrationType::event, "cultivate", pasturecutTypeDDML);
//   conserveID = addRegistration(RegistrationType::event, "conserve", pastureConserveTypeDDML);
//   killID = addRegistration(RegistrationType::event, "kill", pastureKillTypeDDML);
//   residueAddedID = addRegistration(RegistrationType::event, "residue_added", residueAddedTypeDDML);
   prepareID = addRegistration(RegistrationType::respondToEvent, "prepare", "");
   initStepID = addRegistration(RegistrationType::event, "init_step", "");
   doPastureWaterID = addRegistration(RegistrationType::event, "do_pasture_water", "");
   processID = addRegistration(RegistrationType::respondToEvent, "process", "");
   doPastureGrowthID = addRegistration(RegistrationType::event, "do_pasture_growth", "");
   preWaterBalanceID = addRegistration(RegistrationType::event, "prewaterbalance", "");
   postID = addRegistration(RegistrationType::respondToEvent, "post", "");
   endStepID = addRegistration(RegistrationType::event, "end_step", "");
   onUptakeID = addRegistration(RegistrationType::respondToEvent, "on_uptake", pasturenutrientuptakeTypeDDML);
   fomAddedID = addRegistration(RegistrationType::respondToEvent, "fom_added", fom_addedTypeDDML);
   cropwaterdemandID = addRegistration(RegistrationType::respondToEvent, "cropwaterdemand", pasturewaterdemandTypeDDML);
   cropwatersupplyID = addRegistration(RegistrationType::event, "cropwatersupply", pasturewatersupplyTypeDDML);
   incorpFOMID = addRegistration(RegistrationType::event, "incorp_fom", "");
   swDepthID = addRegistration(RegistrationType::get, "sw_dep", singleArrayTypeDDML);
   swID = addRegistration(RegistrationType::get, "sw", singleArrayTypeDDML);
   ll15DepthID = addRegistration(RegistrationType::get, "ll15_dep", singleArrayTypeDDML);
   dltSWDepthID = addRegistration(RegistrationType::set,"dlt_sw_dep", singleArrayTypeDDML);
   dltNO3ID = addRegistration(RegistrationType::set,"dlt_no3", singleArrayTypeDDML);
   dltNH4ID = addRegistration(RegistrationType::set,"dlt_nh4", singleArrayTypeDDML);
   dltPOxID = addRegistration(RegistrationType::set,"dlt_pox", singleArrayTypeDDML);
   dltSO4ID = addRegistration(RegistrationType::set,"dlt_so4", singleArrayTypeDDML);

   }
// ------------------------------------------------------------------
// Init2 phase.
// ------------------------------------------------------------------
void PastureConverter::doInit2(void)
//===========================================================================
   {
   readParameters (); // Read constants
   }

// ------------------------------------------------------------------
// Event handler.
// ------------------------------------------------------------------
void PastureConverter::respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
//===========================================================================
{
   if (eventID == prepareID)
      doPrepare(fromID, eventID, variant);
   else if (eventID == processID)
      doProcess(fromID, eventID, variant);
   else if (eventID == postID)
      doPost(fromID, eventID, variant);
//   else if (eventID == cropwaterdemandID)
//      doCropWaterUptake(fromID, eventID, variant);
   else if (eventID == onUptakeID)
      doCropNutrientUptake(fromID, eventID, variant);
   else if (eventID == fomAddedID)
      doAddFOM(fromID, eventID, variant);
   else if (eventID == sowPastureID)
      dosowPasture(fromID, eventID, variant);
   else
   {} //not interested an other events

}
// ------------------------------------------------------------------
void PastureConverter::doPrepare(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
//===========================================================================
{
   char* null = "";
//   publish (initStepID, null);
//   publish (doPastureWaterID, null);
}
// ------------------------------------------------------------------
void PastureConverter::doProcess(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
//===========================================================================
{
   char* null = "";
//   publish (doPastureGrowthID, null);
//   publish (preWaterBalanceID, null);
}
// ------------------------------------------------------------------
void PastureConverter::doPost(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
//===========================================================================
{
   char* null = "";
//   publish (endStepID, null);
}
// ------------------------------------------------------------------
void PastureConverter::dosowPasture(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
//===========================================================================
{
   protocol::pasturesowType pastureSow;

    double   value;

    protocol::ApsimVariant incomingApsimVariant(this);
    incomingApsimVariant.aliasTo(variant.getMessageData());

    if (incomingApsimVariant.get("rate", protocol::DTdouble, false, value) == true)
    {
         pastureSow.rate = value;

         ostringstream msg;
         msg << "Pasture Sow rate = " << value << " (kg/ha)" << ends;
         writeString (msg.str().c_str());
    }
    else
     pastureSow.rate = 0.0;

   publish (sowID, pastureSow);
}
// ------------------------------------------------------------------
void PastureConverter::doCropNutrientUptake(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
//===========================================================================
{
   protocol::pasturenutrientuptakeType nutrientUptake;

   variant.unpack(nutrientUptake);
   int numLayers = nutrientUptake.layers.size();

   if (cDebug == "on")
   {
      float uptakeTotal = 0.0;
      ostrstream msg;
      msg << nutrientUptake.nutrient << endl;
      for (int layer = 0; layer < numLayers; layer++)
      {
         msg << "   layer " << layer+1 << " = " << nutrientUptake.uptake[layer]  << " (kg/ha)" << endl;
         uptakeTotal +=  nutrientUptake.uptake[layer];
      }

      msg << endl << "   Uptake total = " << uptakeTotal << " (kg/ha)" << endl << ends;

      writeString (msg.str());
   }

     protocol::vector<float> dltNutrient;
     for (int layer = 0; layer < numLayers; layer++)
         dltNutrient.push_back(nutrientUptake.uptake[layer]);

     if (nutrientUptake.nutrient == "no3")
         setVariable(dltNO3ID, dltNutrient);
     else if (nutrientUptake.nutrient == "nh4")
         setVariable(dltNH4ID, dltNutrient);
     else if (nutrientUptake.nutrient == "so4")
         setVariable(dltSO4ID, dltNutrient);
     else if (nutrientUptake.nutrient == "pox")
         setVariable(dltPOxID, dltNutrient);
}
// ------------------------------------------------------------------
void PastureConverter::doCropWaterUptake(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
//===========================================================================
{
   // get demand by plant
   // get supply in each layer (sw_dep - ll_15) = esw
   // satisify demand proportionately to rlv in each layer down to root depth

   protocol::pasturewaterdemandType waterDemand;

   variant.unpack(waterDemand);
   if (cDebug == "on")
   {
      ostrstream msg1;
      msg1 << endl << "Water demand:-" << endl;
      float demandTotal = 0.0;

      for (unsigned int subpopulation = 0; subpopulation < waterDemand.demands.size(); subpopulation++)
      {
         msg1 << "   sub-population " << subpopulation+1 << "(" << waterDemand.demands[subpopulation].crop_ident << "-" << waterDemand.demands[subpopulation].crop_type << ") = " << waterDemand.demands[subpopulation].demand  << " (mm)" << endl;
         demandTotal +=  waterDemand.demands[subpopulation].demand;
      }

      msg1 << "   Demand total = " << demandTotal << " (mm)" << endl << ends;

      writeString (msg1.str());
   }
//         sendWaterUptake(queryData);

      protocol::Variant* variantGet;
      vector <float> swDepth;
      vector <float> ll15Depth;

      bool ok = getVariable(swDepthID, variantGet, true);
      if (ok)
      {
//         vector <float> swDepth;
         bool ok = variantGet->unpack(swDepth);
//         bool ok = variant.unpack(swDepth);
         if (ok && swDepth.size() >= 1)
         {
               // ok
         }
         else
         {
            throw std::runtime_error("Couldn't unpack sw_dep");
         }
      }
      else
      {
         throw std::runtime_error("Couldn't get variable sw_dep");
      }

      ok = getVariable(ll15DepthID, variantGet, true);
      if (ok)
      {
//         vector <float> ll15Depth;
         bool ok = variantGet->unpack(ll15Depth);
         if (ok && ll15Depth.size() >= 1)
         {
               // ok
         }
         else
         {
            throw std::runtime_error("Couldn't unpack ll15_dep");
         }
      }
      else
      {
         throw std::runtime_error("Couldn't get variable ll15_dep");
      }

      int numLayers = ll15Depth.size();

      protocol::pasturewatersupplyType waterUptake;
      protocol::suppliesType supply;
      float demand_tot = 0.0;
      float rlv_tot = 0.0;
      for (unsigned int subpopulation = 0; subpopulation < waterDemand.demands.size(); subpopulation++)
      {
         demand_tot += waterDemand.demands[subpopulation].demand;
         for (unsigned int layer = 0; layer < waterDemand.demands[subpopulation].rlv_layer.layers.size(); layer++)
            rlv_tot += waterDemand.demands[subpopulation].rlv_layer.rlv[layer];
      }
      if (rlv_tot <= 0.0)
         rlv_tot = 1.0;

      ostrstream msg2;
      if (cDebug == "on")
         msg2 << endl << "Water supply:-" << endl;
      float supplyTotal = 0.0;

      for (unsigned int subpopulation = 0; subpopulation < waterDemand.demands.size(); subpopulation++)
      {
         supply.crop_ident = waterDemand.demands[subpopulation].crop_ident;
         msg2 << "   sub-population " << subpopulation+1 << "(" << supply.crop_ident << ")" << endl;

         for (unsigned int layer = 0; layer < waterDemand.demands[subpopulation].rlv_layer.layers.size(); layer++)
         {
            supply.layers.push_back(waterDemand.demands[subpopulation].rlv_layer.layers[layer]);
            double wSupply =  (waterDemand.demands[subpopulation].rlv_layer.rlv[layer] / rlv_tot) * demand_tot;
            supply.supply.push_back(wSupply);

            if (cDebug == "on")
               msg2 << "   Layer (" << layer << ") = " << wSupply  << " (mm)" << " RLV = " << waterDemand.demands[subpopulation].rlv_layer.rlv[layer] << " Thickness = " << waterDemand.demands[subpopulation].rlv_layer.layers[layer] << endl;
            supplyTotal +=  wSupply;

         }
         waterUptake.supplies.push_back(supply);
      }
      if (cDebug == "on")
      {
         msg2 << "   Supply total = " << supplyTotal << " (mm)" << endl << ends;

         writeString (msg2.str());
      }


//     publish (cropwatersupplyID, waterUptake);

     protocol::vector<float> dltSWDepth;
     for (unsigned int layer = 0; layer < numLayers; layer++)  //FIXME to remove water from soilwat?
     {
         float dltSWDep = -supply.supply[layer];
         dltSWDepth.push_back(dltSWDep);
     }

//     setVariable(dltSWDepthID, dltSWDepth);


}

// ------------------------------------------------------------------
void PastureConverter::doAddFOM(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
//===========================================================================
{
   protocol::fom_addedType FOMAdded;

   variant.unpack(FOMAdded);
   int numLayers = FOMAdded.layers.size();
   float weightTotal = 0.0;
   float NTotal = 0.0;
   float PTotal = 0.0;
   float STotal = 0.0;
   float ashAlkTotal = 0.0;

   if (cDebug == "on")
   {
      ostrstream msg;
      msg << "FOM Added:" << endl;
      for (int layer = 0; layer < numLayers; layer++)
      {
         msg << "   layer (" << layer+1 << "): ";
         msg << "weight = " << FOMAdded.fom[layer].weight  << " (kg/ha); ";
         msg << "N = " << FOMAdded.fom[layer].n  << " (kg/ha); ";
         msg << "P = " << FOMAdded.fom[layer].p  << " (kg/ha); ";
         msg << "S = " << FOMAdded.fom[layer].s  << " (kg/ha); ";
         msg << "ash_alk = " << FOMAdded.fom[layer].ash_alk  << " (mol/ha) " << endl;
         weightTotal +=  FOMAdded.fom[layer].weight;
         NTotal +=  FOMAdded.fom[layer].n;
         PTotal +=  FOMAdded.fom[layer].p;
         STotal +=  FOMAdded.fom[layer].s;
         ashAlkTotal +=  FOMAdded.fom[layer].ash_alk;
      }

      msg << endl << "  Totals: ";
      msg << "weight = " << weightTotal << " (kg/ha); ";
      msg << "N = " << NTotal << " (kg/ha); ";
      msg << "P = " << PTotal << " (kg/ha); ";
      msg << "S = " << STotal << " (kg/ha); ";
      msg << "Ash Alk = " << ashAlkTotal << " (mol/ha)" << endl << ends;

      writeString (msg.str());
   }

   protocol::vector<float> dltDMincorp;
   protocol::vector<float> dltNincorp;
   protocol::vector<float> dltPincorp;

   for (int layer = 0; layer < numLayers; layer++)
   {
      dltDMincorp.push_back(FOMAdded.fom[layer].weight);
      dltNincorp.push_back(FOMAdded.fom[layer].n);
      dltPincorp.push_back(FOMAdded.fom[layer].p);
   }
   protocol::ApsimVariant incorpFOM(this);

   incorpFOM.store("dlt_fom_type", protocol::DTstring, false, FString("pasture"));
   incorpFOM.store("dlt_fom_wt", protocol::DTsingle, true, dltDMincorp);
   incorpFOM.store("dlt_fom_n", protocol::DTsingle, true, dltNincorp);
   incorpFOM.store("dlt_fom_p", protocol::DTsingle, true, dltPincorp);

   publish (incorpFOMID, incorpFOM);

}


// ------------------------------------------------------------------
// return a variable to caller.  Return true if we own variable.
// ------------------------------------------------------------------
void PastureConverter::respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData)
//===========================================================================
{
     // sand_layer
   if (queryData.ID == sandID) sendSand(queryData);
   else if (queryData.ID == vpdID) sendVPD(queryData);
   else if (queryData.ID == nh4_ppmID) sendNH4(queryData);
   else if (queryData.ID == no3_ppmID) sendNO3(queryData);
   else if (queryData.ID == weatherID) sendWeather(queryData);
   else if (queryData.ID == swLayerID) sendSWLayer(queryData);

   else
   {   // don't respond to any other gets.
   }
}

void PastureConverter::readParameters ( void )
//===========================================================================
   {
   const char*  section_name = "parameters" ;
   float sandLayer[100];

   writeString (" - reading parameters");

    cDebug = readParameter (section_name, "debug");
    readParameter (section_name, "sand", sandLayer, numLayers, 0.0, 1.0);
    readParameter (section_name,"svp_fract", cSVPFract, 0.0, 1.0);

   for (int layer = 0; layer < numLayers; layer++)
      pSandLayer.push_back(sandLayer[layer]);

   ostringstream msg;
   msg << "sand (kg/kg) = ";
   for (unsigned int layer = 0; layer < pSandLayer.size(); layer++)
      msg << pSandLayer[layer] << " ";
   msg << endl << ends;
   writeString (msg.str().c_str());
   }

void PastureConverter::sendSand (protocol::QueryValueData& queryData)
//===========================================================================
{
   vector <double> sandLayers;
//   for (int layer = 0; layer != numLayers; layer++)
   for (unsigned int layer = 0; layer != pSandLayer.size(); layer++)
      sandLayers.push_back(pSandLayer[layer]);

   if (cDebug == "on")
   {
      ostringstream msg;
      msg << "send sand (kg/kg) = ";
      for (unsigned int layer = 0; layer < pSandLayer.size(); layer++)
         msg << pSandLayer[layer] << " ";
      msg << endl << ends;
      writeString (msg.str().c_str());
   }

   sendVariable(queryData, sandLayers);
}

void PastureConverter::sendNH4 (protocol::QueryValueData& queryData)
//==========================================================================
{
      protocol::Variant* variantNH4;
      bool okNH4 = getVariable(nh4ppmID, variantNH4, true);
      if (okNH4)
      {
         vector <float> nh4ppm;
         bool ok = variantNH4->unpack(nh4ppm);  // what happens if this is not ok?
         vector <double> nh4_ppm;
         for (unsigned int layer = 0; layer < nh4ppm.size(); layer++)
            nh4_ppm.push_back(static_cast<double>(nh4ppm[layer]));

         sendVariable(queryData, nh4_ppm);
////
////         ostringstream msg;
////         msg << "send nh4_ppm (mg/kg) = ";
////         for (int layer = 0; layer < nh4ppm.size(); layer++)
////            msg << nh4ppm[layer] << " ";
////         msg << endl << ends;
////         writeString (msg.str().c_str());

      }
      else
      {   // didn't get the nh4ppm ID ok. Do nothing about it.
////         ostringstream msg;
////         msg << "nh4_ppm didn't get ";
////         msg << endl << ends;
////         writeString (msg.str().c_str());
      }
}

void PastureConverter::sendNO3 (protocol::QueryValueData& queryData)
//==========================================================================
{
      protocol::Variant* variantNO3;
      bool okNO3 = getVariable(no3ppmID, variantNO3, true);
      if (okNO3)
      {
         vector <float> no3ppm;
         bool ok = variantNO3->unpack(no3ppm);  // what happens if this is not ok?
         vector <double> no3_ppm;
         for (unsigned int layer = 0; layer < no3ppm.size(); layer++)
            no3_ppm.push_back(static_cast<double>(no3ppm[layer]));
         sendVariable(queryData, no3_ppm);

      }
      else
      {   // didn't get the no3ppm ID ok. Do nothing about it.
      }
}

void PastureConverter::sendSWLayer (protocol::QueryValueData& queryData)
//==========================================================================
{
      protocol::Variant* variantDLayer;
      bool okDLayer = getVariable(dlayerID, variantDLayer, true);
      if (okDLayer)
      {
         vector <float> dlayer;
         bool ok = variantDLayer->unpack(dlayer);  // what happens if this is not ok?

         protocol::Variant* variantsw;
         bool oksw = getVariable(swID, variantsw, true);
         if (oksw)
         {
            vector <float> sw;
            bool ok = variantsw->unpack(sw);  // what happens if this is not ok?
            protocol::soillayersType sw_depth;
            for (unsigned int layer = 0; layer < sw.size(); layer++)
            {
               sw_depth.layers.push_back(static_cast<double>(dlayer[layer]));
               sw_depth.value.push_back(static_cast<double>(sw[layer]));
            }
            sendVariable(queryData, sw_depth);

         }
         else
         {   // didn't get the sw_depth ID ok. Do nothing about it.
         }
      }
      else
      {   // didn't get the dlayer ID ok. Do nothing about it.
      }
}

void PastureConverter::sendVPD (protocol::QueryValueData& queryData)
//==========================================================================
{
      protocol::Variant* variantMaxT;
      bool okMaxt = getVariable(maxtID, variantMaxT, true);
      if (okMaxt)
      {
         protocol::Variant* variantMinT;
         bool ok = getVariable(mintID, variantMinT, true);
         if (ok)
         {
         float maxt;
         bool ok = variantMaxT->unpack(maxt);  // what happens if this is not ok?
         float mint;
         ok = variantMinT->unpack(mint);  // what happens if this is not ok?
         float VPD = vpd(cSVPFract, maxt, mint);

         sendVariable(queryData, VPD);
         }
      }
      else
      {   // didn't get the maxT ID ok. Do nothing about it.
      }
}

void PastureConverter::sendWeather (protocol::QueryValueData& queryData)
//==========================================================================
{
      protocol::Variant* variantMaxT;
      bool okMaxt = getVariable(maxtID, variantMaxT, true);
      if (okMaxt)
      {
         protocol::Variant* variantMinT;
         bool ok = getVariable(mintID, variantMinT, true);
         if (ok)
         {
            protocol::Variant* variantRain;
            bool ok = getVariable(rainID, variantRain, true);
            if (ok)
            {
               protocol::Variant* variantRadn;
               bool ok = getVariable(radnID, variantRadn, true);
               if(ok)
               {
                  float rain;
                  bool ok = variantRain->unpack(rain);  // what happens if this is not ok?
                  float radn;
                  ok = variantRadn->unpack(radn);  // what happens if this is not ok?
                  float maxt;
                  ok = variantMaxT->unpack(maxt);  // what happens if this is not ok?
                  float mint;
                  ok = variantMinT->unpack(mint);  // what happens if this is not ok?
                  float VPD = vpd(cSVPFract, maxt, mint);

                  protocol::pastureweatherType weather;
                  weather.maxt = static_cast<double>(maxt);
                  weather.mint = static_cast<double>(mint);
                  weather.rain = static_cast<double>(rain);
                  weather.radn = static_cast<double>(radn);
                  weather.vpd = static_cast<double>(VPD);

                  sendVariable(queryData, weather);
               }
            }

         }
      }
      else
      {   // didn't get the maxT ID ok. Do nothing about it.
      }
}

float PastureConverter::vpd(float cSVPFract, float maxt, float mint) //(INPUT)
//==========================================================================
{
      float vpd = max (cSVPFract * ( svp(maxt) - svp(mint)), 0.01);
      return vpd;
}

//==========================================================================
float PastureConverter::svp(float temp) //(INPUT)  fraction of distance between svp at mi
//==========================================================================
/*  Purpose
*
*  Mission Statement
*    function to get saturation vapour pressure for a given temperature in oC (kpa)
*
*  Changes
*       21/5/2003 ad converted to BC++
*
*/
   {
      const double ES0 = 6.1078;            // Teten coefficients -SATURATION VAPOR PRESSURE (MB) OVER WATER AT 0C
      const double TC_B = 17.269388;        // Teten coefficients
      const double TC_C = 237.3;            // Teten coefficients
      const float mb2kpa = 100.0/1000.0;    // convert pressure mbar to kpa 1000 mbar = 100 kpa

   float val = ES0 * exp(TC_B * temp / (TC_C + temp)) * mb2kpa;
   return val;
   }

//===========================================================================
float PastureConverter::divide (float dividend, float divisor, float default_value)
//===========================================================================

/*Definition
 *   Returns (dividend / divisor) if the division can be done
 *   without overflow or underflow.  If divisor is zero or
 *   overflow would have occurred, a specified default is returned.
 *   If underflow would have occurred, zero is returned.
 *Assumptions
 *   largest/smallest real number is 1.0e+/-30
 *Parameters
 *   dividend:     dividend
 *   divisor:      divisor
 *   defaultValue: default value to return if overflow
 *Calls
 *   reals_are_equal
 */

   {
   //Constant Values
   const float LARGEST = 1.0e30;    //largest acceptable no. for quotient
   const float SMALLEST = 1.0e-30;  //smallest acceptable no. for quotient
   const float nought = 0.0;
   const float one = 1.0;
   const float granularity = 1.0e-6;

   //Local Varialbes
   float quotient;

   //Implementation
   if(floatsAreEqual(dividend, nought, granularity))      //multiplying by 0
      {
      quotient = nought;
      }
   else if(floatsAreEqual(divisor, nought, granularity))  //dividing by 0
      {
      quotient = default_value;
      }
   else if(fabs(divisor) < one)            //possible overflow
      {
      if(fabs(dividend) > fabs(LARGEST * divisor)) //overflow
         {
         quotient = default_value;
         }
      else
         {
         quotient = dividend / divisor;          //ok
         }
      }
   else if(fabs(divisor) > one)             //possible underflow
      {
      if(fabs(dividend) < fabs(SMALLEST * divisor))    //underflow
         {
         quotient = nought;
         }
      else
         {
         quotient = dividend / divisor;                //ok
         }
      }
   else
      {
      quotient = dividend / divisor;                   //ok
      }
   return quotient;
   }





