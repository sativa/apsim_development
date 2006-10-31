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
#define singleTypeDDML "<type  kind=\"single\"/>"

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
   swLayerID = addRegistration(RegistrationType::respondToGet, "sw_layer", protocol::DDML(protocol::soillayersType()).c_str());
   maxtID = addRegistration(RegistrationType::get, "maxt", singleTypeDDML);
   mintID = addRegistration(RegistrationType::get, "mint", singleTypeDDML);
   rainID = addRegistration(RegistrationType::get, "rain", singleTypeDDML);
   radnID = addRegistration(RegistrationType::get, "radn", singleTypeDDML);
   windID = addRegistration(RegistrationType::get, "wind", singleTypeDDML);
   weatherID = addRegistration(RegistrationType::respondToGet, "weather", protocol::DDML(protocol::pastureweatherType()).c_str());
   dlayerID = addRegistration(RegistrationType::get, "dlayer", singleArrayTypeDDML);
   nh4ppmID = addRegistration(RegistrationType::get, "nh4ppm", singleArrayTypeDDML);
   no3ppmID = addRegistration(RegistrationType::get, "no3ppm", singleArrayTypeDDML);
   nh4_ppmID = addRegistration(RegistrationType::respondToGet, "nh4_ppm", doubleArrayTypeDDML);
   no3_ppmID = addRegistration(RegistrationType::respondToGet, "no3_ppm", doubleArrayTypeDDML);
   sowID = addRegistration(RegistrationType::event, "sow", protocol::DDML(protocol::pasturesowType()).c_str());
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
   onUptakeID = addRegistration(RegistrationType::respondToEvent, "on_uptake", protocol::DDML(protocol::pasturenutrientuptakeType()).c_str());
   fomAddedID = addRegistration(RegistrationType::respondToEvent, "fom_added", protocol::DDML(protocol::fom_addedType()).c_str());
   newProfileID = addRegistration(RegistrationType::respondToEvent, "new_profile", protocol::DDML(protocol::new_profileType()).c_str());
   cropwaterdemandID = addRegistration(RegistrationType::respondToEvent, "cropwaterdemand", protocol::DDML(protocol::pasturewaterdemandType()).c_str());
   cropwatersupplyID = addRegistration(RegistrationType::event, "cropwatersupply", protocol::DDML(protocol::pasturewatersupplyType()).c_str());
   incorpFOMID = addRegistration(RegistrationType::event, "incorp_fom", "");
   swDepthID = addRegistration(RegistrationType::get, "sw_dep", singleArrayTypeDDML);
   waterInfoID = addRegistration(RegistrationType::get, "water_info", protocol::DDML(protocol::waterinfoType()).c_str());
   rtDepID = addRegistration(RegistrationType::get, "rtdep", singleTypeDDML);
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
   else if (eventID == cropwaterdemandID)
      doCropWaterUptake(fromID, eventID, variant);
   else if (eventID == onUptakeID)
      doCropNutrientUptake(fromID, eventID, variant);
   else if (eventID == fomAddedID)
      doAddFOM(fromID, eventID, variant);
   else if (eventID == sowPastureID)
      dosowPasture(fromID, eventID, variant);
   else if (eventID == newProfileID)
      doNewProfile(fromID, eventID, variant);
   else
   {} //not interested an other events

}
// ------------------------------------------------------------------
void PastureConverter::doPrepare(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
//===========================================================================
{
//   char* null = "";
//   publish (initStepID, null);
//   publish (doPastureWaterID, null);
}
// ------------------------------------------------------------------
void PastureConverter::doProcess(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
//===========================================================================
{
//   char* null = "";
//   publish (doPastureGrowthID, null);
//   publish (preWaterBalanceID, null);
}
// ------------------------------------------------------------------
void PastureConverter::doPost(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
//===========================================================================
{
//   char* null = "";
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
void PastureConverter::doNewProfile(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
//===========================================================================
{
    protocol::ApsimVariant av(this);
    av.aliasTo(variant.getMessageData());
    protocol::vector<float> scratch;
    av.get("dlayer", protocol::DTsingle, true, scratch);

    unsigned int num_layers = scratch.size();

//    vector<float> dlayer;
    for (unsigned layer = 0; layer != scratch.size(); layer++)
       {
//       dlayer.push_back(scratch[layer]);
         dlayer[layer] = scratch[layer];
       }

    av.get("ll15_dep", protocol::DTsingle, true, scratch);
    for (unsigned layer = 0; layer != num_layers; layer++) { ll15_dep[layer] = scratch[layer]; }
    av.get("dul_dep", protocol::DTsingle, true, scratch);
    for (unsigned layer = 0; layer != num_layers; layer++) { dul_dep[layer] = scratch[layer]; }
    av.get("sat_dep", protocol::DTsingle, true, scratch);
    for (unsigned layer = 0; layer != num_layers; layer++) { sat_dep[layer] = scratch[layer]; }
    av.get("sw_dep", protocol::DTsingle, true, scratch);
    for (unsigned layer = 0; layer != num_layers; layer++) { sw_dep[layer] = scratch[layer]; }
    av.get("bd", protocol::DTsingle, true, scratch);
    for (unsigned layer = 0; layer != num_layers; layer++) { bd[layer] = scratch[layer]; }

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


      float rtDep;
      protocol::Variant* rootDepth;
      bool okRtDep = getVariable(rtDepID, rootDepth, true);
      if (okRtDep)
      {
         bool ok = rootDepth->unpack(rtDep);  // what happens if this is not ok?
      }
      else
      {   // didn't get the rtDep ID ok. Do nothing about it.
      }

      for (unsigned layer = 0; layer != max_layer; layer++)
      {
         ll_dep[layer] = dlayer[layer] * ll[layer];
         sw_dep[layer] = swDepth[layer];
      }

      float swSupply[max_layer];
      float dltSWDepth[max_layer];

      fill_real_array (swSupply, 0.0, max_layer);
      fill_real_array (dltSWDepth, 0.0, max_layer);

      pasture_sw_supply (max_layer, dlayer, rtDep, sw_dep, kl, ll_dep, swSupply);

      protocol::pasturewatersupplyType waterUptake;
      protocol::suppliesType supply;
      float demand_tot = 0.0;
      float rlv_tot = 0.0;
      int num_layers = 0;

      ostrstream msg2;
      if (cDebug == "on")
         msg2 << endl << "Water supply:-" << endl;
      float supplyTotal = 0.0;


      for (unsigned int subpopulation = 0; subpopulation < waterDemand.demands.size(); subpopulation++)
      {
         supply.crop_ident = waterDemand.demands[subpopulation].crop_ident;
         msg2 << "   sub-population " << subpopulation+1 << "(" << supply.crop_ident << ")" << endl;

         int deepest_layer = waterDemand.demands[subpopulation].rlv_layer.layers.size();
         num_layers = max(num_layers, deepest_layer);
         pasture_sw_uptake1 (deepest_layer, dlayer, waterDemand.demands[subpopulation].demand, swSupply, dlt_sw_dep);

         supply.supply.clear();
         for (unsigned int layer = 0; layer < deepest_layer; layer++)
         {
            swSupply[layer] += dlt_sw_dep[layer];
            dltSWDepth[layer] += dlt_sw_dep[layer];

            supply.layers.push_back(dlayer[layer]);
            double wSupply =  -1.0 * dlt_sw_dep[layer];
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


     publish (cropwatersupplyID, waterUptake);


     protocol::vector<float> dltSWDepthValues(dltSWDepth, dltSWDepth+num_layers);
     setVariable(dltSWDepthID, dltSWDepthValues);
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
   msg << endl;

   readParameter (section_name, "ll", ll, numLayers, 0.0, 1.0);
   msg << "ll (mm/mm) = ";
   for (unsigned int layer = 0; layer < numLayers; layer++)
      msg << ll[layer] << " ";
   msg << endl;

   readParameter (section_name, "kl", kl, numLayers, 0.0, 1.0);
   msg << "kl (0-1) = ";
   for (unsigned int layer = 0; layer < numLayers; layer++)
      msg << kl[layer] << " ";
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
void PastureConverter::fill_real_array (float *var  //(OUTPUT) array to set
                                       , float value //(IN) scalar value to set array to
                                       , int limit)   //(IN) number of elements
//===========================================================================

/*Purpose
 *   sets real array var to value up to level limit
 */

   {
   for (int indx = 0; indx < limit; indx++)
      {
      var[indx] = value;
      }
   }

//===========================================================================
int PastureConverter::find_layer_no(float depth, const vector<float> &dlayer )
//===========================================================================
   {
   float progressive_sum = 0.0; //cumulative sum_of
   unsigned int indx;                    //index count_of_real_vals

   for(indx = 0; indx < dlayer.size(); indx++)
      {
      progressive_sum +=  dlayer[indx];
      if(progressive_sum >= depth)
         {
         break;
         }
      }
   if (indx==dlayer.size()) return (indx - 1); // last element in array
   return indx;                                // index of
   }

//===========================================================================
int PastureConverter::find_layer_no(float cum_sum   //sum_of to be found
                                   , float *array    //array to be searched
                                   , int size_of)     // size of the array
//===========================================================================

/*Purpose
 *   Find the first element of an array where a given value
 *   is contained with the cumulative sum_of of the elements.
 *   If sum_of is not reached by the end of the array, then it
 *   is ok to set it to the last element. This will take
 *   account of the case of the number of levels being 0.
 *Definition
 *   Returns ndx where ndx is the smallest value in the range
 *   1.."size_of" such that the sum of "array"(j), j=1..ndx is
 *   greater than or equal to "cum_sum".  If there is no such
 *   value of ndx, then "size_of" will be returned.
 */

   {
   float progressive_sum = 0.0; //cumulative sum_of
   int indx;                    //index count_of_real_vals

   for(indx = 0; indx < size_of; indx++)
      {
      progressive_sum = progressive_sum + array[indx];
      if(progressive_sum >= cum_sum)
         {
         break;
         }
      }
   if (indx==size_of) return (indx - 1); // last element in array
   return indx;                          // index of
   }

//===========================================================================
float PastureConverter::root_proportion (int    layer              // (INPUT) layer to look at
                      , float *dlayr              // (INPUT) array of layer depths
                      , float  root_depth)         // (INPUT) depth of roots
//===========================================================================

 /* Purpose
 *       returns the proportion of layer that has roots in it (0-1).
 *
 *  Definition
 *     Each element of "dlayr" holds the height of  the
 *     corresponding soil layer.  The height of the top layer is
 *     held in "dlayr"(1), and the rest follow in sequence down
 *     into the soil profile.  Given a root depth of "root_depth",
 *     this function will return the proportion of "dlayr"("layer")
 *     which has roots in it  (a value in the range 0..1).
 *
 * Changes
 *     21/5/2003 ad converted to BC++
 *     010994 jngh specified and programmed
 *     230698 jngh corrected to allow for layers below root zone
 *
 */
   {
   float depth_to_layer_bottom = sum_real_array (dlayr, layer+1);              // depth to bottom of layer (mm)
   float depth_to_layer_top = depth_to_layer_bottom - dlayr[layer];            // depth to top of layer (mm)
   float depth_to_root  = min(depth_to_layer_bottom, root_depth);              // depth to root in layer (mm)
   float depth_of_root_in_layer = max(0.0, depth_to_root-depth_to_layer_top);  // depth of root within layer (mm)

   return (divide (depth_of_root_in_layer, dlayr[layer], 0.0));
   }

//===========================================================================
float PastureConverter::sum_real_array (float *var,  // INPUT array to be summed
                      int nelem)   // number of elements
//===========================================================================

/*Definition
 *   Returns sum of all "limit" elements of array "var"[0:limit].
 */

   {
   float total = 0.0; // summary result

   for (int i = 0; i < nelem; i++)
      {
      total = total + var[i];
      }
   return total;
   }

//=========================================================================
void PastureConverter::pasture_sw_supply(int   num_layer        // (INPUT)  number of layers in profile
                                        , float *dlayer          // (INPUT)  thickness of soil layer I (mm)
                                        , float root_depth       // (INPUT)  depth of roots (mm)
                                        , float *sw_dep          // (INPUT)  soil water content of layer L (mm)
                                        , float *kl              // (INPUT)  root length density factor for water
                                        , float *ll_dep          // (INPUT)  lower limit of plant-extractable soi
                                        , float *sw_supply)       // (OUTPUT) potential crop water uptake from each layer (mm) (supply to roots)
//=========================================================================

/*  Purpose
*       Return potential water uptake from each layer of the soil profile
*       by the crop (mm water). This represents the maximum amount in each
*       layer regardless of lateral root distribution but takes account of
*       root depth in bottom layer.
*
*  Mission Statement
*   Calculate today's soil water supply
*
*  Notes
*      This code still allows water above dul to be taken - cnh
*
*  Changes
*       21/5/2003 ad converted to BC++
*       010994 jngh specified and programmed - adapted from barley
*       970216 slw generalised to avoid common blocks, added num_layer
*/
   {
   //  Local Variables
   int deepest_layer;      // index of deepest layer in which the roots are growing
   float sw_avail;         // water available (mm)
   // Implementation Section ----------------------------------

   // get potential uptake

   fill_real_array (sw_supply, 0.0, num_layer);

   deepest_layer = find_layer_no (root_depth, dlayer, num_layer);
   for(int i = 0; i <= deepest_layer; i++)
      {
      sw_avail = (sw_dep[i] - ll_dep[i]);
      sw_supply[i] = sw_avail * kl[i];
      sw_supply[i] = max (sw_supply[i], 0.0);
      }
   //now adjust bottom layer for depth of root
   sw_supply[deepest_layer] = sw_supply[deepest_layer] * root_proportion(deepest_layer, dlayer, root_depth);
   }

//==========================================================================
void PastureConverter::pasture_sw_uptake1(int   deepest_layer        //  (INPUT)  number of rooting layers in profile
                                          , float *dlayer          //  (INPUT)  thickness of soil layer I (mm)
                                          , float sw_demand       //  (INPUT)  total crop demand for water (mm)
                                          , float *sw_supply       //  (INPUT)  potential water to take up (supply)
                                          , float *dlt_sw_dep)      //  (OUTPUT) root water uptake (mm)
//==========================================================================
/*  Purpose
*       Returns actual water uptake from each layer of the soil
*       profile by the crop (mm).
*
*  Mission Statement
*   Calculate the crop uptake of soil water
*
*  Changes
*       21/5/2003 ad converted to BC++
*       200498 nih created from crop_sw_uptake0
*/
   {
   //  Local Variables
   int layer;              // layer number of profile ()
   float sw_supply_sum;    // total potential over profile (mm)
   // Implementation Section ----------------------------------

   //find total root water potential uptake as sum of all layers

   sw_supply_sum = sum_real_array (sw_supply, deepest_layer+1);

   if ((sw_supply_sum < 0.0) || (sw_demand  < 0.0))
      {
      //we have no uptake - there is no demand or potential
      fill_real_array (dlt_sw_dep, 0.0, max_layer);
      }
   else
      {
      // get actual uptake
      fill_real_array (dlt_sw_dep, 0.0, max_layer);
      if (sw_demand < sw_supply_sum)
         {
         // demand is less than what roots could take up.
         // water is non-limiting.
         // distribute demand proportionately in all layers.
         for(layer = 0; layer <= deepest_layer; layer++)
            {
            dlt_sw_dep[layer] = -1 * divide (sw_supply[layer], sw_supply_sum, 0.0) * sw_demand;
            }
         }
      else
         {
         // water is limiting - not enough to meet demand so take
         // what is available (potential)
         for(layer = 0; layer <= deepest_layer; layer++)
            {
            dlt_sw_dep[layer] = -1 * sw_supply[layer];
            }
         }
      }
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





