#include <general\pch.h>
#include <vcl.h>
#include <math.h>
#pragma hdrstop

#include <ApsimShared\ApsimComponentData.h>
#include <ComponentInterface\DataTypes.h>
#include "ScienceConverterComponent.h"

#include <string>
#include <ComponentInterface\MessageDataExt.h>
#include <ComponentInterface\ApsimVariant.h>
#include <ApsimShared\fstringext.h>
#include <general\string_functions.h>
#include <general\stl_functions.h>
#include <strstream>
#include <iomanip.h>


#pragma package(smart_init)
using namespace std;

#define singleArrayTypeDDML \
   "<type  array=\"T\" kind=\"single\"/>"

      const int  GREEN = 0 ;
      const int  SENESCED = 1 ;
      const int  DEAD = 2 ;

//      enum plantPart {ROOT, LEAF, STEM, POD, MEAL, OIL};
 //      indices of plant part names
      const int  ROOT = 0 ;
      const int  LEAF = 1 ;
      const int  STEM = 2 ;
      const int  POD  = 3 ;
      const int  MEAL = 4 ; // excludes oil component
      const int  OIL  = 5 ; // seed oil

// number of plant parts
// const int  max_part = 6 ; // NB. implies for (i=0; i < max_part; max_part++) usage

      const float kg2g = 1000.0 ;
      const float ha2sm = 10000.0 ;
      const float g2kg = 1.0/kg2g ;
      const float sm2ha = 1.0/ha2sm ;
      const float cmol2mol = 1.0/100.0 ;
      const float mm2m = 1.0/1000.0;

//      const float dmdValue[numDmdPools] = {0.8, 0.7, 0.6, 0.5, 0.4, 0.3};

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
   {
   return new ScienceConverterComponent;
   }
// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
ScienceConverterComponent::ScienceConverterComponent(void)
   {
   }
// ------------------------------------------------------------------
// Destructor
// ------------------------------------------------------------------
ScienceConverterComponent::~ScienceConverterComponent(void)
   {
   }
// ------------------------------------------------------------------
// Init1 phase.
// ------------------------------------------------------------------
void ScienceConverterComponent::doInit1(const FString& sdml)
   {
   protocol::Component::doInit1(sdml);

   day_lengthID = addRegistration(RegistrationType::get, "day_length", singleTypeDDML);
   dayLengthID = addRegistration(RegistrationType::respondToGet, "dayLength", singleTypeDDML);
   plant2stockID = addRegistration(RegistrationType::respondToGet, "plant2stock", plant2stockTypeDDML);
   removeHerbageID = addRegistration(RegistrationType::respondToEvent, "remove_herbage", remove_herbageTypeDDML);

   dmFeedOnOfferID = addRegistration(RegistrationType::respondToGet, "dm_feed_on_offer", singleArrayTypeDDML);
   dmFeedRemovedID = addRegistration(RegistrationType::respondToGet, "dm_feed_removed", singleArrayTypeDDML);

   stockBuyID = addRegistration(RegistrationType::respondToEvent, "buystock", stringTypeDDML);
   buyID = addRegistration(RegistrationType::event, "buy", buystockTypeDDML);

   stockSellID = addRegistration(RegistrationType::respondToEvent, "sellstock", stringTypeDDML);
   sellID = addRegistration(RegistrationType::event, "sell", sellstockTypeDDML);

   }
// ------------------------------------------------------------------
// Init2 phase.
// ------------------------------------------------------------------
void ScienceConverterComponent::doInit2(void)
   {
      plant2StockSent = false;
      readParameters (); // Read constants
      readHerbageModuleParameters ();
      doRunTimeReg ();
//     zero_variables (); // Zero global states
//     init ();           // Site specific init
//     get_other_variables (); // sw etc..
   }
// ------------------------------------------------------------------
// Runtime Registrations.
// ------------------------------------------------------------------
void ScienceConverterComponent::doRunTimeReg(void)
   {
   dmGreenID = addRegistration(RegistrationType::get, "dm_green", singleArrayTypeDDML,"", c.herbageModuleName.c_str());   // parameter crop name=lablab
   dmGreenDeltaID = addRegistration(RegistrationType::get, "dlt_dm_green", singleArrayTypeDDML,"", c.herbageModuleName.c_str());   // parameter crop name=lablab
   dmGreenRetransDeltaID = addRegistration(RegistrationType::get, "dlt_dm_green_retrans", singleArrayTypeDDML,"", c.herbageModuleName.c_str());   // parameter crop name=lablab
   nGreenID = addRegistration(RegistrationType::get, "n_green", singleArrayTypeDDML,"", c.herbageModuleName.c_str());   // parameter crop name=lablab
   pGreenID = addRegistration(RegistrationType::get, "p_green", singleArrayTypeDDML,"", c.herbageModuleName.c_str());   // parameter crop name=lablab

   dmSenescedID = addRegistration(RegistrationType::get, "dm_senesced", singleArrayTypeDDML,"", c.herbageModuleName.c_str());   // parameter crop name=lablab
   dmSenescedDeltaID = addRegistration(RegistrationType::get, "dlt_dm_senesced", singleArrayTypeDDML,"", c.herbageModuleName.c_str());   // parameter crop name=lablab
   dmSenescedDetachedDeltaID = addRegistration(RegistrationType::get, "dlt_dm_detached", singleArrayTypeDDML,"", c.herbageModuleName.c_str());   // parameter crop name=lablab
   nSenescedID = addRegistration(RegistrationType::get, "n_senesced", singleArrayTypeDDML,"", c.herbageModuleName.c_str());   // parameter crop name=lablab
   pSenescedID = addRegistration(RegistrationType::get, "p_senesced", singleArrayTypeDDML,"", c.herbageModuleName.c_str());   // parameter crop name=lablab

   dmDeadID = addRegistration(RegistrationType::get, "dm_dead", singleArrayTypeDDML,"", c.herbageModuleName.c_str());   // parameter crop name=lablab
   dmGreenDeadDeltaID = addRegistration(RegistrationType::get, "dlt_dm_green_dead", singleArrayTypeDDML,"", c.herbageModuleName.c_str());   // parameter crop name=lablab
   dmSenescedDeadDeltaID = addRegistration(RegistrationType::get, "dlt_dm_senesced_dead", singleArrayTypeDDML,"", c.herbageModuleName.c_str());   // parameter crop name=lablab
   dmDeadDetachedDeltaID = addRegistration(RegistrationType::get, "dlt_dm_dead_detached", singleArrayTypeDDML,"", c.herbageModuleName.c_str());   // parameter crop name=lablab
   nDeadID = addRegistration(RegistrationType::get, "n_dead", singleArrayTypeDDML,"", c.herbageModuleName.c_str());   // parameter crop name=lablab
   pDeadID = addRegistration(RegistrationType::get, "p_dead", singleArrayTypeDDML,"", c.herbageModuleName.c_str());   // parameter crop name=lablab

   heightID = addRegistration(RegistrationType::get, "height", singleTypeDDML,"", c.herbageModuleName.c_str());
   thermalTimeID = addRegistration(RegistrationType::get, "tt_tot()", singleTypeDDML,"", c.herbageModuleName.c_str());
   thermalTimeBGID = addRegistration(RegistrationType::get, "tt_tot(1-2)", singleArrayTypeDDML,"", c.herbageModuleName.c_str());

   removeCropBiomassID = addRegistration(RegistrationType::event, "remove_crop_biomass", removeCropDmTypeDDML,"", c.herbageModuleName.c_str());
   }
// ------------------------------------------------------------------
// Event handler.
// ------------------------------------------------------------------
void ScienceConverterComponent::respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
{
   if (eventID == removeHerbageID && plant2StockSent == true)
   {
      variant.unpack(grazed);
      if (c.debug == "on")
      {
         ostrstream msg1;
         msg1 << endl << "Remove herbage dmd pools:-" << endl;
         float dmTotal1 = 0.0;

         for (unsigned int pool = 0; pool < feed.herbage.size(); pool++)
         {
            msg1 << "   dm pool " << pool+1 << " (" << feed.herbage[pool].dmd << ") = " << grazed.herbage[pool] << " (kg/ha)" << endl;
            dmTotal1 +=  grazed.herbage[pool];
         }

         msg1 << endl << "   dm total = " << dmTotal1 << " (kg/ha)" << endl << ends;

         writeString (msg1.str());
      }

      for (unsigned int dmdPool = 0; dmdPool < grazed.herbage.size(); dmdPool++)
      {
         if (grazed.herbage[dmdPool] > feed.herbage[dmdPool].dm)
         {
            ostrstream msg;
            msg << "Attempting to remove more herbage from dmd pool " << dmdPool+1 << " (dmd " << feed.herbage[dmdPool].dmd << ")" << " than available:-" << endl;
            msg << "Removing " << grazed.herbage[dmdPool] << " (kg/ha) from " << feed.herbage[dmdPool].dm << " (kg/ha) available." << ends;
            throw std::runtime_error (msg.str());
         }
      }


      protocol::removeCropDmType crop;
      protocol::dmType dm;
      crop.dm.erase(crop.dm.begin(), crop.dm.end());
      dm.dlt.erase(dm.dlt.begin(), dm.dlt.end());
      dm.part.erase(dm.part.begin(), dm.part.end());

      dm.pool = "green";
      dm.part.push_back("leaf");
      float dmPart = 0.0;
      for (int pool = 0; pool < c.numDmdPools; pool++) dmPart += grazed.herbage[pool]*partFraction[pool].green.leaf;
      dm.dlt.push_back(dmPart * kg2g / ha2sm);

      dm.part.push_back("stem");
      dmPart = 0.0;
      for (int pool = 0; pool < c.numDmdPools; pool++) dmPart += grazed.herbage[pool]*partFraction[pool].green.stem;
      dm.dlt.push_back(dmPart * kg2g / ha2sm);

      crop.dm.push_back(dm);
      dm.dlt.erase(dm.dlt.begin(), dm.dlt.end());
      dm.part.erase(dm.part.begin(), dm.part.end());

      dm.pool = "senesced";
      dm.part.push_back("leaf");
      dmPart = 0.0;
      for (int pool = 0; pool < c.numDmdPools; pool++) dmPart += grazed.herbage[pool]*partFraction[pool].senesced.leaf;
      dm.dlt.push_back(dmPart * kg2g / ha2sm);

      dm.part.push_back("stem");
      dmPart = 0.0;
      for (int pool = 0; pool < c.numDmdPools; pool++) dmPart += grazed.herbage[pool]*partFraction[pool].senesced.stem;
      dm.dlt.push_back(dmPart * kg2g / ha2sm);

      crop.dm.push_back(dm);
      dm.dlt.erase(dm.dlt.begin(), dm.dlt.end());
      dm.part.erase(dm.part.begin(), dm.part.end());

      dm.pool = "dead";
      dm.part.push_back("leaf");
      dmPart = 0.0;
      for (int pool = 0; pool < c.numDmdPools; pool++) dmPart += grazed.herbage[pool]*partFraction[pool].dead.leaf;
      dm.dlt.push_back(dmPart * kg2g / ha2sm);

      dm.part.push_back("stem");
      dmPart = 0.0;
      for (int pool = 0; pool < c.numDmdPools; pool++) dmPart += grazed.herbage[pool]*partFraction[pool].dead.stem;
      dm.dlt.push_back(dmPart * kg2g / ha2sm);

      crop.dm.push_back(dm);
      dm.dlt.erase(dm.dlt.begin(), dm.dlt.end());
      dm.part.erase(dm.part.begin(), dm.part.end());

      float dmTotal = 0.0;
      for (unsigned int pool=0; pool < crop.dm.size(); pool++)
      {
         for (unsigned int part = 0; part < crop.dm[pool].part.size(); part++)
         {
            dmTotal +=  crop.dm[pool].dlt[part];
         }
      }

      if (c.debug == "on")
      {
         ostrstream msg;
         msg << endl << "Remove herbage plant parts:-" << endl;

         for (unsigned int pool=0; pool < crop.dm.size(); pool++)
         {
            for (unsigned int part = 0; part < crop.dm[pool].part.size(); part++)
            {
               msg << "   dm " << crop.dm[pool].pool << " " << crop.dm[pool].part[part] << " = " << crop.dm[pool].dlt[part] << " (g/m2)" << endl;
            }
         }

         msg << endl << "   dm total = " << dmTotal << " (g/m2)" << endl << ends;

         writeString (msg.str());
      }

      if (dmTotal > 1.0e-6)
      {
         publish (removeCropBiomassID, crop);
      }

   }
   else if (eventID == stockBuyID)
   {
      stockBuy(variant);
   }
   else if (eventID == stockSellID)
   {
      stockSell(variant);
   }
   else
   {   // Don't respond to any other events.
   }
}
// ------------------------------------------------------------------
// Event handler.
// ------------------------------------------------------------------
void ScienceConverterComponent::stockBuy (protocol::Variant &v/*(INPUT) message variant*/)
{
    std::string  valuestr;
    int      value4;
    double   value;
    protocol::buystockType buystock;

    protocol::ApsimVariant incomingApsimVariant(this);
    incomingApsimVariant.aliasTo(v.getMessageData());

    if (incomingApsimVariant.get("number", protocol::DTint4, false, value4) == true)
    {
         buystock.number = value4;

         ostrstream msg;
         msg << "Buy stock :-" << endl
             << "   number = " << setw(10) << value4 << " (-)" << ends;
         writeString (msg.str());
    }
    else
    {
         buystock.number = 0;
    }

    if (incomingApsimVariant.get("sex", protocol::DTstring, false, valuestr) == true)
    {
         buystock.sex = valuestr;

         ostrstream msg;
         msg << "   sex = " << valuestr << " (-)" << ends;
         writeString (msg.str());
    }
    else
    {
         buystock.sex = "";
    }

    if (incomingApsimVariant.get("age", protocol::DTdouble, false, value) == true)
    {
         buystock.age = value;

         ostrstream msg;
         msg << "   age = "  << value << " (months)" << ends;
         writeString (msg.str());
    }
    else
    {
         buystock.age = 0.0;
    }

   if (incomingApsimVariant.get("weight", protocol::DTdouble, false, value) == true)
    {
         buystock.weight = value;

         ostrstream msg;
         msg << "   weight = "  << value << " (kg)" << ends;
         writeString (msg.str());

    }
    else
    {
         buystock.weight = 0.0;
    }

    if (incomingApsimVariant.get("fleece_wt", protocol::DTdouble, false, value) == true)
    {
         buystock.fleece_wt = value;

         ostrstream msg;
         msg << "   fleece_wt = "  << value << " (kg)" << ends;
         writeString (msg.str());
    }
    else
    {
         buystock.fleece_wt = 0.0;
    }

    if (incomingApsimVariant.get("pregnant", protocol::DTint4, false, value4) == true)
    {
         buystock.pregnant = value4;
    }
    else
    {
         buystock.pregnant = 0;
    }

    if (incomingApsimVariant.get("lactating", protocol::DTint4, false, value4) == true)
   {
         buystock.lactating = value4;
    }
    else
    {
         buystock.lactating = 0;
    }

    if (incomingApsimVariant.get("no_young", protocol::DTint4, false, value4) == true)
    {
         buystock.no_young = value4;
    }
    else
    {
         buystock.no_young = 0;
    }

    if (incomingApsimVariant.get("young_wt", protocol::DTdouble, false, value) == true)
    {
         buystock.young_wt = value;
    }
    else
    {
         buystock.young_wt = 0.0;
    }

    if (incomingApsimVariant.get("young_fleece_wt", protocol::DTdouble, false, value) == true)
    {
         buystock.young_fleece_wt = value;
    }
    else
    {
         buystock.young_fleece_wt = 0.0;
    }

    publish (buyID, buystock);
}

void ScienceConverterComponent::stockSell (protocol::Variant &v/*(INPUT) message variant*/)
{
    int      value4;
    double   value;
    protocol::sellstockType sellstock;

    protocol::ApsimVariant incomingApsimVariant(this);
    incomingApsimVariant.aliasTo(v.getMessageData());

    if (incomingApsimVariant.get("number", protocol::DTint4, false, value4) == true)
    {
         sellstock.number = value4;

         ostrstream msg;
         msg << "Sell stock :-" << endl
             << "   number = " << setw(10) << value4 << " (-)" << ends;
         writeString (msg.str());
    }
    else
    {
         sellstock.number = 0;
    }

    if (incomingApsimVariant.get("group", protocol::DTint4, false, value4) == true)
    {
         sellstock.group = value4;

         ostrstream msg;
         msg << " Group = " << setw(10) << value4 << " (-)" << ends;
         writeString (msg.str());
    }
    else
    {
         sellstock.group = 0;
    }

    publish (sellID, sellstock);
}
// ------------------------------------------------------------------
// return a variable to caller.  Return true if we own variable.
// ------------------------------------------------------------------
void ScienceConverterComponent::respondToGet(unsigned int& fromID,
                                             protocol::QueryValueData& queryData)
{
   // Daylength
   if (queryData.ID == dayLengthID) daylengthRelay(queryData);

   // dm Feed on offer
   else if (queryData.ID == dmFeedOnOfferID) sendFeedOnOffer(queryData);

   //dm feed removed
   else if (queryData.ID == dmFeedRemovedID) sendFeedRemoved(queryData);

   // plant2stock
   else if (queryData.ID == plant2stockID)  sendPlant2Stock(queryData);

   else
   {   // don't respond to any other gets.
   }
}

void ScienceConverterComponent::daylengthRelay (protocol::QueryValueData& queryData)
{
      protocol::Variant* variant;
      bool ok = getVariable(day_lengthID, variant, true);
      if (ok)
      {
         float dayLength;
         bool ok = variant->unpack(dayLength);  // what happens if this is not ok?
         sendVariable(queryData, dayLength);
      }
      else
      {   // didn't get the day_length ID ok. Do nothing about it.
      }
}

void ScienceConverterComponent::sendFeedOnOffer(protocol::QueryValueData& queryData)
{
      float dmFeedOnOffer[6] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
      for (unsigned i = 0; i != feed.herbage.size(); i++)
      {
         dmFeedOnOffer[i] = feed.herbage[i].dm;
      }
      sendVariable(queryData, vector <float> (dmFeedOnOffer, dmFeedOnOffer+6));
}

void ScienceConverterComponent::sendFeedRemoved(protocol::QueryValueData& queryData)
{
      float dmFeedRemoved[6] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
      for (unsigned i = 0; i != grazed.herbage.size(); i++)
      {
         dmFeedRemoved[i] = grazed.herbage[i];
      }
      sendVariable(queryData, vector <float> (dmFeedRemoved, dmFeedRemoved+6));
}

void ScienceConverterComponent::getParts(PlantPartType &parts, unsigned partsID)
{
      protocol::Variant* variant;
      bool ok = getVariable(partsID, variant, true);
      if (ok)
      {
         vector <float> partsArray;
         bool ok = variant->unpack(partsArray);
         if (ok && partsArray.size() >= 2)
         {
            parts.stem = partsArray[STEM]*g2kg/sm2ha;
            parts.leaf = partsArray[LEAF]*g2kg/sm2ha;
         }
         else
         {
            throw std::runtime_error("Couldn't unpack partsArray");
         }
      }
      else
      {
         throw std::runtime_error("Couldn't get variable partsID");
      }
}


void ScienceConverterComponent::getPGreen(PlantPartType &pGreen, PlantPool &dm)
{
      protocol::Variant* variant;
         bool ok = getVariable(pGreenID, variant, true);
         if (ok)
         {
            vector <float> P;
            bool ok = variant->unpack(P);
            if (ok && P.size() >= 2)
            {
               if (P[STEM] && P[LEAF] > 0.0)
               {
                  pGreen.stem = P[STEM]*g2kg/sm2ha;
                  pGreen.leaf = P[LEAF]*g2kg/sm2ha;
               }
               else
               {
                  pGreen.stem = c.pConcGreenStemDefault * dm.green.stem;  // parameter for default P contents
                  pGreen.leaf = c.pConcGreenStemDefault * dm.green.leaf;
               }
            }
            else
            {
               throw std::runtime_error("Couldn't unpack pGreen");
           }
         }
         else
         {
            throw std::runtime_error("Couldn't get variable pGreenID");
         }
}

void ScienceConverterComponent::getPSenesced(PlantPartType &pSenesced, PlantPool &dm)
{
      protocol::Variant* variant;
         bool ok = getVariable(pSenescedID, variant, true);
         if (ok)
         {
            vector <float> P;
            bool ok = variant->unpack(P);
            if (ok && P.size() >= 2)
            {
               if (P[STEM] && P[LEAF] > 0.0)
               {
                  pSenesced.stem = P[STEM]*g2kg/sm2ha;
                  pSenesced.leaf = P[LEAF]*g2kg/sm2ha;
               }
               else
               {
                  pSenesced.stem = c.pConcSenescedStemDefault * dm.senesced.stem;  // parameter for default P contents
                  pSenesced.leaf = c.pConcSenescedLeafDefault * dm.senesced.leaf;
               }
            }
            else
            {
               throw std::runtime_error("Couldn't unpack pSenesced");
           }
         }
         else
         {
            throw std::runtime_error("Couldn't get variable pSenescedID");
         }
}

void ScienceConverterComponent::getPDead(PlantPartType &pDead, PlantPool &dm)
{
      protocol::Variant* variant;
         bool ok = getVariable(pDeadID, variant, true);
         if (ok)
         {
            vector <float> P;
            bool ok = variant->unpack(P);
            if (ok && P.size() >= 2)
            {
               if (P[STEM] && P[LEAF] > 0.0)
               {
                  pDead.stem = P[STEM]*g2kg/sm2ha;
                  pDead.leaf = P[LEAF]*g2kg/sm2ha;
               }
               else
               {
                  pDead.stem = c.pConcDeadStemDefault * dm.dead.stem;  // parameter for default P contents
                  pDead.leaf = c.pConcDeadLeafDefault * dm.dead.leaf;
               }
            }
            else
            {
               throw std::runtime_error("Couldn't unpack pDead");
           }
         }
         else
         {
            throw std::runtime_error("Couldn't get variable pDeadID");
         }
}

void ScienceConverterComponent::getHeight(float &height)
{
      protocol::Variant* variant;
         bool ok = getVariable(heightID, variant, true);
         if (ok)
         {
            bool ok = variant->unpack(height);
            if (ok)
            { // do nothing
            }
            else
            {
               throw std::runtime_error("Couldn't unpack height");
            }
         }
         else
         {
            throw std::runtime_error("Couldn't get variable heightID");
         }
}

void ScienceConverterComponent::getThermalTime(float &thermalTime)
{
      protocol::Variant* variant;
         bool ok = getVariable(thermalTimeID, variant, true);
         if (ok)
         {
            bool ok = variant->unpack(thermalTime);
            if (ok)
            { // do nothing
            }
            else
            {
               throw std::runtime_error("Couldn't unpack thermalTime");
            }
         }
         else
         {
            throw std::runtime_error("Couldn't get variable thermalTimeID");
         }

         ok = getVariable(thermalTimeBGID, variant, true);
         if (ok)
         {
            vector<float> thermalTimeBG;
            bool ok = variant->unpack(thermalTimeBG);
            if (ok)
            {
               for (int stage = 0; stage < thermalTimeBG.size(); stage++)
               {
                  thermalTime -= thermalTimeBG[stage];
               }
               thermalTime = max(0, thermalTime);
            }
            else
            {
               throw std::runtime_error("Couldn't unpack thermalTimeBG");
            }
         }
         else
         {
            throw std::runtime_error("Couldn't get variable thermalTimeBGID");
         }
}

void ScienceConverterComponent::getVariables(PlantPool &dm, PlantPool &N, PlantPool &P, float &height, float &thermalTime)
{
         // Get dm GREEN
      PlantPartType dmGreen;
      getParts(dmGreen, dmGreenID);

         // Get dm SENESCED
      PlantPartType dmSenesced;
      getParts(dmSenesced, dmSenescedID);

         // Get dm DEAD
      PlantPartType dmDead;
      getParts(dmDead, dmDeadID);

      dm.setValue(dmGreen, dmSenesced, dmDead);

      if (dm.total() > 0.0)
      {
            // Get delta dm GREEN
         PlantPartType  dmGreenDelta;
         getParts(dmGreenDelta, dmGreenDeltaID);

//         dmdPoolDm[?]

            // Get delta dm GREEN retrans
         PlantPartType dmGreenRetransDelta;
         getParts(dmGreenRetransDelta, dmGreenRetransDeltaID);

            // Get dm SENESCED
         PlantPartType dmSenescedDelta;
         getParts(dmSenescedDelta, dmSenescedDeltaID);

         PlantPartType dmSenescedDetachedDelta;
         getParts(dmSenescedDetachedDelta, dmSenescedDetachedDeltaID);

            // Get dm DEAD
         PlantPartType dmGreenDeadDelta;
         getParts(dmGreenDeadDelta, dmGreenDeadDeltaID);

         PlantPartType dmSenescedDeadDelta;
         getParts(dmSenescedDeadDelta, dmSenescedDeadDeltaID);

         PlantPartType dmDeadDetachedDelta;
         getParts(dmDeadDetachedDelta, dmDeadDetachedDeltaID);


//         int dmdClass = dmdClassMax.green.
//
//         dmdPoolDm[?].addGreen(dmGreenDelta);
//         dmdPoolDm[?].addGreen(dmGreenRetransDelta);
//         dmdPoolDm[?].removeGreen(dmSenescedDelta);
//         dmdPoolDm[?].removeGreen(dmGreenDeadDelta);
//
//         dmdPoolDm[?].addSenesced(dmSenescedDelta);
//         dmdPoolDm[?].removeSenesced(dmSenescedDetachedDelta);
//         dmdPoolDm[?].removeSenesced(dmSenescedDeadDelta);
//
//         dmdPoolDm[?].addDead(dmGreenDeadDelta);
//         dmdPoolDm[?].addDead(dmSenescedDeadDelta);
//         dmdPoolDm[?].removeDead(dmDeadDetachedDelta);

            // Get N GREEN
         PlantPartType nGreen;
         getParts(nGreen, nGreenID);

         // Get N SENESCED
         PlantPartType nSenesced;
         getParts(nSenesced, nSenescedID);

         // Get N DEAD
         PlantPartType nDead;
         getParts(nDead, nDeadID);

         N.setValue(nGreen, nSenesced, nDead);

         // Get P GREEN
         PlantPartType pGreen;
         getPGreen(pGreen, dm);

         // Get P SENESCED
         PlantPartType pSenesced;
         getPSenesced(pSenesced, dm);

         // Get P DEAD
         PlantPartType pDead;
         getPDead(pDead, dm);

         P.setValue(pGreen, pSenesced, pDead);

        // Get HEIGHT
//         PlantPool  heightRatio;

         getHeight(height);
        // Get Thermal Time
         getThermalTime(thermalTime);
      }

}

void ScienceConverterComponent::calcDmdDecline(const float &thermalTime, PlantPool &dQ)
{

   const float ADJ = 1.1;
   const float TTCorrection = 20.0;

   dQ = dmdMax - dmdAvg;
//   dQ = (dmdAvg - dmdMin) * (KQ5 * thermalTime);
//   dQ.green.leaf = exp(-KQ5*thermalTime*max(0.0,1.0-thermalTime/KQ4)*ADJ) * (dmdAvg.green.leaf - dmdMin.green.leaf)*ADJ + dmdMin.green.leaf;
//   dQ.green.stem = exp(-KQ5*thermalTime*ADJ) * (dmdAvg.green.stem - dmdMin.green.stem)*ADJ + dmdMin.green.stem;
   dQ.green.leaf = max(0.0, (1.0-exp(-c.KQ5Leaf*(thermalTime-c.KQ4-TTCorrection))*ADJ)) * (dmdMax.green.leaf - dmdMin.green.leaf);
   dQ.green.stem = max(0.0, (1.0-exp(-c.KQ5Stem*(thermalTime-TTCorrection))*ADJ)) * (dmdMax.green.stem - dmdMin.green.stem);

}

void ScienceConverterComponent::calcDmdClass(PlantPool &dmdClassMax, PlantPool &dmdClassMin)
{

// get GREEN Leaf & stem dmd classes
      dmdClass (dmdMax.green.leaf, dmdMin.green.leaf, dmdClassMax.green.leaf, dmdClassMin.green.leaf);
      dmdClass (dmdMax.green.stem, dmdMin.green.stem, dmdClassMax.green.stem, dmdClassMin.green.stem);

// get SENESCED Leaf & dmd classes
      dmdClass (dmdMax.senesced.leaf, dmdMin.senesced.leaf, dmdClassMax.senesced.leaf, dmdClassMin.senesced.leaf);
      dmdClass (dmdMax.senesced.stem, dmdMin.senesced.stem, dmdClassMax.senesced.stem, dmdClassMin.senesced.stem);

// get DEAD Leaf & stem dmd classes
      dmdClass (dmdMax.dead.leaf, dmdMin.dead.leaf, dmdClassMax.dead.leaf, dmdClassMin.dead.leaf);
      dmdClass (dmdMax.dead.stem, dmdMin.dead.stem, dmdClassMax.dead.stem, dmdClassMin.dead.stem);
}

void ScienceConverterComponent::calcDmdDistribution(PlantPool dmdFraction[], PlantPool dQ)
{
      PlantPool dmdDeclined = dmdMax - dQ;

      float fraction[maxDmdPools];

// get GREEN Leaf dmd fractions
      for (int pool = 0; pool < c.numDmdPools; pool++) fraction[pool] = 0.0;
      proportion (dmdDeclined.green.leaf, dmdMax.green.leaf, dmdMin.green.leaf, fraction);
      for (int pool = 0; pool < c.numDmdPools; pool++) dmdFraction[pool].green.leaf = fraction[pool];

// get GREEN stem dmd fractions
      for (int pool = 0; pool < c.numDmdPools; pool++) fraction[pool] = 0.0;
      proportion (dmdDeclined.green.stem, dmdMax.green.stem, dmdMin.green.stem, fraction);
      for (int pool = 0; pool < c.numDmdPools; pool++) dmdFraction[pool].green.stem = fraction[pool];

// get SENESCED Leaf dmd fractions
      for (int pool = 0; pool < c.numDmdPools; pool++) fraction[pool] = 0.0;
      proportion (dmdDeclined.senesced.leaf, dmdMax.senesced.leaf, dmdMin.senesced.leaf, fraction);
      for (int pool = 0; pool < c.numDmdPools; pool++) dmdFraction[pool].senesced.leaf = fraction[pool];

// get SENESCED stem dmd fractions
      for (int pool = 0; pool < c.numDmdPools; pool++) fraction[pool] = 0.0;
      proportion (dmdDeclined.senesced.stem, dmdMax.senesced.stem, dmdMin.senesced.stem, fraction);
      for (int pool = 0; pool < c.numDmdPools; pool++) dmdFraction[pool].senesced.stem = fraction[pool];

// get DEAD Leaf dmd fractions
      for (int pool = 0; pool < c.numDmdPools; pool++) fraction[pool] = 0.0;
      proportion (dmdDeclined.dead.leaf, dmdMax.dead.leaf, dmdMin.dead.leaf, fraction);
      for (int pool = 0; pool < c.numDmdPools; pool++) dmdFraction[pool].dead.leaf = fraction[pool];

// get DEAD Stem dmd fractions
      for (int pool = 0; pool < c.numDmdPools; pool++) fraction[pool] = 0.0;
      proportion (dmdDeclined.dead.stem, dmdMax.dead.stem, dmdMin.dead.stem, fraction);
      for (int pool = 0; pool < c.numDmdPools; pool++) dmdFraction[pool].dead.stem = fraction[pool];
}

void ScienceConverterComponent::calcDmdDistributionB(PlantPool dmdFraction[], PlantPool dQ)
{
      PlantPool dmdDeclined = dmdMax - dQ;

      float fraction[maxDmdPools];

// get GREEN Leaf dmd fractions
      dmdFraction[0].green.leaf = 1.0;
      c.dmdValue[0] = dmdDeclined.green.leaf;

// get GREEN stem dmd fractions
      dmdFraction[1].green.stem = 1.0;
      c.dmdValue[1] = dmdDeclined.green.stem;

// get SENESCED Leaf dmd fractions
      dmdFraction[2].senesced.leaf = 1.0;
      c.dmdValue[2] = dmdDeclined.senesced.leaf;

// get SENESCED stem dmd fractions
      dmdFraction[3].senesced.stem = 1.0;
      c.dmdValue[3] = dmdDeclined.senesced.stem;

// get DEAD Leaf dmd fractions
      dmdFraction[4].dead.leaf = 1.0;
      c.dmdValue[4] = dmdDeclined.dead.leaf;

// get DEAD Stem dmd fractions
      dmdFraction[5].dead.stem = 1.0;
      c.dmdValue[5] = dmdDeclined.dead.stem;
}

void ScienceConverterComponent::sendPlant2Stock(protocol::QueryValueData& queryData)
{
      protocol::herbageType herbage;
      PlantPool dm;
      PlantPool N;
      PlantPool P;
      PlantPool dQ;
      float  height;
      float  thermalTime;

      getVariables(dm, N, P, height, thermalTime);
      calcDmdDecline(thermalTime, dQ);

      if (dm.total() > 0.0)
      {

// Now PREPARE herbage
         feed.herbage.erase(feed.herbage.begin(), feed.herbage.end());

// distribute herbage

      PlantPool dmdFraction[maxDmdPools];

//      calcDmdDistribution(dmdFraction, dQ);
      calcDmdDistributionB(dmdFraction, dQ);

// LEAF  - GREEN
      PlantPool poolDm;
//      PlantPool poolDmd[maxDmdPools];
      PlantPool poolN;
      PlantPool poolP;
      PlantPool poolS;
      PlantPool poolAA;
      PlantPool partAshAlk(c.AshAlkGreenLeafDefault, c.AshAlkGreenStemDefault, c.AshAlkSenescedLeafDefault, c.AshAlkSenescedStemDefault, c.AshAlkDeadLeafDefault, c.AshAlkDeadStemDefault);
      PlantPool NSRatio(c.NSRatioGreenLeafDefault, c.NSRatioGreenStemDefault, c.NSRatioSenescedLeafDefault, c.NSRatioSenescedStemDefault, c.NSRatioDeadLeafDefault, c.NSRatioDeadStemDefault);
      PlantPool NPRatio(c.NPRatioGreenLeafDefault,  c.NPRatioGreenStemDefault,  c.NPRatioSenescedLeafDefault,  c.NPRatioSenescedStemDefault,  c.NPRatioDeadStemDefault,  c.NPRatioDeadStemDefault);

      partAshAlk = partAshAlk*cmol2mol;

      float dmTot;
      float nTot;
      float sTot;
      float pTot;
      float aaTot;

      float nConc;
      float pConc;
      float sConc;
      float ashAlk;

      for (int pool = 0; pool < c.numDmdPools; pool++)
      {
         if (c.debug == "on")
         {
            ostrstream msgFraction;
            msgFraction << endl << "Herbage dmd distribution, pool " << pool+1 << ":-" << endl;
            msgFraction << dmdFraction[pool] << ends;
            writeString (msgFraction.str());
         }

         poolDm = dm * dmdFraction[pool];
         dmTot = poolDm.total();
         herbage.dm = dmTot;
         partFraction[pool] = poolDm / dmTot;

         herbage.dmd = c.dmdValue[pool];        // kg/ha

         poolN     = N * dmdFraction[pool];
         nTot = poolN.total();
         nConc = divide (nTot, herbage.dm, 0.0);
         herbage.cp_conc = nConc * c.cpNRatio;   // (kg/ha) - parameter cpNRatio = 6.25

//         poolP = P * dmdFraction[pool];
         poolP = N/NPRatio * dmdFraction[pool];
         pTot = poolP.total();
         pConc = divide (pTot, herbage.dm, 0.0);
         herbage.p_conc = pConc;

         poolS = N/NSRatio * dmdFraction[pool];
         sTot = poolS.total();
        sConc = divide (sTot, herbage.dm, 0.0);
         herbage.s_conc = sConc;      //parameter NSRatioLeaf = 19.0, NSRatioStem = 11.0;  herbage.s_conc = 0.0023;  kg/ha

         herbage.prot_dg = herbage.dmd + 0.1;     // parameter ProtDegrade = 0.1;  herbage.prot_dg = 0.7;    // kg/ha

         poolAA    = partAshAlk * dm * dmdFraction[pool];  // ash alk to be got from lablab
         aaTot = poolAA.total();
         ashAlk = divide (aaTot, herbage.dm, 0.0);
         herbage.ash_alk = ashAlk;      // herbage.ash_alk = 2.0;    // mol/kg

//         herbage.height_ratio = heightRatio.green.leaf;   //   herbage.height_ratio = 0.0006;

//         float bd = divide(herbage.dm *kg2g/ha2sm, height*mm2m, 0.0);
         float bd = divide(dm.total() *kg2g/ha2sm, height*mm2m, 0.0);
         herbage.height_ratio = divide(100.0, 0.03*bd, 0.0);
//         if (pool==4) herbage.height_ratio = heightRatio.senesced.leaf + heightRatio.senesced.stem;   //   herbage.height_ratio = 0.0006;

         feed.herbage.push_back(herbage);

         if (herbage.dm > 0.0)
         {
            if (c.debug == "on")
            {
               ostrstream msg;
               msg << endl << "Herbage on offer, pool " << pool+1 << ":-" << endl
                   << "   dm           = " <<              herbage.dm <<      " (kg/ha)" << endl
                   << "   dmd          = " <<              herbage.dmd <<     " (-)" <<     endl
                   << "   cp_conc      = " <<              herbage.cp_conc << " (kg/kg)" << endl
                   << "   p_conc       = " <<              herbage.p_conc <<  " (kg/kg)" << endl
                   << "   s_conc       = " <<              herbage.s_conc <<  " (kg/kg)" << endl
                   << "   ash_alk      = " <<              herbage.ash_alk << " (mol/kg)" << endl
                   << "   prot_dg      = " <<              herbage.prot_dg << " (kg/kg)" << endl
                   << "   bd           = " <<              bd << " (g/m^3)" << endl
                   << "   dm total     = " <<              dm.total() *kg2g/ha2sm << " (g/m2)" << endl
                   << "   height       = " <<              height*mm2m << " (m)" << endl
                   << "   height_ratio = " <<              herbage.height_ratio << " (-)" << ends;
               writeString (msg.str());
            }
         }

      } // end of Pools loop
   // REST
         float dm_green = dm.green.stem + dm.green.leaf;
         float dm_dead = dm.senesced.stem + dm.senesced.leaf + dm.dead.stem + dm.dead.leaf;
         float dm_total = dm_green + dm_dead;
         feed.propn_green = divide (dm_green, dm_total, 0.0);
         feed.legume = 1.0;
         feed.select_factor = 0.0; // ??

         plant2StockSent = true;
      }
      else
      {
         // No dry matter so Clear herbage
         feed.herbage.erase(feed.herbage.begin(), feed.herbage.end());
         herbage.dm = 0.0;        // kg/ha
         herbage.dmd = 0.0;        // kg/ha
         herbage.cp_conc = 0.0;   // (kg/ha)
         herbage.p_conc = 0.0;
         herbage.s_conc = 0.0;      //  kg/ha
         herbage.prot_dg = 0.0;     //  kg/ha
         herbage.ash_alk = 0.0;      //  mol/kg
         herbage.height_ratio = 0.0;   //

         feed.herbage.push_back(herbage);

   // REST
         feed.propn_green = 0.0;
         feed.legume = 0.0;
         feed.select_factor = 0.0;
      }
      // Now SEND feed off
      sendVariable(queryData, feed);

}

//===========================================================================
void ScienceConverterComponent::proportion (float dmdAvg, float dmdMax, float dmdMin, float dmdFraction[])
//===========================================================================

//Definition
//Assumptions
//Parameters

{
   //Constant Values

   const float MAXDMD = c.dmdValue[0];
   const float MINDMD = c.dmdValue[c.numDmdPools-1];
//   const float MAXDMD = 0.8;
//   const float MINDMD = 0.3;
   const float errorMargin = 1.0e-5;

   //Local Varialbes

   //Implementation

   // Check that dmds are legal

   if (dmdAvg > dmdMax + errorMargin)
   {  ostrstream msg;
      msg << endl << "Average digestibility > Maximum digestibility:-" << endl
          << "   Average      = " <<  dmdAvg << endl
          << "   Maximum      = " <<  dmdMax << endl  << ends;
      throw std::runtime_error(msg.str());
   }
   if (dmdAvg < dmdMin - errorMargin)
   {  ostrstream msg;
      msg << endl << "Average digestibility < Minimum digestibility:-" << endl
          << "   Average      = " <<  dmdAvg << endl
          << "   Minimum      = " <<  dmdMin << endl  << ends;
      throw std::runtime_error(msg.str());
   }
   if (dmdMin > dmdAvg + errorMargin)
   {  ostrstream msg;
      msg << endl << "Minimum digestibility > Average digestibility:-" << endl
          << "   Minimum      = " <<  dmdMin << endl
          << "   Average      = " <<  dmdAvg << endl  << ends;
      throw std::runtime_error(msg.str());
   }
   if (dmdMin < MINDMD - errorMargin)
   {  ostrstream msg;
      msg << endl << "Minimum digestibility < Lower Limit:-" << endl
          << "   Minimum      = " <<  dmdMin << endl
          << "   Lower Limit  = " <<  MINDMD << endl  << ends;
      throw std::runtime_error(msg.str());
   }
   if (dmdMax > MAXDMD + errorMargin)
   {  ostrstream msg;
      msg << endl << "Maximum digestibility > Upper Limit:-" << endl
          << "   Maximum      = " <<  dmdMax << endl
          << "   Upper Limit  = " <<  MAXDMD << endl  << ends;
      throw std::runtime_error(msg.str());
   }
   if (dmdMax < dmdAvg - errorMargin)
   {  ostrstream msg;
      msg << endl << "Maximum digestibility < Average digestibility:-" << endl
          << "   Maximum      = " <<  dmdMax << endl
          << "   Average      = " <<  dmdAvg << endl  << ends;
      throw std::runtime_error(msg.str());
   }


   float x = (dmdAvg - dmdMin) / (dmdMax - dmdMin);
   int startDmd = (MAXDMD - dmdMax)*10.0 + errorMargin;
   int endDmd = (MAXDMD - dmdMin)*10.0 + errorMargin;
   int numPools = (endDmd - startDmd) + 1;

   switch (numPools)
   {
      case 1:
         {
            dmdFraction[startDmd] = 1.0;
         }
         break;
      case 2:
         {
            dmdFraction[startDmd] = x;
            dmdFraction[startDmd+1] = 1.0-x;
         }
         break;
      case 3:
         {
            dmdFraction[startDmd] = pow(x, 2);
            dmdFraction[startDmd+1] = 2.0 * x * (1.0-x);
            dmdFraction[startDmd+2] = pow(1.0-x, 2);
         }
         break;
      case 4:
         {
            dmdFraction[startDmd] = pow(x, 3);
            dmdFraction[startDmd+1] = 3.0 * pow(x, 2) * (1.0-x);
            dmdFraction[startDmd+2] = 3.0 * x * pow(1.0-x, 2);
            dmdFraction[startDmd+3] = pow(1.0-x, 3);
         }
         break;
      case 5:
         {
            dmdFraction[startDmd] = pow(x, 4);
            dmdFraction[startDmd+1] = 4.0 * pow(x, 3) * (1.0-x);
            dmdFraction[startDmd+2] = 6.0 * pow(x, 2) * pow(1.0-x, 2);
            dmdFraction[startDmd+3] = 4.0 * x * pow(1.0-x,3);
            dmdFraction[startDmd+4] = pow(1.0-x, 4);
         }
         break;
      default:
         throw std::runtime_error("Too many digestibility classes");

   }
}

//===========================================================================
void ScienceConverterComponent::dmdClass (float dmdMax, float dmdMin, float &dmdClassMax, float &dmdClassMin)
//===========================================================================

//Definition
//Assumptions
//Parameters

{
   //Constant Values

   const float MAXDMD = c.dmdValue[0];
   const float MINDMD = c.dmdValue[c.numDmdPools-1];
   const float errorMargin = 1.0e-5;

   //Local Varialbes

   //Implementation

   // Check that dmds are legal

   if (dmdMin < MINDMD - errorMargin)
   {  ostrstream msg;
      msg << endl << "Minimum digestibility < Lower Limit:-" << endl
          << "   Minimum      = " <<  dmdMin << endl
          << "   Lower Limit  = " <<  MINDMD << endl  << ends;
      throw std::runtime_error(msg.str());
   }

   if (dmdMax > MAXDMD + errorMargin)
   {  ostrstream msg;
      msg << endl << "Maximum digestibility > Upper Limit:-" << endl
          << "   Maximum      = " <<  dmdMax << endl
          << "   Upper Limit  = " <<  MAXDMD << endl  << ends;
      throw std::runtime_error(msg.str());
   }

   if (dmdMax < dmdMin - errorMargin)
   {  ostrstream msg;
      msg << endl << "Minimum digestibility > Maximum digestibility:-" << endl
          << "   Minimum      = " <<  dmdMin << endl
          << "   Maximum      = " <<  MINDMD << endl  << ends;
      throw std::runtime_error(msg.str());
   }

   for (int dmdClassNum = 0; dmdClassNum < c.numDmdPools; dmdClassNum++)
   {           // Assume dmdValue in descending order
      dmdClassMax = dmdClassNum;
      if (abs(dmdMax - c.dmdValue[dmdClassNum]) < errorMargin)
      {
         exit;
      }
   }

   for (int dmdClassNum = 0; dmdClassNum < c.numDmdPools; dmdClassNum++)
   {           // Assume dmdValue in descending order
      dmdClassMin = dmdClassNum;
      if (abs(dmdMin - c.dmdValue[dmdClassNum]) < errorMargin)
      {
         exit;
      }
   }
}

//===========================================================================
float ScienceConverterComponent::divide (float dividend, float divisor, float default_value)
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

void ScienceConverterComponent::readParameters ( void )
{

//+  Constant Values
    const char*  my_name = "readParameters" ;
    const char*  section_name = "parameters" ;

//+  Local Variables
    int   numvals;                                // number of values returned

//- Implementation Section ----------------------------------

    writeString (" - reading parameters");

    c.herbageModuleName = readParameter (section_name, "herbage_module_name");
    c.debug = readParameter (section_name, "debug");

      ostrstream msg;
      msg << "Herbage module name = " << c.herbageModuleName << endl
          << "Debug = " << c.debug << ends;
      writeString (msg.str());
}

void ScienceConverterComponent::readHerbageModuleParameters ( void )
{

//+  Constant Values
    const char*  my_name = "readHerbageModuleParameters" ;
    const char*  section_name = "parameters" ;

//+  Local Variables
    int   numvals;                                // number of values returned

//- Implementation Section ----------------------------------
      ostrstream msg;
      msg << " - reading  herbage parameters for module '" << c.herbageModuleName.c_str() << "'" << endl << ends;
      writeString (msg.str());

    readParameter (c.herbageModuleName.c_str(), "dmdValue", c.dmdValue, c.numDmdPools, 0.0, 1.0);

    readParameter (c.herbageModuleName.c_str(), "p_conc_green_stem_default", c.pConcGreenStemDefault, 0.0, 1.0);
    readParameter (c.herbageModuleName.c_str(), "p_conc_green_leaf_default", c.pConcGreenLeafDefault, 0.0, 1.0);
    readParameter (c.herbageModuleName.c_str(), "p_conc_senesced_stem_default", c.pConcSenescedStemDefault, 0.0, 1.0);
    readParameter (c.herbageModuleName.c_str(), "p_conc_senesced_leaf_default", c.pConcSenescedLeafDefault, 0.0, 1.0);
    readParameter (c.herbageModuleName.c_str(), "p_conc_dead_stem_default", c.pConcDeadStemDefault, 0.0, 1.0);
    readParameter (c.herbageModuleName.c_str(), "p_conc_dead_leaf_default", c.pConcDeadLeafDefault, 0.0, 1.0);

    readParameter (c.herbageModuleName.c_str(), "ash_alk_green_stem_default", c.AshAlkGreenStemDefault, 0.0, 500.0);
    readParameter (c.herbageModuleName.c_str(), "ash_alk_green_leaf_default", c.AshAlkGreenLeafDefault, 0.0, 500.0);
    readParameter (c.herbageModuleName.c_str(), "ash_alk_senesced_stem_default", c.AshAlkSenescedStemDefault, 0.0, 500.0);
    readParameter (c.herbageModuleName.c_str(), "ash_alk_senesced_leaf_default", c.AshAlkSenescedLeafDefault, 0.0, 500.0);
    readParameter (c.herbageModuleName.c_str(), "ash_alk_dead_stem_default", c.AshAlkDeadStemDefault, 0.0, 500.0);
    readParameter (c.herbageModuleName.c_str(), "ash_alk_dead_leaf_default", c.AshAlkDeadLeafDefault, 0.0, 500.0);

    readParameter (c.herbageModuleName.c_str(), "ns_ratio_green_stem_default", c.NSRatioGreenStemDefault, 0.0, 30.0);
    readParameter (c.herbageModuleName.c_str(), "ns_ratio_green_leaf_default", c.NSRatioGreenLeafDefault, 0.0, 30.0);
    readParameter (c.herbageModuleName.c_str(), "ns_ratio_senesced_stem_default", c.NSRatioSenescedStemDefault, 0.0, 30.0);
    readParameter (c.herbageModuleName.c_str(), "ns_ratio_senesced_leaf_default", c.NSRatioSenescedLeafDefault, 0.0, 30.0);
    readParameter (c.herbageModuleName.c_str(), "ns_ratio_dead_stem_default", c.NSRatioDeadStemDefault, 0.0, 30.0);
    readParameter (c.herbageModuleName.c_str(), "ns_ratio_dead_leaf_default", c.NSRatioDeadLeafDefault, 0.0, 30.0);

    readParameter (c.herbageModuleName.c_str(), "np_ratio_green_stem_default", c.NPRatioGreenStemDefault, 0.0, 10.0);
    readParameter (c.herbageModuleName.c_str(), "np_ratio_green_leaf_default", c.NPRatioGreenLeafDefault, 0.0, 10.0);
    readParameter (c.herbageModuleName.c_str(), "np_ratio_senesced_stem_default", c.NPRatioSenescedStemDefault, 0.0, 10.0);
    readParameter (c.herbageModuleName.c_str(), "np_ratio_senesced_leaf_default", c.NPRatioSenescedLeafDefault, 0.0, 10.0);
    readParameter (c.herbageModuleName.c_str(), "np_ratio_dead_stem_default", c.NPRatioDeadStemDefault, 0.0, 10.0);
    readParameter (c.herbageModuleName.c_str(), "np_ratio_dead_leaf_default", c.NPRatioDeadLeafDefault, 0.0, 10.0);

    int numClasses = 3;
    readParameter (c.herbageModuleName.c_str(), "dmd_green_leaf", c.dmdGreenLeaf, numClasses, 0.0, 1.0);
    readParameter (c.herbageModuleName.c_str(), "dmd_green_stem", c.dmdGreenStem, numClasses, 0.0, 1.0);
    readParameter (c.herbageModuleName.c_str(), "dmd_senesced_leaf", c.dmdSenescedLeaf, numClasses, 0.0, 1.0);
    readParameter (c.herbageModuleName.c_str(), "dmd_senesced_stem", c.dmdSenescedStem, numClasses, 0.0, 1.0);
    readParameter (c.herbageModuleName.c_str(), "dmd_dead_leaf", c.dmdDeadLeaf, numClasses, 0.0, 1.0);
    readParameter (c.herbageModuleName.c_str(), "dmd_dead_stem", c.dmdDeadStem, numClasses, 0.0, 1.0);

    readParameter (c.herbageModuleName.c_str(), "KQ5Leaf", c.KQ5Leaf, 0.0, 1.0);
    readParameter (c.herbageModuleName.c_str(), "KQ5Stem", c.KQ5Stem, 0.0, 1.0);
    readParameter (c.herbageModuleName.c_str(), "KQ4", c.KQ4, 0.0, 1000.0);

    readParameter (c.herbageModuleName.c_str(), "cp_n_ratio", c.cpNRatio, 0.0, 10.0);

   const int MAX = 0;
   const int AVG = 1;
   const int MIN = 2;
         //plant pools  GL    GS   SL    SS   DL   DS
//      PlantPool dmdMax(c.dmdGreenLeaf, c.dmdMaxGreenStem, c.dmdMaxSenescedLeaf, c.dmdMaxSenescedStem, c.dmdMaxDeadLeaf, c.dmdMaxDeadStem);
   dmdMax.setValue(c.dmdGreenLeaf[MAX], c.dmdGreenStem[MAX], c.dmdSenescedLeaf[MAX], c.dmdSenescedStem[MAX], c.dmdDeadLeaf[MAX], c.dmdDeadStem[MAX]);
   dmdAvg.setValue(c.dmdGreenLeaf[AVG], c.dmdGreenStem[AVG], c.dmdSenescedLeaf[AVG], c.dmdSenescedStem[AVG], c.dmdDeadLeaf[AVG], c.dmdDeadStem[AVG]);
   dmdMin.setValue(c.dmdGreenLeaf[MIN], c.dmdGreenStem[MIN], c.dmdSenescedLeaf[MIN], c.dmdSenescedStem[MIN], c.dmdDeadLeaf[MIN], c.dmdDeadStem[MIN]);

   calcDmdClass(dmdClassMax, dmdClassMin);
}




