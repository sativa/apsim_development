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
   nGreenID = addRegistration(RegistrationType::get, "n_green", singleArrayTypeDDML,"", c.herbageModuleName.c_str());   // parameter crop name=lablab
   pGreenID = addRegistration(RegistrationType::get, "p_green", singleArrayTypeDDML,"", c.herbageModuleName.c_str());   // parameter crop name=lablab

   dmSenescedID = addRegistration(RegistrationType::get, "dm_senesced", singleArrayTypeDDML,"", c.herbageModuleName.c_str());   // parameter crop name=lablab
   nSenescedID = addRegistration(RegistrationType::get, "n_senesced", singleArrayTypeDDML,"", c.herbageModuleName.c_str());   // parameter crop name=lablab
   pSenescedID = addRegistration(RegistrationType::get, "p_senesced", singleArrayTypeDDML,"", c.herbageModuleName.c_str());   // parameter crop name=lablab

   dmDeadID = addRegistration(RegistrationType::get, "dm_dead", singleArrayTypeDDML,"", c.herbageModuleName.c_str());   // parameter crop name=lablab
   nDeadID = addRegistration(RegistrationType::get, "n_dead", singleArrayTypeDDML,"", c.herbageModuleName.c_str());   // parameter crop name=lablab
   pDeadID = addRegistration(RegistrationType::get, "p_dead", singleArrayTypeDDML,"", c.herbageModuleName.c_str());   // parameter crop name=lablab

   heightID = addRegistration(RegistrationType::get, "height", singleTypeDDML,"", c.herbageModuleName.c_str());

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

void ScienceConverterComponent::getDmGreen(PlantPool &dm, vector<float>  &dmGreen)
{
      protocol::Variant* variant;
      bool ok = getVariable(dmGreenID, variant, true);
      if (ok)
      {
         bool ok = variant->unpack(dmGreen);
         if (ok && dmGreen.size() >= 2)
         {
            float stemFraction = divide (dmGreen[STEM], dmGreen[STEM] + dmGreen[LEAF], 0.0);
//            float stemStubble = 150.0 * stemFraction; // parameter stubble = 150
//            float leafStubble = 150.0 - stemStubble; // parameter stubble = 150
            float stemStubble = 0.0 * stemFraction; // parameter stubble = 150
            float leafStubble = 0.0 - stemStubble; // parameter stubble = 150
            float dmStem = dmGreen[STEM]*g2kg/sm2ha - stemStubble;
            float dmLeaf =  dmGreen[LEAF]*g2kg/sm2ha - leafStubble;
            dm.green.stem = max(0.0, dmStem);
            dm.green.leaf = max(0.0, dmLeaf);
         }
         else
         {
            throw std::runtime_error("Couldn't unpack dmGreen");
         }
      }
      else
      {
         throw std::runtime_error("Couldn't get variable dmGreenID");
      }
}

void ScienceConverterComponent::getDmSenesced(PlantPool &dm, vector<float>  &dmSenesced)
{
      protocol::Variant* variant;
       bool ok = getVariable(dmSenescedID, variant, true);
      if (ok)
      {
         bool ok = variant->unpack(dmSenesced);
         if (ok && dmSenesced.size() >= 2)
         {
            dm.senesced.stem = dmSenesced[STEM]*g2kg/sm2ha;    // needs to be fixed to take account of stubble.
            dm.senesced.leaf = dmSenesced[LEAF]*g2kg/sm2ha;    // needs to be fixed to take account of stubble.
         }
         else
         {
            throw std::runtime_error("Couldn't unpack dmSenesced");
         }
      }
      else
      {
         throw std::runtime_error("Couldn't get variable dmSenescedID");
      }
}

void ScienceConverterComponent::getDmDead(PlantPool &dm, vector<float>  &dmDead)
{
      protocol::Variant* variant;
      bool ok = getVariable(dmDeadID, variant, true);
      if (ok)
      {
         bool ok = variant->unpack(dmDead);
         if (ok && dmDead.size() >= 2)
         {
            dm.dead.stem = dmDead[STEM]*g2kg/sm2ha;
            dm.dead.leaf = dmDead[LEAF]*g2kg/sm2ha;
         }
         else
         {
            throw std::runtime_error("Couldn't unpack dmDead");
         }
      }
      else
      {
         throw std::runtime_error("Couldn't get variable dmDeadID");
      }
}

void ScienceConverterComponent::getNGreen(PlantPool &N, vector<float>  &nGreen, PlantPool &dm, vector<float>  &dmGreen)
{
      protocol::Variant* variant;
      bool ok = getVariable(nGreenID, variant, true);
      if (ok)
      {
         bool ok = variant->unpack(nGreen);
         if (ok && nGreen.size() >= 2)
         {

            float nConcStem = divide (nGreen[STEM], dmGreen[STEM], 0.0);
            float nConcLeaf = divide (nGreen[LEAF], dmGreen[LEAF], 0.0);
            N.green.stem = nConcStem * dm.green.stem;
            N.green.leaf = nConcLeaf * dm.green.leaf;
         }
         else
         {
            throw std::runtime_error("Couldn't unpack dmGreen");
         }
      }
      else
      {
         throw std::runtime_error("Couldn't get variable dmGreenID");
      }
}

void ScienceConverterComponent::getNSenesced(PlantPool &N, vector<float>  &nSenesced, PlantPool &dm, vector<float>  &dmSenesced)
{
      protocol::Variant* variant;
         bool ok = getVariable(nSenescedID, variant, true);
         if (ok)
         {
            bool ok = variant->unpack(nSenesced);
            if (ok && nSenesced.size() >= 2)
            {

               float nConcStem = divide (nSenesced[STEM], dmSenesced[STEM], 0.0);
               float nConcLeaf = divide (nSenesced[LEAF], dmSenesced[LEAF], 0.0);
               N.senesced.stem = nConcStem * dm.senesced.stem;
               N.senesced.leaf = nConcLeaf * dm.senesced.leaf;
            }
            else
            {
               throw std::runtime_error("Couldn't unpack nSenesced");
            }
         }
         else
         {
            throw std::runtime_error("Couldn't get variable nSenescedID");
         }
}

void ScienceConverterComponent::getNDead(PlantPool &N, vector<float>  &nDead, PlantPool &dm, vector<float>  &dmDead)
{
      protocol::Variant* variant;
         bool ok = getVariable(nDeadID, variant, true);
         if (ok)
         {
            bool ok = variant->unpack(nDead);
            if (ok && nDead.size() >= 2)
            {

               float nConcStem = divide (nDead[STEM], dmDead[STEM], 0.0);
               float nConcLeaf = divide (nDead[LEAF], dmDead[LEAF], 0.0);
               N.dead.stem = nConcStem * dm.dead.stem;
               N.dead.leaf = nConcLeaf * dm.dead.leaf;
            }
            else
            {
               throw std::runtime_error("Couldn't unpack nDead");
            }
         }
         else
         {
            throw std::runtime_error("Couldn't get variable nDeadID");
         }
}

void ScienceConverterComponent::getPGreen(PlantPool &P, vector<float>  &pGreen, PlantPool &dm, vector<float>  &dmGreen)
{
      protocol::Variant* variant;
         bool ok = getVariable(pGreenID, variant, true);
         if (ok)
         {
            bool ok = variant->unpack(pGreen);
            if (ok && pGreen.size() >= 2)
            {

               float pConcStem = divide (pGreen[STEM], dmGreen[STEM], 0.0);
               float pConcLeaf = divide (pGreen[LEAF], dmGreen[LEAF], 0.0);

               if (pConcStem && pConcLeaf > 0.0)
               {
                  P.green.stem = pConcStem * dm.green.stem;
                  P.green.leaf = pConcLeaf * dm.green.leaf;
               }
               else
               {
                  P.green.stem = c.pConcGreenStemDefault * dm.green.stem;  // parameter for default P contents
                  P.green.leaf = c.pConcGreenStemDefault * dm.green.leaf;
               }
            }
            else
            {
               throw std::runtime_error("Couldn't unpack nGreen");
           }
         }
         else
         {
            throw std::runtime_error("Couldn't get variable nGreenID");
         }
}

void ScienceConverterComponent::getPSenesced(PlantPool &P, vector<float>  &pSenesced, PlantPool &dm, vector<float>  &dmSenesced)
{
      protocol::Variant* variant;
         bool ok = getVariable(pSenescedID, variant, true);
         if (ok)
         {
            bool ok = variant->unpack(pSenesced);
            if (ok && pSenesced.size() >= 2)
            {

               float pConcStem = divide (pSenesced[STEM], dmSenesced[STEM], 0.0);
               float pConcLeaf = divide (pSenesced[LEAF], dmSenesced[LEAF], 0.0);

               if (pConcStem && pConcLeaf > 0.0)
               {
                  P.senesced.stem = pConcStem * dm.senesced.stem;
                  P.senesced.leaf = pConcLeaf * dm.senesced.leaf;
               }
               else
               {
                  P.senesced.stem = c.pConcSenescedStemDefault * dm.senesced.stem;  // parameter for default P contents
                  P.senesced.leaf = c.pConcSenescedLeafDefault * dm.senesced.leaf;
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

void ScienceConverterComponent::getPDead(PlantPool &P, vector<float>  &pDead, PlantPool &dm, vector<float>  &dmDead)
{
      protocol::Variant* variant;
         bool ok = getVariable(pDeadID, variant, true);
         if (ok)
         {
            bool ok = variant->unpack(pDead);
            if (ok && pDead.size() >= 2)
            {

               float pConcStem = divide (pDead[STEM], dmDead[STEM], 0.0);
               float pConcLeaf = divide (pDead[LEAF], dmDead[LEAF], 0.0);

               if (pConcStem && pConcLeaf > 0.0)
               {
                  P.dead.stem = pConcStem * dm.dead.stem;
                  P.dead.leaf = pConcLeaf * dm.dead.leaf;
               }
               else
               {
                  P.dead.stem = c.pConcDeadStemDefault * dm.dead.stem;  // parameter for default P contents
                  P.dead.leaf = c.pConcDeadLeafDefault * dm.dead.leaf;
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

 void ScienceConverterComponent::getVariables(PlantPool &dm, PlantPool &N, PlantPool &P, float &height)
{
      // Get dm GREEN
      vector<float>  dmGreen;
      getDmGreen(dm, dmGreen);

      // Get dm SENESCED
      vector<float>  dmSenesced;
      getDmSenesced(dm, dmSenesced);

      // Get dm DEAD
      vector<float>  dmDead;
      getDmDead(dm, dmDead);

      if (dm.total() > 0.0)
      {
         // Get N GREEN
         vector<float>  nGreen;
         getNGreen(N, nGreen, dm, dmGreen);

         // Get N SENESCED
         vector<float>  nSenesced;
         getNSenesced(N, nSenesced, dm, dmSenesced);

         // Get N DEAD
         vector<float>  nDead;
         getNDead(N, nDead, dm, dmDead);

         // Get P GREEN
         vector<float>  pGreen;
         getPGreen(P, pGreen, dm, dmGreen);

         // Get P SENESCED
         vector<float>  pSenesced;
         getPSenesced(P, pSenesced, dm, dmSenesced);

         // Get P DEAD
        vector<float>  pDead;
         getPDead(P, pDead, dm, dmDead);

        // Get HEIGHT
//         PlantPool  heightRatio;

         getHeight(height);
      }

}

void ScienceConverterComponent::calcDmdDistribution(PlantPool dmdFraction[])
{
   const int MAX = 0;
   const int AVG = 1;
   const int MIN = 2;
         //plant pools  GL    GS   SL    SS   DL   DS
//      PlantPool dmdMax(c.dmdGreenLeaf, c.dmdMaxGreenStem, c.dmdMaxSenescedLeaf, c.dmdMaxSenescedStem, c.dmdMaxDeadLeaf, c.dmdMaxDeadStem);
      PlantPool dmdMax(c.dmdGreenLeaf[MAX], c.dmdGreenStem[MAX], c.dmdSenescedLeaf[MAX], c.dmdSenescedStem[MAX], c.dmdDeadLeaf[MAX], c.dmdDeadStem[MAX]);
      PlantPool dmdAvg(c.dmdGreenLeaf[AVG], c.dmdGreenStem[AVG], c.dmdSenescedLeaf[AVG], c.dmdSenescedStem[AVG], c.dmdDeadLeaf[AVG], c.dmdDeadStem[AVG]);
      PlantPool dmdMin(c.dmdGreenLeaf[MIN], c.dmdGreenStem[MIN], c.dmdSenescedLeaf[MIN], c.dmdSenescedStem[MIN], c.dmdDeadLeaf[MIN], c.dmdDeadStem[MIN]);

//      PlantPool dmdMax(0.80, 0.60, 0.6, 0.5, 0.4, 0.3);
//      PlantPool dmdAvg(0.65, 0.55, 0.5, 0.35, 0.4, 0.3);
//      PlantPool dmdMin(0.60, 0.50, 0.4, 0.3, 0.4, 0.3);

//      PlantPool dmdAvg(0.7, 0.55, 0.4, 0.4, 0.3, 0.3);
//      PlantPool dmdMax(0.7, 0.55, 0.4, 0.4, 0.3, 0.3);
//      PlantPool dmdMin(0.7, 0.55, 0.4, 0.4, 0.3, 0.3);


      float fraction[maxDmdPools];

// get GREEN Leaf dmd fractions
      for (int pool = 0; pool < c.numDmdPools; pool++) fraction[pool] = 0.0;
      proportion (dmdAvg.green.leaf, dmdMax.green.leaf, dmdMin.green.leaf, fraction);
      for (int pool = 0; pool < c.numDmdPools; pool++) dmdFraction[pool].green.leaf = fraction[pool];

// get GREEN stem dmd fractions
      for (int pool = 0; pool < c.numDmdPools; pool++) fraction[pool] = 0.0;
      proportion (dmdAvg.green.stem, dmdMax.green.stem, dmdMin.green.stem, fraction);
      for (int pool = 0; pool < c.numDmdPools; pool++) dmdFraction[pool].green.stem = fraction[pool];

// get SENESCED Leaf dmd fractions
      for (int pool = 0; pool < c.numDmdPools; pool++) fraction[pool] = 0.0;
      proportion (dmdAvg.senesced.leaf, dmdMax.senesced.leaf, dmdMin.senesced.leaf, fraction);
      for (int pool = 0; pool < c.numDmdPools; pool++) dmdFraction[pool].senesced.leaf = fraction[pool];

// get SENESCED stem dmd fractions
      for (int pool = 0; pool < c.numDmdPools; pool++) fraction[pool] = 0.0;
      proportion (dmdAvg.senesced.stem, dmdMax.senesced.stem, dmdMin.senesced.stem, fraction);
      for (int pool = 0; pool < c.numDmdPools; pool++) dmdFraction[pool].senesced.stem = fraction[pool];

// get DEAD Leaf dmd fractions
      for (int pool = 0; pool < c.numDmdPools; pool++) fraction[pool] = 0.0;
      proportion (dmdAvg.dead.leaf, dmdMax.dead.leaf, dmdMin.dead.leaf, fraction);
      for (int pool = 0; pool < c.numDmdPools; pool++) dmdFraction[pool].dead.leaf = fraction[pool];

// get DEAD Stem dmd fractions
      for (int pool = 0; pool < c.numDmdPools; pool++) fraction[pool] = 0.0;
      proportion (dmdAvg.dead.stem, dmdMax.dead.stem, dmdMin.dead.stem, fraction);
      for (int pool = 0; pool < c.numDmdPools; pool++) dmdFraction[pool].dead.stem = fraction[pool];
}

void ScienceConverterComponent::sendPlant2Stock(protocol::QueryValueData& queryData)
{
      protocol::herbageType herbage;
      PlantPool dm;
      PlantPool N;
      PlantPool P;
      float  height;

      getVariables(dm, N, P, height);

      if (dm.total() > 0.0)
      {

// Now PREPARE herbage
         feed.herbage.erase(feed.herbage.begin(), feed.herbage.end());

// distribute herbage

      PlantPool dmdFraction[maxDmdPools];

      calcDmdDistribution(dmdFraction);

// LEAF  - GREEN
      PlantPool poolDm;
      PlantPool poolN;
      PlantPool poolP;
      PlantPool poolS;
      PlantPool poolAA;
      PlantPool partAshAlk(c.AshAlkGreenLeafDefault, c.AshAlkGreenStemDefault, c.AshAlkSenescedLeafDefault, c.AshAlkSenescedStemDefault, c.AshAlkDeadLeafDefault, c.AshAlkDeadStemDefault);
      PlantPool NSRatio(c.NSRatioGreenLeafDefault, c.NSRatioGreenStemDefault, c.NSRatioSenescedLeafDefault, c.NSRatioSenescedStemDefault, c.NSRatioDeadLeafDefault, c.NSRatioDeadStemDefault);
      PlantPool NPRatio(c.NPRatioGreenLeafDefault,  c.NPRatioGreenStemDefault,  c.NPRatioSenescedLeafDefault,  c.NPRatioSenescedStemDefault,  c.NPRatioDeadStemDefault,  c.NPRatioDeadStemDefault);

//      PlantPool partAshAlk(254.0, 96.0, 254.0, 96.0, 254.0, 96.0);
//      PlantPool NSRatio(19.0, 11.0, 19.0, 11.0, 19.0, 11.0);
//      PlantPool NPRatio( 8.0,  8.0,  8.0,  8.0,  8.0,  8.0);


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

      }
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

    readParameter (c.herbageModuleName.c_str(), "cp_n_ratio", c.cpNRatio, 0.0, 10.0);

//      PlantPool partAshAlk(254.0, 96.0, 254.0, 96.0, 254.0, 96.0);
//      PlantPool NSRatio(19.0, 11.0, 19.0, 11.0, 19.0, 11.0);
//      PlantPool dmdAvg(0.65, 0.55, 0.4, 0.3, 0.3, 0.3);
//      PlantPool dmdMax(0.70, 0.60, 0.5, 0.3, 0.3, 0.3);
//      PlantPool dmdMin(0.60, 0.50, 0.3, 0.3, 0.3, 0.3);
//parameter cpNRatio = 6.25
//                  P.dead.stem = 0.003 * dm.dead.stem;  // parameter for default P contents
//                  P.dead.leaf = 0.004 * dm.dead.leaf;
//            float stemStubble = 150.0 * stemFraction; // parameter stubble = 150

}




//// STEM - GREEN
////      herbage.dm = dm.green.stem + dm.green.leaf;        // kg/ha
//      herbage.dm = dm.green.stem;        // kg/ha
//
////      partFraction.green.leaf = divide (dm.green.leaf, herbage.dm, 0.0);
////      partFraction.green.stem = divide (dm.green.stem, herbage.dm, 0.0);
//
//      herbage.dmd = 0.5;        // kg/ha
//
//       nConc = divide (N.green.stem, dm.green.stem, 0.0);
//      herbage.cp_conc = nConc * 6.25;   // (kg/ha) - parameter cpNRatio = 6.25
//
//       pConc = divide (P.green.stem, dm.green.stem, 0.0);
//      herbage.p_conc = pConc;
//
//       sConc = divide(N.green.stem/11.0, dm.green.stem, 0.0);  // ash alk to be got from lablab
//      herbage.s_conc = sConc;      //parameter NSRatioLeaf = 19.0, NSRatioStem = 11.0;  herbage.s_conc = 0.0023;  kg/ha
//
//      herbage.prot_dg = herbage.dmd + 0.1;     // parameter ProtDegrade = 0.1;  herbage.prot_dg = 0.7;    // kg/ha
//
//       ashAlk = 96.0*cmol2mol;  // ash alk to be got from lablab
//      herbage.ash_alk = ashAlk;      // herbage.ash_alk = 2.0;    // mol/kg
//
//      herbage.height_ratio = heightRatio.green.stem;   //   herbage.height_ratio = 0.0006;
//
//      feed.herbage.push_back(herbage);
//
//         ostrstream msg0;
//         msg0 << endl << "Herbage on offer, pool 2:-" << endl
//             << "   dm           = " <<              herbage.dm <<      " (kg/ha)" << endl
//             << "   dmd          = " <<              herbage.dmd <<     " (-)" <<     endl
//             << "   cp_conc      = " <<              herbage.cp_conc << " (kg/kg)" << endl
//             << "   s_conc       = " <<              herbage.s_conc <<  " (kg/kg)" << endl
//             << "   prot_dg      = " <<              herbage.prot_dg << " (kg/kg)" << endl
//             << "   ash_alk      = " <<              herbage.ash_alk << " (mol/kg" << endl
//             << "   height_ratio = " <<              herbage.height_ratio << " (-)" << ends;
//         writeString (msg0.str());
//
//// LEAF + STEM  - SENESCED
//      herbage.dm = dm.senesced.stem + dm.senesced.leaf;        // kg/ha
//      partFraction.senesced.leaf = divide (dm.senesced.leaf, herbage.dm, 0.0);
//      partFraction.senesced.stem = divide (dm.senesced.stem, herbage.dm, 0.0);
//
//      herbage.dmd = 0.4;        // kg/ha
//
//       nConc = divide (N.senesced.stem + N.senesced.leaf, dm.senesced.stem + dm.senesced.leaf, 0.0);
//      herbage.cp_conc = nConc * 6.25;   // (kg/ha) - parameter cpNRatio = 6.25
//
//       pConc = divide (P.senesced.stem + P.senesced.leaf, dm.senesced.stem + dm.senesced.leaf, 0.0);
//      herbage.p_conc = pConc;
//
//       sConc = divide(N.senesced.leaf/19.0 + N.senesced.stem/11.0, dm.senesced.leaf + dm.senesced.stem, 0.0);  // ash alk to be got from lablab
//      herbage.s_conc = sConc;      //parameter NSRatioLeaf = 19.0, NSRatioStem = 11.0;  herbage.s_conc = 0.0023;  kg/ha
//
//     herbage.prot_dg = herbage.dmd + 0.1;     // parameter ProtDegrade = 0.1;  herbage.prot_dg = 0.7;    // kg/ha
//
//       ashAlk = divide(254.0*cmol2mol*dm.senesced.leaf + 96.0*cmol2mol*dm.senesced.stem, dm.senesced.leaf + dm.senesced.stem, 0.0);  // ash alk to be got from lablab
//      herbage.ash_alk = ashAlk;      // herbage.ash_alk = 2.0;    // mol/kg
//
//      herbage.height_ratio = heightRatio.senesced.leaf + heightRatio.senesced.stem;   //   herbage.height_ratio = 0.0006;
//
//      feed.herbage.push_back(herbage);
//
//         ostrstream msg1;
//         msg1 << endl << "Herbage on offer, pool 3:-" << endl
//             << "   dm           = " <<              herbage.dm <<      " (kg/ha)" << endl
//             << "   dmd          = " <<              herbage.dmd <<     " (-)" <<     endl
//             << "   cp_conc      = " <<              herbage.cp_conc << " (kg/kg)" << endl
//             << "   s_conc       = " <<              herbage.s_conc <<  " (kg/kg)" << endl
//             << "   prot_dg      = " <<              herbage.prot_dg << " (kg/kg)" << endl
//             << "   ash_alk      = " <<              herbage.ash_alk << " (mol/kg" << endl
//             << "   height_ratio = " <<              herbage.height_ratio << " (-)" << ends;
//         writeString (msg1.str());
//
//
//// LEAF + STEM  - DEAD
//      herbage.dm = dm.dead.stem + dm.dead.leaf;        // kg/ha
//      partFraction.dead.leaf = divide (dm.dead.leaf, herbage.dm, 0.0);
//      partFraction.dead.stem = divide (dm.dead.stem, herbage.dm, 0.0);
//
//      herbage.dmd = 0.3;        // kg/ha
//
//       nConc = divide (N.dead.stem + N.dead.leaf, dm.dead.stem + dm.dead.leaf, 0.0);
//      herbage.cp_conc = nConc * 6.25;   // (kg/ha) - parameter cpNRatio = 6.25
//
//       pConc = divide (P.dead.stem + P.dead.leaf, dm.dead.stem + dm.dead.leaf, 0.0);
//      herbage.p_conc = pConc;
//
//       sConc = divide(N.dead.leaf/19.0 + N.dead.stem/11.0, dm.dead.leaf + dm.dead.stem, 0.0);  // ash alk to be got from lablab
//      herbage.s_conc = sConc;      //parameter NSRatioLeaf = 19.0, NSRatioStem = 11.0;  herbage.s_conc = 0.0023;  kg/ha
//
//      herbage.prot_dg = herbage.dmd + 0.1;     // parameter ProtDegrade = 0.1;  herbage.prot_dg = 0.7;    // kg/ha
//
//       ashAlk = divide(254.0*cmol2mol*dm.dead.leaf + 96.0*cmol2mol*dm.dead.stem, dm.dead.leaf + dm.dead.stem, 0.0);  // ash alk to be got from lablab
//      herbage.ash_alk = ashAlk;      // herbage.ash_alk = 2.0;    // mol/kg
//
//      herbage.height_ratio = heightRatio.dead.leaf + heightRatio.dead.stem;   //   herbage.height_ratio = 0.0006;
//
//      feed.herbage.push_back(herbage);
//
//         ostrstream msg2;
//         msg2 << endl << "Herbage on offer, pool 4:-" << endl
//             << "   dm           = " <<              herbage.dm <<      " (kg/ha)" << endl
//             << "   dmd          = " <<              herbage.dmd <<     " (-)" <<     endl
//             << "   cp_conc      = " <<              herbage.cp_conc << " (kg/kg)" << endl
//             << "   s_conc       = " <<              herbage.s_conc <<  " (kg/kg)" << endl
//             << "   prot_dg      = " <<              herbage.prot_dg << " (kg/kg)" << endl
//             << "   ash_alk      = " <<              herbage.ash_alk << " (mol/kg" << endl
//             << "   height_ratio = " <<              herbage.height_ratio << " (-)" << ends;
//         writeString (msg2.str());
//



